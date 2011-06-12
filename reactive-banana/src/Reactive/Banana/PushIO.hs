{-----------------------------------------------------------------------------
    Reactive Banana
    
    A push-driven implementation
------------------------------------------------------------------------------}
{-# LANGUAGE TypeFamilies, FlexibleInstances, EmptyDataDecls, GADTs,
     TupleSections, BangPatterns #-}
module Reactive.Banana.PushIO where

import Reactive.Banana.Model hiding (Event, Behavior, interpret)
import qualified Reactive.Banana.Model as Model

import Reactive.Banana.TypedDict (Dict)
import qualified Reactive.Banana.TypedDict as Dict


import Control.Applicative
import qualified Data.List
import Prelude hiding (filter)
import Data.Monoid

import Control.Monad.Trans.Identity
import Control.Monad.State
import Control.Monad.Writer

import Data.IORef
import System.IO.Unsafe
import Data.Dynamic

{-----------------------------------------------------------------------------
    Observable sharing
    
    References can be used in the  Store  monad.
    This mimicks the case where unique IDs are used
    to look up a value in the environment.
    In this case, the environment is passed around by the  Store  monad.
------------------------------------------------------------------------------}
-- store monad
type Store = IO
-- references to observe sharing
type Ref a = IORef (Maybe a)

runStore :: Store a -> IO a
runStore = id

-- create a new reference.
newRef   :: Store (Ref a)
-- read a reference. Only possible in the  Store  monad.
readRef  :: Ref a -> Store (Maybe a)
writeRef :: Ref a -> a -> Store ()

newRef   = newIORef Nothing
readRef  = readIORef
writeRef ref = writeIORef ref . Just

-- invalid reference that may not store values
invalidRef = error "Store: invalidRef. This is an internal bug."

{-----------------------------------------------------------------------------
    Cache
------------------------------------------------------------------------------}
-- A cache stores values of different types
-- and finalizers to change them.
data Cache = Cache { dict :: Dict, finalizers :: [Finalizer] }
type Finalizer = Dict -> IO Dict

emptyCache = Cache Dict.empty []

-- monad to build the network in
type Compile = StateT Cache Store
-- monad to run the network in
type Run     = StateT Cache IO

runCompile :: Compile a -> Store (a, Cache)
runCompile m = runStateT m []

registerFinalizer :: Finalizer -> Compile ()
registerFinalizer m = modify $
    \cache -> cache { finalizers = finalizers cache ++ [m] }

runRun :: Run a -> Cache -> IO (a, Cache)
runRun m cache = do
    (x,cache') <- runStateT m cache                 -- run the action
    foldr (>=>) return (finalizers cache') cache'   -- run all the finalizers
    return (x,cache')                               -- return new cache

-- helper functions for reading and writing keys into  dict cache
writeCacheKey ref x = do
    cache <- get
    dict' <- liftIO $ Dict.insert ref x (dict cache)
    put $ cache { dict = dict' }
readCacheKey ref = do
    cache <- get
    liftIO $ Dict.lookup ref (dict cache)


-- A simple value to be cached. Lasts one phase.
type CacheRef a = Dict.Key a

newCacheRef   :: Compile (CacheRef a)
readCacheRef  :: CacheRef a -> Run (Maybe a)
writeCacheRef :: CacheRef a -> a -> Run ()

newCacheRef      = do
    key <- liftIO $ Dict.newKey
    registerFinalizer $ Dict.delete key
    return key
readCacheRef  = readCacheKey
writeCacheRef = writeCacheKey

-- Accumulation values.
-- Cache a value over several phases
type AccumRef a = Dict.Key a

newAccumRef   :: a -> Compile (AccumRef a)
updateAccum   :: AccumRef a -> (a -> a) -> Run a

newAccumRef x     = do
    ref   <- liftIO $ Dict.newKey
    writeCacheKey ref x
updateAccum ref f = do
    Just x <- readCacheKey ref 
    let !y = f x
    writeCacheKey ref y
    return y

-- behaviors
-- Cache a value over several phases,
-- but updates are only visible at the beginning of a new phase.
type BehaviorRef a = (Dict.Key a, Dict.Key a)

newBehaviorRef    :: a -> Compile (BehaviorRef a)
readBehaviorRef   :: BehaviorRef a -> Run a
updateBehaviorRef :: BehaviorRef a -> (a -> a) -> Run () -- Strict!

newBehaviorRef x = do
    ref  <- liftIO $ Dict.newKey
    temp <- liftIO $ Dict.newKey
    registerFinalizer $ \dict ->
        Just x <- Dict.lookup temp dict
        Dict.insert ref x dict
    return (ref,temp)
readBehaviorRef (ref,temp) = do
    Just x <- readCacheKey ref
    return x
updateBehaviorRef (ref,temp) f = do
    Just x <- readCacheKey temp
    writeCacheKey temp $! f x -- strict!

{-----------------------------------------------------------------------------
    Abstract syntax tree
------------------------------------------------------------------------------}
data Accum
data Shared
data Linear

type EventStore a = [(Channel, CacheRef a)]

type family   Event t a
type instance Event Accum  a = (Ref (EventStore a), EventD Accum a)
type instance Event Shared a = (Ref (EventStore a), EventD Shared a)
type instance Event Linear a = EventD Linear a

data EventD t :: * -> * where
    Filter    :: (a -> Bool) -> Event t a -> EventD t a
    ApplyE    :: Behavior t (a -> b) -> Event t a -> EventD t b
    AccumE    :: a -> Event t (a -> a) -> EventD t a
    Union     :: Event t a -> Event t a -> EventD t a
    Never     :: EventD t a
    
    -- internal combinators
    Input         :: Typeable a => Channel -> EventD t a
    Reactimate    :: Event t (IO ()) -> EventD t ()
    
    ReadCache     :: Channel -> CacheRef a -> EventD t a
    WriteCache    :: CacheRef a -> Event t a -> EventD t a
    
    UpdateAccum   :: AccumRef a -> Event t (a -> a) -> EventD t a
    WriteBehavior :: BehaviorRef a -> Event t (a -> a) -> EventD t ()


type BehaviorStore a = BehaviorRef a

type family   Behavior t a
type instance Behavior Accum  a = (Ref (BehaviorStore a), BehaviorD Accum a)
type instance Behavior Shared a = (Ref (BehaviorStore a), BehaviorD Linear a)
type instance Behavior Linear a = (Ref (BehaviorStore a), BehaviorD Linear a)

data BehaviorD t a where
    Pure         :: a -> BehaviorD t a
    ApplyB       :: Behavior t (a -> b) -> Behavior t a -> BehaviorD t b
    AccumB       :: a -> Event t (a -> a) -> BehaviorD t a
    
    -- internal combinators
    ReadBehavior :: BehaviorRef a -> BehaviorD t a

{-----------------------------------------------------------------------------
    Dynamic types for input
------------------------------------------------------------------------------}
type Channel  = Integer
type Universe = (Channel, Dynamic)

fromUniverse :: Typeable a => Channel -> Universe -> Maybe a
fromUniverse i (j,x) = if i == j then fromDynamic x else Nothing

toUniverse :: Typeable a => Channel -> a -> Universe
toUniverse i x = (i, toDyn x)

{-----------------------------------------------------------------------------
    Compilation
------------------------------------------------------------------------------}
-- replace every occurence of  accumB  with reading from a cached event
type CompileAccumB = WriterT [Event Shared ()] Compile

compileAccumB :: Event Accum () -> Compile (Event Shared ())
compileAccumB e1 = do
        (e,es) <- runWriterT (goE e1)
        -- include updates to Behavior as additional events
        return $ foldr1 union (e:es)
    where
    union e1 e2 = (invalidRef, Union e1 e2)
    
    -- boilerplate traversal for events
    goE :: Event Accum a -> CompileAccumB (Event Shared a)
    goE (ref, Filter p e )  = (ref,) <$> (Filter p   <$> goE e)
    goE (ref, Union e1 e2)  = (ref,) <$> (Union      <$> goE e1 <*> goE e2)
    goE (ref, ApplyE b e )  = (ref,) <$> (ApplyE     <$> goB b  <*> goE e )
    goE (ref, AccumE x e )  = (ref,) <$> (AccumE x   <$> goE e)
    goE (ref, Reactimate e) = (ref,) <$> (Reactimate <$> goE e)
    goE (ref, Never)        = (ref,) <$> (pure Never)
    goE (ref, Input c)      = (ref,) <$> (pure $ Input c)
    
    -- almost boilerplate traversal for behaviors
    goB :: Behavior Accum a -> CompileAccumB (Behavior Shared a)
    goB (ref, Pure x      ) = (ref,) <$> (Pure   <$> return x)
    goB (ref, ApplyB bf bx) = (ref,) <$> (ApplyB <$> goB bf <*> goB bx)
    goB (ref, AccumB x e  ) = (ref,) <$> (ReadBehavior <$> makeRef)
        where
        makeRef = do
            m <- lift . lift $ readRef ref
            case m of
                Just r  -> return r
                Nothing -> do
                    r <- lift $ newBehaviorRef x
                    -- immedately store the cached reference
                    lift . lift $ writeRef ref r
                    -- remove  accumB  from the other events
                    e <- goE e
                    tell [(invalidRef, WriteBehavior r e)]
                    return r


-- fan out unions into linear paths
type EventLinear a = (Channel, Event Linear a)

compileUnion :: Event Shared a -> Compile [Event Linear a]
compileUnion e = map snd <$> goE e
    where
    goE :: Event Shared a -> Compile [EventLinear a]
    goE (ref, Filter p e )       = cacheEvents ref (map2 (Filter p) <$> goE e)
    goE (ref, ApplyE b e )       = cacheEvents ref (map2 (ApplyE b) <$> goE e)
    goE (ref, AccumE x e )       = cacheEvents ref (compileAccumE x =<< goE e)
    goE (_  , WriteBehavior b e) = map2 (WriteBehavior b) <$> goE e
    goE (_  , Reactimate e)      = map2 (Reactimate)      <$> goE e
    goE (_  , Union e1 e2)       = (++) <$> goE e1 <*> goE e2
    goE (_  , Never      )       = return []
    goE (_  , Input channel)     = return [(channel, Input channel)]
    
    compileAccumE :: a -> [EventLinear (a -> a)] -> Compile [EventLinear a]
    compileAccumE x es = do
        ref <- newAccumRef x
        return $ map2 (UpdateAccum ref) es
    
    cacheEvents :: Ref (EventStore a)
                -> Compile [EventLinear a] -> Compile [EventLinear a]
    cacheEvents ref mes = do
        m <- lift $ readRef ref
        case m of
            Just cached -> do
                return $ map (\(c,r) -> (c,ReadCache c r)) cached
            Nothing     -> do
                -- compile input events
                es     <- mes
                -- allocate corresponding cache references
                cached <- forM es $ \(c,_) -> do r <- newCacheRef; return (c,r)
                lift $ writeRef ref cached
                -- return events that also write to the cache
                return $ zipWith (second . (WriteCache . snd)) cached es

second f (a,b) = (a, f b)
map2 = map . second

-- compile a behavior
-- FIXME: take care of sharing, caching
compileBehavior :: Behavior Linear a -> Run a
compileBehavior = goB
    where
    goB :: Behavior Linear a -> Run a
    goB (ref, Pure x)            = return x
    goB (ref, ApplyB bf bx)      = goB bf <*> goB bx
    goB (ref, ReadBehavior refb) = readBehaviorRef refb


-- compile path into an IO action
type Path = (Channel, Universe -> Run ())

compilePath :: Event Linear () -> Path
compilePath e = goE e return
    where
    goE :: Event Linear a -> (a -> Run ()) -> (Channel, Universe -> Run ())
    goE (Filter p e)        k = goE e $ \x -> when (p x) (k x)
    goE (ApplyE b e)        k = goE e $ \x -> goB b >>= \f -> k (f x)
    goE (UpdateAccum ref e) k = goE e $ \f -> updateAccum ref f >>= k
    goE (WriteBehavior b e) _ = goE e $ \x -> updateBehaviorRef b x
        -- note: no  k  here because writing behaviors is the end of a path
    goE (Reactimate e)      _ = goE e $ \x -> liftIO x
    goE (ReadCache c ref)   k =
            (c, \_ -> readCacheRef ref >>= maybe (return ()) k)
    goE (WriteCache ref e)  k = goE e $ \x -> writeCacheRef ref x >> k x
    goE (Input channel)     k =
            (channel, maybe (error "wrong channel") k . fromUniverse channel)
    
    goB :: Behavior Linear a -> Run a
    goB = compileBehavior

-- compilation function
compile :: Event Accum () -> IO ([Path], Cache)
compile e = runStore $ runCompile $
    return . map compilePath =<< compileUnion =<< compileAccumB e

-- debug :: MonadIO m => String -> m ()
-- debug = liftIO . putStrLn

{-----------------------------------------------------------------------------
    Class instances
------------------------------------------------------------------------------}
data PushIO

-- type Behavior = Model.Behavior PushIO
newtype instance Model.Behavior PushIO a = Behavior (Behavior Accum a)

-- type Event = Model.Event PushIO
newtype instance Model.Event PushIO a = Event (Event Accum a)

unEvent (Event e) = e

-- sharing
behavior :: BehaviorD Accum a -> Model.Behavior PushIO a
behavior b = Behavior pair
    where
    {-# NOINLINE pair #-}
    -- mention argument to prevent let-floating  
    pair = unsafePerformIO (fmap (,b) newRef)

event :: EventD Accum a -> Model.Event PushIO a
event e = Event pair
    where
    {-# NOINLINE pair #-}
    -- mention argument to prevent let-floating
    pair = unsafePerformIO (fmap (,e) newRef)

-- boilerplate class instances
instance Functor (Model.Event PushIO) where
    fmap f e = apply (pure f) e
    
instance Applicative (Model.Behavior PushIO) where
    pure x = behavior $ Pure x
    (Behavior bf) <*> (Behavior bx) = behavior $ ApplyB bf bx

instance Functor (Model.Behavior PushIO) where
    fmap = liftA

instance FRP PushIO where
    never = event $ Never
    union (Event e1) (Event e2) = event $ Union e1 e2
    filter p (Event e) = event $ Filter p e
    apply (Behavior bf) (Event ex) = event $ ApplyE bf ex
    accumB x (Event e) = behavior $ AccumB x e
    accumE x (Event e) = event $ AccumE x e


