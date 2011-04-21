{-----------------------------------------------------------------------------
    Reactive Banana
    
    A push-driven implementation
------------------------------------------------------------------------------}
{-# LANGUAGE TypeFamilies, FlexibleInstances, EmptyDataDecls,
TupleSections, GADTs, UndecidableInstances #-}
module Reactive.Banana.Implementation (
    module Reactive.Banana.Model,
    run,
    
    -- * Setting up a network of events
    Prepare, prepareEvents, reactimate, liftIO,
    
    -- $EventSource
    EventSource(..), fromEventSource,
    module Data.Dynamic,
    ) where

import Reactive.Banana.Model hiding (Event, Behavior, behavior, run)
import qualified Reactive.Banana.Model as Model

import Control.Applicative
import qualified Data.List
import Prelude hiding (filter)
import Data.List (nub)
import Data.Monoid

import Control.Monad.RWS
import Control.Monad.Trans.Identity
import Control.Monad.State
import Control.Monad.Writer as Monad

import Data.IORef
import System.IO.Unsafe
import Data.Dynamic

-- argl, missing instance
instance MonadIO m => MonadIO (IdentityT m) where
    liftIO = IdentityT . liftIO

instance Monad m => Applicative (StateT s m) where
    pure  = return
    (<*>) = ap

instance (Monad m, Monoid w) => Applicative (WriterT w m) where
    pure  = return
    (<*>) = ap

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

-- create a new reference. Dummy argument to prevent let floating
newRef   :: b -> Ref a
-- read a reference. Only possible in the  Store  monad.
readRef  :: Ref a -> Store (Maybe a)
writeRef :: Ref a -> a -> Store ()

newRef b = unsafePerformIO . seq [b] . newIORef $ Nothing
readRef  = readIORef
writeRef ref = writeIORef ref . Just

-- invalid reference that may not store values
invalidRef = error "Store: invalidRef. This is an internal bug."

{-----------------------------------------------------------------------------
    Cache
------------------------------------------------------------------------------}
-- a cache stores values of different types
-- This is done with IORefs and a list of finalizerss
type Cache = [IO ()]

emptyCache = []

-- FIXME: add initializers to the Cache, so we can use it properly!

-- monad to build the network in
type Compile = StateT Cache Store
-- monad to run the network in
type Run     = IdentityT IO

runCompile :: Compile a -> Store (a, Cache)
runCompile m = runStateT m []

registerFinalizer :: IO () -> Compile ()
registerFinalizer m = modify $ (++[m])

runRun :: Run a -> Cache -> IO (a, Cache)
runRun m cache = do
    x <- runIdentityT m   -- run the action
    sequence_ cache       -- run all the finalizers
    return (x,cache)      -- return dummy argument

-- a simple value to be cached. Lasts one phase.
type CacheRef a = IORef (Maybe a)

newCacheRef   :: Compile (CacheRef a)
readCacheRef  :: CacheRef a -> Run (Maybe a)
writeCacheRef :: CacheRef a -> a -> Run ()

newCacheRef       = do
    ref <- liftIO $ newIORef Nothing
    registerFinalizer $ writeIORef ref Nothing
    return ref

readCacheRef      = liftIO . readIORef
writeCacheRef ref = liftIO . writeIORef ref . Just

-- behaviors
-- Cache a value over several phases,
-- but updates are only visible at the beginning of a new phase.
type BehaviorRef a = (IORef a, IORef a)

newBehaviorRef    :: a -> Compile (BehaviorRef a)
readBehaviorRef   :: BehaviorRef a -> Run a
updateBehaviorRef :: BehaviorRef a -> (a -> a) -> Run () -- Strict!

newBehaviorRef x = do
    ref  <- liftIO $ newIORef x
    temp <- liftIO $ newIORef x
    registerFinalizer $ do
        x <- readIORef temp
        writeIORef ref x
    return (ref,temp)

readBehaviorRef (ref,temp) = liftIO $ readIORef ref

updateBehaviorRef (ref,temp) f = liftIO $ do
    x <- readIORef temp
    writeIORef temp $! f x -- strict!

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
    Union     :: Event t a -> Event t a -> EventD t a
    Never     :: EventD t a
    
    -- internal combinators
    Input         :: Typeable a => Channel -> EventD t a
    Reactimate    :: Event t (IO ()) -> EventD t ()
    ReadCache     :: Channel -> CacheRef a -> EventD t a
    WriteCache    :: CacheRef a -> Event t a -> EventD t a
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
    goE (_  , WriteBehavior b e) = map2 (WriteBehavior b) <$> goE e
    goE (_  , Reactimate e)      = map2 (Reactimate)      <$> goE e
    goE (_  , Union e1 e2)       = (++) <$> goE e1 <*> goE e2
    goE (_  , Never      )       = return []
    goE (_  , Input channel)     = return [(channel, Input channel)]
    
    second f (a,b) = (a, f b)
    map2 = map . second
        
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
    goE (WriteBehavior b e) _ = goE e $ \x -> updateBehaviorRef b x
        -- note: no  k  here because writing behaviors is the end of a path
    goE (Reactimate e)      _ = goE e $ \x -> liftIO x
    goE (ReadCache c ref)   k =
            (c, \_ -> readCacheRef ref >>= maybe (return ()) k)
    goE (WriteCache ref e)  k = goE e $ \x -> writeCacheRef ref x >> k x
    goE (Input channel)     k =
            (channel, maybe (error "wrong channel") k . fromUniverse channel)
    
    goB = compileBehavior

-- compilation function
compile :: Event Accum () -> IO ([Path], Cache)
compile e = runStore $ runCompile $
    return . map compilePath =<< compileUnion =<< compileAccumB e

-- debug :: MonadIO m => String -> m ()
-- debug = liftIO . putStrLn

{-----------------------------------------------------------------------------
    Setting up an event network
------------------------------------------------------------------------------}
type AddHandler   = (Channel, (Universe -> IO ()) -> IO ())
type Preparations = ([Model.Event PushIO (IO ())], [AddHandler])
type Prepare = RWST () Preparations Channel IO

reactimate :: Model.Event PushIO (IO ()) -> Prepare ()
reactimate e = tell ([e], [])

-- | Set up an event network.
prepareEvents :: Prepare () -> IO ()
prepareEvents m = do
    (_,_,(outputs,inputs)) <- runRWST m () 0
    let
        -- union of all  reactimates    
        Event network = mconcat outputs :: Model.Event PushIO (IO ())
    -- compile network
    (paths,cache) <- compile (invalidRef, Reactimate network)
    -- reduce to one path per channel
    let paths1 = groupChannelsBy (\p q x -> p x >> q x) paths

    -- prepare threading the cache as state
    rcache <- newIORef emptyCache
    writeIORef rcache cache
    let run m = do
            cache <- readIORef rcache
            (_,cache') <- runRun m cache
            writeIORef rcache cache'
        paths2 = map (\(i,p) -> (i, run . p)) $ paths1
    
    -- register event handlers
    sequence_ . map snd . applyChannels inputs $ paths2

-- FIXME: make this faster
groupChannelsBy :: (a -> a -> a) -> [(Channel, a)] -> [(Channel, a)]
groupChannelsBy f xs = [(i, foldr1 f [x | (j,x) <- xs, i == j]) | i <- channels]
    where channels = nub . map fst $ xs

-- FIXME: make this faster
applyChannels :: [(Channel, a -> b)] -> [(Channel, a)] -> [(Channel, b)]
applyChannels fs xs =
    [(i, f x) | (i,f) <- fs, (j,x) <- xs, i == j]


{-$EventSource

* Event Sources

    After having read all about 'Event's and 'Behavior's,
    you want to hook things up to an existing event-based framework,
    like @wxHaskell@ or @Gtk2Hs@.
    How do you do that?

    'EventSource's are a small bookkeeping device that helps you with that.
    Basically, they store event handlers.
    Sometimes, you can just obtain them from
    corresponding bookkeeping devices from your framework,
    but often you have to create your own 'EventSource'
    and use the 'fire' function to hook it into the framework.

    After creating an 'EventSource',
    you can finally obtain an 'Event' via the `fromEventSource' function.
-}

-- | An 'EventSource' is just a facility for adding callback functions,
-- aka event handlers.
data EventSource a = EventSource { addHandler :: (a -> IO ()) -> IO () }

-- | Obtain an 'Event' from an 'EventSource'.
-- An event is generated whenever the callback function is called
fromEventSource :: Typeable a => EventSource a -> Prepare (Model.Event PushIO a)
fromEventSource es = do
        channel <- newChannel
        let addHandler' k = addHandler es $ k . toUniverse channel
        tell ([], [(channel, addHandler')])
        return $ event (Input channel) 
    where
    newChannel = do c <- get; put $! c+1; return c

{-----------------------------------------------------------------------------
    Run function for testing
------------------------------------------------------------------------------}
run :: Typeable a
    => (Model.Event PushIO a -> Model.Event PushIO b) -> [a] -> IO [[b]] 
run f xs = do
    oref <- newIORef []
    
    href <- newIORef []
    let addHandler k = modifyIORef href (++[k])

    prepareEvents $ do
        e <- fromEventSource $ EventSource addHandler
        reactimate $ fmap (\b -> modifyIORef oref (++[b])) (f e)
    
    handler <- (\ks x -> mapM ($ x) ks) <$> readIORef href
    
    forM xs $ \x -> do
        handler x
        bs <- readIORef oref
        writeIORef oref []
        return bs
    

{-----------------------------------------------------------------------------
    Class instances
------------------------------------------------------------------------------}
data PushIO

-- type Behavior = Model.Behavior PushIO
newtype instance Model.Behavior PushIO a = Behavior (Behavior Accum a)

-- type Event = Model.Event PushIO
newtype instance Model.Event PushIO a = Event { unEvent :: Event Accum a }


-- sharing
behavior :: BehaviorD Accum a -> Model.Behavior PushIO a
behavior b = Behavior (ref, b)
    where
    {-# NOINLINE ref #-}    
    ref = newRef b

event :: EventD Accum a -> Model.Event PushIO a
event e = Event (ref, e)
    where
    {-# NOINLINE ref #-}
    ref = newRef e

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


