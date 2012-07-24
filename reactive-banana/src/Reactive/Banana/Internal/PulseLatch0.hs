{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types, RecursiveDo, ExistentialQuantification,
    TypeSynonymInstances #-}
module Reactive.Banana.Internal.PulseLatch0 where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.RWS
import Control.Monad.IO.Class

import Data.Monoid (Endo(..))

import Reactive.Banana.Internal.Cached

import Data.Hashable
import Data.Unique.Really
import qualified Data.Vault as Vault
import qualified Data.HashMap.Lazy as Map

type Map = Map.HashMap

{-----------------------------------------------------------------------------
    Graph data type
------------------------------------------------------------------------------}
data Graph = Graph
    { grPulse  :: Values                    -- pulse values
    , grLatch  :: Values                    -- latch values
    
    , grCache  :: Values                    -- cache for initialization
    , grDeps   :: Map SomeNode [SomeNode]   -- dependency information
    }

instance HasVault Network where
    retrieve key = Vault.lookup key . grCache <$> get
    write key a  = modify $ \g -> g { grCache = Vault.insert key a (grCache g) }

type Values = Vault.Vault
type Key    = Vault.Key

emptyGraph :: Graph
emptyGraph = Graph
    { grPulse  = Vault.empty
    , grLatch  = Vault.empty
    , grCache  = Vault.empty
    , grDeps   = Map.empty
    }

{-----------------------------------------------------------------------------
    Graph evaluation
------------------------------------------------------------------------------}
step :: Pulse a -> Graph -> IO (Maybe a, Graph)
step = undefined
    -- collect demands, chase dependencies
    -- perform evaluations in the right order
    -- read output value

{-----------------------------------------------------------------------------
    Network monad
------------------------------------------------------------------------------}
-- reader / writer / state monad
type Network = RWST Graph (Endo Graph) Graph IO

-- change a graph "atomically"
runNetworkAtomic :: Network a -> Graph -> IO (a, Graph)
runNetworkAtomic m g1 = mdo
    (x, g2, w2) <- runRWST m g3 g1  -- apply early graph gransformations
    let g3 = appEndo w2 g2          -- apply late  graph transformations
    return (x, g3)
    
-- write pulse value immediately
writePulse :: Key (Maybe a) -> Maybe a -> Network ()
writePulse key x =
    modify $ \g -> g { grPulse = Vault.insert key x $ grPulse g }

-- read pulse value immediately
readPulse :: Key (Maybe a) -> Network (Maybe a)
readPulse key = (join . Vault.lookup key . grPulse) <$> get

-- write latch value for later
writeLatch :: Key a -> a -> Network ()
writeLatch key x =
    tell $ Endo $ \g -> g { grLatch = Vault.insert key x $ grLatch g }

-- read latch value immediately
readLatch :: Key a -> Network a
readLatch key = (maybe err id . Vault.lookup key . grLatch) <$> get
    where err = error "readLatch: latch not initialized!"

-- read future latch value
-- warning: forcing the value early will likely result in an infinite loop
readLatchFuture :: Key a -> Network a
readLatchFuture key = (maybe err id . Vault.lookup key . grLatch) <$> ask
    where err = error "readLatchFuture: latch not found!"

-- add a dependency
dependOn :: SomeNode -> SomeNode -> Network ()
dependOn x (P y) = -- dependency on a pulse is added directly
    modify $ \g -> g { grDeps = Map.insertWith (++) x [P y] $ grDeps g }
dependOn x (L y) = -- dependcy on a latch breaks the vicious cycle
    undefined

dependOns :: SomeNode -> [SomeNode] -> Network ()
dependOns x = mapM_ $ dependOn x

{-----------------------------------------------------------------------------
    Pulse and Latch types
------------------------------------------------------------------------------}
{-
    evaluateL/P
        calculates the next value and makes sure that it's cached
    valueL/P
        retrieves the current value
-}

data Pulse a = Pulse
    { evaluateP :: Network ()
    , valueP    :: Network (Maybe a)
    , uidP      :: Unique
    }

data Latch a = Latch
    { evaluateL :: Network ()
    , valueL    :: Network a
    , uidL      :: Unique
    }

-- make pulse from evaluation function
pulse :: Network (Maybe a) -> Network (Pulse a)
pulse eval = do
    key <- liftIO Vault.newKey
    uid <- liftIO newUnique
    return $ Pulse
        { evaluateP = writePulse key =<< eval
        , valueP    = readPulse key
        , uidP      = uid
        }

neverP :: Network (Pulse a)
neverP = do
    uid <- liftIO newUnique
    return $ Pulse
        { evaluateP = return ()
        , valueP    = return Nothing
        , uidP      = uid
        }

-- make latch from initial value and evaluation function
latch :: a -> Network (Maybe a) -> Network (Latch a)
latch a eval = do
    key <- liftIO Vault.newKey
    uid <- liftIO newUnique
    writeLatch key a    -- initialize latch value
    return $ Latch
        { evaluateL = maybe (return ()) (writeLatch key) =<< eval
        , valueL    = readLatch key
        , uidL      = uid
        }

data SomeNode = forall a. P (Pulse a) | forall a. L (Latch a)

instance Eq SomeNode where
    (L x) == (L y)  =  uidL x == uidL y
    (P x) == (P y)  =  uidP x == uidP y
    _     == _      =  False

instance Hashable SomeNode where
    hashWithSalt s (P p) = hashWithSalt s $ uidP p
    hashWithSalt s (L l) = hashWithSalt s $ uidL l

{-----------------------------------------------------------------------------
    Combinators - basic
------------------------------------------------------------------------------}
stepperL :: a -> Pulse a -> Network (Latch a)
stepperL a p = do
    x <- latch a (valueP p)
    (L x) `dependOn` (P p)
    return x

accumP :: a -> Pulse (a -> a) -> Network (Pulse a)
accumP a p = mdo
        x       <- stepperL a result
        result  <- pulse $ eval <$> valueL x <*> valueP p
        P result `dependOns` [L x, P p]
        return result
    where
    eval a Nothing  = Nothing
    eval a (Just f) = Just (f a)

mapP :: (a -> b) -> Pulse a -> Network (Pulse b)
mapP f p = do
    result <- pulse $ fmap f <$> valueP p
    P result `dependOn` P p
    return result

unionWith :: (a -> a -> a) -> Pulse a -> Pulse a -> Network (Pulse a)
unionWith f px py = do
        result <- pulse $ eval <$> valueP px <*> valueP py
        P result `dependOns` [P px, P py]
        return result
    where
    eval (Just x) (Just y) = Just (f x y)
    eval (Just x) Nothing  = Just x
    eval Nothing  (Just y) = Just y
    eval Nothing  Nothing  = Nothing

{-----------------------------------------------------------------------------
    Combinators - dynamic event switching
------------------------------------------------------------------------------}
observeP :: Pulse (Network (Maybe a)) -> Network (Pulse a)
observeP pn = do
    result <- pulse $ do
        mp <- valueP pn
        case mp of
            Just p  -> p
            Nothing -> return Nothing
    P result `dependOn` P pn
    return result

switchP :: Pulse (Pulse a) -> Network (Pulse a)
switchP pp = mdo
    never <- neverP
    lp    <- stepperL never pp
    let
        eval = do
            newPulse <- valueP pp
            case newPulse of
                Nothing -> return ()
                Just p  -> P result `dependOn` P p  -- check in new pulse
            valueP =<< valueL lp                    -- fetch value from old pulse
    result <- pulse eval
    P result `dependOns` [L lp, P pp]
    return result




