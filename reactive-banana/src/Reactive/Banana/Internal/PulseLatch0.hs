{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types, RecursiveDo, ExistentialQuantification,
    TypeSynonymInstances #-}
module Reactive.Banana.Internal.PulseLatch0 where

import Control.Arrow (second)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.RWS
import Control.Monad.IO.Class

import Data.Monoid (Endo(..))

import Reactive.Banana.Internal.Cached
import Reactive.Banana.Internal.InputOutput
import qualified Reactive.Banana.Internal.DependencyGraph as Deps

import Data.Hashable
import Data.Unique.Really
import qualified Data.Vault as Vault
import qualified Data.HashMap.Lazy as Map

type Map  = Map.HashMap
type Deps = Deps.Deps

{-----------------------------------------------------------------------------
    Graph data type
------------------------------------------------------------------------------}
data Graph = Graph
    { grPulse   :: Values                    -- pulse values
    , grLatch   :: Values                    -- latch values
    
    , grCache   :: Values                    -- cache for initialization

    , grDeps    :: Deps SomeNode             -- dependency information
    , grInputs  :: [Input]                   -- input  nodes
    }

type Values = Vault.Vault
type Key    = Vault.Key
type Input  =
    ( SomeNode
    , InputValue -> Values -> Values         -- write input value into graph
    )

emptyGraph :: Graph
emptyGraph = Graph
    { grPulse  = Vault.empty
    , grLatch  = Vault.empty
    , grCache  = Vault.empty
    , grDeps   = Deps.empty
    , grInputs = []
    }

{-----------------------------------------------------------------------------
    Graph evaluation
------------------------------------------------------------------------------}
step :: Pulse a -> [InputValue] -> Graph -> IO (Maybe a, Graph)
step result inputs =
    uncurry (\nodes -> runNetworkAtomic $ do
        performEvaluation nodes
        readOutputValue result)
    . buildEvaluationOrder
    . writeInputValues inputs

readOutputValue = valueP

writeInputValues inputs g = g { grPulse =
    concatenate [f x | (_,f) <- grInputs g, x <- inputs] Vault.empty }

concatenate :: [a -> a] -> (a -> a)
concatenate = foldr (.) id

performEvaluation = mapM_ evaluate
    where
    evaluate (P p) = evaluateP p
    evaluate (L l) = evaluateL l

-- Figure out which nodes need to be evaluated.
--
-- All nodes that are connected to current input nodes must be evaluated.
-- The other nodes don't have to be evaluated, because they yield
-- Nothing / don't change anyway.
buildEvaluationOrder :: Graph -> ([SomeNode], Graph)
buildEvaluationOrder g = (Deps.topologicalSort $ grDeps g, g)


{-----------------------------------------------------------------------------
    Network monad
------------------------------------------------------------------------------}
-- reader / writer / state monad
type Network = RWST Graph (Endo Graph) Graph IO

-- access initialization cache
instance HasVault Network where
    retrieve key = Vault.lookup key . grCache <$> get
    write key a  = modify $ \g -> g { grCache = Vault.insert key a (grCache g) }

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

-- write latch value immediately
writeLatch :: Key a -> a -> Network ()
writeLatch key x =
    modify $ \g -> g { grLatch = Vault.insert key x $ grLatch g }

-- read latch value immediately
readLatch :: Key a -> Network a
readLatch key = (maybe err id . Vault.lookup key . grLatch) <$> get
    where err = error "readLatch: latch not initialized!"

-- write latch value for future
writeLatchFuture :: Key a -> a -> Network ()
writeLatchFuture key x =
    tell $ Endo $ \g -> g { grLatch = Vault.insert key x $ grLatch g }

-- read future latch value
-- Note [LatchFuture]:
--   warning: forcing the value early will likely result in an infinite loop
readLatchFuture :: Key a -> Network a
readLatchFuture key = (maybe err id . Vault.lookup key . grLatch) <$> ask
    where err = error "readLatchFuture: latch not found!"

-- add a dependency
dependOn :: SomeNode -> SomeNode -> Network ()
dependOn x y = modify $ \g -> g { grDeps = Deps.dependOn x y $ grDeps g }

dependOns :: SomeNode -> [SomeNode] -> Network ()
dependOns x = mapM_ $ dependOn x

-- link a Pulse key to an input channel
addInput :: Key (Maybe a) -> Pulse a -> InputChannel a -> Network ()
addInput key pulse channel =
    modify $ \g -> g { grInputs = (P pulse, input) : grInputs g }
    where
    input value
        | getChannel value == getChannel channel =
            Vault.insert key (fromValue channel value)
        | otherwise = id

{-----------------------------------------------------------------------------
    Pulse and Latch types
------------------------------------------------------------------------------}
{-
    evaluateL/P
        calculates the next value and makes sure that it's cached
    valueL/P
        retrieves the current value
    futureL
        future value of the latch
        see note [LatchFuture]
    uidL/P
        used for dependency tracking and evaluation order
-}

data Pulse a = Pulse
    { evaluateP :: Network ()
    , valueP    :: Network (Maybe a)
    , uidP      :: Unique
    }

data Latch a = Latch
    { evaluateL :: Network ()
    , valueL    :: Network a
    , futureL   :: Network a
    , uidL      :: Unique
    }

{- Note [LatchCreation]

When creating a new latch from a pulse, we assume that the
pulse cannot fire at the moment that the latch is created.
This is important when switching latches, because of note [PulseCreation].

Likewise, when creating a latch, we assume that we do not
have to calculate the previous latch value.

Note [PulseCreation]

We assume that we do not have to calculate a pulse occurrence
at the moment we create the pulse. Otherwise, we would have
to recalculate the dependencies *while* doing evaluation;
this is a recipe for desaster.

-}

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

-- create a pulse that listens to input values
inputP :: InputChannel a -> Network (Pulse a)
inputP channel = do
    key <- liftIO Vault.newKey
    uid <- liftIO newUnique
    
    let pulse = Pulse
            { evaluateP = return ()
            , valueP    = readPulse key
            , uidP      = uid
            }
    addInput key pulse channel
    return pulse

-- make latch from initial value, a future value and evaluation function
latch :: a -> a -> Network (Maybe a) -> Network (Latch a)
latch now future eval = do
    key <- liftIO Vault.newKey
    uid <- liftIO newUnique

    -- Initialize with current and future latch value.
    -- See note [LatchCreation].
    writeLatch key now
    writeLatchFuture key future

    return $ Latch
        { evaluateL = maybe (return ()) (writeLatchFuture key) =<< eval
        , valueL    = readLatch key
        , futureL   = readLatchFuture key
        , uidL      = uid
        }

pureL :: a -> Network (Latch a)
pureL a = do
    uid <- liftIO newUnique
    return $ Latch
        { evaluateL = return ()
        , valueL    = return a
        , futureL   = return a
        , uidL      = uid
        }

{-----------------------------------------------------------------------------
    Existential quantification over Pulse and Latch
    for dependency tracking
------------------------------------------------------------------------------}
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
    -- @a@ is indeed the future latch value. See note [LatchCreation].
    x <- latch a a (valueP p)
    L x `dependOn` P p
    return x

accumP :: a -> Pulse (a -> a) -> Network (Pulse a)
accumP a p = mdo
        x       <- stepperL a result
        result  <- pulse $ eval <$> valueL x <*> valueP p
        -- Evaluation order of the result pulse does *not*
        -- depend on the latch. It does depend on latch value,
        -- though, so don't garbage collect that one.
        P result `dependOn` P p
        return result
    where
    eval a Nothing  = Nothing
    eval a (Just f) = let b = f a in b `seq` Just b  -- strict evaluation

applyP :: Latch (a -> b) -> Pulse a -> Network (Pulse b)
applyP f x = do
    result <- pulse $ fmap <$> valueL f <*> valueP x
    P result `dependOn` P x
    return result


mapP :: (a -> b) -> Pulse a -> Network (Pulse b)
mapP f p = do
    result <- pulse $ fmap f <$> valueP p
    P result `dependOn` P p
    return result

filterJustP :: Pulse (Maybe a) -> Network (Pulse a)
filterJustP p = do
    result <- pulse $ join <$> valueP p
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


applyL :: Latch (a -> b) -> Latch a -> Network (Latch b)
applyL lf lx = do
    -- The value in the next cycle is always the future value.
    -- See note [LatchCreation].
    let eval = ($) <$> futureL lf <*> futureL lx
    future <- eval
    now    <- ($) <$> valueL lf <*> valueL lx
    result <- latch now future $ fmap Just eval
    L result `dependOns` [L lf, L lx]
    return result

{-----------------------------------------------------------------------------
    Combinators - dynamic event switching
------------------------------------------------------------------------------}
observeP :: Pulse (Network a) -> Network (Pulse a)
observeP pn = do
    result <- pulse $ do
        mp <- valueP pn
        case mp of
            Just p  -> Just <$> p
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
            -- we have to use the *old* event value due to note [LatchCreation]
    result <- pulse eval
    P result `dependOns` [L lp, P pp]
    return result


switchL :: Latch a -> Pulse (Latch a) -> Network (Latch a)
switchL l p = mdo
    ll <- stepperL l p
    let
        -- switch to a new latch
        switchTo l = do
            L result `dependOn` L l
            futureL l
        -- calculate future value of the result latch
        eval = do
            mp <- valueP p
            case mp of
                Nothing -> futureL =<< valueL ll
                Just l  -> switchTo l

    now    <- valueL  l                 -- see note [LatchCreation]
    future <- futureL l
    result <- latch now future $ Just <$> eval
    L result `dependOns` [L l, P p]
    return result


