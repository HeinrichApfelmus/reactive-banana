{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types, RecursiveDo, ExistentialQuantification,
    TypeSynonymInstances, FlexibleInstances, BangPatterns #-}
module Reactive.Banana.Internal.PulseLatch0 where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Data.IORef
import Data.Monoid (Endo(..))
-- import Data.Strict.Tuple

import Control.Concurrent.MVar
import qualified Control.Exception as Strict (evaluate)

import Reactive.Banana.Internal.Cached
import Reactive.Banana.Internal.InputOutput
import qualified Reactive.Banana.Internal.DependencyGraph as Deps
import Reactive.Banana.Frameworks.AddHandler

import Data.Hashable
import Data.Unique.Really
import qualified Data.Vault as Vault

import Data.Functor.Identity
import System.IO.Unsafe


import Debug.Trace

type Deps = Deps.Deps

debug   s m = m
debugIO s m = liftIO (putStrLn s) >> m

{-----------------------------------------------------------------------------
    Graph data type
------------------------------------------------------------------------------}
-- A 'Graph' represents the connections between pulses and events.
data Graph = Graph
    { grInputs  :: [Input]                   -- input  nodes
    , grOutputs :: [Output]                  -- output actions

    , grCache   :: Values                    -- cache for initialization

    , grDeps    :: Deps SomeNode             -- dependency information
    }

type Values = Vault.Vault
type Key    = Vault.Key
type Input  =
    ( SomeNode
    , InputValue -> Values -> Values         -- write input value into latch values
    )
type Output = Pulse (IO ())
type Reactimate = IO ()

emptyGraph :: Graph
emptyGraph = Graph
    { grCache   = Vault.empty
    , grDeps    = Deps.empty
    , grInputs  = [(P alwaysP, const id)]
    , grOutputs = []
    }

-- A 'NetworkState' represents the state of a pulse network,
-- which consists of a 'Graph' and the values of all latches in the graph.
data NetworkState = NetworkState
    { nsGraph       :: !Graph
    , nsLatchValues :: !Values
    }

emptyState :: NetworkState
emptyState = NetworkState emptyGraph Vault.empty

{-----------------------------------------------------------------------------
    Graph evaluation
------------------------------------------------------------------------------}
-- Evaluate all the pulses in the graph,
-- rebuild the graph as necessary and update the latch values.
step :: Callback -> [InputValue] -> NetworkState -> IO (Reactimate, NetworkState)
step callback inputs state1 = {-# SCC step #-} mdo
    let graph1 = nsGraph state1
        latch1 = nsLatchValues state1
        pulse1 = writeInputs graph1 inputs
    
    (pulse2, state2) <- runBuildIO callback state1
            $ runEvalP latch2 pulse1
            $ evaluatePulses graph1
    
    let
        graph2 = nsGraph state2
        latch2 = evaluateLatches graph2 pulse2 $ nsLatchValues state2
        output = readOutputs graph2 pulse2

    -- make sure that state is WHNF
    state3 <- Strict.evaluate $ NetworkState graph2 latch2
    return (output, state3)

-- Evaluate all pulses in the graph.
evaluatePulses :: Graph -> EvalP ()
evaluatePulses = mapM_ evaluatePulse . buildEvaluationOrder
    where
    evaluatePulse (P p) = evaluateP p
    evaluatePulse (L _) = return ()

-- Update all latch values.
evaluateLatches :: Graph -> Values -> Values -> Values
evaluateLatches graph pulse latch = {-# SCC evaluateLatches #-}
    runEvalL pulse latch . mapM_ evaluateLatch $ buildEvaluationOrder graph
    where
    evaluateLatch (P _) = return ()
    evaluateLatch (L l) = evaluateL l   

readOutputs graph pulses =
    sequence_ [action | out <- grOutputs graph
                      , Just action <- [getValueP out pulses]]

writeInputs graph inputs =
    concatenate [f x | (_,f) <- grInputs graph, x <- inputs] Vault.empty

concatenate :: [a -> a] -> (a -> a)
concatenate = foldr (.) id


-- Figure out which nodes need to be evaluated.
--
-- All nodes that are connected to current input nodes must be evaluated.
-- The other nodes don't have to be evaluated, because they yield
-- Nothing / don't change anyway.
buildEvaluationOrder :: Graph -> [SomeNode]
buildEvaluationOrder = Deps.topologicalSort . grDeps

{-----------------------------------------------------------------------------
    Evaluation monads / monoids
------------------------------------------------------------------------------}
-- The 'EvalP' monad is used to evaluate pulses.
type EvalP = RWST Values () Values BuildIO
    -- read : future latch values
    -- state: current pulse values

runEvalP :: Values -> Values -> EvalP a -> BuildIO Values
runEvalP latch pulse m = do
    (_,s,_) <- runRWST m latch pulse
    return s

readLatchP  :: Latch a -> EvalP a
readLatchP latch = lift $ getValueL latch . nsLatchValues <$> get

readLatchFutureP :: Latch a -> EvalP a
readLatchFutureP latch = getValueL latch <$> ask

writePulseP :: Key a -> a -> EvalP ()
writePulseP key a = modify $ Vault.insert key a

readPulseP  :: Pulse a -> EvalP (Maybe a)
readPulseP pulse = getValueP pulse <$> get

liftBuildIOP :: BuildIO a -> EvalP a
liftBuildIOP = lift

liftBuildP :: Build a -> EvalP a
liftBuildP = liftBuildIOP . liftBuild


-- The 'EvalL' monad is used to evaluate latches.
type EvalL = RWS Values () Values
    -- read  : current pulse values
    -- state : current latch values

runEvalL :: Values -> Values -> EvalL () -> Values
runEvalL pulse latch1 m = let (_,latch2,_) = runRWS m pulse latch1 in latch2

readLatchL  :: Latch a -> EvalL a
readLatchL latch = getValueL latch <$> get

-- TODO: Writing a latch should be strict in both value and key!
writeLatchL :: Key a -> a -> EvalL ()
writeLatchL key a = modify $ Vault.insert key a

readPulseL :: Pulse a -> EvalL (Maybe a)
readPulseL pulse = getValueP pulse <$> ask


{-----------------------------------------------------------------------------
    Build monad
------------------------------------------------------------------------------}
-- The 'Build' monad is used to change the graph, for instance to
-- * add nodes
-- * change dependencies
-- * add inputs or outputs
type BuildT  = RWST () BuildConf NetworkState
type Build   = BuildT Identity 
type BuildIO = BuildT IO

type BuildConf =
    ( [AddHandler [InputValue]]   -- fromAddHandler
    , [IO ()]                     -- liftIOLater
    )

{- Note [BuildT]

It is very convenient to be able to perform some IO functions
while (re)building a network graph. At the same time,
we need a good  MonadFix  instance to build recursive networks.
These requirements clash, so the solution is to split the types
into a pure variant and IO variant, the former having a good
MonadFix  instance while the latter can do arbitrary IO.

-}

-- Lift a pure  Build  computation into any monad.
-- See note [BuildT]
liftBuild :: Monad m => Build a -> BuildT m a
liftBuild m = RWST $ \r s -> return . runIdentity $ runRWST m r s

runBuildIO :: Callback -> NetworkState -> BuildIO a -> IO (a, NetworkState)
runBuildIO callback s1 m = do
    (a,s2,(addHandlers,liftIOLaters)) <- runRWST m () s1
    mapM_ ($ callback) addHandlers  -- register new event handlers
    sequence_ liftIOLaters          -- execute late IOs
    return (a,s2)

modifyGraph f = modify $ \s -> s { nsGraph = f (nsGraph s) }

addLatchValue :: Key a -> a -> Build ()
addLatchValue key a = modify $ \s ->
    s { nsLatchValues = Vault.insert key a (nsLatchValues s) }

readLatchB :: Latch a -> Build a
readLatchB latch = getValueL latch . nsLatchValues <$> get

-- access initialization cache
instance (MonadFix m, Functor m) => HasVault (BuildT m) where
    retrieve key = Vault.lookup key . grCache . nsGraph <$> get
    write key a  = modifyGraph $ \g -> g { grCache = Vault.insert key a (grCache g) }

-- Add a dependency.
dependOn :: SomeNode -> SomeNode -> Build ()
dependOn x y = modifyGraph $ \g -> g { grDeps = Deps.dependOn x y $ grDeps g }

dependOns :: SomeNode -> [SomeNode] -> Build ()
dependOns x = mapM_ $ dependOn x

-- Link a 'Pulse' key to an input channel.
addInput :: Key a -> Pulse a -> InputChannel a -> Build ()
addInput key pulse channel =
    modifyGraph $ \g -> g { grInputs = (P pulse, input) : grInputs g }
    where
    input value
        | getChannel value == getChannel channel =
            maybe id (Vault.insert key) $ fromValue channel value
        | otherwise = id

addOutput :: Output -> Build ()
addOutput x = modifyGraph $ \g -> g { grOutputs = grOutputs g ++ [x] }

liftIOLater :: IO () -> Build ()
liftIOLater x = tell ([],[x])

registerHandler :: AddHandler [InputValue] -> Build ()
registerHandler x = tell ([x],[])


{-----------------------------------------------------------------------------
    Compilation.
    State machine IO stuff.
------------------------------------------------------------------------------}
type Callback = [InputValue] -> IO ()

data EventNetwork = EventNetwork
    { actuate :: IO ()
    , pause   :: IO ()
    }

-- compile to an event network
compile :: BuildIO () -> IO EventNetwork
compile setup = do
    actuated <- newIORef False                   -- flag to set running status
    rstate   <- newEmptyMVar                     -- setup callback machinery
    let
        whenFlag flag action = readIORef flag >>= \b -> when b action
        callback inputs = whenFlag actuated $ do
            state1 <- takeMVar rstate            -- read and take lock
            -- pollValues <- sequence polls      -- poll mutable data
            (reactimates, state2)
                <- step callback inputs state1   -- calculate new state
            putMVar rstate state2                -- write state
            reactimates                          -- run IO actions afterwards

    (_, state) <-
        runBuildIO callback emptyState setup     -- compile initial graph
    putMVar rstate state                         -- set initial state
        
    return $ EventNetwork
        { actuate = writeIORef actuated True
        , pause   = writeIORef actuated False
        }

{- TODO
-- make an interpreter
interpret :: (Pulse a -> BuildIO (Pulse b)) -> [Maybe a] -> IO [Maybe b]
interpret f xs = do
    i <- newInputChannel
    (result,graph) <- discardSetup $
        runNetworkAtomicT (f =<< liftBuild (inputP i)) emptyGraph

    let
        step Nothing  g0 = return (Nothing,g0)
        step (Just a) g0 = do
            g1 <- discardSetup $ evaluateGraph [toValue i a] g0
            return (readPulseValue result g1, g1)
    
    mapAccumM step graph xs
-}

mapAccumM :: Monad m => (a -> s -> m (b,s)) -> s -> [a] -> m [b]
mapAccumM _ _  []     = return []
mapAccumM f s0 (x:xs) = do
    (b,s1) <- f x s0
    bs     <- mapAccumM f s1 xs
    return (b:bs)

{-----------------------------------------------------------------------------
    Pulse and Latch types
------------------------------------------------------------------------------}
{-
    evaluateL/P
        calculates the next value and makes sure that it's cached
    getValueL/P
        retrieves the current value
    uidL/P
        used for dependency tracking and evaluation order
-}

data Pulse a = Pulse
    { evaluateP :: EvalP ()
    , getValueP :: Values -> Maybe a
    , uidP      :: Unique
    }

data Latch a = Latch
    { evaluateL :: EvalL ()
    , getValueL :: Values -> a
    , uidL      :: Unique
    }

{-
* Note [LatchCreation]

When creating a new latch from a pulse, we assume that the
pulse cannot fire at the moment that the latch is created.
This is important when switching latches, because of note [PulseCreation].

When creating a latch, we have to write its past value
into the network state. In particular, when we are in an  EvalP  context,
this value has to present.

* Note [PulseCreation]

We assume that we do not have to calculate a pulse occurrence
at the moment we create the pulse. Otherwise, we would have
to recalculate the dependencies *while* doing evaluation;
this is a recipe for desaster.

* Note [unsafePerformIO]

We're using @unsafePerformIO@ only to get @Key@ and @Unique@.
It's not great, but it works.

Unfortunately, using @IO@ as the base of the @Network@ monad
transformer doens't work because it doesn't support recursion
and @mfix@ very well.

We could use the @ST@ monad, but this would add a type parameter
to everything. A refactoring of this scope is too annoying for
my taste right now.

-}

-- make pulse from evaluation function
mkPulse :: EvalP (Maybe a) -> Build (Pulse a)
mkPulse eval = unsafePerformIO $ do
    key <- Vault.newKey
    uid <- newUnique
    return $ do
        let write = maybe (return ()) (writePulseP key)
        return $ Pulse
            { evaluateP = {-# SCC evaluateP #-} write =<< eval
            , getValueP = Vault.lookup key
            , uidP      = uid
            }

neverP :: Build (Pulse a)
neverP = debug "neverP" $ unsafePerformIO $ do
    uid <- newUnique
    return $ return $ Pulse
        { evaluateP = return ()
        , getValueP = const Nothing
        , uidP      = uid
        }

-- create a pulse that listens to input values
inputP :: InputChannel a -> Build (Pulse a)
inputP channel = debug "inputP" $ unsafePerformIO $ do
    key <- Vault.newKey
    uid <- newUnique
    return $ do
        let
            p = Pulse
                { evaluateP = return ()
                , getValueP = Vault.lookup key
                , uidP      = uid
                }
        addInput key p channel
        return p

-- event that always fires whenever the network processes events
alwaysP :: Pulse ()
alwaysP = debug "alwaysP" $ unsafePerformIO $ do
    uid <- newUnique
    return $ Pulse
        { evaluateP = return ()
        , getValueP = return $ Just ()
        , uidP      = uid
        }

{-

* Note [LatchStrictness]

Any value that is stored in the graph over a longer
period of time must be stored in WHNF.

This implies that the values in a latch must be forced to WHNF
when storing them. That doesn't have to be immediately
since we are tying a knot, but it definitely has to be done
before  evaluateGraph  is done.

Conversely, since latches are the only way to store values over time,
this is enough to guarantee that there are no space leaks in this regard.

-}

-- make latch from initial value and an evaluation function
mkLatch :: a -> EvalL (Maybe a) -> Build (Latch a)
mkLatch now eval = unsafePerformIO $ do
    key <- Vault.newKey
    uid <- {-# SCC "latch/newUnique" #-} newUnique
    return $ do
        -- Initialize with current value.
        -- See note [LatchCreation].
        addLatchValue key $! now
        
        -- See note [LatchStrictness].
        let write = maybe (return ()) (writeLatchL key $!)
        let err = error "getValueL: latch not initialized!"
        
        return $ Latch
            { evaluateL = {-# SCC evaluateL #-} write =<< eval
            , getValueL = {-# SCC getValueL  #-} maybe err id . Vault.lookup key
            , uidL      = uid
            }

pureL :: a -> Build (Latch a)
pureL a = debug "pureL" $ unsafePerformIO $ do
    uid <- liftIO newUnique
    return $ return $ Latch
        { evaluateL = return ()
        , getValueL = const a
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
stepperL :: a -> Pulse a -> Build (Latch a)
stepperL a p = debug "stepperL" $ do
    x <- mkLatch a $ {-# SCC stepperL #-} readPulseL p
    L x `dependOn` P p
    return x

accumP :: a -> Pulse (a -> a) -> Build (Pulse a)
accumP a p = debug "accumP" $ mdo
    x       <- stepperL a result
    result  <- mkPulse $
        {-# SCC accumP #-} (\x -> fmap ($ x)) <$> readLatchP x <*> readPulseP p
    -- Evaluation order of the result pulse does *not*
    -- depend on the latch. It does depend on latch value,
    -- though, so don't garbage collect that one.
    P result `dependOn` P p
    return result

applyP :: Latch (a -> b) -> Pulse a -> Build (Pulse b)
applyP f x = debug "applyP" $ do
    result <- mkPulse $ {-# SCC applyP #-} fmap <$> readLatchP f <*> readPulseP x
    P result `dependOn` P x
    return result

-- Tag a pulse with future values of a latch.
-- Caveat emptor. These values are not defined until after the  EvalL  phase.
tagFuture :: Latch a -> Pulse b -> Build (Pulse a)
tagFuture f x = debug "tagFuture" $ do
    result <- mkPulse $ fmap . const <$> readLatchFutureP f <*> readPulseP x
    P result `dependOn` P x
    return result


mapP :: (a -> b) -> Pulse a -> Build (Pulse b)
mapP f p = debug "mapP" $ do
    result <- mkPulse $ {-# SCC mapP #-} fmap f <$> readPulseP p
    P result `dependOn` P p
    return result

filterJustP :: Pulse (Maybe a) -> Build (Pulse a)
filterJustP p = debug "filterJustP" $ do
    result <- mkPulse $ {-# SCC filterJustP #-} join <$> readPulseP p
    P result `dependOn` P p
    return result

unionWith :: (a -> a -> a) -> Pulse a -> Pulse a -> Build (Pulse a)
unionWith f px py = debug "unionWith" $ do
        result <- mkPulse $
            {-# SCC unionWith #-} eval <$> readPulseP px <*> readPulseP py
        P result `dependOns` [P px, P py]
        return result
    where
    eval (Just x) (Just y) = Just (f x y)
    eval (Just x) Nothing  = Just x
    eval Nothing  (Just y) = Just y
    eval Nothing  Nothing  = Nothing


applyL :: Latch (a -> b) -> Latch a -> Build (Latch b)
applyL lf lx = debug "applyL" $ do
    let evalL = {-# SCC applyL #-} ($) <$> readLatchL lf <*> readLatchL lx
    now    <- ($) <$> readLatchB lf <*> readLatchB lx
    result <- mkLatch now $ Just <$> evalL
    L result `dependOns` [L lf, L lx]
    return result

{-----------------------------------------------------------------------------
    Combinators - dynamic event switching
------------------------------------------------------------------------------}
executeP :: Pulse (BuildIO a) -> Build (Pulse a)
executeP pn = do
    result <- mkPulse $ do
        mp <- readPulseP pn
        case mp of
            Just p  -> liftBuildIOP $ Just <$> p
            Nothing -> return Nothing
    P result `dependOn` P pn
    return result

switchP :: Pulse (Pulse a) -> Build (Pulse a)
switchP pp = mdo
    never <- neverP
    lp    <- stepperL never pp
    let
        eval = do
            newPulse <- readPulseP pp
            case newPulse of
                Nothing -> return ()
                Just p  -> liftBuildP $
                                  P result `dependOn` P p  -- check in new pulse
            readPulseP =<< readLatchP lp                   -- fetch value from old pulse
    result <- mkPulse eval
    P result `dependOns` [L lp, P pp]
    return result


switchL :: Latch a -> Pulse (Latch a) -> Build (Latch a)
switchL l p = mdo
    ll <- stepperL l p2
    let -- register the new dependency
        evalP = do
            ml <- readPulseP p
            case ml of
                Just l -> do
                    liftBuildP $ L result `dependOn` L l
                    return $ Just l
                Nothing -> return Nothing
    p2 <- mkPulse $ evalP
    
    let -- calculate value of result latch
        evalL = readLatchL =<< readLatchL ll
    
    now    <- readLatchB l
    result <- mkLatch now $ Just <$> evalL
    L result `dependOns` [L l, P p]
    return result


