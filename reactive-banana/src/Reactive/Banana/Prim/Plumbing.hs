{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecordWildCards, RecursiveDo, BangPatterns #-}
module Reactive.Banana.Prim.Plumbing where

import           Control.Monad (join)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.ReaderWriterIO as RW
import qualified Control.Monad.Trans.RWSIO as RWS
import           Data.Function                        (on)
import           Data.Functor
import           Data.List                            (sortBy)
import           Data.Monoid
import qualified Data.Vault.Lazy as Lazy
import           System.IO.Unsafe

import qualified Reactive.Banana.Prim.Dependencies as Deps
import Reactive.Banana.Prim.Types
import Reactive.Banana.Prim.Util

{-----------------------------------------------------------------------------
    Build primitive pulses and latches
------------------------------------------------------------------------------}
-- | Make 'Pulse' from evaluation function
newPulse :: String -> EvalP (Maybe a) -> Build (Pulse a)
newPulse name eval = liftIO $ do
    key <- Lazy.newKey
    newRef $ Pulse
        { _keyP      = key
        , _seenP     = agesAgo
        , _evalP     = eval
        , _childrenP = []
        , _parentsP  = []
        , _levelP    = ground
        , _nameP     = name
        }

{-
* Note [PulseCreation]

We assume that we do not have to calculate a pulse occurrence
at the moment we create the pulse. Otherwise, we would have
to recalculate the dependencies *while* doing evaluation;
this is a recipe for desaster.

-}

-- | 'Pulse' that never fires.
neverP :: Build (Pulse a)
neverP = liftIO $ do
    key <- Lazy.newKey
    newRef $ Pulse
        { _keyP      = key
        , _seenP     = agesAgo
        , _evalP     = return Nothing
        , _childrenP = []
        , _parentsP  = []
        , _levelP    = ground
        , _nameP     = "neverP"
        }

alwaysP :: Build (Pulse ())
alwaysP = error "FIXME: alwaysP not implemented"

-- | Return a 'Latch' that has a constant value
pureL :: a -> Latch a
pureL a = unsafePerformIO $ newRef $ Latch
    { _seenL  = beginning
    , _valueL = a
    , _evalL  = return a
    }

-- | Make new 'Latch' that can be updated by a 'Pulse'
newLatch :: a -> Build (Pulse a -> Build (), Latch a)
newLatch a = mdo
    latch <- liftIO $ newRef $ Latch
        { _seenL  = beginning
        , _valueL = a
        , _evalL  = do
            Latch {..} <- readRef latch
            RW.tell _seenL  -- indicate timestamp
            return _valueL  -- indicate value
        }
    let
        err        = error "incorrect Latch write"
        updateOn p = do
            w  <- liftIO $ mkWeakRefValue latch latch 
            lw <- liftIO $ newRef $ LatchWrite
                { _evalLW  = maybe err id <$> readPulseP p
                , _latchLW = w
                }
            -- writer is alive only as long as the latch is alive
            _  <- liftIO $ mkWeakRefValue latch lw
            (P p) `addChild` (L lw)
    
    return (updateOn, latch)

-- | Make a new 'Latch' that caches a previous computation.
cachedLatch :: EvalL a -> Latch a
cachedLatch eval = unsafePerformIO $ mdo
    latch <- newRef $ Latch
        { _seenL  = agesAgo
        , _valueL = error "Undefined value of a cached latch."
        , _evalL  = do
            Latch{..} <- liftIO $ readRef latch
            -- calculate current value (lazy!) with timestamp
            (a,time)  <- RW.listen eval
            liftIO $ if time <= _seenL
                then return _valueL     -- return old value
                else do                 -- update value
                    let _seenL  = time
                    let _valueL = a
                    a `seq` put latch (Latch {..})
                    return a
        }
    return latch

-- | Add a new output that depends on a 'Pulse'.
--
-- TODO: Return function to unregister the output again.
addOutput :: Pulse EvalO -> Build ()
addOutput p = do
    o <- liftIO $ newRef $ Output
        { _evalO     = maybe (return $ debug "nop") id <$> readPulseP p
        , _positionO = 1 -- FIXME: Update position with global state!
        }
    (P p) `addChild` (O o)
    RW.tell (mempty,mempty,[o])

{-----------------------------------------------------------------------------
    Build monad
------------------------------------------------------------------------------}
runBuildIO :: Time -> BuildIO a -> IO (a, Action, [Output])
runBuildIO time m = {-# SCC runBuild #-} do
    (a,(topologyUpdates,liftIOLaters,os)) <- RW.runReaderWriterIOT m time
    doit $ liftIOLaters          -- execute late IOs
    return (a,Action $ Deps.buildDependencies topologyUpdates,os)

liftBuild :: Build a -> BuildIO a
liftBuild = id

getTimeB :: Build Time
getTimeB = RW.ask

readLatchB :: Latch a -> Build a
readLatchB latch = do
    Latch{..} <- readRef latch
    liftIO $ fst <$> RW.runReaderWriterIOT _evalL ()

dependOn :: Pulse child -> Pulse parent -> Build ()
dependOn child parent = (P parent) `addChild` (P child)

keepAlive :: Pulse child -> Pulse parent -> Build ()
keepAlive child parent = liftIO $ mkWeakRefValue child parent >> return ()

addChild :: SomeNode -> SomeNode -> Build ()
addChild parent child =
    RW.tell (Deps.addChild parent child,mempty,mempty)

changeParent :: Pulse child -> Pulse parent -> Build ()
changeParent node parent =
    RW.tell (Deps.changeParent node parent,mempty,mempty)

liftIOLater :: IO () -> Build ()
liftIOLater x = RW.tell (mempty, Action x, mempty)

{-----------------------------------------------------------------------------
    EvalP monad
------------------------------------------------------------------------------}
getValueL :: Latch a -> EvalL a
getValueL l = do
    Latch{..} <- readRef l
    _evalL

runEvalP :: Lazy.Vault -> EvalP a -> Build (a, EvalLW, EvalO)
runEvalP s1 m = RW.readerWriterIOT $ \r2 -> do
    (a,_,((wl,wo),w2)) <- RWS.runRWSIOT m r2 s1
    return ((a,wl, sequence_ <$> sequence (sortOutputs wo)), w2)

sortOutputs :: Ord k => [(k,a)] -> [a]
sortOutputs = map snd . sortBy (compare `on` fst)

liftBuildP :: Build a -> EvalP a
liftBuildP m = RWS.rwsT $ \r2 s -> do
    (a,w2) <- RW.runReaderWriterIOT m r2
    return (a,s,(mempty,w2))

askTime :: EvalP Time
askTime = RWS.ask

readPulseP :: Pulse a -> EvalP (Maybe a)
readPulseP p = do
    Pulse{..} <- readRef p
    join . Lazy.lookup _keyP <$> RWS.get

readLatchP :: Latch a -> EvalP a
readLatchP = liftBuildP . readLatchB

writeLatchP :: Lazy.Key (Maybe a) -> Maybe a -> EvalP ()
writeLatchP key a = do
    s <- RWS.get
    RWS.put $ Lazy.insert key a s

readLatchFutureP :: Latch a -> EvalP (Future a)
readLatchFutureP latch = error "FIXME: readLatchFutureP not implemented."

rememberLatchUpdate :: IO () -> EvalP ()
rememberLatchUpdate x = RWS.tell ((Action x,mempty),mempty)

rememberOutput :: (Position, EvalO) -> EvalP ()
rememberOutput x = RWS.tell ((mempty,[x]),mempty)

-- worker wrapper to break sharing and support better inlining
unwrapEvalP r m = RWS.run m r
wrapEvalP   m   = RWS.R m
