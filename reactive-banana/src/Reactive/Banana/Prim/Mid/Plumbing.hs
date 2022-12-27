{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reactive.Banana.Prim.Mid.Plumbing where

import Control.Monad
    ( join )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Maybe
    ( fromMaybe )

import qualified Control.Monad.Trans.RWSIO          as RWS
import qualified Control.Monad.Trans.ReaderWriterIO as RW
import           Data.Functor
import           Data.IORef
import qualified Data.Vault.Lazy                    as Lazy
import           System.IO.Unsafe

import qualified Reactive.Banana.Prim.Low.Ref as Ref
import qualified Reactive.Banana.Prim.Mid.Dependencies as Deps
import           Reactive.Banana.Prim.Mid.Types

{-----------------------------------------------------------------------------
    Build primitive pulses and latches
------------------------------------------------------------------------------}
-- | Make 'Pulse' from evaluation function
newPulse :: String -> EvalP (Maybe a) -> Build (Pulse a)
newPulse name eval = liftIO $ do
    _key <- Lazy.newKey
    _nodeP <- Ref.new $ P $ PulseD
        { _keyP      = _key
        , _seenP     = agesAgo
        , _evalP     = eval
        , _nameP     = name
        }
    pure $ Pulse{_key,_nodeP}

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
    _key <- Lazy.newKey
    _nodeP <- Ref.new $ P $ PulseD
        { _keyP      = _key
        , _seenP     = agesAgo
        , _evalP     = pure Nothing
        , _nameP     = "neverP"
        }
    pure $ Pulse{_key,_nodeP}

-- | Return a 'Latch' that has a constant value
pureL :: a -> Latch a
pureL a = unsafePerformIO $ Ref.new $ Latch
    { _seenL  = beginning
    , _valueL = a
    , _evalL  = return a
    }

-- | Make new 'Latch' that can be updated by a 'Pulse'
newLatch :: forall a. a -> Build (Pulse a -> Build (), Latch a)
newLatch a = mdo
    latch <- liftIO $ Ref.new $ Latch
        { _seenL  = beginning
        , _valueL = a
        , _evalL  = do
            Latch {..} <- Ref.read latch
            RW.tell _seenL  -- indicate timestamp
            return _valueL  -- indicate value
        }
    let
        err        = error "incorrect Latch write"

        updateOn :: Pulse a -> Build ()
        updateOn p = do
            w  <- liftIO $ Ref.mkWeak latch latch Nothing
            lw <- liftIO $ Ref.new $ L $ LatchWriteD
                { _evalLW  = fromMaybe err <$> readPulseP p
                , _latchLW = w
                }
            -- writer is alive only as long as the latch is alive
            _  <- liftIO $ Ref.mkWeak latch lw Nothing
            _nodeP p `addChild` lw

    return (updateOn, latch)

-- | Make a new 'Latch' that caches a previous computation.
cachedLatch :: EvalL a -> Latch a
cachedLatch eval = unsafePerformIO $ mdo
    latch <- Ref.new $ Latch
        { _seenL  = agesAgo
        , _valueL = error "Undefined value of a cached latch."
        , _evalL  = do
            Latch{..} <- liftIO $ Ref.read latch
            -- calculate current value (lazy!) with timestamp
            (a,time)  <- RW.listen eval
            liftIO $ if time <= _seenL
                then return _valueL     -- return old value
                else do                 -- update value
                    let _seenL  = time
                    let _valueL = a
                    a `seq` Ref.put latch (Latch {..})
                    return a
        }
    return latch

-- | Add a new output that depends on a 'Pulse'.
--
-- TODO: Return function to unregister the output again.
addOutput :: Pulse EvalO -> Build ()
addOutput p = do
    o <- liftIO $ Ref.new $ O $ Output
        { _evalO = fromMaybe (pure $ pure ()) <$> readPulseP p
        }
    _nodeP p `addChild` o
    RW.tell $ BuildW (mempty, [o], mempty, mempty)

{-----------------------------------------------------------------------------
    Build monad
------------------------------------------------------------------------------}
runBuildIO :: BuildR -> BuildIO a -> IO (a, DependencyChanges, [Output])
runBuildIO i m = do
    (a, BuildW (topologyUpdates, os, liftIOLaters, _)) <- unfold mempty m
    doit liftIOLaters          -- execute late IOs
    return (a,topologyUpdates,os)
  where
    -- Recursively execute the  buildLater  calls.
    unfold :: BuildW -> BuildIO a -> IO (a, BuildW)
    unfold w m = do
        (a, BuildW (w1, w2, w3, later)) <- RW.runReaderWriterIOT m i
        let w' = w <> BuildW (w1,w2,w3,mempty)
        w'' <- case later of
            Just m  -> snd <$> unfold w' m
            Nothing -> return w'
        return (a,w'')

buildLater :: Build () -> Build ()
buildLater x = RW.tell $ BuildW (mempty, mempty, mempty, Just x)

-- | Pretend to return a value right now,
-- but do not actually calculate it until later.
--
-- NOTE: Accessing the value before it's written leads to an error.
--
-- FIXME: Is there a way to have the value calculate on demand?
buildLaterReadNow :: Build a -> Build a
buildLaterReadNow m = do
    ref <- liftIO $ newIORef $
        error "buildLaterReadNow: Trying to read before it is written."
    buildLater $ m >>= liftIO . writeIORef ref
    liftIO $ unsafeInterleaveIO $ readIORef ref

liftBuild :: Build a -> BuildIO a
liftBuild = id

getTimeB :: Build Time
getTimeB = fst <$> RW.ask

alwaysP :: Build (Pulse ())
alwaysP = snd <$> RW.ask

readLatchB :: Latch a -> Build a
readLatchB = liftIO . readLatchIO

dependOn :: Pulse child -> Pulse parent -> Build ()
dependOn child parent = _nodeP parent `addChild` _nodeP child

keepAlive :: Pulse child -> Pulse parent -> Build ()
keepAlive child parent = liftIO $ void $
    Ref.mkWeak (_nodeP child) (_nodeP parent) Nothing

addChild :: SomeNode -> SomeNode -> Build ()
addChild parent child =
    RW.tell $ BuildW (Deps.addChild parent child, mempty, mempty, mempty)

changeParent :: Pulse child -> Pulse parent -> Build ()
changeParent node parent =
    RW.tell $ BuildW (Deps.changeParent node parent, mempty, mempty, mempty)

liftIOLater :: IO () -> Build ()
liftIOLater x = RW.tell $ BuildW (mempty, mempty, Action x, mempty)

{-----------------------------------------------------------------------------
    EvalL monad
------------------------------------------------------------------------------}
-- | Evaluate a latch (-computation) at the latest time,
-- but discard timestamp information.
readLatchIO :: Latch a -> IO a
readLatchIO latch = do
    Latch{..} <- Ref.read latch
    liftIO $ fst <$> RW.runReaderWriterIOT _evalL ()

getValueL :: Latch a -> EvalL a
getValueL latch = do
    Latch{..} <- Ref.read latch
    _evalL

{-----------------------------------------------------------------------------
    EvalP monad
------------------------------------------------------------------------------}
runEvalP :: Lazy.Vault -> EvalP a -> Build (a, EvalPW)
runEvalP s1 m = RW.readerWriterIOT $ \r2 -> do
    (a,_,(w1,w2)) <- RWS.runRWSIOT m r2 s1
    return ((a,w1), w2)

liftBuildP :: Build a -> EvalP a
liftBuildP m = RWS.rwsT $ \r2 s -> do
    (a,w2) <- RW.runReaderWriterIOT m r2
    return (a,s,(mempty,w2))

askTime :: EvalP Time
askTime = fst <$> RWS.ask

readPulseP :: Pulse a -> EvalP (Maybe a)
readPulseP Pulse{_key} =
    join . Lazy.lookup _key <$> RWS.get

writePulseP :: Lazy.Key (Maybe a) -> Maybe a -> EvalP ()
writePulseP key a = do
    s <- RWS.get
    RWS.put $ Lazy.insert key a s

readLatchP :: Latch a -> EvalP a
readLatchP = liftBuildP . readLatchB

readLatchFutureP :: Latch a -> EvalP (Future a)
readLatchFutureP = return . readLatchIO

rememberLatchUpdate :: IO () -> EvalP ()
rememberLatchUpdate x = RWS.tell ((Action x,mempty),mempty)

rememberOutput :: (Output, EvalO) -> EvalP ()
rememberOutput x = RWS.tell ((mempty,[x]),mempty)

-- worker wrapper to break sharing and support better inlining
unwrapEvalP :: RWS.Tuple r w s -> RWS.RWSIOT r w s m a -> m a
unwrapEvalP r m = RWS.run m r

wrapEvalP :: (RWS.Tuple r w s -> m a) -> RWS.RWSIOT r w s m a
wrapEvalP m = RWS.R m
