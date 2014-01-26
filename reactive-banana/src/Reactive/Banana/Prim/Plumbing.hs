{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecordWildCards, RecursiveDo #-}
module Reactive.Banana.Prim.Plumbing where

import           Control.Monad (join)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.RWS    as RWS
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.Writer as Writer
import           Data.Function                        (on)
import           Data.Functor
import           Data.IORef
import           Data.List                            (sortBy)
import           Data.Monoid
import qualified Data.Vault.Lazy as Lazy
import           System.IO.Unsafe

import Reactive.Banana.Prim.Types
import Reactive.Banana.Prim.Util

{-----------------------------------------------------------------------------
    Build primitive pulses and latches
------------------------------------------------------------------------------}
-- | Make 'Pulse' from evaluation function
newPulse :: String -> EvalP (Maybe a) -> Build (Pulse a)
newPulse name eval = liftIO $ do
    key <- Lazy.newKey
    newIORef $ Pulse
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
    newIORef $ Pulse
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

-- | Make new 'Latch' that can be updated by a 'Pulse'
newLatch :: a -> Build (Pulse a -> Build (), Latch a)
newLatch a = mdo
    latch <- liftIO $ newIORef $ Latch
        { _seenL  = agesAgo
        , _valueL = a
        , _evalL  = _valueL <$> get latch
        }
    let
        err        = error "incorrect Latch write"
        updateOn p = do
            w  <- liftIO $ mkWeakIORefValue latch latch 
            lw <- liftIO $ newIORef $ LatchWrite
                { _evalLW  = maybe err id <$> readPulseP p
                , _latchLW = w
                }
            -- writer is alive only as long as the latch is alive
            _  <- liftIO $ mkWeakIORefValue latch lw
            (P p) `addChild` (L lw)
    
    return (updateOn, latch)

-- | Make a new 'Latch' that caches a previous computation.
cachedLatch :: EvalL a -> Latch a
cachedLatch eval = unsafePerformIO $ newIORef $ Latch
    { _seenL  = agesAgo
    , _valueL = undefined
    , _evalL  = eval        -- TODO: Cache computation!
    }

-- | Add a new output that depends on a 'Pulse'.
--
-- TODO: Return function to unregister the output again.
addOutput :: Pulse EvalO -> Build ()
addOutput p = do
    o <- liftIO $ newIORef $ Output
        { _evalO     = maybe (return nop) id <$> readPulseP p
        , _positionO = 1 -- FIXME: Update position with global state!
        }
    (P p) `addChild` (O o)
    RWS.tell (mempty,mempty,[o])

{-----------------------------------------------------------------------------
    Build
------------------------------------------------------------------------------}
runBuildIO :: Time -> BuildIO a -> IO (a, Action, [Output])
runBuildIO time m = {-# SCC runBuild #-} do
    (a,_,(topologyUpdates,liftIOLaters,os)) <- RWS.runRWST m time ()
    doit $ liftIOLaters          -- execute late IOs
    return (a,topologyUpdates,os)

liftBuild :: Build a -> BuildIO a
liftBuild = id

getTimeB :: Build Time
getTimeB = RWS.ask

readLatchB :: Latch a -> Build a
readLatchB latch = do
    time      <- RWS.ask
    Latch{..} <- get latch
    liftIO $ Reader.runReaderT _evalL time

dependOn :: Pulse child -> Pulse parent -> Build ()
dependOn child parent = (P parent) `addChild` (P child)

addChild :: SomeNode -> SomeNode -> Build ()
addChild (P parent) (P child) = RWS.tell (Action action,mempty,mempty)
    where
    action = do
        wparent <- mkWeakIORefValue child (P parent)  -- child keeps parent alive
        level1 <- _levelP <$> get child
        level2 <- _levelP <$> get parent
        let level = level1 `max` (level2 + 1)
        modify' child  $ update parentsP  (wparent:) . set levelP level
        wchild <- mkWeakIORefValue child (P child)
        modify' parent $ update childrenP (wchild :)
addChild (P parent) (L child) = RWS.tell (Action action,mempty,mempty)
    where
    action = do
        _ <- mkWeakIORefValue child (P parent)  -- child keeps parent alive
        w <- mkWeakIORefValue child (L child)
        modify' parent $ update childrenP (w:)
addChild (P parent) (O child) = RWS.tell (Action action,mempty,mempty)
    where
    action = do
        _ <- mkWeakIORefValue child (P parent)  -- child keeps parent alive
            -- child keeps parent alive
        w <- mkWeakIORefValue child (O child)
        modify' parent $ update childrenP (w:)


changeParent :: Pulse child -> Pulse parent -> Build ()
changeParent child parent = error "FIXME: changeParent not implemented."

liftIOLater :: IO () -> Build ()
liftIOLater x = RWS.tell (mempty, Action x, mempty)

{-----------------------------------------------------------------------------
    EvalP monad
------------------------------------------------------------------------------}
getValueL :: Latch a -> EvalL a
getValueL l = do
    Latch{..} <- get l
    _evalL

runEvalP :: Lazy.Vault -> EvalP a -> Build (a, EvalLW, EvalO)
runEvalP s m = do
    (a,_,(wl,wo)) <- RWS.runRWST m () s
    return (a,wl, sequence_ <$> sequence (sortOutputs wo))

sortOutputs :: Ord k => [(k,a)] -> [a]
sortOutputs = map snd . sortBy (compare `on` fst)

liftBuildP :: Build a -> EvalP a
liftBuildP = lift

getTime :: EvalP Time
getTime = liftBuildP $ RWS.ask

readPulseP :: Pulse a -> EvalP (Maybe a)
readPulseP p = do
    Pulse{..} <- get p
    join . Lazy.lookup _keyP <$> RWS.get

writePulseP :: Lazy.Key (Maybe a) -> Maybe a -> EvalP ()
writePulseP key ma = RWS.modify $ Lazy.insert key ma

readLatchP :: Latch a -> EvalP a
readLatchP = lift . readLatchB

readLatchFutureP :: Latch a -> EvalP (Future a)
readLatchFutureP latch = error "FIXME: readLatchFutureP not implemented."

rememberLatchUpdate :: IO () -> EvalP ()
rememberLatchUpdate x = RWS.tell (Action x,mempty)

rememberOutput :: (Position, EvalO) -> EvalP ()
rememberOutput x = RWS.tell (mempty,[x])

{-----------------------------------------------------------------------------
    IORef
------------------------------------------------------------------------------}
get :: MonadIO m => IORef a -> m a
get = liftIO . readIORef

put :: MonadIO m => IORef a -> a -> m ()
put ref = liftIO . writeIORef ref

-- | Strictly modify an 'IORef'.
modify' :: MonadIO m => IORef a -> (a -> a) -> m ()
modify' ref f = get ref >>= \x -> put ref $! f x

