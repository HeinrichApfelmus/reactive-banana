{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Reactive.Banana.Prim.Plumbing where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.RWS
import           Data.Functor
import           Data.Functor.Identity
import           Data.Monoid
import           Data.Unique.Really
import qualified Data.Vault.Strict       as Strict
import qualified Data.Vault.Lazy         as Lazy
import           System.IO.Unsafe                  (unsafePerformIO)

import           Reactive.Banana.Prim.Cached                (HasCache(..))
import qualified Reactive.Banana.Prim.Dependencies as Deps
import           Reactive.Banana.Prim.Types

{-----------------------------------------------------------------------------
    Build primitive pulses and latches
------------------------------------------------------------------------------}
-- | Make 'Pulse' from evaluation function
newPulse :: EvalP (Maybe a) -> Build (Pulse a)
newPulse eval = unsafePerformIO $ do
    key <- Lazy.newKey
    uid <- newUnique
    return $ do
        let write = maybe (return Deps.Done) ((Deps.Children <$) . writePulseP key)
        return $ Pulse
            { evaluateP = {-# SCC evaluateP #-} write =<< eval
            , getValueP = Lazy.lookup key
            , uidP      = uid
            }

-- | 'Pulse' that never fires.
neverP :: Build (Pulse a)
neverP = unsafePerformIO $ do
    uid <- newUnique
    return $ return $ Pulse
        { evaluateP = return Deps.Done
        , getValueP = const Nothing
        , uidP      = uid
        }

-- | Make new 'Latch' that can be updated.
newLatch :: a -> Build (Pulse a -> Build (), Latch a)
newLatch a = unsafePerformIO $ do
    key <- Strict.newKey
    uid <- newUnique
    return $ do
        let
            write        = maybe mempty (Endo . Strict.insert key)
            latchWrite p = LatchWrite
                { evaluateL = {-# SCC evaluateL #-} write <$> readPulseP p
                , uidL      = uid
                }
            updateOn p   = P p `addChild` L (latchWrite p)
        return
            (updateOn, Latch { getValueL = maybe a id . Strict.lookup key })

-- | Make a new 'Latch' that caches a previous computation
cachedLatch :: (Strict.Vault -> a) -> Latch a
cachedLatch eval = Latch
    { getValueL = eval  -- FIXME: Actually cache the computation!
    }

-- | Add a new output that depends on a 'Pulse'.
--
-- TODO: Return function to unregister the output again.
addOutput :: Pulse EvalO -> Build ()
addOutput p = unsafePerformIO $ do
    uid <- newUnique
    let
        read = maybe (const $ return ()) id
        o = Output
            { evaluateO = read <$> readPulseP p
            , uidO      = uid
            }
    return (P p `addChild` O o)
    

{-----------------------------------------------------------------------------
    Build monad - add and delete nodes from the graph
------------------------------------------------------------------------------}
runBuildIO :: Network -> BuildIO a -> IO (a, Network)
runBuildIO s1 m = do
    (a,s2,liftIOLaters) <- runRWST m () s1
    sequence_ liftIOLaters          -- execute late IOs
    return (a,s2)

-- Lift a pure  Build  computation into any monad.
-- See note [BuildT]
liftBuild :: Monad m => Build a -> BuildT m a
liftBuild m = RWST $ \r s -> return . runIdentity $ runRWST m r s

readLatchB :: Latch a -> Build a
readLatchB latch = getValueL latch . nLatchValues <$> get

instance (MonadFix m, Functor m) => HasCache (BuildT m) where
    retrieve key = Lazy.lookup key . grCache . nGraph <$> get
    write key a  = modify $ updateGraph $ updateCache $ Lazy.insert key a

dependOn :: Pulse child -> Pulse parent -> Build ()
dependOn child parent = (P parent) `addChild` (P child)

changeParent :: Pulse child -> Pulse parent -> Build ()
changeParent child parent =
    modify . updateGraph . updateDeps $ Deps.changeParent (P child) (P parent)

addChild :: SomeNode -> SomeNode -> Build ()
addChild parent child =
    modify . updateGraph . updateDeps $ Deps.addChild parent child

liftIOLater :: IO () -> Build ()
liftIOLater x = tell [x]

{-----------------------------------------------------------------------------
    EvalP - evaluate pulses
------------------------------------------------------------------------------}
runEvalP :: Lazy.Vault -> Strict.Vault -> EvalP a
    -> BuildIO (Lazy.Vault, EvalL, EvalO)
runEvalP pulse latch m = do
    (_,s,(wl,wo)) <- runRWST m latch pulse
    return (s,wl, sequence_ . (sequence wo))

readLatchP :: Latch a -> EvalP a
readLatchP latch = lift $ getValueL latch . nLatchValues <$> get

readLatchFutureP :: Latch a -> EvalP a
readLatchFutureP latch = RWST $ \r s -> return (getValueL latch r,s,mempty)

writePulseP :: Lazy.Key a -> a -> EvalP ()
writePulseP key a = modify $ Lazy.insert key a

readPulseP :: Pulse a -> EvalP (Maybe a)
readPulseP pulse = getValueP pulse <$> get

rememberLatchUpdate :: EvalL -> EvalP ()
rememberLatchUpdate x = tell (x,mempty)

rememberOutput :: EvalO -> EvalP ()
rememberOutput x = tell (mempty,[x])

liftBuildIOP :: BuildIO a -> EvalP a
liftBuildIOP = lift

liftBuildP :: Build a -> EvalP a
liftBuildP = liftBuildIOP . liftBuild


