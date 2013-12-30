{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.Plumbing where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.RWS
import           Data.Functor
import           Data.Functor.Identity
import           Data.Monoid
import           Data.Unique.Really
import qualified Data.Vault.Strict       as Strict
import           System.IO.Unsafe                  (unsafePerformIO)

import qualified Reactive.Banana.Prim.Dependencies as Deps
import           Reactive.Banana.Prim.Types

{-----------------------------------------------------------------------------
    Build primitive pulses and latches
------------------------------------------------------------------------------}
-- | Make 'Pulse' from evaluation function
newPulse :: EvalP (Maybe a) -> Build (Pulse a)
newPulse eval = unsafePerformIO $ do
    key <- Strict.newKey
    uid <- {-# SCC "newPulse/newUnique" #-} newUnique
    return $ do
        let write = maybe (return ()) (writePulseP key)
        return $ Pulse
            { evaluateP = {-# SCC evaluateP #-} write =<< eval
            , getValueP = Strict.lookup key
            , uidP      = uid
            }

-- | 'Pulse' that never fires.
neverP :: Build (Pulse a)
neverP = unsafePerformIO $ do
    uid <- newUnique
    return $ return $ Pulse
        { evaluateP = return ()
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
                { evaluateL = write <$> readPulseP p
                , uidL      = uid
                }
            updateOn p   = L (latchWrite p) `dependOnNode` P p
        return
            (updateOn, Latch { getValueL = maybe a id . Strict.lookup key })

-- | Make a new 'Latch' that caches a previous computation
cachedLatch :: (Strict.Vault -> a) -> Latch a
cachedLatch eval = Latch
    { getValueL = eval  -- FIXME: Actually cache the computation!
    }


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

{-
-- Cache for memoizing valus.
-- The cache has to be lazy in both spine and values,
-- so that it can be used for recursion.
instance (MonadFix m, Functor m) => HasVault (BuildT m) where
    retrieve key = Vault.Lazy.lookup key . grCache . gsGraph <$> get
    write key a  = modifyGraph $ \g ->
        g { grCache = Vault.Lazy.insert key a (grCache g) }

-}

dependOn :: Pulse child -> Pulse parent -> Build ()
dependOn x y = dependOnNode (P x) (P y)

dependOnNode :: SomeNode -> SomeNode -> Build ()
dependOnNode x y = modify $ updateGraph $ updateDeps $ Deps.dependOn x y

-- TODO: Return function to unregister the output again.
addOutput :: Output -> Build ()
addOutput x = modify $ updateGraph $ updateOutputs $ (++ [x])

liftIOLater :: IO () -> Build ()
liftIOLater x = tell [x]

{-----------------------------------------------------------------------------
    EvalP - evaluate pulses
------------------------------------------------------------------------------}
runEvalP
    :: Strict.Vault -> Strict.Vault -> EvalP a
    -> BuildIO (Strict.Vault, EvalL)
runEvalP latch pulse m = do
    (_,s,w) <- runRWST m latch pulse
    return (s,w)

readLatchP :: Latch a -> EvalP a
readLatchP latch = lift $ getValueL latch . nLatchValues <$> get

readLatchFutureP :: Latch a -> EvalP a
readLatchFutureP latch = getValueL latch <$> ask

writePulseP :: Strict.Key a -> a -> EvalP ()
writePulseP key a = modify $ Strict.insert key a

readPulseP  :: Pulse a -> EvalP (Maybe a)
readPulseP pulse = getValueP pulse <$> get

rememberLatchUpdate :: EvalL -> EvalP ()
rememberLatchUpdate = tell

liftBuildIOP :: BuildIO a -> EvalP a
liftBuildIOP = lift

liftBuildP :: Build a -> EvalP a
liftBuildP = liftBuildIOP . liftBuild


