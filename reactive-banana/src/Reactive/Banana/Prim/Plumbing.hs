{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Reactive.Banana.Prim.Plumbing where

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.RWS
import qualified Control.Monad.Trans.State as State
import           Data.Function
import           Data.Functor
import           Data.Functor.Identity
import           Data.List
import           Data.Monoid
import           Data.Unique.Really
import qualified Data.Vault.Lazy           as Lazy
import           System.IO.Unsafe                  (unsafePerformIO)

import           Reactive.Banana.Prim.Cached                (HasCache(..))
import qualified Reactive.Banana.Prim.Dated        as Dated
import qualified Reactive.Banana.Prim.Dependencies as Deps
import           Reactive.Banana.Prim.Types

{-----------------------------------------------------------------------------
    Build primitive pulses and latches
------------------------------------------------------------------------------}
-- | Make 'Pulse' from evaluation function
newPulse :: String -> EvalP (Maybe a) -> Build (Pulse a)
newPulse name eval = unsafePerformIO $ do
    key <- Lazy.newKey
    uid <- newUnique
    return $ do
        let write = maybe (return Deps.Done) ((Deps.Children <$) . writePulseP key)
        return $ Pulse
            { evaluateP = {-# SCC evaluateP #-} write =<< eval
            , getValueP = Lazy.lookup key
            , uidP      = uid
            , nameP     = name
            }

-- | 'Pulse' that never fires.
neverP :: Build (Pulse a)
neverP = unsafePerformIO $ do
    uid <- newUnique
    return $ return $ Pulse
        { evaluateP = return Deps.Done
        , getValueP = const Nothing
        , uidP      = uid
        , nameP     = "neverP"
        }

-- | Make new 'Latch' that can be updated.
newLatch :: a -> Build (Pulse a -> Build (), Latch a)
newLatch a = unsafePerformIO $ do
    key <- Dated.newKey
    uid <- newUnique
    return $ do
        let
            write time   = maybe mempty (Endo . Dated.update' key time)
            latchWrite p = LatchWrite
                { evaluateL = {-# SCC evaluateL #-} do
                    time <- lift $ nTime <$> get
                    write (Dated.next time) <$> readPulseP p
                , uidL      = uid
                }
            updateOn p   = P p `addChild` L (latchWrite p)
        return
            (updateOn, Latch { getValueL = Dated.findWithDefault a key })

-- | Make a new 'Latch' that caches a previous computation
cachedLatch :: Dated.Dated (Dated.Box a) -> Latch a
cachedLatch eval = unsafePerformIO $ do
    key <- Dated.newKey
    return $ Latch { getValueL = {-# SCC getValueL #-} Dated.cache key eval }

-- | Add a new output that depends on a 'Pulse'.
--
-- TODO: Return function to unregister the output again.
addOutput :: Pulse EvalO -> Build ()
addOutput p = unsafePerformIO $ do
    uid <- newUnique
    return $ do
        pos <- grOutputCount . nGraph <$> get
        let o = Output
                { evaluateO = {-# SCC evaluateO #-} maybe nop id <$> readPulseP p
                , uidO      = uid
                , positionO = pos
                }
        modify $ updateGraph $ updateOutputCount $ (+1)
        P p `addChild` O o

{-----------------------------------------------------------------------------
    Build monad - add and delete nodes from the graph
------------------------------------------------------------------------------}
runBuildIO :: Network -> BuildIO a -> IO (a, Network)
runBuildIO s1 m = {-# SCC runBuildIO #-} do
    (a,s2,liftIOLaters) <- runRWST m () s1
    sequence_ liftIOLaters          -- execute late IOs
    return (a,s2)

-- Lift a pure  Build  computation into any monad.
-- See note [BuildT]
liftBuild :: Monad m => Build a -> BuildT m a
liftBuild m = RWST $ \r s -> return . runIdentity $ runRWST m r s

readLatchB :: Latch a -> Build a
readLatchB latch = state $ \network ->
    let (a,v) = Dated.runDated (getValueL latch) (nLatchValues network)
    in  (Dated.unBox a, network { nLatchValues = v } )

alwaysP :: Build (Pulse ())
alwaysP = grAlwaysP . nGraph <$> get

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
runEvalP :: Lazy.Vault -> EvalP (EvalL, [(Position, EvalO)])
    -> BuildIO (Lazy.Vault, EvalL, EvalO)
runEvalP pulse m = do
        ((wl,wo),s) <- State.runStateT m pulse
        return (s,wl, sequence_ <$> sequence (sortOutputs wo))
    where
    sortOutputs = map snd . sortBy (compare `on` fst)

readLatchP :: Latch a -> EvalP a
readLatchP = {-# SCC readLatchP #-} lift . liftBuild . readLatchB

readLatchFutureP :: Latch a -> EvalP (Future a)
readLatchFutureP latch = State.state $ \s -> (Dated.unBox <$> getValueL latch,s)

writePulseP :: Lazy.Key a -> a -> EvalP ()
writePulseP key a = {-# SCC writePulseP #-} State.modify $ Lazy.insert key a

readPulseP :: Pulse a -> EvalP (Maybe a)
readPulseP pulse = {-# SCC readPulseP #-} getValueP pulse <$> State.get

liftBuildIOP :: BuildIO a -> EvalP a
liftBuildIOP = lift

liftBuildP :: Build a -> EvalP a
liftBuildP = liftBuildIOP . liftBuild


