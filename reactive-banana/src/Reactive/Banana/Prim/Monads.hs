{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Reactive.Banana.Prim.Monads where

import Control.Monad.Fix
import Control.Monad.Trans.RWS.Lazy
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Functor

import qualified Data.Vault.Strict as Vault
import qualified Data.Vault.Lazy   as Vault.Lazy

import qualified Reactive.Banana.Prim.DependencyGraph as Deps
import           Reactive.Banana.Prim.Types
import           Reactive.Banana.Internal1.Cached          (HasVault(..))



{-----------------------------------------------------------------------------
    EvalP - evaluate pulses
------------------------------------------------------------------------------}

runEvalP :: Values -> Values -> EvalP a -> BuildIO Values
runEvalP latch pulse m = do
    (_,s,_) <- runRWST m latch pulse
    return s

readLatchP  :: Latch a -> EvalP a
readLatchP latch = lift $ getValueL latch . gsLatchValues <$> get

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

{-----------------------------------------------------------------------------
    EvalL - evaluate latches
------------------------------------------------------------------------------}

runEvalL :: Values -> Values -> EvalL () -> Values
runEvalL pulse latch1 m = let (_,latch2,_) = runRWS m pulse latch1 in latch2

readLatchL  :: Latch a -> EvalL a
readLatchL latch = getValueL latch <$> get

writeLatchL :: Key a -> a -> EvalL ()
writeLatchL key a = modify $ Vault.insert key a

readPulseL :: Pulse a -> EvalL (Maybe a)
readPulseL pulse = getValueP pulse <$> ask

{-----------------------------------------------------------------------------
    Build monad - add and delete nodes from the graph
------------------------------------------------------------------------------}

-- Lift a pure  Build  computation into any monad.
-- See note [BuildT]
liftBuild :: Monad m => Build a -> BuildT m a
liftBuild m = RWST $ \r s -> return . runIdentity $ runRWST m r s

runBuildIO :: GraphState -> BuildIO a -> IO (a, GraphState)
runBuildIO s1 m = do
    (a,s2,liftIOLaters) <- runRWST m () s1
    sequence_ liftIOLaters          -- execute late IOs
    return (a,s2)

modifyGraph f = modify $ \s -> s { gsGraph = f (gsGraph s) }

addLatchValue :: Key a -> a -> Build ()
addLatchValue key a = modify $ \s ->
    s { gsLatchValues = Vault.insert key a (gsLatchValues s) }

readLatchB :: Latch a -> Build a
readLatchB latch = getValueL latch . gsLatchValues <$> get

-- Cache for memoizing valus.
-- The cache has to be lazy in both spine and values,
-- so that it can be used for recursion.
instance (MonadFix m, Functor m) => HasVault (BuildT m) where
    retrieve key = Vault.Lazy.lookup key . grCache . gsGraph <$> get
    write key a  = modifyGraph $ \g ->
        g { grCache = Vault.Lazy.insert key a (grCache g) }


-- Add a dependency.
dependOn :: SomeNode -> SomeNode -> Build ()
dependOn x y = modifyGraph $ \g -> g { grDeps = Deps.dependOn x y $ grDeps g }

dependOns :: SomeNode -> [SomeNode] -> Build ()
dependOns x = mapM_ $ dependOn x

-- FIXME: Return function to unregister the output again.
addOutput :: Output -> Build ()
addOutput x = modifyGraph $ \g -> g { grOutputs = grOutputs g ++ [x] }

liftIOLater :: IO () -> Build ()
liftIOLater x = tell [x]

