{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.Mid.Compile where

import Control.Exception
    ( evaluate )
import Data.Functor
    ( void )
import Data.IORef
    ( newIORef, readIORef, writeIORef )

import qualified Reactive.Banana.Prim.Low.GraphGC as GraphGC
import qualified Reactive.Banana.Prim.Low.OrderedBag as OB
import           Reactive.Banana.Prim.Mid.Combinators (mapP)
import           Reactive.Banana.Prim.Mid.Evaluation (applyDependencyChanges)
import           Reactive.Banana.Prim.Mid.IO
import           Reactive.Banana.Prim.Mid.Plumbing
import           Reactive.Banana.Prim.Mid.Types

{-----------------------------------------------------------------------------
   Compilation
------------------------------------------------------------------------------}
-- | Change a 'Network' of pulses and latches by
-- executing a 'BuildIO' action.
compile :: BuildIO a -> Network -> IO (a, Network)
compile m Network{nTime, nOutputs, nAlwaysP, nGraphGC} = do
    (a, dependencyChanges, os) <- runBuildIO (nTime, nAlwaysP) m

    applyDependencyChanges dependencyChanges nGraphGC
    let state2 = Network
            { nTime    = next nTime
            , nOutputs = OB.inserts nOutputs os
            , nAlwaysP
            , nGraphGC
            }
    return (a,state2)

emptyNetwork :: IO Network
emptyNetwork = do
  (alwaysP, _, _) <- runBuildIO undefined $ newPulse "alwaysP" (return $ Just ())
  nGraphGC <- GraphGC.new
  pure Network
    { nTime    = next beginning
    , nOutputs = OB.empty
    , nAlwaysP = alwaysP
    , nGraphGC
    }

{-----------------------------------------------------------------------------
    Testing
------------------------------------------------------------------------------}
-- | Simple interpreter for pulse/latch networks.
--
-- Mainly useful for testing functionality
--
-- Note: The result is not computed lazily, for similar reasons
-- that the 'sequence' function does not compute its result lazily.
interpret :: (Pulse a -> BuildIO (Pulse b)) -> [Maybe a] -> IO [Maybe b]
interpret f xs = do
    o   <- newIORef Nothing
    let network = do
            (pin, sin) <- liftBuild newInput
            pmid       <- f pin
            pout       <- liftBuild $ mapP return pmid
            liftBuild $ addHandler pout (writeIORef o . Just)
            return sin

    -- compile initial network
    (sin, state) <- compile network =<< emptyNetwork

    let go Nothing  s1 = return (Nothing,s1)
        go (Just a) s1 = do
            (reactimate,s2) <- sin a s1
            reactimate              -- write output
            ma <- readIORef o       -- read output
            writeIORef o Nothing
            return (ma,s2)

    fst <$> mapAccumM go state xs         -- run several steps

-- | Execute an FRP network with a sequence of inputs.
-- Make sure that outputs are evaluated, but don't display their values.
--
-- Mainly useful for testing whether there are space leaks.
runSpaceProfile :: Show b => (Pulse a -> BuildIO (Pulse b)) -> [a] -> IO ()
runSpaceProfile f xs = do
    let g = do
        (p1, fire) <- liftBuild newInput
        p2 <- f p1
        p3 <- mapP return p2                -- wrap into Future
        addHandler p3 (void . evaluate)
        return fire
    (step,network) <- compile g =<< emptyNetwork

    let fire x s1 = do
            (outputs, s2) <- step x s1
            outputs                     -- don't forget to execute outputs
            return ((), s2)

    mapAccumM_ fire network xs

-- | 'mapAccum' for a monad.
mapAccumM :: Monad m => (a -> s -> m (b,s)) -> s -> [a] -> m ([b],s)
mapAccumM f s0 = go s0 []
  where
    go s1 bs []     = pure (reverse bs,s1)
    go s1 bs (x:xs) = do
        (b,s2) <- f x s1
        go s2 (b:bs) xs

-- | Strict 'mapAccum' for a monad. Discards results.
mapAccumM_ :: Monad m => (a -> s -> m (b,s)) -> s -> [a] -> m ()
mapAccumM_ _ _   []     = return ()
mapAccumM_ f !s0 (x:xs) = do
    (_,s1) <- f x s0
    mapAccumM_ f s1 xs
