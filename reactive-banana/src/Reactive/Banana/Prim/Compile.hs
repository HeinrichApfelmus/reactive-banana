{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.Compile where

import           Data.Functor
import           Data.IORef
import           Reactive.Banana.Prim.Combinators
import           Reactive.Banana.Prim.IO
import           Reactive.Banana.Prim.Plumbing
import           Reactive.Banana.Prim.Types

{-----------------------------------------------------------------------------
   Compilation
------------------------------------------------------------------------------}
-- | Change a 'Network' of pulses and latches by 
-- executing a 'BuildIO' action.
compile :: BuildIO a -> Network -> IO (a, Network)
compile m state1 = do
    let time1    = nTime state1
        outputs1 = nOutputs state1

    (a, topology, os) <- runBuildIO (nTime state1) m
    doit topology

    let state2 = Network { nTime = next time1, nOutputs = os ++ outputs1 }
    return (a,state2)

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
            (pin, sin) <- liftBuild $ newInput
            pmid       <- f pin
            pout       <- liftBuild $ mapP return pmid
            liftBuild $ addHandler pout (writeIORef o . Just)
            return sin
    
    -- compile initial network
    (sin, state) <- compile network emptyNetwork

    let go Nothing  s1 = return (Nothing,s1)
        go (Just a) s1 = do
            (reactimate,s2) <- sin a s1
            reactimate              -- write output
            ma <- readIORef o       -- read output
            writeIORef o Nothing
            return (ma,s2)
    
    mapAccumM go state xs         -- run several steps

-- | Execute an FRP network with a sequence of inputs, but discard results.
-- 
-- Mainly useful for testing whether there are space leaks. 
runSpaceProfile :: (Pulse a -> BuildIO void) -> [a] -> IO ()
runSpaceProfile f xs = do
    let g = do
        (p1, fire) <- liftBuild $ newInput
        f p1
        return fire
    (fire,network) <- compile g emptyNetwork
    
    mapAccumM_ fire network xs

-- | 'mapAccum' for a monad.
mapAccumM :: Monad m => (a -> s -> m (b,s)) -> s -> [a] -> m [b]
mapAccumM _ _  []     = return []
mapAccumM f s0 (x:xs) = do
    (b,s1) <- f x s0
    bs     <- mapAccumM f s1 xs
    return (b:bs)

-- | Strict 'mapAccum' for a monad. Discards results.
mapAccumM_ :: Monad m => (a -> s -> m (b,s)) -> s -> [a] -> m ()
mapAccumM_ _ _  []     = return ()
mapAccumM_ f s0 (x:xs) = do
    (_,s1) <- f x s0
    mapAccumM_ f s1 xs

