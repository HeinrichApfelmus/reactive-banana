{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.Compile where

import           Data.IORef
import qualified Data.Vault.Strict             as Strict
import           Reactive.Banana.Prim.IO
import           Reactive.Banana.Prim.Plumbing
import           Reactive.Banana.Prim.Types

{-----------------------------------------------------------------------------
   Compilation
------------------------------------------------------------------------------}
-- | Change a 'Network' of pulses and latches by 
-- executing a 'BuildIO' action.
compile :: BuildIO a -> Network -> IO (a, Network)
compile = flip runBuildIO

{-----------------------------------------------------------------------------
    Simple interpretation
------------------------------------------------------------------------------}
-- | Simple interpreter for pulse/latch networks.
--
-- Mainly useful for testing.
interpret :: (Pulse a -> BuildIO (Pulse b)) -> [Maybe a] -> IO [Maybe b]
interpret f xs = do
    key <- Strict.newKey
    o   <- newIORef Nothing
    let network = do
            (pin, sin) <- liftBuild $ newInput key
            pout       <- f pin
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


mapAccumM :: Monad m => (a -> s -> m (b,s)) -> s -> [a] -> m [b]
mapAccumM _ _  []     = return []
mapAccumM f s0 (x:xs) = do
    (b,s1) <- f x s0
    bs     <- mapAccumM f s1 xs
    return (b:bs)


