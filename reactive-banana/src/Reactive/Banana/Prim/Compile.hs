{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.Compile where

import Data.IORef
import Reactive.Banana.Prim.IO
import Reactive.Banana.Prim.Plumbing
import Reactive.Banana.Prim.Types

{-----------------------------------------------------------------------------
   Compilation
------------------------------------------------------------------------------}
-- | Compile build instructions to a Network
compile :: BuildIO a -> IO (a, Network)
compile setup = runBuildIO emptyNetwork setup

{-----------------------------------------------------------------------------
    Simple interpretation
------------------------------------------------------------------------------}
-- | 
interpret :: (Pulse a -> BuildIO (Pulse b)) -> [Maybe a] -> IO [Maybe b]
interpret f xs = do
    o <- newIORef Nothing    
    let network = do
            (pin, sin) <- liftBuild $ newInputPulse
            pout       <- f pin
            liftBuild $ addHandler pout (writeIORef o . Just)
            return sin
    
    -- compile initial network
    (sin, state) <- compile network

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


