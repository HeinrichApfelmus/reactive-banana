{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Internal0.IO where

import Data.IORef
import Data.Unique.Really
import qualified Data.Vault.Strict as Vault
import System.IO.Unsafe

import Reactive.Banana.Internal0.Combinators (mapP)
import Reactive.Banana.Internal0.Evaluation  (step)
import Reactive.Banana.Internal0.Monads
import Reactive.Banana.Internal0.Types

{-----------------------------------------------------------------------------
    Simple interpretation
------------------------------------------------------------------------------}
interpret :: (Pulse a -> BuildIO (Pulse b)) -> [Maybe a] -> IO [Maybe b]
interpret f xs = do
    o <- newIORef Nothing    
    let network = do
            (pin, sin) <- liftBuild $ newPulse
            pout       <- f pin
            liftBuild $ addHandler pout (writeIORef o . Just)
            return sin
    
    -- compile initial network
    (sin, state) <- runBuildIO emptyState network

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

{-----------------------------------------------------------------------------
    Creating events and handlers
------------------------------------------------------------------------------}

-- | Create a new pulse in the network and a function to trigger it.
--
-- Together with 'addHandler', this function can be used to operate with
-- pulses as with standard callback-based events.
newPulse :: Build (Pulse a, a -> EvalGraph (IO ()))
newPulse = debug "newPulse" $ unsafePerformIO $ do
    key <- Vault.newKey
    uid <- newUnique
    let pulse = Pulse
            { evaluateP = return ()
            , getValueP = Vault.lookup key
            , uidP      = uid
            }
    let inputs a = (Vault.insert key a Vault.empty, [P pulse])
    return $ return (pulse, step . inputs)


-- | Register a handler to be executed whenever a pulse occurs.
--
-- Together with 'addHandler', this function can be used to operate with
-- pulses as with standard callback-based events.
addHandler :: Pulse a -> (a -> IO ()) -> Build ()
addHandler p1 f = do
    p2 <- mapP f p1
    addOutput p2

-- | Read the value of a 'Latch' at a particular moment in time.
readLatch :: Latch a -> Build a
readLatch = readLatchB

