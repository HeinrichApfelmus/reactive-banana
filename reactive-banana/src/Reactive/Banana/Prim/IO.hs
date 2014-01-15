{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.IO where

import           Data.Functor
import           Data.Unique.Really
import qualified Data.Vault.Strict  as Strict
import qualified Data.Vault.Lazy    as Lazy
import           System.IO.Unsafe             (unsafePerformIO)

import Reactive.Banana.Prim.Combinators  (mapP)
import Reactive.Banana.Prim.Dependencies (maybeContinue)
import Reactive.Banana.Prim.Evaluation   (step)
import Reactive.Banana.Prim.Plumbing
import Reactive.Banana.Prim.Types

debug s = id

{-----------------------------------------------------------------------------
    Primitives connecting to the outside world
------------------------------------------------------------------------------}
-- | Create a new pulse in the network and a function to trigger it.
--
-- Together with 'addHandler', this function can be used to operate with
-- pulses as with standard callback-based events.
newInput :: Lazy.Key a -> Build (Pulse a, a -> Step)
newInput key = debug "newInput" $ unsafePerformIO $ do
    uid <- newUnique
    let pulse = Pulse
            { evaluateP = maybeContinue <$> readPulseP pulse
            , getValueP = Lazy.lookup key
            , uidP      = uid
            }
    let inputs a = (Lazy.insert key a Lazy.empty, [P pulse])
    return $ return (pulse, step . inputs)

-- | Register a handler to be executed whenever a pulse occurs.
addHandler :: Pulse a -> (a -> IO ()) -> Build ()
addHandler p1 f = do
    p2 <- mapP (const . f) p1
    addOutput p2

-- | Register a handler that reads an updated latch value
-- whenever a pulse occurs.
addHandlerLatch :: Pulse () -> Latch a -> (a -> IO ()) -> Build ()
addHandlerLatch p1 l f = do
    p2 <- mapP (const $ f . getValueL l) p1
    addOutput p2

-- | Read the value of a 'Latch' at a particular moment in time.
readLatch :: Latch a -> Build a
readLatch = readLatchB
