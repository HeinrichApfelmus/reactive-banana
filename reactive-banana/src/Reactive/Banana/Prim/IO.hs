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
newInput key = unsafePerformIO $ do
    uid <- newUnique
    let pulse = Pulse
            { evaluateP = maybe Done Pure <$> Lazy.lookup key
            , getValueP = Lazy.lookup key
            , writeP    = const id
            , uidP      = uid
            , nameP     = "newInput"
            }
    return $ do
        always <- alwaysP
        let inputs a = (Lazy.insert key a Lazy.empty, [P pulse, P always])
        return (pulse, step . inputs)

-- | Register a handler to be executed whenever a pulse occurs.
--
-- The pulse may refer to future latch values.
addHandler :: Pulse (Future a) -> (a -> IO ()) -> Build ()
addHandler p1 f = do
    p2 <- mapP (fmap f) p1
    addOutput p2

-- | Read the value of a 'Latch' at a particular moment in time.
readLatch :: Latch a -> Build a
readLatch = readLatchB
