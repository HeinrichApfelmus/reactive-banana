{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reactive.Banana.Prim.Low.IO where

import           Control.Monad.IO.Class
import qualified Data.Vault.Lazy        as Lazy

import Reactive.Banana.Prim.Mid.Combinators (mapP)
import Reactive.Banana.Prim.Low.Evaluation  (step)
import Reactive.Banana.Prim.Low.Plumbing
import Reactive.Banana.Prim.Low.Types
import Reactive.Banana.Prim.Low.Util

debug :: String -> a -> a
debug _ = id

{-----------------------------------------------------------------------------
    Primitives connecting to the outside world
------------------------------------------------------------------------------}
-- | Create a new pulse in the network and a function to trigger it.
--
-- Together with 'addHandler', this function can be used to operate with
-- pulses as with standard callback-based events.
newInput :: forall a. Build (Pulse a, a -> Step)
newInput = mdo
    always <- alwaysP
    key    <- liftIO Lazy.newKey
    pulse  <- liftIO $ mdo
        me <- newRef $ Pulse
            { _keyP      = key
            , _seenP     = agesAgo
            , _evalP     = readPulseP pulse    -- get its own value
            , _childrenP = []
            , _parentsP  = []
            , _levelP    = ground
            , _nameP     = "newInput"
            , _pulsePtr  = w
            }
        w <- mkWeakRefValue me (P me)
        return me
    -- Also add the  alwaysP  pulse to the inputs.
    let run :: a -> Step
        run a = step ([P pulse, P always], Lazy.insert key (Just a) Lazy.empty)
    return (pulse, run)

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
