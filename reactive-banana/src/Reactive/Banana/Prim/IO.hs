{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo #-}
module Reactive.Banana.Prim.IO where

import Control.Monad.IO.Class
import Data.IORef
import Data.Functor
import qualified Data.Vault.Lazy as Lazy

import Reactive.Banana.Prim.Combinators  (mapP)
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
newInput :: Build (Pulse a, a -> Step)
newInput = mdo
    key   <- liftIO $ Lazy.newKey
    pulse <- liftIO $ newIORef $ Pulse
        { _keyP      = key
        , _seenP     = agesAgo
        , _evalP     = readPulseP pulse    -- get its own value
        , _childrenP = []
        , _parentsP  = []
        , _levelP    = ground
        , _nameP     = "newInput"
        }
    let run a = step ([P pulse], Lazy.insert key (Just a) Lazy.empty)
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
