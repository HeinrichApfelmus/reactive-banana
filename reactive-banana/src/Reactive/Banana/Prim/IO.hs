{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo #-}
module Reactive.Banana.Prim.IO where

import Control.Monad.IO.Class
import Data.IORef
import Data.Functor

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
newInput :: void -> Build (Pulse a, a -> Step)
newInput key = mdo
    time  <- getTimeB
    pulse <- liftIO $ newIORef $ Pulse
        { _seenP     = time
        , _valueP    = Nothing
        , _evalP     = _valueP <$> get pulse    -- get its own value
        , _childrenP = []
        , _parentsP  = []
        , _levelP    = ground
        , _nameP     = "newInput"
        }
    let run a network = do
            modify pulse $ set valueP (Just a)
            a <- step [P pulse] network
            modify pulse $ set valueP Nothing
            return a
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
