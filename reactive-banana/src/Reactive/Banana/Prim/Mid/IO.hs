{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.Mid.IO where

import Control.Monad.IO.Class
    ( liftIO )
import qualified Data.Vault.Lazy        as Lazy

import Reactive.Banana.Prim.Mid.Combinators (mapP)
import Reactive.Banana.Prim.Mid.Evaluation  (step)
import Reactive.Banana.Prim.Mid.Plumbing
import Reactive.Banana.Prim.Mid.Types
import qualified Reactive.Banana.Prim.Low.Ref as Ref

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
    _key   <- liftIO Lazy.newKey
    nodeP  <- liftIO $ Ref.new $ P $ PulseD
        { _keyP      = _key
        , _seenP     = agesAgo
        , _evalP     = readPulseP pulse    -- get its own value
        , _nameP     = "newInput"
        }
    let pulse = Pulse{_key,_nodeP=nodeP}
    -- Also add the  alwaysP  pulse to the inputs.
    let run :: a -> Step
        run a = step ([nodeP, _nodeP always], Lazy.insert _key (Just a) Lazy.empty)
    pure (pulse, run)

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
