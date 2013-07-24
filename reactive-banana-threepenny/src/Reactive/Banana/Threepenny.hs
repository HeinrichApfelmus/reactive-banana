{-----------------------------------------------------------------------------
    reactive-banana-threepenny
------------------------------------------------------------------------------}
{-# LANGUAGE ExistentialQuantification #-}

module Reactive.Banana.Threepenny (
    -- * Synopsis
    -- | Utility functions for interfacing with "Graphics.UI.Threepenny".
    -- Note: Useful, but I haven't done any serious design work on these.
    
    -- * General
    event, behavior, sink,
    module Reactive.Banana.Frameworks,
    
    -- * Specific widgets
    eventValue, behaviorValue,
    ) where

import Control.Monad (void)

import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (Event)

{-----------------------------------------------------------------------------
    General
------------------------------------------------------------------------------}
-- | Obtain an event for an element.
event :: Frameworks t => (x -> UI.Event a) -> x -> Moment t (Event t a)
event e widget = fromAddHandler . register $ e widget

-- | Behavior from an attribute.
-- Uses 'fromPoll', so may behave as you expect.
behavior :: Frameworks t => UI.ReadWriteAttr x i o -> x -> Moment t (Behavior t o)
behavior widget attr = fromPoll $ get widget attr

-- | "Animate" an attribute with a behavior.
sink :: Frameworks t
    => UI.ReadWriteAttr x i o -> Behavior t i -> Moment t x -> Moment t ()
sink attr b mx = do
    x <- mx
    i <- initial b
    liftIOLater $ void $ return x # set attr i
    e <- changes b
    reactimate $ (\i -> void $ return x # set attr i) <$> e 

{-----------------------------------------------------------------------------
    Specific widgets
------------------------------------------------------------------------------}
-- | Event that occurs when the /user/ changed the value of the input element.
eventValue :: Frameworks t => UI.Element -> Moment t (Event t String)
eventValue = event $ \widget ->
    UI.mapIO (const $ get value widget) (domEvent "livechange" widget)

-- | Behavior corresponding to user input in the element.
behaviorValue :: Frameworks t => UI.Element -> String -> Moment t (Behavior t String)
behaviorValue w s = stepper s <$> eventValue w

