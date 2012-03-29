{-----------------------------------------------------------------------------
    reactive-banana-wx
------------------------------------------------------------------------------}
{-# LANGUAGE ExistentialQuantification #-}

module Reactive.Banana.WX (
    -- * Synopsis
    -- | Utility functions for interfacing with wxHaskell.
    -- Note: Useful, but I haven't done any serious design work on these.
    
    -- * General
    event1, event0, behavior,
    Prop'(..), sink,
        
    -- * Utilities
    event1ToAddHandler, event0ToEvent1,
    mapIO,
    ) where

import Reactive.Banana
import qualified Graphics.UI.WX as WX
import Graphics.UI.WX  hiding (Event, Attr)
import Graphics.UI.WXCore hiding (Event)
-- import Graphics.UI.WX (on, Prop(..))

{-----------------------------------------------------------------------------
    Connection with events and behaviors
------------------------------------------------------------------------------}
-- | Event with exactly one parameter.
event1 :: w -> WX.Event w (a -> IO ()) -> NetworkDescription t (Event t a)
event1 widget e = do
    addHandler <- liftIO $ event1ToAddHandler widget e
    fromAddHandler addHandler

    -- NOTE: Some events don't work, for instance   leftKey  and  rightKey
    -- "user error (WX.Events: the key event is write-only.)"
    -- That's because they are actually just derived from the  key  event
    -- Not sure what to do with this.

-- | Event without parameters.
event0 :: w -> WX.Event w (IO ()) -> NetworkDescription t (Event t ())
event0 widget = event1 widget . event0ToEvent1

-- | Behavior from an attribute.
-- Uses 'fromPoll', so may behave as you expect.
behavior :: w -> WX.Attr w a -> NetworkDescription t (Behavior t a)
behavior widget attr = fromPoll . liftIO $ get widget attr

-- | Variant of wx properties that accept a 'Behavior'.
data Prop' t w = forall a. (WX.Attr w a) :== Behavior t a

infixr 0 :==

-- | "Animate" a property with a behavior
sink :: w -> [Prop' t w] -> NetworkDescription t ()
sink widget props = mapM_ sink1 props
    where
    sink1 (attr :== b) = do
        x <- initial b
        liftIOLater $ set widget [attr := x]
        e <- changes b
        reactimate $ (\x -> set widget [attr := x]) <$> e

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
-- | Obtain an 'AddHandler' from a 'WX.Event'.
event1ToAddHandler :: w -> WX.Event w (a -> IO ()) -> IO (AddHandler a)
event1ToAddHandler widget e = do
    (addHandler, runHandlers) <- newAddHandler
    set widget [on e :~ \h x -> h x >> runHandlers x]
    return addHandler

-- | Obtain an 'AddHandler' from a 'WX.Event'.
event0ToEvent1 :: WX.Event w (IO ()) -> WX.Event w (() -> IO ())
event0ToEvent1 = mapEvent const (\_ e -> e ())

-- | Apply a function with side effects to an 'AddHandler'
mapIO :: (a -> IO b) -> AddHandler a -> AddHandler b
mapIO f addHandler = \h -> addHandler $ \x -> f x >>= h 
