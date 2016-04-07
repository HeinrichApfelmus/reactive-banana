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
    module Reactive.Banana.Frameworks,
    
    -- * Specific widgets
    eventText, behaviorText, eventSelection,

    -- * Mouse events
    filterModifiers,
    eventMouseMotion, eventMouseEnter, eventMouseLeave,
    eventLeftDown, eventLeftUp, eventLeftDClick, eventLeftDrag,
    eventRightDown, eventRightUp, eventRightDClick, eventRightDrag,
    eventMiddleDown, eventMiddleUp, eventMiddleDClick, eventMiddleDrag,
    eventMouseWheelDown, eventMouseWheelUp,

    -- * Utilities
    event1ToAddHandler, event0ToEvent1,
    ) where

import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified Graphics.UI.WX     as WX
import           Graphics.UI.WX               hiding (Event, Attr)
import qualified Graphics.UI.WXCore as WXCore
-- import Graphics.UI.WX (on, Prop(..))

{-----------------------------------------------------------------------------
    General
------------------------------------------------------------------------------}
-- | Event with exactly one parameter.
event1 :: w -> WX.Event w (a -> IO ()) -> MomentIO (Event a)
event1 widget e = do
    addHandler <- liftIO $ event1ToAddHandler widget e
    fromAddHandler addHandler

    -- NOTE: Some events don't work, for instance   leftKey  and  rightKey
    -- "user error (WX.Events: the key event is write-only.)"
    -- That's because they are actually just derived from the  key  event
    -- Not sure what to do with this.

-- | Event without parameters.
event0 :: w -> WX.Event w (IO ()) -> MomentIO (Event ())
event0 widget = event1 widget . event0ToEvent1

-- | Behavior from an attribute.
-- Uses 'fromPoll', so may behave as you expect.
behavior :: w -> WX.Attr w a -> MomentIO (Behavior a)
behavior widget attr = fromPoll $ get widget attr

-- | Variant of wx properties that accept a 'Behavior'.
data Prop' w = forall a. (WX.Attr w a) :== Behavior a

infixr 0 :==

-- | "Animate" a property with a behavior
sink :: w -> [Prop' w] -> MomentIO ()
sink widget = mapM_ sink1
    where
    sink1 (attr :== b) = do
        x <- valueBLater b
        liftIOLater $ set widget [attr := x]
        e <- changes b
        reactimate' $ (fmap $ \x -> set widget [attr := x]) <$> e

{-----------------------------------------------------------------------------
    Specific widgets
------------------------------------------------------------------------------}
-- | Event that occurs when the /user/ changed
-- the text in text edit widget.
eventText :: TextCtrl w -> MomentIO (Event String)
eventText w = do
    addHandler <- liftIO $ event1ToAddHandler w (event0ToEvent1 onText)
    fromAddHandler
        $ filterIO (const $ WXCore.textCtrlIsModified w)
        $ mapIO (const $ get w text) addHandler

onText :: WX.Event (WXCore.Control a) (IO ())
onText = WX.newEvent "onText" WXCore.controlGetOnText WXCore.controlOnText

-- observe "key up" events (many thanks to Abu Alam)
-- this should probably be in the wxHaskell library
-- keyboardUp  :: WX.Event (Window a) (EventKey -> IO ())
-- keyboardUp  = WX.newEvent "keyboardUp" WXCore.windowGetOnKeyUp WXCore.windowOnKeyUp

-- | Behavior corresponding to user input the text field.
behaviorText :: TextCtrl w -> String -> MomentIO (Behavior String)
behaviorText w s = stepper s =<< eventText w

-- | Event that occurs when the /user/ changed
-- the selection marker in a list box widget.
eventSelection :: SingleListBox b -> MomentIO (Event Int)
eventSelection w = do
    liftIO $ fixSelectionEvent w
    addHandler <- liftIO $ event1ToAddHandler w (event0ToEvent1 select)
    fromAddHandler $ mapIO (const $ get w selection) addHandler

-- Fix @select@ event not being fired when items are *un*selected.
fixSelectionEvent :: (Selecting w, Reactive w, Selection w) => w -> IO ()
fixSelectionEvent listbox =
    set listbox [ on unclick := handler ]
    where
    handler _ = do
        propagateEvent
        s <- get listbox selection
        when (s == -1) $ get listbox (on select) >>= id

{-----------------------------------------------------------------------------
    Mouse events
------------------------------------------------------------------------------}

-- | Filter for events matching the given predicate on the 'WX.Modifiers'.
filterModifiers :: (WX.Modifiers -> Bool) -> Event (a, WX.Modifiers) -> Event a
filterModifiers f = fmap fst . filterE (f . snd)

-- | Event that occurs when the mouse moves.
eventMouseMotion :: Reactive w => w -> MomentIO (Event (WX.Point, WX.Modifiers))
eventMouseMotion = eventMouse f
    where
    f MouseMotion{} = True
    f _ = False

-- | Event that occurs when the mouse enters the boundary.
eventMouseEnter :: Reactive w => w -> MomentIO (Event (WX.Point, WX.Modifiers))
eventMouseEnter = eventMouse f
    where
    f MouseEnter{} = True
    f _ = False

-- | Event that occurs when the mouse leaves the boundary.
eventMouseLeave :: Reactive w => w -> MomentIO (Event (WX.Point, WX.Modifiers))
eventMouseLeave = eventMouse f
    where
    f MouseLeave{} = True
    f _ = False

-- | Event that occurs when the left mouse button is pressed.
eventLeftDown :: Reactive w => w -> MomentIO (Event (WX.Point, WX.Modifiers))
eventLeftDown = eventMouse f
    where
    f MouseLeftDown{} = True
    f _ = False

-- | Event that occurs when the left mouse button is released.
eventLeftUp :: Reactive w => w -> MomentIO (Event (WX.Point, WX.Modifiers))
eventLeftUp = eventMouse f
    where
    f MouseLeftUp{} = True
    f _ = False

-- | Event that occurs when the left mouse button is double-clicked.
eventLeftDClick :: Reactive w => w -> MomentIO (Event (WX.Point, WX.Modifiers))
eventLeftDClick = eventMouse f
    where
    f MouseLeftDClick{} = True
    f _ = False

-- | Event that occurs when the mouse is dragged around with the left button
-- pressed.
eventLeftDrag :: Reactive w => w -> MomentIO (Event (WX.Point, WX.Modifiers))
eventLeftDrag = eventMouse f
    where
    f MouseLeftDClick{} = True
    f _ = False

-- | Event that occurs when the right mouse button is pressed.
eventRightDown :: Reactive w => w -> MomentIO (Event (WX.Point, WX.Modifiers))
eventRightDown = eventMouse f
    where
    f MouseRightDown{} = True
    f _ = False

-- | Event that occurs when the right mouse button is released.
eventRightUp :: Reactive w => w -> MomentIO (Event (WX.Point, WX.Modifiers))
eventRightUp = eventMouse f
    where
    f MouseRightUp{} = True
    f _ = False

-- | Event that occurs when the right mouse button is double-clicked.
eventRightDClick :: Reactive w => w -> MomentIO (Event (WX.Point, WX.Modifiers))
eventRightDClick = eventMouse f
    where
    f MouseRightDClick{} = True
    f _ = False

-- | Event that occurs when the mouse is dragged around with the right button
-- pressed.
eventRightDrag :: Reactive w => w -> MomentIO (Event (WX.Point, WX.Modifiers))
eventRightDrag = eventMouse f
    where
    f MouseRightDClick{} = True
    f _ = False

-- | Event that occurs when the middle mouse button is pressed.
eventMiddleDown :: Reactive w => w -> MomentIO (Event (WX.Point, WX.Modifiers))
eventMiddleDown = eventMouse f
    where
    f MouseMiddleDown{} = True
    f _ = False

-- | Event that occurs when the middle mouse button is released.
eventMiddleUp :: Reactive w => w -> MomentIO (Event (WX.Point, WX.Modifiers))
eventMiddleUp = eventMouse f
    where
    f MouseMiddleUp{} = True
    f _ = False

-- | Event that occurs when the middle mouse button is double-clicked.
eventMiddleDClick :: Reactive w => w -> MomentIO (Event (WX.Point, WX.Modifiers))
eventMiddleDClick = eventMouse f
    where
    f MouseMiddleDClick{} = True
    f _ = False

-- | Event that occurs when the mouse is dragged around with the middle button
-- pressed.
eventMiddleDrag :: Reactive w => w -> MomentIO (Event (WX.Point, WX.Modifiers))
eventMiddleDrag = eventMouse f
    where
    f MouseMiddleDClick{} = True
    f _ = False

-- | Event that occurs when the mouse wheel is scrolled downward.
eventMouseWheelDown :: Reactive w => w -> MomentIO (Event (WX.Point, WX.Modifiers))
eventMouseWheelDown = eventMouse f
    where
    f (MouseWheel down _ _) = down
    f _ = False

-- | Event that occurs when the mouse wheel is scrolled upward.
eventMouseWheelUp :: Reactive w => w -> MomentIO (Event (WX.Point, WX.Modifiers))
eventMouseWheelUp = eventMouse f
    where
    f (MouseWheel down _ _) = not down
    f _ = False

eventMouse :: Reactive w => (WX.EventMouse -> Bool) -> w -> MomentIO (Event (WX.Point, WX.Modifiers))
eventMouse f w = do
    event <- event1 w mouse
    return $ (\x -> (WX.mousePos x, WX.mouseModifiers x)) <$> filterE f event

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

