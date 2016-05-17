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

    -- * Mouse event helpers
    mouseMotion, mouseEnter, mouseLeave,
    leftDown, leftUp, leftDClick, leftDrag,
    rightDown, rightUp, rightDClick, rightDrag,
    middleDown, middleUp, middleDClick, middleDrag,
    mouseWheel, mouseWheelDown, mouseWheelUp,

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

-- | Use a time-varying paint function for a widget,
-- but also make sure that the widget is redrawn whenever said function changes.
sinkPaint :: Paint w => w -> Behavior (DC () -> Rect -> IO ()) -> MomentIO ()
sinkPaint w b = do
    sink w [on paint :== b]
    e <- changes b
    reactimate $ repaint w <$ e

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
    Mouse event helpers
------------------------------------------------------------------------------}

-- | Return 'Just' in the case that the mouse moves.
mouseMotion :: WX.EventMouse -> Maybe (WX.Point, WX.Modifiers)
mouseMotion (MouseMotion point mod) = Just (point, mod)
mouseMotion _ = Nothing

-- | Return 'Just' in the case that the mouse enters the boundary.
mouseEnter :: WX.EventMouse -> Maybe (WX.Point, WX.Modifiers)
mouseEnter (MouseEnter point mod) = Just (point, mod)
mouseEnter _ = Nothing

-- | Return 'Just' in the case that the mouse leaves the boundary.
mouseLeave :: WX.EventMouse -> Maybe (WX.Point, WX.Modifiers)
mouseLeave (MouseLeave point mod) = Just (point, mod)
mouseLeave _ = Nothing

-- | Return 'Just' in the case that the left mouse button is pressed.
leftDown :: WX.EventMouse -> Maybe (WX.Point, WX.Modifiers)
leftDown (MouseLeftDown point mod) = Just (point, mod)
leftDown _ = Nothing

-- | Return 'Just' in the case that the left mouse button is released.
leftUp :: WX.EventMouse -> Maybe (WX.Point, WX.Modifiers)
leftUp (MouseLeftUp point mod) = Just (point, mod)
leftUp _ = Nothing

-- | Return 'Just' in the case that the left mouse button is double-clicked.
leftDClick :: WX.EventMouse -> Maybe (WX.Point, WX.Modifiers)
leftDClick (MouseLeftDClick point mod) = Just (point, mod)
leftDClick _ = Nothing

-- | Return 'Just' in the case that the mouse is dragged around with the left button
-- pressed.
leftDrag :: WX.EventMouse -> Maybe (WX.Point, WX.Modifiers)
leftDrag (MouseLeftDrag point mod) = Just (point, mod)
leftDrag _ = Nothing

-- | Return 'Just' in the case that the right mouse button is pressed.
rightDown :: WX.EventMouse -> Maybe (WX.Point, WX.Modifiers)
rightDown (MouseRightDown point mod) = Just (point, mod)
rightDown _ = Nothing

-- | Return 'Just' in the case that the right mouse button is released.
rightUp :: WX.EventMouse -> Maybe (WX.Point, WX.Modifiers)
rightUp (MouseRightUp point mod) = Just (point, mod)
rightUp _ = Nothing

-- | Return 'Just' in the case that the right mouse button is double-clicked.
rightDClick :: WX.EventMouse -> Maybe (WX.Point, WX.Modifiers)
rightDClick (MouseRightDClick point mod) = Just (point, mod)
rightDClick _ = Nothing

-- | Return 'Just' in the case that the mouse is dragged around with the right button
-- pressed.
rightDrag :: WX.EventMouse -> Maybe (WX.Point, WX.Modifiers)
rightDrag (MouseRightDrag point mod) = Just (point, mod)
rightDrag _ = Nothing

-- | Return 'Just' in the case that the middle mouse button is pressed.
middleDown :: WX.EventMouse -> Maybe (WX.Point, WX.Modifiers)
middleDown (MouseMiddleDown point mod) = Just (point, mod)
middleDown _ = Nothing

-- | Return 'Just' in the case that the middle mouse button is released.
middleUp :: WX.EventMouse -> Maybe (WX.Point, WX.Modifiers)
middleUp (MouseMiddleUp point mod) = Just (point, mod)
middleUp _ = Nothing

-- | Return 'Just' in the case that the middle mouse button is double-clicked.
middleDClick :: WX.EventMouse -> Maybe (WX.Point, WX.Modifiers)
middleDClick (MouseMiddleDClick point mod) = Just (point, mod)
middleDClick _ = Nothing

-- | Return 'Just' in the case that the mouse is dragged around with the middle button
-- pressed.
middleDrag :: WX.EventMouse -> Maybe (WX.Point, WX.Modifiers)
middleDrag (MouseMiddleDrag point mod) = Just (point, mod)
middleDrag _ = Nothing

-- | Return 'Just' in the case that the mouse wheel is scrolled.
mouseWheel :: WX.EventMouse -> Maybe (Bool, WX.Point, WX.Modifiers)
mouseWheel (MouseWheel down point mod) = Just (down, point, mod)
mouseWheel _ = Nothing

-- | Return 'Just' in the case that the mouse wheel is scrolled downward.
mouseWheelDown :: WX.EventMouse -> Maybe (WX.Point, WX.Modifiers)
mouseWheelDown (MouseWheel True point mod) = Just (point, mod)
mouseWheelDown _ = Nothing

-- | Return 'Just' in the case that the mouse wheel is scrolled upward.
mouseWheelUp :: WX.EventMouse -> Maybe (WX.Point, WX.Modifiers)
mouseWheelUp (MouseWheel False point mod) = Just (point, mod)
mouseWheelUp _ = Nothing

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
