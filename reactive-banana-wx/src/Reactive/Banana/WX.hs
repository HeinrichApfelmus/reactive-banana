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

    -- * Utilities
    event1ToAddHandler, event0ToEvent1,
    mapIO, filterAddHandler,
    ) where

import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified Graphics.UI.WX as WX
import Graphics.UI.WX  hiding (Event, Attr)
import qualified Graphics.UI.WXCore as WXCore
-- import Graphics.UI.WX (on, Prop(..))

{-----------------------------------------------------------------------------
    General
------------------------------------------------------------------------------}
-- | Event with exactly one parameter.
event1 :: Frameworks t =>
    w -> WX.Event w (a -> IO ()) -> Moment t (Event t a)
event1 widget e = do
    addHandler <- liftIO $ event1ToAddHandler widget e
    fromAddHandler addHandler

    -- NOTE: Some events don't work, for instance   leftKey  and  rightKey
    -- "user error (WX.Events: the key event is write-only.)"
    -- That's because they are actually just derived from the  key  event
    -- Not sure what to do with this.

-- | Event without parameters.
event0 :: Frameworks t =>
    w -> WX.Event w (IO ()) -> Moment t (Event t ())
event0 widget = event1 widget . event0ToEvent1

-- | Behavior from an attribute.
-- Uses 'fromPoll', so may behave as you expect.
behavior :: Frameworks t =>
    w -> WX.Attr w a -> Moment t (Behavior t a)
behavior widget attr = fromPoll $ get widget attr

-- | Variant of wx properties that accept a 'Behavior'.
data Prop' t w = forall a. (WX.Attr w a) :== Behavior t a

infixr 0 :==

-- | "Animate" a property with a behavior
sink :: Frameworks t =>
    w -> [Prop' t w] -> Moment t ()
sink widget props = mapM_ sink1 props
    where
    sink1 (attr :== b) = do
        x <- initial b
        liftIOLater $ set widget [attr := x]
        e <- changes b
        reactimate $ (\x -> set widget [attr := x]) <$> e

{-----------------------------------------------------------------------------
    Specific widgets
------------------------------------------------------------------------------}
-- | Event that occurs when the /user/ changed
-- the text in text edit widget.
eventText :: Frameworks t =>
    TextCtrl w -> Moment t (Event t String)
eventText w = do
    addHandler <- liftIO $ event1ToAddHandler w (event0ToEvent1 onText)
    fromAddHandler
        $ filterAddHandler (const $ WXCore.textCtrlIsModified w)
        $ mapIO (const $ get w text) addHandler

onText :: WX.Event (WXCore.Control a) (IO ())
onText = WX.newEvent "onText" WXCore.controlGetOnText WXCore.controlOnText

-- observe "key up" events (many thanks to Abu Alam)
-- this should probably be in the wxHaskell library
-- keyboardUp  :: WX.Event (Window a) (EventKey -> IO ())
-- keyboardUp  = WX.newEvent "keyboardUp" WXCore.windowGetOnKeyUp WXCore.windowOnKeyUp

-- | Behavior corresponding to user input the text field.
behaviorText :: Frameworks t =>
    TextCtrl w -> String -> Moment t (Behavior t String)
behaviorText w s = stepper s <$> eventText w

-- | Event that occurs when the /user/ changed
-- the selection marker in a list box widget.
eventSelection :: Frameworks t =>
    SingleListBox b -> Moment t (Event t Int)
eventSelection w = do
    liftIO $ fixSelectionEvent w
    addHandler <- liftIO $ event1ToAddHandler w (event0ToEvent1 select)
    fromAddHandler $ mapIO (const $ get w selection) addHandler

-- Fix @select@ event not being fired when items are *un*selected.
fixSelectionEvent listbox =
    set listbox [ on unclick := handler ]
    where
    handler _ = do
        propagateEvent
        s <- get listbox selection
        when (s == -1) $ (get listbox (on select)) >>= id


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

