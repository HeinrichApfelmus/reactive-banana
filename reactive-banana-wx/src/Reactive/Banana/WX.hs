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
    
    -- * Specific widgets
    eventText, behaviorText, eventSelection,

    -- * Utilities
    event1ToAddHandler, event0ToEvent1,
    mapIO, filterAddHandler,
    ) where

import Reactive.Banana
import qualified Graphics.UI.WX as WX
import Graphics.UI.WX  hiding (Event, Attr)
import qualified Graphics.UI.WXCore as WXCore
-- import Graphics.UI.WX (on, Prop(..))

{-----------------------------------------------------------------------------
    General
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
    Specific widgets
------------------------------------------------------------------------------}
-- | Event that occurs when the /user/ changed
-- the text in text edit widget.
eventText :: TextCtrl w -> NetworkDescription t (Event t String)
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
behaviorText :: TextCtrl w -> String -> NetworkDescription t (Behavior t String)
behaviorText w s = stepper s <$> eventText w

-- | Event that occurs when the /user/ changed
-- the selection marker in a list box widget.
eventSelection :: SingleListBox b -> NetworkDescription t (Event t Int)
eventSelection w = do
    liftIO $ fixSelectionEvent w
    addHandler <- liftIO $ event1ToAddHandler w (event0ToEvent1 select)
    fromAddHandler $ mapIO (const $ get w selection) addHandler

-- Fix @select@ event not being fired when items are *un*selected.
fixSelectionEvent listbox =
    liftIO $ set listbox [ on unclick := handler ]
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

-- | Apply a function with side effects to an 'AddHandler'
mapIO :: (a -> IO b) -> AddHandler a -> AddHandler b
mapIO f addHandler = \h -> addHandler $ \x -> f x >>= h 

-- | Filter event occurrences that don't return 'True'.
filterAddHandler :: (a -> IO Bool) -> AddHandler a -> AddHandler a
filterAddHandler f addHandler = \h ->
    addHandler $ \x -> f x >>= \b -> if b then h x else return ()
