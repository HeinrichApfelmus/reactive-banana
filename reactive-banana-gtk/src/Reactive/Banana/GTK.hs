{-----------------------------------------------------------------------------
    reactive-banana-gtk
------------------------------------------------------------------------------}
{-# LANGUAGE ExistentialQuantification #-}

module Reactive.Banana.GTK (
    -- * Synopsis
    -- | Utility functions for interfacing with Gtk.
    
    -- * General
    Attr'(..), sink,

    -- * Behavior
    behavior,

    -- * Event 
    eventEntry,

    -- * Mapping signal to AddHandler
    event0,
    signal0ToAddHandler,
    ) where

import Graphics.UI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks

data Attr' t w = forall a. (Attr w a) :== Behavior t a

infix 0 :==

-- | Animate a attribute from a Behavior
sink :: Frameworks t => w -> [Attr' t w] -> Moment t ()
sink widget = mapM_ sink1
    where 
        sink1 (attr :== b) = do
            x <- initial b
            liftIOLater $ set widget [attr := x]
            e <- changes b
            reactimate' $ (fmap $ \x -> set widget [attr := x]) <$> e

-- | Event with zero parameter from a Signal
event0 :: Frameworks t => w -> Signal w (IO ()) -> Moment t (Event t ())
event0 widget signal = do
    addHandler <- liftIO $ signal0ToAddHandler widget signal
    fromAddHandler addHandler

-- | Event that occure when the text in the entry change
eventEntry :: (Frameworks t, EntryClass e, EditableClass e) => e -> Moment t (Event t String)
eventEntry widget = do
    addHandler <- liftIO $ signal0ToAddHandler widget editableChanged
    fromAddHandler $ mapIO (const $ get widget entryText) addHandler

-- | Behavior from attribute.
behavior :: Frameworks t => w -> Attr w a -> Moment t (Behavior t a)
behavior widget attr = fromPoll $ get widget attr

-- | Obtain an 'AddHandler' from a Signal.
-- The callback in the signal take one parameter
signal1ToAddHandler :: w -> Signal w (a -> IO ()) -> IO (AddHandler a)
signal1ToAddHandler widget signal = do
    (addHandler, addEvent) <- newAddHandler
    on widget signal addEvent
    return addHandler

-- | Obtain an 'AddHandler' from a Signal.
-- The callback in the signal take one parameter
signal2ToAddHandler :: w -> Signal w (a -> b -> IO ()) -> IO (AddHandler (a,b))
signal2ToAddHandler widget signal = do
    (addHandler, addEvent) <- newAddHandler
    on widget signal (curry addEvent)
    return addHandler

-- | Obtain an 'AddHandler from a Signal.
signal0ToAddHandler :: w -> Signal w (IO ()) -> IO (AddHandler ())
signal0ToAddHandler widget signal = do
    (addHandler, addEvent) <- newAddHandler
    on widget signal (addEvent ())
    return addHandler 

