{-----------------------------------------------------------------------------
    reactive-banana-gtk
------------------------------------------------------------------------------}
{-# LANGUAGE ExistentialQuantification #-}

module Reactive.Banana.GTK (
    -- * Synopsis
    -- | Utility functions for interfacing with Gtk.
    
    -- * General
    Attr'(..), sink,

    -- * Mapping signal to AddHandler
    buttonActivatedToAddHandler
    ) where

import Graphics.UI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks

data Attr' t w = forall a. (Attr w a) :== Behavior t a

infix 0 :==

sink :: Frameworks t => w -> [Attr' t w] -> Moment t ()
sink widget = mapM_ sink1
    where 
        sink1 (attr :== b) = do
            x <- initial b
            liftIOLater $ set widget [attr := x]
            e <- changes b
            reactimate' $ (fmap $ \x -> set widget [attr := x]) <$> e

buttonActivatedToAddHandler :: ButtonClass self => self -> IO (AddHandler ())
buttonActivatedToAddHandler button = do
    (addHandler, addEvent) <- newAddHandler
    on button buttonActivated (addEvent ())
    return addHandler
