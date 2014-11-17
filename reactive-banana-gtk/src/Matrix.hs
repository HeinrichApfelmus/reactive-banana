{-# LANGUAGE ScopedTypeVariables #-}

import           Graphics.UI.Gtk
import           Data.Vector
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Banana.GTK



main :: IO ()
main = do
    initGUI
    window <- windowNew
    entry <- entryNew
    label <- labelNew (Just "Enter text")

    -- Layout
    hbox <- hBoxNew True 0
    containerAdd hbox entry
    containerAdd hbox label

    set window [ containerBorderWidth := 10, containerChild := hbox]
    
    -- Network
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription  = do
            entryE <- eventEntry entry
            sink label [labelLabel :== stepper "" entryE]

    network <- compile networkDescription
    actuate network
    onDestroy window mainQuit
    widgetShowAll window

    mainGUI
