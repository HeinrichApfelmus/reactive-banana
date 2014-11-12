{-# LANGUAGE ScopedTypeVariables #-}

import Graphics.UI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GTK

main :: IO ()
main = do
    initGUI
    
    -- Element
    window <- windowNew
    frame <- frameNew
    buttonAdd <- buttonNew
    buttonSub <- buttonNew
    label <- labelNew (Just "0")

    -- Attribute
    set buttonAdd [buttonLabel := "Add"]
    set buttonSub [buttonLabel := "Sub"]
    set frame [frameLabel := "Counter"]

    -- Layout
    vbox <- vBoxNew True 2
    containerAdd vbox buttonAdd
    containerAdd vbox buttonSub
    
    hbox <- hBoxNew True 10
    containerAdd hbox vbox
    containerAdd hbox label
    containerAdd frame hbox

    set frame [ frameLabel := "Counter"]
    set window [ containerBorderWidth := 10, containerChild := frame ]

    handlerAdd <- buttonActivatedToAddHandler buttonAdd
    handlerSub <- buttonActivatedToAddHandler buttonSub

    -- Network
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            eAdd <- fromAddHandler handlerAdd
            eSub <- fromAddHandler handlerSub
            let counter :: Behavior t Int
                counter = accumB 0 $ ((+1) <$ eAdd) `union` (subtract 1 <$ eSub)
            sink label [labelLabel :== show <$> counter]

    network <- compile networkDescription            
    actuate network
    onDestroy window mainQuit
    widgetShowAll window

    mainGUI
