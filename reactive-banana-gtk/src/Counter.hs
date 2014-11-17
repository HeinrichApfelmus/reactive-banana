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
    vbox <- vBoxNew True 0
    containerAdd vbox buttonAdd
    containerAdd vbox buttonSub
    set vbox [boxChildPadding buttonAdd := 10]
    set vbox [boxChildPadding buttonSub := 10]
    
    hbox <- hBoxNew True 0
    containerAdd hbox vbox
    containerAdd hbox label
    set hbox [boxChildPadding vbox := 10]
    set hbox [boxChildPadding label := 10]
    containerAdd frame hbox

    set frame [ frameLabel := "Counter"]
    set window [ containerBorderWidth := 10, containerChild := frame ]

    -- Network
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            eAdd <- event0 buttonAdd buttonActivated
            eSub <- event0 buttonSub buttonActivated
            let counter :: Behavior t Int
                counter = accumB 0 $ ((+1) <$ eAdd) `union` (subtract 1 <$ eSub)
            sink label [labelLabel :== show <$> counter]

    network <- compile networkDescription            
    actuate network
    onDestroy window mainQuit
    widgetShowAll window

    mainGUI
