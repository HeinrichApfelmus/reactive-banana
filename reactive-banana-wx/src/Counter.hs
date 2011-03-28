{-----------------------------------------------------------------------------
    reactive-banan-wx
    
    Example: Counter
------------------------------------------------------------------------------}
module Main where

import Reactive
import Reactive.WX
import Graphics.UI.WX hiding (Event)

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main = start $ do
    f       <- frame [text := "Counter"]
    bup     <- button f [text := "Up"]
    bdown   <- button f [text := "Down"]
    output  <- staticText f []
        
    set f [layout := margin 10 $
            column 5 [widget bup, widget bdown, widget output]]
    
    let
        eup, edown :: Event ()
        eup   = event0 bup   command
        edown = event0 bdown command
        
        counter :: Behavior Int
        counter = accumulate ($) 0 $
            ((+1) <$ eup) `union` (subtract 1 <$ edown)
    
    sink output [text :== (show <$> counter)]
