{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: Counter
------------------------------------------------------------------------------}
module Main where

import Control.Monad
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

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
    
    network <- compile $ do
        eup   <- event0 bup   command
        edown <- event0 bdown command
        
        let
            counter :: Event Int
            counter = accumE 0 $ ((+1) <$ eup) `union` (subtract 1 <$ edown)
    
        sink output [text :== ("0", show <$> counter)]
    
    actuate network
