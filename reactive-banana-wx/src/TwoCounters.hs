{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: Two Counters.
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
    f       <- frame [text := "Two Counters"]
    bup     <- button f [text := "Up"]
    bdown   <- button f [text := "Down"]
    bswitch <- button f [text := "Switch Counters"]
    out1    <- staticText f []
    out2    <- staticText f []
    
    set f [layout := margin 10 $
            column 5 [row 5 [widget bup, widget bdown, widget bswitch],
                      grid 5 5 [[label "First Counter:" , widget out1]
                               ,[label "Second Counter:", widget out2]]]]
    
    network <- compile $ do
        eup     <- event0 bup   command
        edown   <- event0 bdown command
        eswitch <- event0 bswitch command
        
        let
            -- do we act on the left button?
            firstcounter :: Behavior Bool
            firstcounter = accumB True $ not <$ eswitch
        
            -- joined state of the two counters
            counters :: Discrete (Int, Int)
            counters = accumD (0,0) $
                union ((increment <$> firstcounter) `apply` eup)
                      ((decrement <$> firstcounter) `apply` edown)
            
            increment left _ (x,y) = if left then (x+1,y) else (x,y+1)
            decrement left _ (x,y) = if left then (x-1,y) else (x,y-1)
    
        sink out1 [text :== show . fst <$> counters]
        sink out2 [text :== show . snd <$> counters]
    
    actuate network
