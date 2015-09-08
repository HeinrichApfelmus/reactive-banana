{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: Two Counters.
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-}
    -- allows pattern signatures like
    -- do
    --     (b :: Behavior Int) <- stepper 0 ...
{-# LANGUAGE RecursiveDo #-}
    -- allows recursive do notation
    -- mdo
    --     ...

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
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
    
    let networkDescription :: forall t. MomentIO ()
        networkDescription = mdo

            eup     <- event0 bup     command
            edown   <- event0 bdown   command
            eswitch <- event0 bswitch command
        
            let
            -- do we act on the left button?
            (firstcounter :: Behavior Bool)
                <- accumB True $ not <$ eswitch
        
            -- joined state of the two counters
            (counters :: Behavior (Int, Int))
                <- accumB (0,0) $ unions
                    [ increment <$> firstcounter <@> eup
                    , decrement <$> firstcounter <@> edown
                    ]
            let
                increment left _ (x,y) = if left then (x+1,y) else (x,y+1)
                decrement left _ (x,y) = if left then (x-1,y) else (x,y-1)
    
            sink out1 [text :== show . fst <$> counters]
            sink out2 [text :== show . snd <$> counters]
    
    network <- compile networkDescription    
    actuate network

