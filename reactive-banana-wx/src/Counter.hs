{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: Counter
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = start $ do
    f       <- frame [text := "Counter"]
    bup     <- button f [text := "Up"]
    bdown   <- button f [text := "Down"]
    output  <- staticText f []
    
    set f [layout := margin 10 $
            column 5 [widget bup, widget bdown, widget output]]

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
        
        eup   <- event0 bup   command
        edown <- event0 bdown command
        
        let
            counter :: Behavior t Int
            counter = accumB 0 $ ((+1) <$ eup) `union` (subtract 1 <$ edown)
    
        sink output [text :== show <$> counter] 

    network <- compile networkDescription    
    actuate network
