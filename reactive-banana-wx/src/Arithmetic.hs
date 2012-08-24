{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: Very simple arithmetic
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Data.Maybe

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main = start $ do
    f         <- frame [text := "Arithmetic"]
    input1    <- entry f []
    input2    <- entry f []
    output    <- staticText f []
    
    set f [layout := margin 10 $ row 10 $
            [widget input1, label "+", widget input2
            , label "=", minsize (sz 40 20) $ widget output]]

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
        
        binput1  <- behaviorText input1 ""
        binput2  <- behaviorText input2 ""
        
        let
            result :: Behavior t (Maybe Int)
            result = f <$> binput1 <*> binput2
                where
                f x y = liftA2 (+) (readNumber x) (readNumber y)
            
            readNumber s = listToMaybe [x | (x,"") <- reads s]    
            showNumber   = maybe "--" show
    
        sink output [text :== showNumber <$> result]   

    network <- compile networkDescription    
    actuate network
