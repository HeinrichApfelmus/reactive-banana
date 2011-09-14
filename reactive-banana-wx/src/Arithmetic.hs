{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: Very simple arithmetic
------------------------------------------------------------------------------}
import Data.Maybe
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main = start $ do
    f         <- frame    [text := "Arithmetic"]
    calculate <- button f [text := "="]
    input1    <- entry f  [processEnter := True]
    input2    <- entry f  [processEnter := True]
    output    <- staticText f [ size := sz 40 20 ]
    
    set f [layout := margin 10 $ row 10 $
            [widget input1, label "+", widget input2
            , widget calculate, widget output]]

    network <- compile $ do
        -- TODO: Maybe include real-time updates? (see CurrencyConverter.hs)
        eenter1  <- event0 input1    command
        eenter2  <- event0 input2    command
        ebutton  <- event0 calculate command
        
        binput1  <- behavior input1 text
        binput2  <- behavior input2 text
        
        let
            ecalculate :: Event ()
            ecalculate = ebutton `union` eenter1 `union` eenter2
            
            result :: Discrete (Maybe Int)
            result = stepperD Nothing $
                (f <$> binput1 <*> binput2) <@ ecalculate
                where
                f x y = liftA2 (+) (readNumber x) (readNumber y)
            
            readNumber s = listToMaybe [x | (x,"") <- reads s]    
            showNumber   = maybe "--" show
    
        sink output [text :== showNumber <$> result]
    
    actuate network
