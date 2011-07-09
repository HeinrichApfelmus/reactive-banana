{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: Currency Converter
------------------------------------------------------------------------------}
import Data.Maybe
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main = start $ do
    f        <- frame    [text := "Currency Converter"]
    convert  <- button f [text := "Convert Dollar to Euro"]
    input    <- entry f  [processEnter := True]
    output   <- staticText f []
    
    set f [layout := margin 10 $
            column 10 [widget input, widget convert, widget output]]

    network <- compile $ do
        -- I can't seem to find a way to react to real-time text updates
        -- The  keyboard  event always lags one character behind.
        -- Where is wxEVT_COMMAND_TEXT_UPDATED in wxHaskell?
        eenter   <- event0 input   command
        ebutton  <- event0 convert command
        
        binput   <- behavior input text
        
        let
            econvert :: Event ()
            econvert = ebutton `union` eenter
            
            converted :: Discrete (Maybe Double)
            converted = stepperD Nothing $
                (fmap (* 0.7)) <$> ((readDollar <$> binput) <@ econvert)
            
            readDollar s = listToMaybe [x | (x,"") <- reads s]    
            showEuro     = maybe "--" show
    
        sink output [text :== showEuro <$> converted]
    
    actuate network
