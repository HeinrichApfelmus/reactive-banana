{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: Currency Converter
------------------------------------------------------------------------------}

import Data.Maybe
import Text.Printf

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = start $ do
    f        <- frame   [ text := "Currency Converter", tabTraversal := True ]
    p        <- panel f []  -- Use panel for tab traversal
    dollar   <- entry p []
    euro     <- entry p []
    
    set p [layout := margin 10 $
            column 10 [
                grid 10 10 [[label "Dollar:", widget dollar],
                            [label "Euro:"  , widget euro  ]]
            , label "Amounts update while typing."
            ]]
    set f [layout := widget p]
    focusOn dollar

    let networkDescription :: MomentIO ()
        networkDescription = do
        
        euroIn   <- behaviorText euro   "0"
        dollarIn <- behaviorText dollar "0"
        
        let rate = 0.7 :: Double
            withString f s
                = maybe "-" (printf "%.2f") . fmap f 
                $ listToMaybe [x | (x,"") <- reads s] 
        
            -- define output values in terms of input values
            dollarOut, euroOut :: Behavior String
            dollarOut = withString (/ rate) <$> euroIn
            euroOut   = withString (* rate) <$> dollarIn
    
        sink euro   [text :== euroOut  ]
        sink dollar [text :== dollarOut] 

    network <- compile networkDescription    
    actuate network

