{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: Currency Converter
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo #-}
import Data.Maybe
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX
import Text.Printf

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main = start $ do
    f        <- frame    [text := "Currency Converter"]
    
    dollar   <- entry f  [processEnter := True]
    euro     <- entry f  [processEnter := True]
    
    set f [layout := margin 10 $
            column 10 [
                grid 10 10 [[label "Dollar:", widget dollar],
                            [label "Euro:"  , widget euro  ]]
            , label "Press enter to convert"
            ]]

    network <- compile $ mdo        
        euroIn   <- reactimateTextEntry euro    euroOut
        dollarIn <- reactimateTextEntry dollar  dollarOut

        let rate = 0.7 :: Double
            withString f s
                = maybe "-" (printf "%.2f") . fmap f 
                $ listToMaybe [x | (x,"") <- reads s] 
        
            -- define output values in terms of input values
            dollarOut, euroOut :: Discrete String
            dollarOut = withString (/ rate) <$> stepperD "0" euroIn
            euroOut   = withString (* rate) <$> stepperD "0" dollarIn

        return ()
    
    actuate network


-- text entry widget in terms of discrete time-varying values
reactimateTextEntry
    :: TextCtrl a
    -> Discrete String                    -- set programmatically (view)
    -> NetworkDescription (Event String)  -- read from user (controller)
reactimateTextEntry entry input = do
    sink entry [ text :== input ]

    e <- event0   entry command
    b <- behavior entry text
    return $ b <@ e


