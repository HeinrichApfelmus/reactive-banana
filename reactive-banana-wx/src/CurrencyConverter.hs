{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: Currency Converter
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. NetworkDescription t"
{-# LANGUAGE RecursiveDo #-}

import Data.Bits
import Data.Maybe
import Text.Printf

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main = start $ do
    -- FIXME: Why does tab traversal not work?
    f        <- frame   [ text := "Currency Converter", tabTraversal := True ]
    dollar   <- entry f [ processEnter := True ]
    euro     <- entry f [ processEnter := True ]
    
    set f [layout := margin 10 $
            column 10 [
                grid 10 10 [[label "Dollar:", widget dollar],
                            [label "Euro:"  , widget euro  ]]
            , label "Amounts update while typing."
            ]]

    let networkDescription :: forall t. NetworkDescription t ()
        networkDescription = do
        
        euroIn   <- behaviorText euro   "0"
        dollarIn <- behaviorText dollar "0"
        
        let rate = 0.7 :: Double
            withString f s
                = maybe "-" (printf "%.2f") . fmap f 
                $ listToMaybe [x | (x,"") <- reads s] 
        
            -- define output values in terms of input values
            dollarOut, euroOut :: Behavior t String
            dollarOut = withString (/ rate) <$> euroIn
            euroOut   = withString (* rate) <$> dollarIn
    
        sink euro   [text :== euroOut  ]
        sink dollar [text :== dollarOut] 

    network <- compile networkDescription    
    actuate network

