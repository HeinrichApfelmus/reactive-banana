{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: Currency Converter
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"
{-# LANGUAGE RecursiveDo #-}

import Data.Maybe
import Text.Printf

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (Event)

import Reactive.Banana
import Reactive.Banana.Threepenny

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = do
    startGUI Config
        { tpPort       = 10000
        , tpCustomHTML = Nothing
        , tpStatic     = ""
        } setup

setup :: Window -> IO ()
setup window = do
    return window # set title "Currency Converter"

    dollar <- UI.input
    euro   <- UI.input
    
    getBody window #+ [
            column [
                grid [[string "Dollar:", element dollar]
                     ,[string "Euro:"  , element euro  ]]
            , string "Amounts update while typing."
            ]]

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
        
        euroIn   <- behaviorValue euro   "0"
        dollarIn <- behaviorValue dollar "0"
        
        let rate = 0.7 :: Double
            withString f s
                = maybe "-" (printf "%.2f") . fmap f 
                $ listToMaybe [x | (x,"") <- reads s] 
        
            -- define output values in terms of input values
            dollarOut, euroOut :: Behavior t String
            dollarOut = withString (/ rate) <$> euroIn
            euroOut   = withString (* rate) <$> dollarIn
    
        return euro   # sink value euroOut
        return dollar # sink value dollarOut

    network <- compile networkDescription    
    actuate network

