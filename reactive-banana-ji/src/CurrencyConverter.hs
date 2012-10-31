{-----------------------------------------------------------------------------
    reactive-banana-ji
    
    Example: Currency Converter
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"
{-# LANGUAGE RecursiveDo #-}

import Data.Bits
import Data.Maybe
import Text.Printf

import Graphics.UI.Ji
import Reactive.Banana
import Reactive.Banana.Ji

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main = serve Config
    { jiPort     = 10001
    , jiRun      = runJiIO
    , jiWorker   = setup >> handleEvents
    , jiInitHTML = "CurrencyConverter.html"
    , jiStatic   = "wwwroot"
    }


setup = do
    [dollar, euro] <- getElementsByTagName "input"

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
    
        sinkValue euro   euroOut
        sinkValue dollar dollarOut

    network <- compile networkDescription    
    actuate network

