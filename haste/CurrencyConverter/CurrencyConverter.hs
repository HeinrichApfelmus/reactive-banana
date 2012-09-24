{-----------------------------------------------------------------------------
    reactive-banana && haste-compiler
    
    Example: Currency Converter
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"
{-# LANGUAGE RecursiveDo #-}

import Data.Maybe

import Haste hiding (Event)
import Haste.DOM

import Reactive.Banana
import Reactive.Banana.Frameworks

main = main3

{-----------------------------------------------------------------------------
    DOM stuff
    https://gist.github.com/3766494
------------------------------------------------------------------------------}
setOnChanged :: String -> IO () -> IO Bool
setOnChanged eName cb = withElem eName $ \e -> setCallback e OnChange cb

onEuroChange :: IO ()
onEuroChange = do
    c <- get euro "value"
    set dollar "value" c

main1 = do
    setOnChanged "euro" $ onEuroChange

type Widget = String
set w prop x = withElem w $ \e -> setProp e prop x
get w prop   = withElem w $ \e -> getProp e prop


addHandlerText :: Widget -> IO (AddHandler String)
addHandlerText w = do
    addHandler <- liftIO $ do
        (addHandler, fire) <- newAddHandler
        setOnChanged w (fire ())
        return addHandler
    return $ mapIO (const $ get w "value") addHandler

eventText w = fromAddHandler =<< liftIO (addHandlerText w)
behaviorText w s = fromChanges s =<< liftIO (addHandlerText w)

sinkText :: Frameworks t => Widget -> Behavior t String -> Moment t ()
sinkText w b = do
    i <- initial b
    e <- changes b
    liftIOLater $ set w "value" i
    reactimate  $ set w "value" <$> e


{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
euro   = "euro"
dollar = "dollar"

main4 = do
    a <- addHandlerText euro
    a (set dollar "value")
    return ()

main3 = do
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            eEuro <- eventText euro
            reactimate $ set dollar "value" <$> eEuro

    network <- compile networkDescription
    actuate network


main2 = do

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
        
        euroIn   <- behaviorText euro   "0"
        dollarIn <- behaviorText dollar "0"
        
        let rate = 0.7 :: Double
            withString f s
                = maybe "-" showDouble . fmap f 
                $ listToMaybe [x | (x,"") <- reads s] 
        
            -- define output values in terms of input values
            dollarOut, euroOut :: Behavior t String
            dollarOut = withString (/ rate) <$> euroIn
            euroOut   = withString (* rate) <$> dollarIn
    
        sinkText euro   euroOut
        sinkText dollar dollarOut
        
    network <- compile networkDescription    
    actuate network

showDouble :: Double -> String
showDouble = show . (/(100.0 :: Double)) . fromIntegral . ceiling . (*100)

