{-----------------------------------------------------------------------------
    reactive-banana-threepenny
    
    Example: Very simple arithmetic
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Data.Maybe

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
    return window # set title "Arithmetic"
    
    input1 <- UI.input
    input2 <- UI.input
    output <- UI.span
    
    getBody window #+ [row
        [ element input1, UI.string " + ", element input2
        , UI.string " = ", element output]]


    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
        
        binput1  <- behaviorValue input1 ""
        binput2  <- behaviorValue input2 ""
        
        let
            result :: Behavior t (Maybe Int)
            result = f <$> binput1 <*> binput2
                where
                f x y = liftA2 (+) (readNumber x) (readNumber y)
            
            readNumber s = listToMaybe [x | (x,"") <- reads s]    
            showNumber   = maybe "--" show
    
        return output # sink text (showNumber <$> result)

    network <- compile networkDescription    
    actuate network
