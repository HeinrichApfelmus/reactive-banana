{-----------------------------------------------------------------------------
    reactive-banana-threepenny
    
    Example: Bar tab with a variable number of widgets
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Control.Monad (void)
import Data.Maybe (listToMaybe)
import Data.Traversable (sequenceA)

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
    return window # set title "Bar Tab"

    msg    <- UI.span # set text "Sum:"
    total  <- UI.span
    add    <- UI.button #+ [string "Add"]
    remove <- UI.button #+ [string "Remove"]
    body   <- UI.getBody window
    
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            eAdd    <- event UI.click add
            eRemove <- event UI.click remove
            
            let
                newEntry :: Frameworks s
                         => Moment s (Element, AnyMoment Behavior String) 
                newEntry = do
                    wentry <- liftIO $ UI.input # set value "0"
                    bentry <- trimB =<< behaviorValue wentry "0"
                    return (wentry, bentry)
            
            eNewEntry <- execute $ (FrameworksMoment newEntry <$ eAdd)
            
            let
                eDoRemove = whenE (not . null <$> bEntries) eRemove
            
                eEntries :: Event t [(Element, AnyMoment Behavior String)]
                eEntries = accumE [] $
                    ((\x -> (++ [x])) <$> eNewEntry) `union` (init <$ eDoRemove)
            
                bEntries = stepper [] eEntries
            
            let hideElement e = void $ element e # set style [("hidden","true")]
            reactimate $ (hideElement . fst . last)
                <$> bEntries <@ eDoRemove
            
            let
                ePrices :: Event t [AnyMoment Behavior Number]
                ePrices = map (fmap readNumber . snd) <$> eEntries
                
                bLayout :: Behavior t [IO Element]
                bLayout = mkLayout . map fst <$> bEntries
                
                mkLayout entries = [column $
                    [row [element add, element remove]] ++ map element entries
                    ++ [row [element msg, element total]]]
                
                bTotal :: Behavior t Number
                bTotal = switchB (pure Nothing) $
                            (fmap sum . sequenceA) <$> ePrices

            return total # sink text (showNumber <$> bTotal)
            return body  # sink layout bLayout 
            
    network <- compile networkDescription    
    actuate network

layout :: WriteAttr Element [IO Element]
layout = mkWriteAttr $ \i x -> void $ element x # set children [] #+ i

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
type Number = Maybe Double

instance Num Number where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

readNumber s = listToMaybe [x | (x,"") <- reads s]    
showNumber   = maybe "--" show
