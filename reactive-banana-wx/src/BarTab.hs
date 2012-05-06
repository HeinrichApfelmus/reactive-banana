{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: Bar tab with a variable number of widgets
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. NetworkDescription t"
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Data.Maybe (listToMaybe)

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.Experimental.Switch
import Reactive.Banana.WX

import Data.Traversable (sequenceA)

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main = start $ do
    f      <- frame [text := "Bar Tab"]
    total  <- staticText f []
    add    <- button f [text := "Add"]
    
    let networkDescription :: forall t. NetworkDescription t ()
        networkDescription = do
            eadd <- event0 add command
            
            let
                newEntry :: NetworkDescription s (Layout, Trimmed Behavior String) 
                newEntry = do
                    wentry <- liftIO $ entry f []
                    bentry <- trimB =<< behaviorText wentry ""
                    return (widget wentry, bentry)
            
            eNewEntry <- compileNew $ (F newEntry <$ eadd)
            
            let
                bentries :: Behavior t [(Layout, Trimmed Behavior String)]
                bentries = accumB [] $ (:) <$> eNewEntry
            
                bprices  :: Behavior t [Trimmed Behavior Number]
                bprices = map (fmap readNumber . snd) <$> bentries
                
                blayout :: Behavior t Layout
                blayout = mkLayout . map fst <$> bentries
                
                mkLayout entries = margin 10 $ column 10 $
                    [widget add] ++ entries
                    ++ [row 10 $ [label "Sum:", minsize (sz 40 20) $ widget total]]
        
            -- btotal :: Behavior t Number
            btotal <- switchB $ (fmap sum . sequenceA) <$> bprices

            sink total [text   :== showNumber <$> btotal]   
            sink f     [layout :== blayout ]

    network <- compile networkDescription    
    actuate network

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
type Number = Maybe Int

instance Num Number where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

readNumber s = listToMaybe [x | (x,"") <- reads s]    
showNumber   = maybe "--" show
