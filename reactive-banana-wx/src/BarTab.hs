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
    msg    <- staticText f [ text := "Sum:" ]
    total  <- staticText f []
    add    <- button f [text := "Add"]
    remove <- button f [text := "Remove"]
    
    let networkDescription :: forall t. NetworkDescription t ()
        networkDescription = do
            eAdd    <- event0 add command
            eRemove <- event0 remove command
            
            let
                newEntry :: NetworkDescription s (TextCtrl (), Trimmed Behavior String) 
                newEntry = do
                    wentry <- liftIO $ entry f []
                    bentry <- trimB =<< behaviorText wentry ""
                    return (wentry, bentry)
            
            eNewEntry <- compileNew $ (F newEntry <$ eAdd)
            
            let
                eDoRemove = whenE (not . null <$> bentries) eRemove
            
                bentries :: Behavior t [(TextCtrl (), Trimmed Behavior String)]
                bentries = accumB [] $
                    ((\x -> (++ [x])) <$> eNewEntry) `union` (init <$ eDoRemove)
            
            reactimate $ ((\w -> set w [ visible := False]) . fst . last)
                <$> bentries <@ eDoRemove
            
            let
                bprices  :: Behavior t [Trimmed Behavior Number]
                bprices = map (fmap readNumber . snd) <$> bentries
                
                blayout :: Behavior t Layout
                blayout = mkLayout . map fst <$> bentries
                
                mkLayout entries = margin 10 $ column 10 $
                    [row 10 [widget add, widget remove]] ++ map widget entries
                    ++ [row 10 $ [widget msg, minsize (sz 40 20) $ widget total]]
        
            -- btotal :: Behavior t Number
            btotal <- switchB $ (fmap sum . sequenceA) <$> bprices

            sink total [text   :== showNumber <$> btotal]   
            sink f     [layout :== blayout ]

    network <- compile networkDescription    
    actuate network

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
