{-----------------------------------------------------------------------------
    reactive-banana-wx

    Example: Bar tab with a variable number of widgets
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-}
    -- allows pattern signatures like
    -- do
    --     (b :: Behavior Int) <- stepper 0 ...
{-# LANGUAGE RecursiveDo #-}
    -- allows recursive do notation
    -- mdo
    --     ...
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Data.Maybe (listToMaybe)

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

import Data.Traversable (sequenceA)

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = start $ do
    f      <- frame [text := "Bar Tab"]
    msg    <- staticText f [ text := "Sum:" ]
    total  <- staticText f []
    add    <- button f [text := "Add"]
    remove <- button f [text := "Remove"]

    let networkDescription :: MomentIO ()
        networkDescription = mdo
            eAdd    <- event0 add command
            eRemove <- event0 remove command

            let
                newEntry :: MomentIO (TextCtrl (), Behavior String)
                newEntry = do
                    wentry <- liftIO $ entry f []
                    bentry <- behaviorText wentry ""
                    return (wentry, bentry)

            eNewEntry <- execute $ newEntry <$ eAdd

            let eDoRemove = whenE (not . null <$> bEntries) eRemove

            (eEntries :: Event [(TextCtrl (), Behavior String)])
                <- accumE [] $ unions
                    [ (\x -> (++ [x])) <$> eNewEntry
                    , init <$ eDoRemove
                    ]
            bEntries <- stepper [] eEntries

            reactimate $ ((\w -> set w [ visible := False]) . fst . last)
                <$> bEntries <@ eDoRemove

            let
                ePrices :: Event [Behavior Number]
                ePrices = map (fmap readNumber . snd) <$> eEntries

                bLayout :: Behavior Layout
                bLayout = mkLayout . map fst <$> bEntries

                mkLayout entries = margin 10 $ column 10 $
                    [row 10 [widget add, widget remove]] ++ map widget entries
                    ++ [row 10 [widget msg, minsize (sz 40 20) $ widget total]]

            (bTotal :: Behavior Number)
                <- switchB (pure Nothing) $
                    (fmap sum . sequenceA) <$> ePrices

            sink total [text   :== showNumber <$> bTotal]
            sink f     [layout :== bLayout]

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

readNumber :: Read a => String -> Maybe a
readNumber s = listToMaybe [x | (x,"") <- reads s]

showNumber :: Maybe Double -> String
showNumber   = maybe "--" show
