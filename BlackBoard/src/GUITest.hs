{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-----------------------------------------------------------------------------
    A really simple GUI library, similar to SOE.
    
    Currently implemented as a wrapper on wxHaskell.
------------------------------------------------------------------------------}

import Graphics

import Data.Monoid

main = runGUI $ do
    w <- window (sz 300 300) [title := "Hello World"]
    set w [on mouse := draw w]
    -- image <- newImage (sz 300 300)
    let image = screen w
    overpaint picture image
    writeImagePNG "test.png" image
    
    where
    picture   = mconcat . map graphic $ [2,1]
    graphic 1 = mask red . stroke $
                polyline [pt 100 100, pt 200 200, pt 100 200, pt 100 100]
    graphic 2 = mask white . fill $ circle (pt 100 100) 20
    
    draw window (MouseLeftDown pt _) = do
        overpaint (green `mask` fill (circle pt 5)) (screen window)
    draw _ _ = return ()