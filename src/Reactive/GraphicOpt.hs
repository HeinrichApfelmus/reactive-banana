{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-----------------------------------------------------------------------------
    A really simple GUI library, similar to SOE.
    
    Currently implemented as a wrapper on wxHaskell.
    
    
    Efficient Graphics updates
    
------------------------------------------------------------------------------}

module Reactive.GraphicOpt where

import Graphics
import Reactive

{-----------------------------------------------------------------------------
    Optimization methods
------------------------------------------------------------------------------}
    -- Method 1: 
    -- Instead of specifying the whole graphic, only specify
    -- the additional strokes that have been painted over it
    --  g ->  x `over` g
type Overpaint = Graphic


    -- Method 2a:
    -- Instead of always painting the whole graphic, we
    -- only paint the part contained in a particular rectangle
type GraphicOpt  = Rect -> Graphic

    -- use a bounding box to turn a graphic into an optimized graphic
    -- FIXME: We really have to make sure that only the dirty rectangle
    --        is drawn! Otherwise, havoc will happen. Need clipping path!
withinBox :: Rect -> Graphic -> GraphicOpt
withinBox r g = \r' ->
    if isEmpty (r `intersect` r') then empty else g

    -- overlay optimized graphics
overOpt :: GraphicOpt -> GraphicOpt -> GraphicOpt
overOpt g1 g2 = \r -> g1 r `over` g2 r


    -- Method 2b:
    -- With the above, we can define a graphic update to 
    -- be a new (optimized) graphic together with a dirty rectangle
type GraphicUpdate = (GraphicOpt, Rect)

    -- "differential" of the overlay function
    -- only the second argument is updated, though
overUpdate :: GraphicOpt -> GraphicUpdate -> GraphicUpdate
overUpdate g1 (g2,r) = (g1 `overOpt` g2, r)

    -- perform a graphic update by drawing it on top of the existing screen
updateToPaint :: GraphicUpdate -> Overpaint
updateToPaint (g,r) = g r `over` erase r
    where
    erase r = empty -- how to erase depends pretty much on the window...

{-----------------------------------------------------------------------------
    Time varying graphics
------------------------------------------------------------------------------}
    -- a time varying graphic with efficient updates
data TimeGraphic = TimeGraphic GraphicOpt (Event GraphicUpdate)
    -- TimeGraphic (initial graphic) (updates)

    -- overlay time varying graphics
overTT :: TimeGraphic -> TimeGraphic -> TimeGraphic
overTT (TimeGraphic g1 u1) (TimeGraphic g2 u2) =
    TimeGraphic (g1 `overOpt` g2)
    . snd . mapAccum go (g1,g2)
    $ u1 `merge` u2
    where
    go (g1,g2) (Left  (g1',r)) = ((g1', g2 ), (g1' `overOpt` g2 , r))
    go (g1,g2) (Right (g2',r)) = ((g1 , g2'), (g1  `overOpt` g2', r))


    -- overlay behavior graphic with time varying graphic
    -- but only when the latter changes!
    -- FIXME: this is a fundamentally "unsafe" operation
overBT :: Behavior GraphicOpt -> TimeGraphic -> TimeGraphic
overBT bg1 (TimeGraphic g2 u2) =
    TimeGraphic (g1 `overOpt` g2) $
        (overUpdate `fmap` bg1) `apply` u2
    where
    (g1,u1) = (initial bg1, changes bg1)

overTB :: TimeGraphic -> Behavior GraphicOpt -> TimeGraphic
overTB (TimeGraphic g1 u1) bg2 =
    TimeGraphic (g1 `overOpt` g2) $
        ((\g2 (g1,r) -> (g1 `overOpt` g2,r)) `fmap` bg2) `apply` u1
    where
    g2 = initial bg2

{-
    -- reactimate a time graphic on an Image
reactimateTimeGraphic :: Image -> TimeGraphic -> IO ()
reactimateTimeGraphic image (TimeGraphic g u) = do
    overpaint (g . rect (pt 0 0) $ size image) image
    reactimate $ fmap (\(r,g) -> overpaint (g r `over` clean r) image) u
    where
    clean r = mask black . fill $ rectangle r
-}

