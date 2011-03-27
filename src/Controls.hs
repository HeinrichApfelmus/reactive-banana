{-# LANGUAGE TupleSections #-}
{-----------------------------------------------------------------------------
    Blackbaord is an application for drawing presentation slides.
    
    Controls - buttons and stuff
------------------------------------------------------------------------------}
module Controls where

import Data.Monoid

import Graphics
import Reactive
import qualified Reactive as R
import Reactive.GraphicOpt


{-----------------------------------------------------------------------------
    Buttons
------------------------------------------------------------------------------}
    -- a simple push button with an image as graphic
imageButton :: Image -> Point -> Event EventMouse -> (Event (), TimeGraphic)
imageButton image position emouse = (clicks, picture)
    where
    r       = rect position (imageSize image)
    picture = TimeGraphic (withinBox r $ mask (image `at` position) r) never
    clicks  = id
        . fmap (const ())
        . R.filter (\event -> leftMouseInRectangle r event && isClick event)
        $ emouse

{-----------------------------------------------------------------------------
    Label
------------------------------------------------------------------------------}
label :: Rect -> Behavior String -> TimeGraphic
label r bstring =
    TimeGraphic (initial graphic) (fmap (,r) (changes graphic))
    where
    graphic = fmap draw bstring
    draw s  = withinBox r . mask white . stroke $ text (topLeft r) s


{-----------------------------------------------------------------------------
    ListBox
    
    Displays a list of alternatives, one has the focus
------------------------------------------------------------------------------}
dx = 10
dy = 14

{-
    -- display list of strings
    -- FIXME: add focus!
listBox :: Rect -> Behavior [String] -> Event EventMouse -> TimeGraphic
listBox r contents emouse = picture
    where
    picture = TimeGraphic (const empty) $ fmap (withinBox r . draw) contents
    
    draw = mconcat . map (\(i,s) -> mask white . stroke . text (position i) s)
    position i = pt 0 (dy*fromInteger i) -- FIXME: add absolute position
-}

{-----------------------------------------------------------------------------
    Helper Functions
    
    Filtering mouse events by rectangle
------------------------------------------------------------------------------}
leftMouseInRectangle :: Rect -> EventMouse -> Bool
leftMouseInRectangle rect event = maybe False (rect `contains`) (point event)
    where
    point (MouseLeftDown pt _) = Just pt
    point (MouseLeftDrag pt _) = Just pt
    point (MouseLeftUp   pt _) = Just pt
    point _                    = Nothing

isClick :: EventMouse -> Bool
isClick (MouseLeftDown _ _) = True
isClick _                   = False





