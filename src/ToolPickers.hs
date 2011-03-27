{-# LANGUAGE TupleSections #-}
{-----------------------------------------------------------------------------
    Blackbaord is an application for drawing presentation slides.
    
    Toolbar - Color Picker, Tool Picker
------------------------------------------------------------------------------}
module ToolPickers where

import Data.Monoid

import Graphics
import Reactive
import Reactive.GraphicOpt

{-----------------------------------------------------------------------------
    Color Picker
------------------------------------------------------------------------------}
    -- the Color Picker
colorPicker :: [Color] -> Point
            -> Event EventMouse -> (Behavior Color, TimeGraphic)
colorPicker colors position mouse = (behaveColor, picture)
    where
    box   = rect position (sz 60 (50*length colors+20))
    boxes = [rect (at i) (sz 40 40) | i <- [0..]]
    at i  = pt (pointX position + 10) (pointY position + 10 + 50*i)
   
    colorBoxes = zip boxes colors 
    picture = TimeGraphic (\_ -> drawColors box colorBoxes) never
    
    behaveColor  = accumulate clickPick initialColor mouse
    initialColor = head colors
    
    clickPick (MouseLeftDown point _) _ = pick colorBoxes point
    clickPick _ _ = Keep

    -- draw the color Picker
drawColors :: Rect -> [(Rect,Color)] -> Graphic
drawColors box colors = mconcat $ frame : palette
    where
    frame   = mask white . stroke . rectangle $ box
    palette = map (\(r,c) -> mask c . fill . rectangle $ r) colors

    -- pick an element from a list of rectangles
pick :: [(Rect,a)] -> Point -> Change a
pick xs point =
    maybe Keep Change
    . lookup True
    $ [(rect `contains` point, x) | (rect, x) <- xs]


{-----------------------------------------------------------------------------
    Tool Picker
------------------------------------------------------------------------------}
data Tool = Draw | Text | Erase

toolPicker :: Point -> Event EventMouse -> (Behavior Tool, TimeGraphic)
toolPicker = undefined









