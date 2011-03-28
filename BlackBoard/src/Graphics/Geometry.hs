{-----------------------------------------------------------------------------
    A really simple GUI library, similar to SOE.
    
    Currently implemented as a wrapper on wxHaskell.
    
    Geometric types and functions
------------------------------------------------------------------------------}

module Graphics.Geometry (
    module Graphics.UI.WX,
    size, topLeft,
    contains, intersect, isEmpty,
    ) where

import Graphics.UI.WX (Point, pt, pointX, pointY, Size, sz, Rect, rect)
import qualified Graphics.UI.WX as WX

topLeft :: Rect -> Point
topLeft = WX.rectTopLeft

size :: Rect -> Size
size = WX.rectSize

    -- tests whether a rectangle contains a point
contains :: Rect -> Point -> Bool
contains = WX.rectContains

intersect :: Rect -> Rect -> Rect
intersect = WX.rectOverlap

isEmpty :: Rect -> Bool
isEmpty = WX.rectIsEmpty
