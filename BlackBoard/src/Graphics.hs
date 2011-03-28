{-----------------------------------------------------------------------------
    A really simple GUI library, similar to SOE.
    
    Currently implemented as a wrapper on wxHaskell.
    
    Events and attributes
------------------------------------------------------------------------------}

module Graphics (
    module Graphics.Events,
    module Graphics.Geometry,
    module Graphics.GUI
    ) where

import Graphics.Events
import Graphics.Geometry
import Graphics.GUI
