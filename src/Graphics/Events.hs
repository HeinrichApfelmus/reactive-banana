{-----------------------------------------------------------------------------
    A really simple GUI library, similar to SOE.
    
    Currently implemented as a wrapper on wxHaskell.
    
    Events and attributes
------------------------------------------------------------------------------}

module Graphics.Events (
    module Graphics.UI.WX
    ) where

import Graphics.UI.WX (
    Attr, Prop(..), set, get,
    on,
    EventKey(..), Key(..), Modifiers,
    EventMouse(..))
