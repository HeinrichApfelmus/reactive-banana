{-----------------------------------------------------------------------------
    reactive-banana-threepenny
    
    Example: Counter
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (Event)

import Reactive.Banana
import Reactive.Banana.Threepenny

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = do
    startGUI Config
        { tpPort       = 10000
        , tpCustomHTML = Nothing
        , tpStatic     = ""
        } setup

setup :: Window -> IO ()
setup window = do
    return window # set title "Counter"
    
    bup    <- UI.button #+ [UI.string "Up"  ]
    bdown  <- UI.button #+ [UI.string "Down"]
    output <- UI.span
    
    getBody window #+ [column [element bup, element bdown, element output]]

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
        
        eup   <- event UI.click bup
        edown <- event UI.click bdown
        
        let
            counter :: Behavior t Int
            counter = accumB 0 $ ((+1) <$ eup) `union` (subtract 1 <$ edown)
    
        return output # sink text (show <$> counter)

    network <- compile networkDescription    
    actuate network
