{-----------------------------------------------------------------------------
    reactive-banana-threepenny
    
    Example: Two Counters.
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
    return window # set title "Two Counters"
    
    bup     <- UI.button #+ [string "Up"  ]
    bdown   <- UI.button #+ [string "Down"]
    bswitch <- UI.button #+ [string "Switch Counters"]

    out1    <- UI.span
    out2    <- UI.span
    
    getBody window #+ [
        column [row [element bup, element bdown, element bswitch]
               ,grid [[string "First Counter:",  element out1]
                     ,[string "Second Counter:", element out2]]]]

    
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do

            eup     <- event UI.click bup
            edown   <- event UI.click bdown
            eswitch <- event UI.click bswitch
        
            let
                -- do we act on the left button?
                firstcounter :: Behavior t Bool
                firstcounter = accumB True $ not <$ eswitch
        
                -- joined state of the two counters
                counters :: Behavior t (Int, Int)
                counters = accumB (0,0) $
                    union ((increment <$> firstcounter) `apply` eup)
                          ((decrement <$> firstcounter) `apply` edown)
            
                increment left _ (x,y) = if left then (x+1,y) else (x,y+1)
                decrement left _ (x,y) = if left then (x-1,y) else (x,y-1)
    
            return out1 # sink text (show . fst <$> counters)
            return out2 # sink text (show . snd <$> counters)
    
    network <- compile networkDescription    
    actuate network

