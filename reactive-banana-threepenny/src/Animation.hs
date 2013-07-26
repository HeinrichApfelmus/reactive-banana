{-----------------------------------------------------------------------------
    reactive-banana-threepenny
    
    Example: An animated picture follows the mouse pointer.
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

-- import System.Random

import Paths (getDataFile)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (Event)

import Reactive.Banana
import Reactive.Banana.Threepenny

{-----------------------------------------------------------------------------
    Constants
------------------------------------------------------------------------------}
height, width :: Double
height   = 500
width    = 500

dt :: Double
dt = 20 * ms where ms = 1e-3

spriteFile :: IO FilePath
spriteFile = getDataFile "banana.png"

bitmapWidth, bitmapHeight :: Double
bitmapWidth  = 128
bitmapHeight = 128

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
    return window # set title "It's functional programming time!"

    spriteURL <- loadFile window "image/png" =<< spriteFile
    sprite    <- UI.img # set UI.src   spriteURL
    wrap      <- UI.div #. "wrap"
        # set style [("width",double2px width),("height",double2px height)
                    ,("border","solid black 1px")]
        #+ [element sprite]
    getBody window #+ [element wrap]

    timer     <- UI.timer # set UI.interval (ceiling $ dt * 1e3)

    -- event network
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            etick  <- event UI.tick timer        -- frame timer
            emouse <- event UI.mousemove wrap    -- mouse events
            
            let
                -- mouse pointer position
                bmouse = stepper (vec 0 0) (readMouse <$> emouse)
            
                -- sprite velocity
                bvelocity :: Behavior t Vector
                bvelocity =
                    (\pos mouse -> speedup $ mouse `vecSub` pos `vecSub` vec 0 45)
                    <$> bposition <*> bmouse
                    where
                    speedup v = v `vecScale` (vecLength v / 20)
                
                -- sprite position
                bposition :: Behavior t Vector
                bposition = accumB (vec 0 0) $
                    (\v pos -> clipToFrame $ (v `vecScale` dt) `vecAdd` pos)
                    <$> bvelocity <@ etick
            
                clipToFrame v = vec
                        (clip 0 x (width  - bitmapWidth ))
                        (clip 0 y (height - bitmapHeight))
                    where
                    x = vecX v; y = vecY v
                    clip a x b = max a (min x b)

            -- animate the sprite
            return sprite # sink position bposition
    
    network <- compile networkDescription    
    actuate network
    UI.start timer


{-----------------------------------------------------------------------------
    2D Geometry
------------------------------------------------------------------------------}
data Vector = Vec !Double !Double deriving (Show)

vecX (Vec x _) = x
vecY (Vec _ y) = y
vec x y = Vec x y
vecScale (Vec x y) s = Vec (s*x) (s*y)
vecAdd   (Vec x1 y1) (Vec x2 y2) = Vec (x1+x2) (y1+y2)
vecSub   (Vec x1 y1) (Vec x2 y2) = Vec (x1-x2) (y1-y2)
vecLength :: Vector -> Double
vecLength (Vec x y) = sqrt (x*x + y*y)

{-----------------------------------------------------------------------------
    Threepenny stuff
------------------------------------------------------------------------------}
position :: WriteAttr Element Vector
position = mkWriteAttr $ \(Vec x y) el ->
    set' style [("position","absolute")
               ,("left", double2px x) ,("top", double2px y)] el

double2px x = show (ceiling x) ++ "px" 

readMouse :: (Int,Int) -> Vector
readMouse (x,y) = vec (fromIntegral x) (fromIntegral y)

