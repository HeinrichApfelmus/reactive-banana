{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: A simple animation.
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. NetworkDescription t"

import Graphics.UI.WX hiding (Event, Vector)
import Reactive.Banana
import Reactive.Banana.WX
import System.Random

import Paths (getDataFile)

{-----------------------------------------------------------------------------
    Constants
------------------------------------------------------------------------------}
height, width :: Int
height   = 400
width    = 400

dt :: Double
dt = 20 * ms where ms = 1e-3

sprite :: Bitmap ()
sprite = bitmap $ getDataFile "banana.png"

bitmapWidth, bitmapHeight :: Int
bitmapWidth  = 128
bitmapHeight = 128

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = start $ do
    ff <- frame [ text       := "It's functional programming time"
                , bgcolor    := white
                , resizeable := False ]

    t  <- timer ff [ interval := ceiling (dt * 1e3) ]    
    pp <- panel ff [ ]
    set ff [ layout  := minsize (sz width height) $ widget pp ]
    
    -- event network
    let networkDescription :: forall t. NetworkDescription t ()
        networkDescription = do
            etick  <- event0 t command  -- frame timer
            emouse <- event1 pp mouse   -- mouse events
            
            let
                -- mouse pointer position
                bmouse = fromPoint <$> stepper (point 0 0)
                    (filterJust $ justMotion <$> emouse)
            
                -- sprite velocity
                bvelocity :: Behavior t Vector
                bvelocity =
                    (\pos mouse -> speedup $ mouse `vecSub` pos `vecSub` vec 0 45)
                    <$> bposition <*> bmouse
                    where
                    speedup v = v `vecScale` (vecLengthDouble v / 20)
                
                -- sprite position
                bposition :: Behavior t Vector
                bposition = accumB (vec 0 0) $
                    (\v pos -> clipToFrame $ (v `vecScale` dt) `vecAdd` pos)
                    <$> bvelocity <@ etick
            
                clipToFrame v = vec
                        (clip 0 x (fromIntegral $ width  - bitmapWidth ))
                        (clip 0 y (fromIntegral $ height - bitmapHeight))
                    where
                    x = vecX v; y = vecY v
                    clip a x b = max a (min x b)
                
                drawSprite :: Point -> DC a -> b -> IO ()
                drawSprite pos dc _view = drawBitmap dc sprite pos True []
        
            -- animate the sprite
            sink pp [on paint :== drawSprite . toPoint <$> bposition]
            reactimate $ repaint pp <$ etick
    
    network <- compile networkDescription    
    actuate network

{-----------------------------------------------------------------------------
    2D Geometry
------------------------------------------------------------------------------}
type Vector = Vector2 Double

fromPoint :: Point -> Vector
fromPoint pt = vector (fromIntegral (pointX pt)) (fromIntegral (pointY pt))

toPoint :: Vector -> Point
toPoint v = point (ceiling (vecX v)) (ceiling (vecY v))

{-----------------------------------------------------------------------------
    wx stuff
------------------------------------------------------------------------------}
justMotion :: EventMouse -> Maybe Point
justMotion (MouseMotion pt _) = Just pt
justMotion _                  = Nothing


