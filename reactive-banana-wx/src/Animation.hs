{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: A simple animation.
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. NetworkDescription t"

import Graphics.UI.WX hiding (Event)
import Graphics.UI.WXCore as WXCore
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

banana :: Bitmap ()
banana = bitmap $ getDataFile "banana.png"

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
            let bmouse = toPoint2Double <$> stepper (point 0 0)
                    (filterJust $ justMotion <$> emouse)
            
            let
                btime :: Behavior t Double
                btime = (dt*) . fromIntegral <$> (accumB (0::Int) $ (+1) <$ etick)
            
                bvelocity :: Behavior t (Vector2 Double)
                bvelocity = speed <$> liftA2 vecBetween bposition bmouse
                
                bposition :: Behavior t (Point2 Double)
                bposition = accumB (point 0 0) $
                    (\v pos -> clipToFrame $ pointMove (v `vecScale` dt) pos)
                    <$> bvelocity <@ etick
                
                speed v = v `vecScale` (f $ vecLengthDouble v)
                    where
                    f d = (d/20)
            
                clipToFrame (Point x y) = Point
                        (clip 0 x (fromIntegral $ width  - bitmapWidth ))
                        (clip 0 y (fromIntegral $ height - bitmapHeight))
                    where clip a x b = max a (min x b)
                
                drawBanana :: Point -> DC a -> b -> IO ()
                drawBanana pos dc _view = drawBitmap dc banana pos True []
        
            -- draw the game state
            sink pp [on paint :== drawBanana . fromPoint2Double <$> bposition]
            reactimate $ repaint pp <$ etick
    
    network <- compile networkDescription    
    actuate network


toPoint2Double :: Point -> Point2 Double
toPoint2Double (Point x y) = Point (fromIntegral x) (fromIntegral y)

fromPoint2Double :: Point2 Double -> Point
fromPoint2Double (Point x y) = Point (ceiling x) (ceiling y)

justMotion :: EventMouse -> Maybe Point
justMotion (MouseMotion pt _) = Just pt
justMotion _                  = Nothing


