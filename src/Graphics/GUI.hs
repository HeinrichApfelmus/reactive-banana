{-# LANGUAGE ExistentialQuantification, RankNTypes, MultiParamTypeClasses,
    TypeSynonymInstances #-}
{-----------------------------------------------------------------------------
    A really simple GUI library, similar to SOE.
    
    Currently implemented as a wrapper on wxHaskell.
------------------------------------------------------------------------------}

module Graphics.GUI where

import Data.Monoid
import Control.Monad
import Control.Exception (bracket)
import System.IO.Unsafe

import qualified Graphics.UI.WX as WX
import qualified Graphics.UI.WXCore as WX hiding (Timer, Event)
import qualified Graphics.UI.WX.Events as WX

import Graphics.Geometry
import Graphics.Events

{-----------------------------------------------------------------------------
    Windows and Timers
------------------------------------------------------------------------------}
    -- run the stuff
runGUI :: IO () -> IO ()
runGUI = WX.start

data Window = Window {
              frame  :: WX.Frame ()
            , buffer :: Bitmap
            , panel  :: WX.Panel () }

    -- create a new window with corresponding double buffer
window :: Size -> [Prop Window] -> IO Window
window size props = do
    frame  <- WX.frame []        -- create window itself
    buffer <- newBitmap size     -- double buffer to paint in
    panel  <- WX.panel frame []  -- window area to paint in
    
        -- handle paint events by drawing the buffer
    let onPaint dc = WX.drawBitmap dc (wxbitmap buffer) WX.pointZero False []
    WX.set panel [ WX.on WX.paintRaw := \dc _ _ -> onPaint dc ]
    
    WX.set frame [ WX.layout     := WX.fill $ WX.widget panel
                 , WX.clientSize := size ]
    
        -- set user specified properties
    let window = Window { frame = frame, buffer = buffer, panel = panel }
    WX.set window props -- FIXME: title must be set on creation?
    return window

    -- export window attributes
title :: Attr Window String
title = WX.castAttr frame WX.text

mouse :: WX.Event Window (EventMouse -> IO ())
mouse = castEvent "mouse" panel WX.mouse
    
keyboard :: WX.Event Window (EventKey -> IO ())
keyboard = castEvent "keyboard" panel WX.keyboard

castEvent :: String -> (v -> w) -> WX.Event w a -> WX.Event v a
castEvent name coerce event =
    WX.newEvent name
        (\v -> WX.get (coerce v) (WX.on event))
        (\v a -> WX.set (coerce v) [WX.on event := a])

{-----------------------------------------------------------------------------
    Timers
------------------------------------------------------------------------------}
type Timer = WX.Timer

timer :: Window -> [Prop Timer] -> IO Timer
timer window = WX.timer (frame window)

interval :: Attr Timer Int
interval = WX.interval

command :: WX.Event Timer (IO ())
command = WX.command

{-----------------------------------------------------------------------------
    Graphics
------------------------------------------------------------------------------}
type Coordinate = Int
-- type Point      = (Coordinate,Coordinate)
type Length     = Coordinate

newtype Graphic = Graphic (forall a. WX.DC a -> IO ())
newtype Path    = Path (forall a. WX.DC a -> [Prop (WX.DC a)] -> IO ())
data    Mask    = Fill Path | Stroke Path
type    Color   = WX.Color

fill   = Fill
stroke = Stroke

    -- class to make masking operation polymorphic
    -- Ideally, we would use Cairo's drawing model,
    -- but different graphics libraries usually don't support it in
    -- full generality, so we have to implement lots of
    -- special cases in the class
class Draw canvas mask where
    mask :: canvas -> mask -> Graphic

    -- main function for drawing stuff
instance Draw Color Mask where
    mask c (Stroke (Path p)) = Graphic $ \dc -> p dc
        [WX.color := c, WX.brushKind := WX.BrushTransparent]
    mask c (Fill   (Path p)) = Graphic $ \dc -> p dc
        [ WX.brush := WX.BrushStyle WX.BrushSolid c
        , WX.penKind := WX.PenTransparent]

-- rgba :: Double -> Double -> Double -> Double -> Color
-- rgba r g b a = WX.rgb
rgb r g b = WX.rgb (toInt r) (toInt g) (toInt b) -- rgba r g b 1
    where toInt x = round (255*x)

black = rgb 0 0 0
white = rgb 1 1 1
red   = rgb 1 0 0
green = rgb 0 1 0 
blue  = rgb 0 0 1
yellow= rgb 1 1 0

    -- overlay graphics
instance Monoid Graphic where
    mempty  = empty
    mappend = over

empty :: Graphic
empty = Graphic $ \_ -> return ()

    -- paint the first argument over the second argument
over :: Graphic -> Graphic -> Graphic
(Graphic x) `over` (Graphic y) = Graphic $ \dc -> y dc >> x dc

    -- concatenate paths
-- instance Monoid Path where
--    mempty  = Path $ \_ _ -> return ()
--    (Path x) `mappend` (Path y) = Path $ \c dc -> x c dc >> y c dc

    -- drawing primitives
line :: Point -> Point -> Path
line p1 p2 = Path $ \dc -> WX.line dc p1 p2

    -- FIXME: wxHaskell cannot fill polyline, only stroke it
polyline :: [Point] -> Path
polyline ps = Path $ \dc -> WX.polyline dc ps
    -- obsolete definition in terms of line
    -- mconcat . map (uncurry line) . pairs
    -- where pairs xs = zip xs (tail xs)

rectangle :: Rect -> Path
rectangle rect = Path $ \dc -> WX.drawRect dc rect

circle  :: Point -> Length -> Path
circle point radius = Path $ \dc -> WX.circle dc point radius

text :: Point -> String -> Path
text point str = Path $ \dc props ->
    WX.drawText dc str point ((WX.fontFace :="Courier") : props)

    -- drawing images
data PostionedImage = At Image Point
at :: Image -> Point -> PostionedImage
at = At

instance Draw PostionedImage Rect where
    (At image point) `mask` rect =
        Graphic $ \dest -> withWxDC image $ \source ->
            WX.dcBlit dest rect source point' WX.wxCOPY False >> return ()
        where
        point' = pt (pointX (topLeft rect) - pointX point)
                    (pointY (topLeft rect) - pointY point)

-- image :: Point -> Image -> Graphic
-- image point image = undefined -- Path $ \dc -> WX.drawImage dc image point

{-----------------------------------------------------------------------------
    Images
------------------------------------------------------------------------------}
    -- image - either a window screen or a bitmap
data Image = ImageWindow Window
           | ImageBitmap Bitmap

    -- (internal) extract wxBitmap from image
getWxBitmap :: Image -> WX.Bitmap ()
getWxBitmap (ImageWindow window) = wxbitmap . buffer $ window
getWxBitmap (ImageBitmap bitmap) = wxbitmap bitmap

    -- (internal) extract wxDC from image
withWxDC :: Image -> (forall a. WX.DC a -> b) -> b
withWxDC (ImageBitmap bitmap) f = f (dc bitmap)
withWxDC (ImageWindow window) f = undefined -- FIXME

    -- size of an image
imageSize :: Image -> Size
imageSize image = unsafePerformIO $
    liftM2 sz (WX.bitmapGetWidth bitmap) (WX.bitmapGetHeight bitmap)
    where
    bitmap = getWxBitmap image

    -- create a new blank Image of a given size
newImage :: Size -> IO Image
newImage size = ImageBitmap `fmap` newBitmap size

    -- load image from disk, PNG format
readImagePNG  :: FilePath -> IO Image
readImagePNG filename =
    liftM ImageBitmap $
    fromWXBitmap =<< WX.bitmapCreateLoad filename WX.wxBITMAP_TYPE_PNG
    
    -- save image to disk, PNG format
writeImagePNG :: FilePath -> Image -> IO ()
writeImagePNG filename image = do
    palette <- WX.paletteCreateDefault
    WX.bitmapSaveFile (getWxBitmap image) filename WX.wxBITMAP_TYPE_PNG palette
    return ()

    -- the image corresponding to the window on the screen
screen :: Window -> Image
screen = ImageWindow

    -- paint a graphics onto an image
    -- warning: this is a destructive operation
    --          if you use the image to be overwritten in the 
    --          graphics, then you're hosed
overpaint :: Graphic -> Image -> IO ()
overpaint (Graphic g) image@(ImageWindow window) = do
    -- withBitmap (getWxBitmap image) g
    g (dc (buffer window))            -- draw in bitmap first
    WX.withClientDC (panel window) g  -- also draw in the window directly
overpaint (Graphic g) image@(ImageBitmap bitmap) = do
    -- withBitmap (getWxBitmap image) g
    g (dc bitmap)                     -- only draw into bitmap

{-----------------------------------------------------------------------------
    Bitmaps (internal)
------------------------------------------------------------------------------}
data Bitmap = Bitmap { wxbitmap :: WX.Bitmap (), dc :: WX.MemoryDC () }

    -- allocate a new bitmap and corresponding drawing context
newBitmap :: Size -> IO Bitmap
newBitmap size = do
    wximage  <- WX.imageCreateSized size
    wxbitmap <- WX.bitmapCreateFromImage wximage (-1)
    fromWXBitmap wxbitmap

    -- (internal) create a bitmap from a WX.Bitmap
fromWXBitmap :: WX.Bitmap () -> IO Bitmap
fromWXBitmap wxbitmap = do
    dc     <- WX.memoryDCCreate
    WX.memoryDCSelectObject dc wxbitmap
    return $ Bitmap wxbitmap dc


{-
    -- (unused)
    -- draw into a bitmap and delete the drawing context afterwards
withBitmap :: WX.Bitmap a -> (WX.MemoryDC () -> IO b) -> IO b
withBitmap bitmap f =
  bracket
    (do dc <- WX.memoryDCCreate
        WX.memoryDCSelectObject dc bitmap
        return dc)
    (\dc -> WX.memoryDCDelete dc)
    (f)
-}
