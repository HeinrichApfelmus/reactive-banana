{-# LANGUAGE TupleSections #-}
{-----------------------------------------------------------------------------
    Blackbaord is an application for drawing presentation slides.
    
    Main module
------------------------------------------------------------------------------}
module Main where

import qualified Data.Map as M
import Data.Monoid
import Data.List
import Debug.Trace
import System.FilePath
import Data.Functor

-- import EnableGUI
import qualified Graphics.UI.WX as WX

import Graphics as G
import Reactive hiding (filter)
import qualified Reactive as R
import Reactive.GraphicOpt
import Reactive.WX
import ToolPickers
import TextBox
import Controls

{-----------------------------------------------------------------------------
    Main function
    
    TODO
    Think about Controls.
        -> Hide dependence on mouse and keyboard events
        -> Layout
    
    In particular, make a  canvas  control that handles all the
    low-level details of efficient graphics updates and so on.
    We simply copy & paste bitmaps into and from this canvas control, then.
    
------------------------------------------------------------------------------}

main :: IO ()
main = runGUI $ do

    {- create menus
    -- file menu
    mfile  <- menuPane       [text := "&File"]
    mnew   <- menuItem mfile [text := "&New\tCtrl+N"
                             ,help := "Create new project"]
    mopen  <- menuItem mfile [text := "&Open...\tCtrl+O"
                             ,help := "Open existing project"]
    menuLine mfile
    mclose <- menuItem mfile [text := "&Close\tCtrl+W"
                             ,help := "Close project"]
    msave  <- menuItem mfile [text := "&Save\tCtrl+S"
                             ,help := "Save project"]
    msaveAs<- menuItem mfile [text := "&Save as...\tCtrl+Shift+S"
                             ,help := "Save project as"]
    menuLine mfile
    mquit  <- menuQuit mfile [help := "Quit the program"]
    
    medit  <- menuPane       [text := "&Edit"]
    mclear <- menuItem medit [text := "&Clear Screen"]
    -}
    
    -- create window
    w <- window windowSize [title := "Blackboard"]

    {-
    -- handle menu commands
    set f   [ on (menu mquit)   := close f
            , on (menu mclose)  := close f
            , on (menu msaveAs) := saveAs f =<< get vbitmap value
            , on (menu mclear)  := clear p dc
            ]
    set f
        [layout     := fill $ widget p
        ,clientSize := windowSize
        ,menuBar    := [mfile, medit]
        ]
    -}
            
    -- events for drawing
    let
        emouse    = event1 w mouse
        ekeyboard = event1 w keyboard
    
    -- the slides to draw on
    (slides, slideSwitches, slideControls) <- prepareSlides emouse
    
    let
        -- color picker
        (color, picker) =
            colorPicker [yellow, red, blue, green] (pt 620 10) emouse
    
        -- all paint events
        overpaints   = liveDrawings `R.union` controlUpdates
        liveDrawings = drawMouse color emouse
        
        -- controls as they are displayed
        -- FIXME: Wouldn't work for pure images!
        --       later: because??
        controls       = overTT textOverImage $
                         overTT picker $ overTT slideControls $ background
        controlUpdates = updateToPaint <$> updates
            where TimeGraphic _ updates = controls
        
        updateToPaint (g,r) = g r `over` erase r
        erase r = black `mask` fill (rectangle r)
        
        -- text box
        text :: TimeGraphic
        text   = textBox ekeyboard emouse ("" <$ afterTextLocked)
        
        -- time-varying image
        image :: Behavior GraphicOpt
        image = displayCurrentSlide <$> slides
        
        -- image updates with text on top
        textOverImage = overTB (overTT text dummy) image
        -- dummy forces graphics updates when slides are switched
        dummy = TimeGraphic (const G.empty) $
                    (const G.empty,canvasBox) <$
                        (traceEvent "dummy " $ () <$ changes slides)


        toolSwitches = never

        -- draw text on image
        -- events that indicate when the text should be locked
        -- and drawn into the image
        -- FIXME: doesn't work well with slide switches due to simultaneous
        --        occurrence. Until we can control for that, only
        --        react to tool switches, where the problem doesn't happen.
        (beforeTextLocked, afterTextLocked) = orderedDuplicate $
            slideSwitches `R.union` toolSwitches
        
        -- contents of the text box as a graphic
        -- assumption: the graphic updates are always the full things
        textGraphic = behavior G.empty $ (\(g,_) -> g canvasBox) <$> gs
            where TimeGraphic _ gs = text
        -- text box has to be drawn
        textDrawings = ((\g s () -> paintOnSlide g s >> return ())
            <$> textGraphic <*> slides)
            `apply` beforeTextLocked


    
    -- draw on image
    -- FIXME: order of drawing operations matters!
    --        the text should be drawn *before* the slide is switched...
    reactimate $ textDrawings
    -- FIXME: coordinates on screen do not coincide with coordinates
    --        on the bitmap! Need to offset!
    reactimate $ fmap (\s g -> paintOnSlide g s >> return ()) slides
        `apply` liveDrawings
    
    
    -- display initial window contents graphic
    let TimeGraphic g _ = controls
    overpaint (g windowRect) (screen w)
    
    -- reactimate the drawing operations
    reactimate $ (\g -> overpaint g (screen w)) <$> overpaints
    -- reactimate $ const (repaint w) `fmap` drawings

    where
    {-
        -- erase the drawing on the screen
    clear w dc = do
        drawRect dc (rectFromSize windowSize) [color := black, bgcolor := black]
        repaint w
    -}
    
    -- save the current picture
saveAs w = do
    name <- WX.fileSaveDialog (frame w) True True
                "Save project as..." fileTypes "." "My Project.png"
    let filename = maybe "./MyProject.png" id name
    writeImagePNG filename (screen w)

fileTypes = [("Blackboard project files",["*.*"])]

{-----------------------------------------------------------------------------
    UI geometry
------------------------------------------------------------------------------}
windowSize = sz 750 500
windowRect = rect (pt 0 0) windowSize

-- box containing the drawing surface
canvasSize = sz 600 400
canvasBox  = rect (pt 10 10) canvasSize

-- background image for the user interface
background :: TimeGraphic
background = TimeGraphic
    (withinBox box . mask white . stroke . rectangle $ box)
    never
    where box = rect (pt 8 8) (sz 602 402)

{-----------------------------------------------------------------------------
    Drawing with the mouse
------------------------------------------------------------------------------}
type MouseDrawState = [Point]

mouseDrag :: EventMouse -> MouseDrawState -> Change MouseDrawState
mouseDrag (MouseLeftDown new _) []      = Change [new]
mouseDrag (MouseLeftDrag new _) (old:_) = Change [new,old]
mouseDrag (MouseLeftUp   _   _) _       = Change []
mouseDrag _                     s       = Keep

-- myPen = [color := white, penJoin := JoinBevel, penWidth := 2]

drawMouseState :: Color -> MouseDrawState -> Graphic
drawMouseState c []          = G.empty
drawMouseState c [pos]       = G.empty      -- circle pos 1
drawMouseState c [pos1,pos2] = mask c .
                             -- fill $ circle pos1 1
                             stroke $ line pos1 pos2

-- strokes created by the mouse movements
drawMouse :: Behavior Color -> Event EventMouse -> Event Overpaint
drawMouse color emouse = id
    . apply (drawMouseState `fmap` color)       -- stroke state changes
    . changes
    . accumulate mouseDrag []                   -- accumulate state
    . R.filter (leftMouseInRectangle canvasBox) -- draw only in box
    $ emouse

{- what about capturing in the mouse?

    reactimate $ captureMouse mouseEvent w

captureMouse events w =
    const (windowCaptureMouse w) `fmap` filterE isMouseDown events
    where
    isMouseDown (MouseLeftDown _ _) = True
    isMouseDown _                   = False
-}



{-----------------------------------------------------------------------------
    A bag of drawings
------------------------------------------------------------------------------}
type Index  = Int
type Slides = (Index, M.Map Index Image)

emptySlides = (0,M.empty)

-- create a new slide for perusal
newSlide :: Slides -> IO Slides
newSlide (_,slides) = do
    image <- newImage canvasSize
    let focus = M.size slides
    return $ (focus, M.insert focus image slides)

-- paint a graphic on a slide
paintOnSlide :: Overpaint -> Slides -> IO Slides
paintOnSlide g s@(focus, slides) = do
    overpaint g (slides M.! focus)
    return s

-- draw the current slide
displayCurrentSlide :: Slides -> GraphicOpt
displayCurrentSlide (focus, slides) = 
    withinBox r $ mask (slides M.! focus `at` (pt 0 0)) r
    where r = canvasBox


-- create the slides and controls used to change the current slide
prepareSlides :: Event EventMouse
               -> Prepare (Behavior Slides, Event (), TimeGraphic)
prepareSlides emouse = do
    -- the current directory is the directory in which the .app
    -- bundles resides
    let dir = "data/images"
    iup    <- readImagePNG (dir </> "up.png")
    idown  <- readImagePNG (dir </> "down.png")
    iplus  <- readImagePNG (dir </> "plus.png")

    slide0 <- newSlide emptySlides

    let
        (eup  , gup  ) = imageButton iup   (pt 700  30) emouse
        (edown, gdown) = imageButton idown (pt 700  70) emouse
        (eplus, gplus) = imageButton iplus (pt 700 110) emouse
        
        glabel = label (rect (pt 680 10) (sz 750 20)) $
            fmap (\x -> "Slide " ++ show (fst x)) slides
        
        g = foldr1 overTT [gup, gdown, gplus, glabel]
    
        up   (focus,slides) = (min (M.size slides-1) (focus+1), slides)
        down (focus,slides) = (max 0 (focus-1), slides)
        plus = newSlide
        
        lift   f = fmap (\() -> return . f)
        liftIO f = fmap (\() -> f)
    
        slides = accumulate ($) slide0
                . foldr1 R.union
                $ [lift up eup, lift down edown, liftIO plus eplus]
        
        -- We want slidesSwitches events happen *before* the slides are drawn.
        (slideChanges1, slideChanges2) = orderedDuplicate $ changes slides
        slides' = behavior slide0 slideChanges2
        -- records when a slide is switched, as opposed to only being drawn upon
        slideSwitches = () <$ slideChanges1
        
    return (slides', slideSwitches, g)
    

