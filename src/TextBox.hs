{-# LANGUAGE TupleSections #-}
{-----------------------------------------------------------------------------
    Blackbaord is an application for drawing presentation slides.
    
    TextBox - Implementation of a box to type text in
------------------------------------------------------------------------------}
module TextBox (textBox) where

import Data.Monoid

import Graphics as G
import Reactive
import Reactive.GraphicOpt

{-----------------------------------------------------------------------------
    Exported Funciton
------------------------------------------------------------------------------}
    -- tne text box
textBox :: Event EventKey -> Event EventMouse -> Event String -> TimeGraphic
textBox ekeyboard emouse estring = TimeGraphic (const G.empty) $ id
    . fmap toGraphic
    . changes
    . accumulate ($) (initialTextBox { position = position })
    $ (fmap keypress ekeyboard `union` fmap setText estring)
    where
    toGraphic textbox = (withinBox rect (draw textbox), rect)
        where rect = boundingRect textbox
    -- FIXME: calculation of dirty rectangles is wrong!
    position = pt 10 10

{-
    FIXME: Textbox slows to a crawl after a while, 100% CPU usage!
    What happens? Bad calculation of dirty rectangles?
        Size of rectangles does *not* matter.
    Text too long?
        Maybe allocating many new white brushes eats up the CPU?
        In that case, the graphics library needs to be optimized.
        
    Experiment:
    only the last letter of the whole text box
        -> this is really fast!
        -> The problem must be the drawing operations, especially
           always setting the brush to white!
-}

{-
reactimateTextBox :: Window -> Prepare ()
reactimateTextBox window =
    reactimate $
        fmap (\u -> overpaint (updateToPaint u) (screen window))
        $ textBox (event1 window keyboar) (event1 window mouse)
-}

{-----------------------------------------------------------------------------
    Internals
------------------------------------------------------------------------------}
type Text    = String    -- a single string, with newlines
type Cursor  = Int       -- a single position inside the string
                         -- before the character in question
data TextBox = TextBox { contents :: Text
                       , cursor   :: Cursor
                       , position :: Point }

initialTextBox =
    TextBox { contents = "", cursor = 0, position = pt 10 10 } 

    -- draw the contents of the text box
draw :: TextBox -> Graphic
draw textbox =
    mconcat . map drawChar . concat $ positions textbox
    where
    drawChar (pt,c) = mask white . stroke $ text pt [c]

boundingRect :: TextBox -> Rect
boundingRect t = rect (position t) size
    where
    text = lines $ contents t
    size = sz (dx * (maximum (0:map length text) + 1)) (dy * (length text + 1))

dx = 10
dy = 14

    -- calculate the positions of each character
positions :: TextBox -> [[(Point, Char)]]
positions t = result
    where
    text = lines $ contents t
    x0   = pointX $ position t
    y0   = pointY $ position t
    
    result = [[(pt (x0+x*dx) (y0+y*dy), c)
        | (x,c   ) <- zip [0..] line]
        | (y,line) <- zip [0..] text]

    -- set the whole text at once 
setText :: String -> TextBox -> Change TextBox
setText s t = Change $ initialTextBox { contents = s }

    -- adjust the text box depending on which key was pressed
keypress :: EventKey -> TextBox -> Change TextBox
keypress (EventKey key modifiers point) t =
    case key of
        KeyChar c -> Change $ t { contents = char c   , cursor = k+1 }
        KeySpace  -> Change $ t { contents = char ' ' , cursor = k+1 }
        KeyReturn -> Change $ t { contents = char '\n', cursor = k+1 }
        
        KeyLeft   -> Change $ t { cursor   = max 0 (k-1) }
        KeyRight  -> Change $ t { cursor   = min (length text) (k+1) }
        
        KeyBack   -> Change $ t { contents = delete (k-1) text 
                                , cursor   = max 0 (k-1) }
        KeyDelete -> Change $ t { contents = delete k text }

        _         -> Keep
    where
    text  = contents t
    k     = cursor   t

    char c = insertAt k text c
    insertAt k xs x = let (ls,rs)   = splitAt k xs in ls ++ x:rs
    
    delete k    xs
        | k <  0         = xs
        | k == length xs = xs
        | otherwise      = let (ls,x:rs) = splitAt k xs in ls ++ rs
    
    -- changeAt k xs f = let (ls,x:rs) = splitAt k xs in ls ++ f x:rs


