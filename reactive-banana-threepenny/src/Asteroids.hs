{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example:
    Asteroids, adapted from
        http://www.haskell.org/haskellwiki/WxAsteroids
    
    The original example has a few graphics issues
    and I didn't put much work into correcting them.
    For more, see also 
    https://github.com/killerswan/wxAsteroids/issues/1
    http://comments.gmane.org/gmane.comp.lang.haskell.wxhaskell.general/1086
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

import Control.Monad
import System.Random

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (Event)

import Reactive.Banana
import Reactive.Banana.Threepenny


import Paths (getDataFile)
import System.FilePath

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
-- constants
height, width, diameter :: Int
height   = 300
width    = 300
diameter = 24

chance   :: Double 
chance   = 0.1

-- explode :: WXCore.Sound ()
-- explode = sound $ getDataFile "explode.wav" 

{-----------------------------------------------------------------------------
    Game Logic 
------------------------------------------------------------------------------}
-- main game function
main :: IO ()
main = do
    startGUI Config
        { tpPort       = 10000
        , tpCustomHTML = Nothing
        , tpStatic     = ""
        } asteroids

asteroids :: Window -> IO ()
asteroids window = do
    return window # set title "Asteroids"
    
    -- user interface
    canvas <- UI.canvas
        # set UI.height height
        # set UI.width  width
        # set style [("border","solid black 1px")]
        # set (attr "tabindex") "1" -- allow keyboard events here
    status <- UI.span # set text "Welcome to asteroids"
    
    -- resources: images and audio
    let
        loadImage name = do
            url <- loadFile window "image/png" =<< getDataFile (name <.> "png")
            img <- UI.img # set UI.src url
            return $ \(Point x y) -> UI.drawImage img (x,y) canvas

    [drawShip, drawRock, drawBurning] <- mapM loadImage (words "ship rock burning")
    audio <- do
        url <- loadFile window "audio/wav" =<< getDataFile "explode.wav"
        UI.audio # set UI.src url
    let
        clearCanvas   = UI.clearCanvas canvas
        playExplosion = UI.audioPlay audio
        resources     = Resources {..}    
    
    getBody window #+ [column
        [element canvas, element status
        ,string "Click on the canvas and use the arrow keys to move."]
        ,element audio]

{-
    game  <- menuPane      [ text := "&Game" ] 
    new   <- menuItem game [ text := "&New\tCtrl+N", help := "New game" ]
    pause <- menuItem game [ text      := "&Pause\tCtrl+P" 
                           , help      := "Pause game" 
                           , checkable := True
                           ] 
    menuLine game
    quit  <- menuQuit game [help := "Quit the game"] 
	
    set new   [on command := asteroids] 
    set pause [on command := set t [enabled :~ not]] 
    set quit  [on command := close ff]
    
    set ff [menuBar := [game]]
    
    pp <- panel ff []
    set ff [ layout  := minsize (sz width height) $ widget pp ]
    set pp [ on (charKey '-') := set t [interval :~ \i -> i * 2] 
           , on (charKey '+') := set t [interval :~ \i -> max 10 (div i 2)] 
           ]
-}


    timer <- UI.timer # set UI.interval 50

    -- event network
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            -- timer
            etick  <- event UI.tick timer
    
            -- keyboard events
            ekey   <- event UI.keydown canvas
            let eleft  = filterE (== 37) ekey
                eright = filterE (== 39) ekey
        
            -- ship position
            let
                bship :: Behavior t Int
                bship = accumB (width `div` 2) $
                    (goLeft <$ eleft) `union` (goRight <$ eright)
            
                goLeft  x = max 0          (x - 5)
                goRight x = min (width-30) (x + 5)
        
            -- rocks
            brandom <- fromPoll (randomRIO (0,1) :: IO Double)
            let
                brocks :: Behavior t [Rock]
                brocks = accumB [] $
                    (advanceRocks <$ etick) `union`
                    (newRock <$> filterE (< chance) (brandom <@ etick))
        
            -- draw the game state
            reactimate $ (drawGameState resources <$> bship <*> brocks) <@ etick
        
            -- status bar
            let bstatus :: Behavior t String
                bstatus = (\r -> "rocks: " ++ show (length r)) <$> brocks
            return status # sink UI.text bstatus
    
    network <- compile networkDescription    
    actuate network
    UI.start timer


-- rock logic
type Position = Point
type Rock     = [Position] -- lazy list of future y-positions

newRock :: Double -> [Rock] -> [Rock]
newRock r rs = (track . floor $ fromIntegral width * r / chance) : rs

track :: Int -> Rock
track x = [point x (y - diameter) | y <- [0, 6 .. height + 2 * diameter]]

advanceRocks :: [Rock] -> [Rock]
advanceRocks = filter (not . null) . map (drop 1)

collide :: Position -> Position -> Bool
collide pos0 pos1 = 
    let distance = vecLength (vecBetween pos0 pos1) 
    in distance <= fromIntegral diameter

-- draw game state
data Resources = Resources
    { drawShip      :: Position -> IO ()
    , drawRock      :: Position -> IO ()
    , drawBurning   :: Position -> IO ()
    , clearCanvas   :: IO ()
    , playExplosion :: IO ()
    }

drawGameState :: Resources -> Int -> [Rock] -> IO ()
drawGameState r@(Resources{..}) ship rocks = do
    let
        shipLocation = point ship (height - 2 * diameter)
        positions    = map head rocks
        collisions   = map (collide shipLocation) positions

    clearCanvas
    drawShip shipLocation
    mapM_ (drawEnemyRock r) (zip positions collisions) 

    when (or collisions) playExplosion

drawEnemyRock :: Resources -> (Position, Bool) -> IO ()
drawEnemyRock (Resources{..}) (pos, collides) = draw pos
    where draw = if collides then drawBurning else drawRock


{-----------------------------------------------------------------------------
    2D Geometry
------------------------------------------------------------------------------}
data Point  = Point !Int !Int deriving (Show)
data Vector = Vec !Int !Int deriving (Show)

point = Point

vecX (Vec x _) = x
vecY (Vec _ y) = y
vec x y = Vec x y
-- vecScale (Vec x y) s = Vec (s*x) (s*y)
vecAdd   (Vec x1 y1) (Vec x2 y2) = Vec (x1+x2) (y1+y2)
vecSub   (Vec x1 y1) (Vec x2 y2) = Vec (x1-x2) (y1-y2)
vecLength :: Vector -> Double
vecLength (Vec x y) = sqrt $ fromIntegral $ x*x + y*y
vecBetween (Point x1 y1) (Point x2 y2) = Vec (x2-x1) (y2-y1)
