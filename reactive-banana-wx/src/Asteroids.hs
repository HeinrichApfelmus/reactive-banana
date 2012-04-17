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
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. NetworkDescription t"

import Graphics.UI.WX hiding (Event)
import Graphics.UI.WXCore as WXCore
import Reactive.Banana
import Reactive.Banana.WX
import System.Random

import Paths (getDataFile)

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

rock, burning, ship :: Bitmap ()
rock    = bitmap $ getDataFile "rock.ico"
burning = bitmap $ getDataFile "burning.ico"
ship    = bitmap $ getDataFile "ship.ico"

explode :: WXCore.Sound ()
explode = sound $ getDataFile "explode.wav" 

main :: IO ()
main = start asteroids

{-----------------------------------------------------------------------------
    Game Logic 
------------------------------------------------------------------------------}
-- main game function
asteroids :: IO ()
asteroids = do
    ff <- frame [ text       := "Asteroids"
                , bgcolor    := white
                , resizeable := False ]

    status <- statusField [text := "Welcome to asteroids"] 
    set ff [statusBar := [status]] 

    t  <- timer ff [ interval   := 50 ]

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
    
    -- event network
    let networkDescription :: forall t. NetworkDescription t ()
        networkDescription = do
            -- timer
            etick  <- event0 t command
    
            -- keyboard events
            ekey   <- event1 pp keyboard
            let eleft  = filterE ((== KeyLeft ) . keyKey) ekey
                eright = filterE ((== KeyRight) . keyKey) ekey
        
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
            sink pp [on paint :== stepper (\_dc _ -> return ()) $
                     (drawGameState <$> bship <*> brocks) <@ etick]
            reactimate $ repaint pp <$ etick
        
            -- status bar
            let bstatus :: Behavior t String
                bstatus = (\r -> "rocks: " ++ show (length r)) <$> brocks
            sink status [text :== bstatus]
    
    network <- compile networkDescription    
    actuate network


-- rock logic
type Position = Point2 Int
type Rock     = [Position] -- lazy list of future y-positions

newRock :: Double -> [Rock] -> [Rock]
newRock r rs = (track . floor $ fromIntegral width * r / chance) : rs

track :: Int -> Rock
track x = [point x (y - diameter) | y <- [0, 6 .. height + 2 * diameter]]

advanceRocks :: [Rock] -> [Rock]
advanceRocks = filter (not . null) . map (drop 1)



-- draw game state
drawGameState :: Int -> [Rock] -> DC a -> b -> IO ()
drawGameState ship rocks dc _view = do
    let
        shipLocation = point ship (height - 2 * diameter)
        positions    = map head rocks
        collisions   = map (collide shipLocation) positions

    drawShip dc shipLocation
    mapM (drawRock dc) (zip positions collisions) 

    when (or collisions) (play explode)

collide :: Position -> Position -> Bool
collide pos0 pos1 = 
    let distance = vecLength (vecBetween pos0 pos1) 
    in distance <= fromIntegral diameter 

drawShip :: DC a -> Point -> IO ()
drawShip dc pos = drawBitmap dc ship pos True [] 

drawRock :: DC a -> (Point, Bool) -> IO ()
drawRock dc (pos, collides) = 
    let rockPicture = if collides then burning else rock
    in drawBitmap dc rockPicture pos True []
