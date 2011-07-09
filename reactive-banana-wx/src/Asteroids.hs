{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: Asteroids, adapted from
    http://www.haskell.org/haskellwiki/WxAsteroids
------------------------------------------------------------------------------}
import Graphics.UI.WX hiding (Event)
import Graphics.UI.WXCore as WXCore
import Reactive.Banana
import Reactive.Banana.WX
import System.Directory   (getCurrentDirectory, setCurrentDirectory)
import System.Random
-- import Paths_wxAsteroids  (getDataDir)

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
rock    = bitmap "data/rock.ico"
burning = bitmap "data/burning.ico"
ship    = bitmap "data/ship.ico"

explode :: WXCore.Sound ()
explode = sound  "data/explode.wav" 

getDataDir = (++ "/data") <$> getCurrentDirectory

main :: IO ()
main = 
    -- dataDirectory <- getDataDir
    -- setCurrentDirectory dataDirectory 
    start asteroids

{-----------------------------------------------------------------------------
    Game Logic 
------------------------------------------------------------------------------}
-- main game function
asteroids :: IO () 
asteroids = do
    f  <- frame   [ resizeable := False ]

    status <- statusField [text := "Welcome to asteroids"] 
    set f [statusBar := [status]] 

    t  <- timer f [ interval   := 50 ]

    game <- menuPane       [ text := "&Game" ] 
    new  <- menuItem game  [ text := "&New\tCtrl+N", help := "New game" ]
    pause <- menuItem game [ text      := "&Pause\tCtrl+P" 
                           , help      := "Pause game" 
                           , checkable := True
                           ] 
    menuLine game
    quit <- menuQuit game [help := "Quit the game"] 
	
    set new   [on command := asteroids] 
    set pause [on command := set t [enabled :~ not]] 
    set quit  [on command := close f]
    
    set f [menuBar := [game]]
    
    set f [ text        := "Asteroids" 
          , bgcolor     := white 
          , layout      := space width height
          , on (charKey '-') := set t [interval :~ \i -> i * 2] 
          , on (charKey '+') := set t [interval :~ \i -> max 10 (div i 2)] 
          ]
    
    -- event network
    network <- compile $ do
        -- timer
        etick  <- event0 t command
    
        -- keyboard events
        ekey   <- event1 f keyboard
        let eleft  = filterE ((== KeyLeft ) . keyKey) ekey
            eright = filterE ((== KeyRight) . keyKey) ekey
        
        -- ship position
        let
            bship :: Behavior Int
            bship = accumB (width `div` 2) $
                (goLeft <$ eleft) `union` (goRight <$ eright)
            
            goLeft  x = max 0     (x - 5)
            goRight x = min width (x + 5)
        
        -- rocks
        brandom <- fromPoll (randomRIO (0,1) :: IO Double)
        let
            brocks :: Behavior [Rock]
            brocks = accumB [] $
                (advanceRocks <$ etick) `union`
                (newRock <$> filterE (< chance) (brandom <@ etick))
        
        -- draw the game state
        sink f [on paint :== stepperD (\_dc _ -> return ()) $
                (drawGameState <$> bship <*> brocks) <@ etick]
        reactimate $ repaint f <$ etick
        
        -- status bar
        let dstatus :: Discrete String
            dstatus = stepperD "Welcome to asteroids" $
                ((\r -> "rocks: " ++ show (length r)) <$> brocks) <@ etick
        sink status [text :== dstatus]
    
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
