{-----------------------------------------------------------------------------
    reactive-banana
    
    Example: Slot machine
------------------------------------------------------------------------------}
import Reactive

import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import System.Random


main :: IO ()
main = do
    displayHelpMessage
    eventSources <- setupEvents
    eventLoop eventSources

displayHelpMessage :: IO ()
displayHelpMessage = mapM_ putStrLn $
    "-----------------------------":
    "- THE REACTIVE SLOT MACHINE -":
    "------ WIN A BANANA ---------":
    "":
    "Commands are:":
    "   coin    - insert a coin":
    "   play    - play one game":
    "   quit    - quit the program":
    "":
    []
    
-- Read commands and fire corresponding events 
eventLoop :: (EventSource (), EventSource ()) -> IO ()
eventLoop (escoin,esplay) = loop
    where
    loop = do
        putStr "> "
        s <- getLine
        case s of
            "coin" -> fire escoin ()    -- fire corresponding event sources
            "play" -> fire esplay ()
            "quit" -> return ()
            _      -> putStrLn $ s ++ " - unkonwn command"
        when (s /= "quit") loop


{-----------------------------------------------------------------------------
    Program logic
------------------------------------------------------------------------------}
type Money = Int
-- State of the reels, consisting of three numbers from 1-3. Example: "222"
type Reels = (Int,Int,Int)
-- A win consist of either double or triple numbers
data Win = Double | Triple

-- payout for each win
payout :: Win -> Money
payout Double = 20
payout Triple = 200


-- Set up the program logic in terms of events and behaviors.
setupEvents :: Prepare (EventSource (), EventSource ())
setupEvents = do
    -- create two event sources
    escoin <- newEventSource
    esplay <- newEventSource
    
    -- initial random number generator
    initialStdGen <- newStdGen
    
    let
        -- Derive events from the sources.
        -- They correspond to the user commands  coin  and  play
        -- respectively.
        ecoin, eplay :: Event ()
        ecoin = fromEventSource escoin
        eplay = fromEventSource esplay
        
        
        -- The state of the slot machine is captured in Behaviors.
            
        -- State: credits that the player has to play the game
        -- The  ecoin      event adds a coin to the credits
        -- The  edoesplay  event removes money
        -- The  ewin       event adds credits because the player has won
        bcredits :: Behavior Money
        bcredits = accumulate ($) 0 $
            ((addCredit <$ ecoin)
            `union` (removeCredit <$ edoesplay)
            `union` (addWin <$> ewin))
        
        -- functions that change the accumulated state
        addCredit     = (+1)
        removeCredit  = subtract 1
        addWin Double = (+5)
        addWin Triple = (+20)
        
        -- Event: does the player have enough money to play the game?
        emayplay :: Event Bool
        emayplay = apply ((\credits _ -> credits > 0) <$> bcredits) eplay
        -- Event: player has enough coins and plays
        edoesplay :: Event ()
        edoesplay = () <$ Reactive.filter id  emayplay
        -- Event: event that fires when the player doesn't have enough money
        edenied   :: Event ()
        edenied   = () <$ Reactive.filter not emayplay
        
        
        -- State: random number generator
        bstdgen :: Behavior StdGen
        eroll   :: Event Reels
        -- accumulate the random number generator while rolling the reels
        (bstdgen, eroll) = mapAccum roll initialStdGen edoesplay
        
        -- roll the reels
        roll :: StdGen -> () -> (StdGen, Reels)
        roll gen0 () = (gen3, (z1,z2,z3))
            where
            (z1,gen1) = randomR (1,9) gen0
            (z2,gen2) = randomR (1,9) gen1
            (z3,gen3) = randomR (1,9) gen2
        
        -- Event: it's a win!
        ewin :: Event Win
        ewin = fmap fromJust $ Reactive.filter isJust $ fmap checkWin eroll
        checkWin (z1,z2,z3)
            | z1 == z2 || z2 == z3 || z3 == z1 = Just Double
            | z1 == z2 && z2 == z3             = Just Triple
            | otherwise                        = Nothing


    putStrLn "test"    
    reactimate $ putStrLn . showCredit <$> (changes bcredits)
    reactimate $ putStrLn . showRoll   <$> eroll
    reactimate $ putStrLn . showWin    <$> ewin
    reactimate $ putStrLn "Not enough credits!" <$ edenied
    
    return (escoin, esplay)


showCredit money    = "Credits: " ++ show money
showRoll (z1,z2,z3) = "You rolled  " ++ show z1 ++ show z2 ++ show z3
showWin Double = "Wow, a double!"
showWin Triple = "Wowwowow! A triple! So awesome!"



