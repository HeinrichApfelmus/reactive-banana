{-----------------------------------------------------------------------------
    reactive-banana
    
    Example: Slot machine
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-}
    -- allows pattern signatures like
    -- do
    --     (b :: Behavior Int) <- stepper 0 ...
{-# LANGUAGE RecursiveDo #-}
    -- allows recursive do notation
    -- mdo
    --     ...

import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.List (nub)
import System.Random
import System.IO
import Debug.Trace
import Data.IORef

import Reactive.Banana as R
import Reactive.Banana.Frameworks as R


main :: IO ()
main = do
    displayHelpMessage
    sources <- makeSources
    network <- compile $ networkDescription sources
    actuate network
    eventLoop sources

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

-- Create event sources corresponding to  coin  and  play
makeSources = (,) <$> newAddHandler <*> newAddHandler

-- Read commands and fire corresponding events 
eventLoop :: (EventSource (), EventSource ()) -> IO ()
eventLoop (escoin,esplay) = loop
    where
    loop = do
        putStr "> "
        hFlush stdout
        s <- getLine
        case s of
            "coin" -> fire escoin ()    -- fire corresponding events
            "play" -> fire esplay ()
            "quit" -> return ()
            _      -> putStrLn $ s ++ " - unknown command"
        when (s /= "quit") loop

{-----------------------------------------------------------------------------
    Event sources
------------------------------------------------------------------------------}
-- Event Sources - allows you to register event handlers
-- Your GUI framework should provide something like this for you
type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

{-----------------------------------------------------------------------------
    Program logic
------------------------------------------------------------------------------}
type Money = Int
-- State of the reels, consisting of three numbers from 1-4. Example: "222"
type Reels = (Int,Int,Int)
-- A win consist of either double or triple numbers
data Win = Double | Triple


-- Program logic in terms of events and behaviors.
networkDescription :: (EventSource (), EventSource ()) -> MomentIO ()
networkDescription (escoin,esplay) = mdo
    -- initial random number generator
    initialStdGen <- liftIO $ newStdGen

    -- Obtain events corresponding to the  coin  and  play  commands
    ecoin <- fromAddHandler (addHandler escoin)
    eplay <- fromAddHandler (addHandler esplay)
    
    -- The state of the slot machine is captured in Behaviors.
        
    -- State: credits that the player has to play the game
    -- The  ecoin      event adds a coin to the credits
    -- The  edoesplay  event removes money
    -- The  ewin       event adds credits because the player has won
    (ecredits :: Event Money, bcredits :: Behavior Money)
        <- mapAccum 0 . fmap (\f x -> (f x,f x)) $ unions $
            [ addCredit    <$ ecoin
            , removeCredit <$ edoesplay
            , addWin       <$> ewin
            ]
    let
        -- functions that change the accumulated state
        addCredit     = (+1)
        removeCredit  = subtract 1
        addWin Double = (+5)
        addWin Triple = (+20)
        
        -- Event: does the player have enough money to play the game?
        emayplay :: Event Bool
        emayplay = (\credits _ -> credits > 0) <$> bcredits <@> eplay
        
        -- Event: player has enough coins and plays
        edoesplay :: Event ()
        edoesplay = () <$ filterE id  emayplay
        -- Event: event that fires when the player doesn't have enough money
        edenied   :: Event ()
        edenied   = () <$ filterE not emayplay
        
        
    -- State: random number generator
    (eroll :: Event Reels, bstdgen :: Behavior StdGen)
        -- accumulate the random number generator while rolling the reels
        <- mapAccum initialStdGen $ roll <$> edoesplay

    let
        -- roll the reels
        roll :: () -> StdGen -> (Reels, StdGen)
        roll () gen0 = ((z1,z2,z3),gen3)
            where
            random    = randomR(1,4)
            (z1,gen1) = random gen0
            (z2,gen2) = random gen1
            (z3,gen3) = random gen2
        
        -- Event: it's a win!
        ewin :: Event Win
        ewin = fmap fromJust $ filterE isJust $ fmap checkWin eroll
        checkWin (z1,z2,z3)
            | length (nub [z1,z2,z3]) == 1 = Just Triple
            | length (nub [z1,z2,z3]) == 2 = Just Double
            | otherwise                    = Nothing


    -- ecredits <- changes bcredits
    reactimate $ putStrLn . showCredit <$> ecredits
    reactimate $ putStrLn . showRoll   <$> eroll
    reactimate $ putStrLn . showWin    <$> ewin
    reactimate $ putStrLn "Not enough credits!" <$ edenied


showCredit money    = "Credits: " ++ show money
showRoll (z1,z2,z3) = "You rolled  " ++ show z1 ++ show z2 ++ show z3
showWin Double = "Wow, a double!"
showWin Triple = "Wowwowow! A triple! So awesome!"
