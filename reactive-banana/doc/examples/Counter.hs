{-----------------------------------------------------------------------------
    reactive-banana
    
    Example: Activate and pause an event network acting as a counter
------------------------------------------------------------------------------}
import Control.Monad (when)
import System.IO

import Reactive.Banana
import Reactive.Banana.Frameworks


main :: IO ()
main = do
    displayHelpMessage
    sources <- (,,) <$> newAddHandler <*> newAddHandler <*> newAddHandler
    network <- setupNetwork sources
    activate network
    eventLoop sources network

displayHelpMessage :: IO ()
displayHelpMessage = mapM_ putStrLn $
    "Commands are:":
    "   +   - increase counterUp event":
    "   -   - decrease counterUp event":
    "   p   - pause event network":
    "   a   - activate event network":
    "   q   - quit the program":
    "":
    []

-- Read commands and fire corresponding events 
eventLoop :: (EventSource (), EventSource (),EventSource EventNetwork) -> EventNetwork -> IO ()
eventLoop (eplus, eminus, espause) network = loop
    where
    loop = do
        putStr "> "
        hFlush stdout
        hSetBuffering stdin NoBuffering
        s <- getChar
        case s of
            '+'   -> fire eplus ()
            '-'   -> fire eminus ()
            'p'   -> fire espause network
            'a'   -> activate network
            'q'   -> return ()
            _     -> putStrLn $ [s] ++ " - unknown command"
        when (s /= 'q') loop

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
-- Set up the program logic in terms of events and behaviors.
setupNetwork :: (EventSource (), EventSource (), EventSource EventNetwork) -> IO EventNetwork
setupNetwork (eplus, eminus, espause) = compile $ do
    counterUp   <- fromAddHandler (addHandler eplus)
    counterDown <- fromAddHandler (addHandler eminus)
    epause      <- fromAddHandler (addHandler espause)

    ecount <- accumE 0 $ unions
        [ (+1)       <$ counterUp
        , subtract 1 <$ counterDown
        ]

    reactimate $ fmap print ecount
    reactimate $ fmap pause epause

