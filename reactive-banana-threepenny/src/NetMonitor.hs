{-----------------------------------------------------------------------------
    reactive-banana-threepenny
    
    Example: Minuscule network monitor
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Data.Char
import Data.List
import Data.Maybe

import System.Process

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (Event)

import Reactive.Banana
import Reactive.Banana.Threepenny

import Timer

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = do
    startGUI Config
        { tpPort       = 10000
        , tpCustomHTML = Nothing
        , tpStatic     = ""
        } setup

setup :: Window -> IO ()
setup window = do
    return window # set title "Network Monitor"

    out1 <- UI.span
    out2 <- UI.span
    
    getBody window #+
        [column [string "TCP network statistics",
                   grid [[string "Packets sent: ", element out1]
                        ,[string "Packets received: ", element out2]]
                  ]]
    
    timer <- newTimer # set interval 500  -- timer every 500 ms

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            -- The network statistics are polled when and only when
            -- the event network handles an event.
            bnetwork <- fromPoll getNetworkStatistics
            -- That's why we need a timer that generates regular events to handle.
            etick    <- event tick timer
        
            let showSent     = maybe "parse error" show . fst
                showReceived = maybe "parse error" show . snd
        
            return out1 # sink text (showSent     <$> bnetwork)
            return out2 # sink text (showReceived <$> bnetwork)
    
    network <- compile networkDescription
    actuate network
    start timer

-- Obtain network statistics from the  netstat  utility
type NetworkStatistics = (Maybe Int, Maybe Int)

getNetworkStatistics :: IO NetworkStatistics
getNetworkStatistics = do
    s <- readProcess "netstat" ["-s", "-p","tcp"] ""
    return (readField "packets sent" s
           ,readField "packets received" s)

readField :: String -> String -> Maybe Int
readField fieldname = id
    . fmap (read . filter isDigit) . listToMaybe
    . filter (fieldname `isSuffixOf`) . lines
