{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: Minuscule network monitor
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. NetworkDescription t"

import Data.Char
import Data.List
import Data.Maybe

import System.Process

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main = start $ do
    f        <- frame    [text := "Network Monitor"]
    out1     <- staticText f []
    out2     <- staticText f []
    
    set f [layout := margin 10 $
            column 10 [label "TCP network statistics",
                       grid 5 5 [[label "Packets sent: ", widget out1]
                                ,[label "Packets received: ", widget out2]]
                      ]
          , size := sz 250 70]
    
    t <- timer f [ interval := 500 ] -- timer every 500 ms

    let networkDescription :: forall t. NetworkDescription t ()
        networkDescription = do
            -- The network statistics are polled when and only when
            -- the event network handles an event.
            bnetwork <- fromPoll getNetworkStatistics
            -- That's why we need a timer that generates regular events to handle.
            etick    <- event0 t command
        
            let showSent     = maybe "parse error" show . fst
                showReceived = maybe "parse error" show . snd
        
            sink out1 [ text :== showSent     <$> bnetwork ]
            sink out2 [ text :== showReceived <$> bnetwork ]
    
    network <- compile networkDescription
    actuate network

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
