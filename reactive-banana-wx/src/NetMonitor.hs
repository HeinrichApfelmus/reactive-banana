{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: Minuscule network monitor
------------------------------------------------------------------------------}
import Data.Char
import Data.List
import Data.Maybe
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX
import System.Process

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

    network <- compile $ do
        etick    <- event0 t command
        bnetwork <- fromPoll $ getNetworkStatistics
        
        let showStat f = stepperD "" $ f <$> (bnetwork <@ etick)
            sent     = maybe "parse error" show . fst
            received = maybe "parse error" show . snd
        
        sink out1 [ text :== showStat sent ]
        sink out2 [ text :== showStat received ]
    
    actuate network

-- obtain network statistics
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
