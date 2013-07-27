{-----------------------------------------------------------------------------
    reactive-banana-threepenny
    
    Example: Emit a wave of light.
        Demonstrates that reactive-banana is capable of emitting timed events,
        even though it has no built-in notion of time.
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Control.Monad
import qualified Data.List as List
import Data.Maybe
import Data.Ord


import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (Event)

import Reactive.Banana
import Reactive.Banana.Threepenny

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
lightCount = 15  -- number of lights that comprise the wave
waveLength = 4   -- number of lights that are lit at once
dt         = 70  -- half the cycle duration

main :: IO ()
main = do
    startGUI Config
        { tpPort       = 10000
        , tpCustomHTML = Nothing
        , tpStatic     = ""
        } setup

setup :: Window -> IO ()
setup window = do
    return window # set title "Waves of Light"
    
    left   <- UI.button #+ [string "Left" ]
    right  <- UI.button #+ [string "Right"]
    lights <- sequence $ replicate lightCount $ UI.span # set text "•"
    
    getBody window #+ [column
        [row [element left, element right]
        ,row $ map element lights
        ]]
    
    timer  <- UI.timer
    
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do

            eLeft  <- event UI.click left
            eRight <- event UI.click right
        
            -- event describing all the lights
            eWave  <- scheduleQueue timer $ (waveLeft  <$ eLeft ) `union` 
                                            (waveRight <$ eRight)
        
            -- animate the lights
            forM_ [1 .. lightCount] $ \k -> do
                let
                    bulb  = lights !! (k-1)
                    bBulb = stepper False $ snd <$> filterE ((== k) . fst) eWave
                
                    colorize True  = "red"
                    colorize False = "black"
                
                return bulb # sink color (colorize <$> bBulb)

    network <- compile networkDescription    
    actuate network

color = mkWriteAttr $ \i -> set' style [("color",i)]

type Index  = Int
type Action = (Index, Bool)

-- describe wave pattern as a list
wave :: (Index -> Index) -> [(Duration, Action)]
wave f = deltas $ merge ons offs
    where
    merge xs ys = List.sortBy (comparing fst) $ xs ++ ys
    deltas xs = zipWith relativize (0 : map fst xs) xs
        where relativize dt1 (dt2,x) = (dt2-dt1, x)
    
    ons  = [(k*2*dt, (f k, True)) | k <- [1..lightCount]]
    offs = [(dt+(waveLength+k)*2*dt, (f k, False)) | k <- [1..lightCount]]

waveLeft  = wave id
waveRight = wave (\k -> lightCount - k + 1)


{-----------------------------------------------------------------------------
    Timer magic
------------------------------------------------------------------------------}
type Duration  = Int -- in milliseconds
type Queue a   = [(Duration, a)] -- [(time to wait, occurrence to happen)]
type Enqueue a = Queue a

-- Schedule events to happen after a given duration from their occurrence
-- However, new events will *not* be scheduled before the old ones have finished.
scheduleQueue :: Frameworks t =>
    UI.Timer -> Event t (Enqueue a) -> Moment t (Event t a)
scheduleQueue t e = do
    liftIO $ UI.stop t
    eAlarm <- event UI.tick t
    let
        -- (Queue that keeps track of events to schedule
        -- , duration of the new alarm if applicable) 
        (eSetNewAlarmDuration, bQueue) =
            mapAccum [] $ (remove <$ eAlarm) `union` (add <$> e)
        
        -- change queue and change timer
        remove (_:[]) = (stop, [])
        remove (_:xs) = (wait (fst $ head xs), xs)
        add    ys []  = (wait (fst $ head ys), ys)
        add    ys xs  = (idle, xs ++ ys)
        
        wait dt = do { return t # set UI.interval dt; UI.start t }
        stop    = UI.stop t
        idle    = return ()
        
        -- Return topmost value from the queue whenever the alarm rings.
        -- The queue is never empty when the alarm rings.
        eout = fmap (snd . head) $ bQueue <@ eAlarm
    
    reactimate $ eSetNewAlarmDuration
    return eout

