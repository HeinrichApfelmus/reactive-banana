{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: Emit a wave of lights.
        Demonstrates that reactive-banana is capable of emitting timed events,
        even though it has no built-in notion of time.
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Control.Monad
import qualified Data.List as List
import Data.Ord

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
lightCount :: Int
lightCount = 15  -- number of lights that comprise the wave

waveLength :: Int
waveLength = 4   -- number of lights that are lit at once

dt :: Int
dt         = 70  -- half the cycle duration

main :: IO ()
main = start $ do
    -- create window and widgets
    f        <- frame    [text := "Waves of Light"]
    left     <- button f [text := "Left"]
    right    <- button f [text := "Right"]
    lights   <- sequence $ replicate lightCount $ staticText f [text := "•"]
    
    set f [layout := margin 10 $
            column 10 [row 5 [widget left, widget right],
                       row 5 $ map widget lights]
          ]
    
    -- we're going to need a timer
    t  <- timer f []
    
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do

            eLeft  <- event0 left command
            eRight <- event0 right command
        
            -- event describing all the lights
            eWave  <- scheduleQueue t $ (waveLeft  <$ eLeft ) `union` 
                                        (waveRight <$ eRight)
        
            -- animate the lights
            forM_ [1 .. lightCount] $ \k -> do
                let
                    bulb  = lights !! (k-1)
                    bBulb = stepper False $ snd <$> filterE ((== k) . fst) eWave
                
                    colorize True  = red
                    colorize False = black
                
                sink bulb [ color :== colorize <$> bBulb ]        

    network <- compile networkDescription    
    actuate network


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

waveLeft :: [(Duration, Action)]
waveLeft  = wave id

waveRight :: [(Duration, Action)]
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
    Timer -> Event t (Enqueue a) -> Moment t (Event t a)
scheduleQueue t e = do
    liftIO $ set t [ enabled := False ]
    eAlarm <- event0 t command
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
        
        wait dt = set t [ enabled := True, interval := dt ]
        stop    = set t [ enabled := False ]
        idle    = return ()
        
        -- Return topmost value from the queue whenever the alarm rings.
        -- The queue is never empty when the alarm rings.
        eout = fmap (snd . head) $ bQueue <@ eAlarm
    
    reactimate eSetNewAlarmDuration
    return eout

    
    

