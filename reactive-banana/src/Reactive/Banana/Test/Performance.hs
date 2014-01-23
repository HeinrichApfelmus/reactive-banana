{-----------------------------------------------------------------------------
    reactive-banana

    Performance tests
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-}
module Reactive.Banana.Test.Performance where

import           Control.DeepSeq
import           Control.Exception                  (evaluate)
import           Control.Monad
import qualified Data.IntMap                as IM
import           Data.Time
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           System.IO
import           System.Mem
import           System.Random.MWC          as Rand

main :: IO ()
main = void $ benchmark 100 3000

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
doSomething :: String -> IO ()
doSomething = hPutStr stderr

measureTwo :: IO a -> (a -> IO b) -> IO (NominalDiffTime, NominalDiffTime)
measureTwo ma mb = do
    time0 <- getCurrentTime
    a <- ma
    time1 <- getCurrentTime
    _ <- mb a
    time2 <- getCurrentTime
    return (time1 `diffUTCTime` time0, time2 `diffUTCTime` time1)

{-----------------------------------------------------------------------------
    Benchmark courtesy of John Lato
------------------------------------------------------------------------------}
benchmark :: Int -> Int -> IO (NominalDiffTime, NominalDiffTime)
benchmark netsize duration = measureTwo phase1 phase2
    where
    phase1 = do
        (handlers, triggers) <- unzip <$> replicateM netsize newAddHandler
        (clock   , trigger ) <- newAddHandler
            
        let networkD :: forall t. Frameworks t => Moment t ()
            networkD = do
                es <- mapM fromAddHandler handlers
                e  <- fromAddHandler clock
                let countBs = map count es
                trimmedBs <- mapM trimB countBs
                let outputE = doSomething "x" <$ e
                reactimate outputE

            count :: Event t () -> Behavior t Int
            count e = accumB 0 $ (+1) <$ e
        
        network <- compile networkD
        -- force network to get accurate timing
        evaluate . rnf =<< showNetwork network
        actuate network
        
        return (triggers, trigger)
    
    phase2 (triggers, clock) = do
        let trigMap = IM.fromList $ zip [0..netsize-1] triggers
        randGen <- Rand.create

        forM_ [1..duration] $ \step -> do
            randomRs <- replicateM 10 $ Rand.uniformR (0,netsize-1) randGen
            clock step
            forM_ randomRs $ \ev ->
                maybe (error "benchmark: trigger not found") ($ ()) $
                    IM.lookup ev trigMap

