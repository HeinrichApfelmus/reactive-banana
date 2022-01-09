{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
module Main ( main ) where

import Control.Monad (replicateM, replicateM_, forM_)
import qualified Data.IntMap.Strict as IM
import Reactive.Banana.Combinators ( Event, Behavior, MonadMoment, filterE, accumE, switchB, accumB )
import Reactive.Banana.Frameworks (MomentIO, newAddHandler, fromAddHandler, compile, actuate, Handler, reactimate)
import Reactive.Banana ( Event, Behavior, MonadMoment )
import System.Random (randomRIO)
import Test.Tasty (withResource)
import Test.Tasty.Bench (env, defaultMain, bgroup, bench, whnfIO)

main :: IO ()
main = defaultMain $ [ mkBenchmarkGroup netsize | netsize <- [ 1, 2, 4, 8, 16, 32, 64, 128 ] ] ++
                     [ boringBenchmark ]
  where
    mkBenchmarkGroup netsize =
      withResource (setupBenchmark netsize) mempty $ \getEnv ->
        bgroup ("netsize = " <> show netsize)
          [ mkBenchmark getEnv steps | steps <- [ 1, 2, 4, 8, 16, 32, 64, 128] ]
      where
        mkBenchmark getEnv duration = bench ("duration = " <> show duration) $ whnfIO $ do
          (triggers, clock) <- getEnv
          let trigMap = IM.fromList $ zip [0..netsize-1] triggers
          forM_ [1..duration] $ \step -> do
            randomRs <- replicateM 10 $ randomRIO (0,netsize-1)
            clock step
            forM_ randomRs $ \ev ->
                maybe (error "benchmark: trigger not found") ($ ()) $
                    IM.lookup ev trigMap

    boringBenchmark = withResource setup mempty $ \getEnv ->
      bench "Boring" $ whnfIO $ do
        tick <- getEnv
        {-# SCC ticks #-} replicateM_ 10_000_000 $ {-# SCC tick #-} tick ()
      where
        setup = do
          (tick, onTick) <- newAddHandler
          compile $ do
            e <- fromAddHandler tick
            reactimate $ return <$> e
          return onTick

setupBenchmark :: Int -> IO ([Handler ()], Handler Int)
setupBenchmark netsize = do
  (handlers, triggers) <- unzip <$> replicateM netsize newAddHandler
  (clock   , trigger ) <- newAddHandler

  let networkD :: MomentIO ()
      networkD = do
          es :: [Event ()] <-
            mapM fromAddHandler handlers

          e :: Event Int <-
            fromAddHandler clock

          countBs :: [Behavior Int] <-
            traverse count es

          let
            step10E :: Event Int
            step10E = filterE (\cnt -> cnt `rem` 10 == 0) e

          selectedB_E :: Event (Behavior Int) <- do
            fmap head <$> accumE countBs (keepTail <$ step10E)

          selectedB :: Behavior Int <-
            switchB (head countBs) selectedB_E

          return ()

      count :: MonadMoment m => Event () -> m (Behavior Int)
      count e = accumB 0 ((+1) <$ e)

  actuate =<< compile networkD
  return (triggers, trigger)
  where
    keepTail :: [a] -> [a]
    keepTail (_:y:zs) = y:zs
    keepTail [x]      = [x]
    keepTail []       = []
