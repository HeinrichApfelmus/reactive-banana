{-# language BlockArguments #-}
module Main where

import GHC.Debug.Stub
import Control.Monad
import Reactive.Banana.Frameworks
import Reactive.Banana
import Control.Concurrent

main :: IO ()
main = withGhcDebug do
  (addHandler, fire) <- newAddHandler
  let nw = do
        e1 <- fromAddHandler addHandler
        e2 <- switchE never (never <$ e1)
        reactimate $ putStrLn "e2" <$ e2
  compile nw >>= actuate
  replicateM_ 10000 $ do
    putStrLn "Tick"
    fire ()
  threadDelay maxBound
  replicateM_ 10000 $ do
    putStrLn "Tick"
    fire ()
