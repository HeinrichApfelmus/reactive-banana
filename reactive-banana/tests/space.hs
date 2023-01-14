module Main where

import Control.Monad
  ( void )

import qualified Reactive.Banana.Test.Mid.Space as Mid
import qualified Reactive.Banana.Test.High.Space as High

main :: IO ()
main = do
    say "Running..."
    -- void $ High.runNetworkSizes High.executeAccumE1 [1..30000]
    -- void $ High.runNetworkSizes High.observeAccumE1 [1..10000]
    void $ Mid.runNetworkSizes Mid.executeAccum1 [1..50000]
    say "Done"

say :: String -> IO ()
say = putStrLn
