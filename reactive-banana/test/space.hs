{-# LANGUAGE BangPatterns #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Main where

import Control.Monad
  ( foldM, void )

import qualified Reactive.Banana.Test.Mid.Space as Mid
import qualified Reactive.Banana.Test.High.Space as High

main :: IO ()
main = do
    say "Running..."
    -- void $ High.runNetworkSizes High.executeAccumE1 [1..20000]
    -- void $ High.runNetworkSizes High.switchAccumE1 [1..10000]
    -- void $ High.runNetworkSizes High.observeAccumE1 [1..10000]
    -- void $ runMidNetwork Mid.executeAccum1 [1..50000]
    void $ runMidNetwork Mid.switchAccum1 [1..20000]
    say "Done"

say :: String -> IO ()
say = putStrLn

{-----------------------------------------------------------------------------
    Test harness
------------------------------------------------------------------------------}
runMidNetwork f xs = do
    (network0, step) <- Mid.compileToStateMachine f
    void $ runStrict step xs network0

runStrict :: Monad m => (a -> s -> m s) -> [a] -> s -> m s
runStrict f [] !s = pure s
runStrict f (x:xs) !s = runStrict f xs =<< f x s
