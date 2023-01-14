{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
-- | Exemplar tests for space usage and garbage collection.
module Reactive.Banana.Test.Mid.Space where

import Control.Monad.IO.Class
    ( liftIO )
import Test.Tasty
    ( testGroup, TestTree )
import Test.Tasty.QuickCheck
    ( testProperty )

import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Monadic as Q

import qualified Control.Exception as Memory
import qualified Control.Concurrent as System
import qualified System.Mem as System

import Reactive.Banana.Prim.Mid
    ( Build, BuildIO, Network, Pulse, Latch )
import qualified Reactive.Banana.Prim.Mid as Prim

tests :: TestTree
tests = testGroup "Space usage, mid level"
    [ testGroup "Network size stays bounded"
        [ testBoundedNetworkSize "executeP accumL, issue #261" executeAccum1
        ]
    ]

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
executeAccum1 :: Pulse Int -> Build (Pulse (Pulse Int))
executeAccum1 p1 = do
    p2 <- Prim.mapP mkP p1
    Prim.executeP p2 ()
  where
    mkP :: Int -> () -> Build (Pulse Int)
    mkP i () = do
        piId <- Prim.mapP (const id) p1
        (_, pi) <- Prim.accumL i piId
        pure pi

{-----------------------------------------------------------------------------
    Test harness
------------------------------------------------------------------------------}
-- | Execute an FRP network with a sequence of inputs
-- with intermittend of garbage collection and record network sizes.
runNetworkSizes
    :: (Pulse a -> BuildIO (Pulse ignore))
    -> [a] -> IO ([Int], Network)
runNetworkSizes f xs = do
    (step,network) <- Prim.compile build =<< Prim.emptyNetwork

    let fire x network1 = do
            (outputs, network2) <- step x network1
            outputs -- don't forget to execute outputs
            performSufficientGC
            System.yield
            size <- Prim.getSize network2
            pure (size, network2)

    Prim.mapAccumM fire network xs
  where
    build = do
        (p1, step) <- Prim.newInput
        p2 <- f p1
        p3 <- Prim.mapP pure p2 -- wrap into Future
        Prim.addHandler p3 (\_ -> pure ())
        pure step

-- | Test whether the network size stays bounded.
testBoundedNetworkSize
    :: String
    -> (Pulse Int -> Build (Pulse ignore))
    -> TestTree
testBoundedNetworkSize name f = testProperty name $
    Q.once $ Q.monadicIO $ do
        (sizes,_) <- liftIO $ runNetworkSizes f [1..n]
        Q.monitor
            $ Q.counterexample "network size grows"
            . Q.counterexample ("network sizes: " <> show sizes)
        Q.assert $ isBounded sizes
  where
    n = 20 :: Int
    isBounded sizes = sizes !! 3 >= sizes !! (n-1)

performSufficientGC :: IO ()
performSufficientGC = System.performMinorGC

{-----------------------------------------------------------------------------
    Debugging
------------------------------------------------------------------------------}
-- | Print network after a given sequence of inputs
printNetwork
    :: (Pulse Int -> BuildIO (Pulse ignore))
    -> [Int] -> IO String
printNetwork f xs = do
    (_, network) <- runNetworkSizes executeAccum1 xs
    Prim.printDot network
