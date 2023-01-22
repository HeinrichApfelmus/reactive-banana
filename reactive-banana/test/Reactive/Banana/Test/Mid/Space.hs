{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
-- | Exemplar tests for space usage and garbage collection.
module Reactive.Banana.Test.Mid.Space where

import Control.Monad
    ( foldM )
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
        [ testBoundedNetworkSize "executeP accumL" executeAccum1
        , testBoundedNetworkSize "switchP executeP accumL" switchAccum1
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

switchAccum1 :: Pulse Int -> Build (Pulse Int)
switchAccum1 p1 = do
    p2 <- executeAccum1 p1
    Prim.switchP p1 p2

{-----------------------------------------------------------------------------
    Test harness
------------------------------------------------------------------------------}
-- | Compile an FRP network description into a state machine,
-- which also performs garbage collection after every step.
compileToStateMachine
    :: (Pulse a -> BuildIO (Pulse ignore))
    -> IO (Network, a -> Network -> IO Network)
compileToStateMachine f = do
    (step,network0) <- Prim.compile build =<< Prim.emptyNetwork
    pure (network0, doStep step)
  where
    build = do
        (p1, step) <- Prim.newInput
        p2 <- f p1
        p3 <- Prim.mapP pure p2 -- wrap into Future
        Prim.addHandler p3 (\_ -> pure ())
        pure step

    doStep step x network1 = do
        (outputs, network2) <- step x network1
        outputs         -- don't forget to execute outputs
        performSufficientGC
        System.yield    -- wait for finalizers to run
        pure network2

-- | Execute an FRP network with a sequence of inputs
-- with intermittend of garbage collection and record network sizes.
runNetworkSizes
    :: (Pulse a -> BuildIO (Pulse ignore))
    -> [a] -> IO [Int]
runNetworkSizes f xs = do
    (network0, step0) <- compileToStateMachine f
    let step1 x network1 = do
            network2 <- step0 x network1
            size <- Memory.evaluate =<< Prim.getSize network2
            pure (size, network2)
    fst <$> Prim.mapAccumM step1 network0 xs

-- | Test whether the network size stays bounded.
testBoundedNetworkSize
    :: String
    -> (Pulse Int -> Build (Pulse ignore))
    -> TestTree
testBoundedNetworkSize name f = testProperty name $
    Q.once $ Q.monadicIO $ do
        sizes <- liftIO $ runNetworkSizes f [1..n]
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
    :: (Pulse a -> BuildIO (Pulse ignore))
    -> [a] -> IO String
printNetwork f xs = do
    (network0, step) <- compileToStateMachine f
    network1 <- foldM (flip step) network0 xs
    Prim.printDot network1
