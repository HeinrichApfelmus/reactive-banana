{-# LANGUAGE RecursiveDo #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
-- | Exemplar tests for space usage and garbage collection.
module Reactive.Banana.Test.High.Space
    ( tests
    ) where

import Control.Monad
    ( forM )
import Test.Tasty
    ( testGroup, TestTree )
import Test.Tasty.QuickCheck
    ( testProperty )

import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Monadic as Q

import qualified Control.Exception as Memory
import qualified Control.Concurrent as System
import qualified System.Mem as System

import Reactive.Banana
import Reactive.Banana.Frameworks

tests :: TestTree
tests = testGroup "Space usage, high level"
    [ testGroup "Network size stays bounded"
        [ testBoundedNetworkSize "execute" execute1
        , testBoundedNetworkSize "execute accum, issue #261" executeAccumE1
        ]
    ]

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
execute1 :: Event Int -> MomentIO (Event (Event Int))
execute1 e = execute $ (\i -> liftIO $ Memory.evaluate (i <$ e)) <$> e

executeAccumE1 :: Event Int -> MomentIO (Event (Event ()))
executeAccumE1 e = execute (accumE () never <$ e)

{-----------------------------------------------------------------------------
    Test harness
------------------------------------------------------------------------------}
-- | Execute an FRP network with a sequence of inputs
-- with intermittend of garbage collection and record network sizes.
runNetworkSizes
    :: (Event Int -> MomentIO (Event ignore))
    -> Int -> IO [Int]
runNetworkSizes f n = do
    (network, fire) <- setup
    run network fire
  where
    setup = do
        (ah, fire) <- newAddHandler
        network <- compile $ do
            ein  <- fromAddHandler ah
            eout <- f ein
            reactimate $ pure () <$ eout
        performSufficientGC
        actuate network
        pure (network, fire)

    run network fire = forM [1..n] $ \i -> do
        fire i
        performSufficientGC
        System.yield
        getSize network

-- | Test whether the network size stays bounded.
testBoundedNetworkSize
    :: String
    -> (Event Int -> MomentIO (Event ignore))
    -> TestTree
testBoundedNetworkSize name f = testProperty name $
    Q.once $ Q.monadicIO $ do
        sizes <- liftIO $ runNetworkSizes f n
        Q.monitor
            $ Q.counterexample "network size grows"
            . Q.counterexample ("network sizes: " <> show sizes)
        Q.assert $ isBounded sizes
  where
    n = 20 :: Int
    isBounded sizes = sizes !! 3 >= sizes !! (n-1)

performSufficientGC :: IO ()
performSufficientGC = System.performMinorGC