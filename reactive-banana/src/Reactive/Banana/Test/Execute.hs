{-# LANGUAGE RecursiveDo #-}

{- ---------------------------------------------------------------------------
Reactive Banana

Testing "execute", "reactimate" and its relatives.
-}


module Reactive.Banana.Test.Execute where

import Data.IORef
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Test.HUnit (assert, Assertion, (@?=))


-- Storage location for assembling the results.
type TestResult = IORef [String]

-- | Add a
addResult :: String -> TestResult -> Int -> IO ()
addResult prefix ref x = modifyIORef ref (str :)
   where
      str = prefix <> "-" <> show x


-- | Returns "Nothing" if the results are as expected. Otherwise returns the actual results.
checkResult :: [String] -> TestResult -> IO ()
checkResult expected ref = do
   r <- reverse <$> readIORef ref
   r @?= expected


type ReactTest = TestResult -> String -> Event Int -> MomentIO ()

-- Add the events to a "TestResult".
reactimateTest :: ReactTest
reactimateTest ref prefix xs = reactimate $ addResult prefix ref <$> xs


-- Add the events to a "TestResult" until the first zero is encountered, then stop.
reactimateTest1 :: ReactTest
reactimateTest1 ref prefix xs = mdo
   stop <- reactimate1 $ (\x -> if x /= 0 then addResult prefix ref x else stop) <$> xs
   return ()

-- Add the value to a "TestResult" every time a Behavior changes.
reactimate'Test :: ReactTest
reactimate'Test ref prefix xs = do
   xB <- stepper 1 xs
   xs1 <- changes xB
   reactimate' $ fmap (addResult prefix ref) <$> xs1

-- Add the value to a "TestResult" every time a Behavior changes until it equals zero, then stop.
reactimate'Test1 :: ReactTest
reactimate'Test1 ref prefix xs = mdo
      xB <- stepper 1 xs
      xs1 <- changes xB
      stop <- reactimate1' $ fmap (addResult1 stop) <$> xs1
      return ()
   where
      addResult1 stop x = if x /= 0 then addResult prefix ref x else stop

-- | Sequence of integers with a zero in the middle.
testSequence :: [Int]
testSequence = [1, 2, 3, 0, 4, 5]

-- | Expected results for reactimate and reactimate'
expectedAll :: String -> [String]
expectedAll prefix = map (\n -> prefix <> "-" <> show n) testSequence

-- | Expected results for reactimate1 and reactimate1'
expectedSome :: String -> [String]
expectedSome prefix = map (\n -> prefix <> "-" <> show n) $ takeWhile (>0) testSequence


runSimpleTest :: String -> (String -> [String]) -> ReactTest -> IO ()
runSimpleTest prefix expectF script = do
      result <- newIORef []
      handleRef <- newIORef $ error "runSimpleTest handleRef not set"
      net <- compile $ circuit result handleRef
      actuate net
      h <- readIORef handleRef
      mapM_ h testSequence
      checkResult (expectF prefix) result
   where
      circuit :: TestResult -> IORef (Handler Int) -> MomentIO ()
      circuit result handleRef = do
         (ev, h) <- newEvent
         liftIO $ writeIORef handleRef h
         script result prefix ev


reactimateCase :: Test
reactimateCase = testCase "reactimate" $ runSimpleTest "Foo" expectedAll reactimateTest

reactimateCase1 :: Test
reactimateCase1 = testCase "reactimate1" $ runSimpleTest "Foo" expectedSome reactimateTest1

reactimate'Case :: Test
reactimate'Case = testCase "reactimate'" $ runSimpleTest "Foo" expectedAll reactimate'Test

reactimate'Case1 :: Test
reactimate'Case1 = testCase "reactimate1'" $ runSimpleTest "Foo" expectedSome reactimate'Test1
