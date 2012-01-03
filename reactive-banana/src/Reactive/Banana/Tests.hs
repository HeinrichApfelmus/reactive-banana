{-----------------------------------------------------------------------------
    Reactive Banana
    
    Test cases and examples
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types, NoMonomorphismRestriction #-}

module Reactive.Banana.Tests where

import Control.Monad (when)

import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

import Test.QuickCheck

{-----------------------------------------------------------------------------
    Testing
------------------------------------------------------------------------------}
matchesModel :: (Show b, Eq b)
             => (forall t. Event t a -> Event t b) -> [a] -> IO Bool
matchesModel f = \xs -> do
        bs1 <- interpretModel f (map singleton xs)
        bs2 <- interpret f xs
        when (bs1 /= bs2) $ print bs1 >> print bs2
        return $ bs1 == bs2

singleton x = [x]

testSuite = do
        -- trivial unit tests
        test add1
        test filtering
        test counter
        test double
        test sharing
        test decrease
        test accumBvsE
        -- TODO:
        --  * algebraic laws
        --  * larger examples
        --  * quickcheck
    where
    test :: (Show b, Eq b) => (forall t. Event t Int -> Event t b) -> IO ()
    test f = print =<< matchesModel f [1..8::Int]

{-----------------------------------------------------------------------------
    Examples
------------------------------------------------------------------------------}
testModel, testImpl :: (forall t. Event t Int -> Event t b) -> IO [[b]]
testModel f = interpretModel f $ map singleton [1..8::Int]
testImpl  f = interpret      f $ [1..8::Int]

add1      = fmap (+1)
filtering = filterE (>= 3) . fmap (subtract 1)
counter e = apply (pure const <*> bcounter) e
    where bcounter = accumB 0 $ fmap (\_ -> (+1)) e
double e  = union e e
sharing e = union e1 e1
    where e1 = filterE (< 3) e

type Dummy = Int

-- counter that can be decreased as long as it's >= 0
decrease :: Event t Dummy -> Event t Int
decrease edec = apply (const <$> bcounter) ecandecrease
    where
    bcounter     = accumB 4 $ (subtract 1) <$ ecandecrease
    ecandecrease = whenE ((>0) <$> bcounter) edec

-- test accumE vs accumB
accumBvsE :: Event t Dummy -> Event t Int
accumBvsE input = e1 `union` e2
    where
    e  = input `union` input
    e1 = accumE 0 ((+1) <$ e)
    e2 = let b = accumB 0 ((+1) <$ e) in apply (const <$> b) e
    
