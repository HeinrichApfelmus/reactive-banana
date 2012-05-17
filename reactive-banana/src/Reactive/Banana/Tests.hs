{-----------------------------------------------------------------------------
    Reactive Banana
    
    Test cases and examples
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types, NoMonomorphismRestriction #-}

import Control.Monad (when)

import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks (interpretFrameworks)

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Test.HUnit (assert, Assertion)

-- import Test.QuickCheck
-- import Test.QuickCheck.Property

main = defaultMain
    [ testGroup "Single Combinators"
        [ testModelMatch "id"      id
        , testModelMatch "fmap1"   fmap1
        , testModelMatch "filter1" filter1
        , testModelMatch "filter2" filter2
        ]
    , testGroup "Small combinations"
        [ testModelMatch "counter"   counter
        , testModelMatch "double"    double
        , testModelMatch "sharing"   sharing
        , testModelMatch "decrease"  decrease
        , testModelMatch "accumBvsE" accumBvsE
        ]
    -- TODO:
    --  * algebraic laws
    --  * larger examples
    --  * quickcheck
    ]

{-----------------------------------------------------------------------------
    Testing
------------------------------------------------------------------------------}
matchesModel :: (Show b, Eq b)
             => (forall t. Event t a -> Event t b) -> [a] -> IO Bool
matchesModel f xs = do
    bs1 <- interpretModel      f (singletons xs)
    bs2 <- interpretPushGraph  f (singletons xs)
    bs3 <- interpretFrameworks f xs
    let bs = [bs1,bs2,bs3]
    let b = all (==bs1) bs
    when (not b) $ mapM_ print bs
    return b

singletons = map (\x -> [x])

testModelMatch
    :: (Show b, Eq b)
    => String -> (forall t. Event t Int -> Event t b) -> Test
testModelMatch name f = testCase name $ assert $ matchesModel f [1..8::Int]

{-----------------------------------------------------------------------------
    Examples
------------------------------------------------------------------------------}
testModel, testPush :: (forall t. Event t Int -> Event t b) -> IO [[b]]
testModel f = interpretModel     f $ singletons [1..8::Int]
testPush  f = interpretPushGraph f $ singletons [1..8::Int]

never1 :: Event t Int -> Event t Int
never1    = const never
fmap1     = fmap (+1)
filter1   = filterE (>= 3)
filter2   = filterE (>= 3) . fmap (subtract 1)
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
    
