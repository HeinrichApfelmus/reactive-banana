{-----------------------------------------------------------------------------
    reactive-banana

    Test cases and examples
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types, NoMonomorphismRestriction #-}

import Control.Monad (when)

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Test.HUnit (assert, Assertion)

-- import Test.QuickCheck
-- import Test.QuickCheck.Property

import Control.Applicative
import Reactive.Banana.Test.Plumbing


main = defaultMain
    [ testGroup "Simple"
        [ testModelMatch "id"      id
        -- , testModelMatch "never1"  never1
        , testModelMatch "fmap1"   fmap1
        , testModelMatch "filter1" filter1
        , testModelMatch "filter2" filter2
        ]
    , testGroup "Complex"
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
matchesModel
    :: (Show b, Eq b)
    => (Event a -> Event b) -> [a] -> IO Bool
matchesModel f xs = do
    let bs1 = interpretModel (return . f) (singletons xs)
    let bs2 = bs1
    -- bs2 <- interpretPushGraph  f (singletons xs)
    -- bs3 <- interpretFrameworks f xs
    let bs = [bs1,bs2]
    let b = all (==bs1) bs
    when (not b) $ mapM_ print bs
    return b

singletons = map Just

testModelMatch
    :: (Show b, Eq b)
    => String -> (Event Int -> Event b) -> Test
testModelMatch name f = testCase name $ assert $ matchesModel f [1..8::Int]

{-----------------------------------------------------------------------------
    Examples
------------------------------------------------------------------------------}
testModel :: (Event Int -> Event b) -> [Maybe b]
testModel f = interpretModel (return . f) $ singletons [1..8::Int]
-- testPush  f = interpretPushGraph f $ singletons [1..8::Int]

never1 :: Event Int -> Event Int
never1    = const never
fmap1     = fmap (+1)

filterE p = filterJust . fmap (\e -> if p e then Just e else Nothing)
filter1   = filterE (>= 3)
filter2   = filterE (>= 3) . fmap (subtract 1)

counter e = applyE (pure const <*> bcounter) e
    where bcounter = accumB 0 $ fmap (\_ -> (+1)) e

merge e1 e2 = unionWith (++) (list e1) (list e2)
    where list = fmap (:[])
    
double e  = merge e e
sharing e = merge e1 e1
    where e1 = filterE (< 3) e

type Dummy = Int

-- counter that can be decreased as long as it's >= 0
decrease :: Event Dummy -> Event Int
decrease edec = applyE (const <$> bcounter) ecandecrease
    where
    bcounter     = accumB 4 $ (subtract 1) <$ ecandecrease
    ecandecrease = whenE ((>0) <$> bcounter) edec


-- test accumE vs accumB
accumBvsE :: Event Dummy -> Event [Int]
accumBvsE e = merge e1 e2
    where
    e1 = accumE 0 ((+1) <$ e)
    e2 = let b = accumB 0 ((+1) <$ e) in applyE (const <$> b) e
