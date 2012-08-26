{-----------------------------------------------------------------------------
    reactive-banana

    Test cases and examples
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, RecursiveDo #-}

import Control.Monad (when, join)

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
        , testModelMatch "accumE1" accumE1
        ]
    , testGroup "Complex"
        [ testModelMatch "counter"    counter
        , testModelMatch "double"     double
        , testModelMatch "sharing"    sharing
        , testModelMatch "recursive1" recursive1
        , testModelMatch "recursive2" recursive2
        , testModelMatch "recursive3" recursive3
        , testModelMatch "accumBvsE"  accumBvsE
        ]
    , testGroup "Dynamic Event Switching"
        [ testModelMatch  "observeE_id"         observeE_id
        , testModelMatchM "initialB_immediate"  initialB_immediate
        , testModelMatchM "initialB_recursive1" initialB_recursive1
        , testModelMatchM "initialB_recursive2" initialB_recursive2
        , testModelMatchM "dynamic_apply"       dynamic_apply
        , testModelMatchM "switchE1"            switchE1
        , testModelMatchM "switchB_two"         switchB_two
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
    => (Event a -> Moment (Event b)) -> [a] -> IO Bool
matchesModel f xs = do
    bs1 <- return $ interpretModel f (singletons xs)
    bs2 <- interpretGraph f (singletons xs)
    -- bs3 <- interpretFrameworks f xs
    let bs = [bs1,bs2]
    let b = all (==bs1) bs
    when (not b) $ mapM_ print bs
    return b

singletons = map Just

-- test whether model matches
testModelMatchM
    :: (Show b, Eq b)
    => String -> (Event Int -> Moment (Event b)) -> Test
testModelMatchM name f = testCase name $ assert $ matchesModel f [1..8::Int]
testModelMatch name f = testModelMatchM name (return . f)

-- individual tests for debugging
testModel :: (Event Int -> Event b) -> [Maybe b]
testModel f = interpretModel (return . f) $ singletons [1..8::Int]
testGraph f = interpretGraph (return . f) $ singletons [1..8::Int]

testModelM f = interpretModel f $ singletons [1..8::Int]
testGraphM f = interpretGraph f $ singletons [1..8::Int]


{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
never1 :: Event Int -> Event Int
never1    = const never
fmap1     = fmap (+1)

filterE p = filterJust . fmap (\e -> if p e then Just e else Nothing)
filter1   = filterE (>= 3)
filter2   = filterE (>= 3) . fmap (subtract 1)
accumE1   = accumE 0 . ((+1) <$)

counter e = applyE (pure const <*> bcounter) e
    where bcounter = accumB 0 $ fmap (\_ -> (+1)) e

merge e1 e2 = unionWith (++) (list e1) (list e2)
    where list = fmap (:[])
    
double e  = merge e e
sharing e = merge e1 e1
    where e1 = filterE (< 3) e
recursive1 e1 = e2
    where
    e2 = applyE b e1
    b  = (+) <$> stepperB 0 e2
recursive2 e1 = e2
    where
    e2 = applyE b e1
    b  = (+) <$> stepperB 0 e3
    e3 = applyE (id <$> b) e1   -- actually equal to e2

type Dummy = Int

-- counter that can be decreased as long as it's >= 0
recursive3 :: Event Dummy -> Event Int
recursive3 edec = applyE (const <$> bcounter) ecandecrease
    where
    bcounter     = accumB 4 $ (subtract 1) <$ ecandecrease
    ecandecrease = whenE ((>0) <$> bcounter) edec

-- test accumE vs accumB
accumBvsE :: Event Dummy -> Event [Int]
accumBvsE e = merge e1 e2
    where
    e1 = accumE 0 ((+1) <$ e)
    e2 = let b = accumB 0 ((+1) <$ e) in applyE (const <$> b) e


observeE_id = observeE . fmap return -- = id

initialB_immediate e = do
    x <- initialB (stepper 0 e)
    return $ x <$ e
initialB_recursive1 e1 = mdo
    _ <- initialB b
    let b = stepper 0 e1
    return $ b <@ e1
    
-- NOTE: This test case tries to reproduce a situation
-- where the value of a latch is used before the latch was created.
-- This was relevant for the CRUD example, but I can't find a way
-- to make it smaller right now. Oh well.
initialB_recursive2 e1 = mdo
    x <- initialB b
    let bf = const x <$ stepper 0 e1 
    let b  = stepper 0 $ (bf <*> b) <@ e1
    return $ b <@ e1

dynamic_apply e = do
    mb <- trimB $ stepper 0 e
    return $ observeE $ (initialB =<< mb) <$ e
    -- = stepper 0 e <@ e
switchE1 e = do
    me <- trimE e
    return $ switchE $ me <$ e
switchB_two e = do
    mb0 <- trimB $ stepper 0 $ filterE even e
    mb1 <- trimB $ stepper 1 $ filterE odd  e
    b0  <- mb0
    let b = switchB b0 $ (\x -> if odd x then mb1 else mb0) <$> e
    return $ b <@ e
