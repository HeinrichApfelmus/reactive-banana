{-----------------------------------------------------------------------------
    reactive-banana

    Test cases and examples
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, RecursiveDo #-}

import Control.Arrow
import Control.Monad (when, join)

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Test.HUnit (assert, Assertion)

-- import Test.QuickCheck
-- import Test.QuickCheck.Property

import Control.Applicative
import Reactive.Banana.Test.Plumbing
import Reactive.Banana.Test.Execute


main = defaultMain
    [ testGroup "Simple"
        [ testModelMatch "id"      id
        , testModelMatch "never1"  never1
        , testModelMatch "fmap1"   fmap1
        , testModelMatch "filter1" filter1
        , testModelMatch "filter2" filter2
        , testModelMatchM "accumE1" accumE1
        ]
    , testGroup "Complex"
        [ testModelMatchM "counter"     counter
        , testModelMatch "double"      double
        , testModelMatch "sharing"     sharing
        , testModelMatch "unionFilter" unionFilter
        , testModelMatchM "recursive1A"  recursive1A
        , testModelMatchM "recursive1B"  recursive1B
        , testModelMatchM "recursive2"  recursive2
        , testModelMatchM "recursive3"  recursive3
        , testModelMatchM "recursive4a" recursive4a
        -- , testModelMatchM "recursive4b" recursive4b
        , testModelMatchM "accumBvsE"   accumBvsE
        ]
    , testGroup "Dynamic Event Switching"
        [ testModelMatch  "observeE_id"         observeE_id
        , testModelMatch  "observeE_stepper"    observeE_stepper
        , testModelMatchM "valueB_immediate"    valueB_immediate
        -- , testModelMatchM "valueB_recursive1" valueB_recursive1
        -- , testModelMatchM "valueB_recursive2" valueB_recursive2
        , testModelMatchM "dynamic_apply"       dynamic_apply
        , testModelMatchM "switchE1"            switchE1
        , testModelMatchM "switchB1"            switchB1
        , testModelMatchM "switchB2"            switchB2
        ]
    , testGroup "Reactimate tests"
        [ reactimateCase
        , reactimateCase1
        , reactimate'Case
        , reactimate'Case1
        ]
    , testGroup "Regression tests"
        [ testModelMatchM "issue79" issue79
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

counter e = do
    bcounter <- accumB 0 $ fmap (\_ -> (+1)) e
    return $ applyE (pure const <*> bcounter) e

merge e1 e2 = unionWith (++) (list e1) (list e2)
    where list = fmap (:[])

double e  = merge e e
sharing e = merge e1 e1
    where e1 = filterE (< 3) e

unionFilter e1 = unionWith (+) e2 e3
    where
    e3 = fmap (+1) $ filterE even e1
    e2 = fmap (+1) $ filterE odd  e1

recursive1A e1 = mdo
    let e2 = applyE ((+) <$> b) e1
    b <- stepperB 0 e2
    return e2
recursive1B e1 = mdo
    b <- stepperB 0 e2
    let e2 = applyE ((+) <$> b) e1
    return e2

recursive2 e1 = mdo
    b  <- fmap ((+) <$>) $ stepperB 0 e3
    let e2 = applyE b e1
    let e3 = applyE (id <$> b) e1   -- actually equal to e2
    return e2

type Dummy = Int

-- Counter that can be decreased as long as it's >= 0 .
recursive3 :: Event Dummy -> Moment (Event Int)
recursive3 edec = mdo
    bcounter <- accumB 4 $ (subtract 1) <$ ecandecrease
    let ecandecrease = whenE ((>0) <$> bcounter) edec
    return $ applyE (const <$> bcounter) ecandecrease

-- Recursive 4 is an example reported by Merijn Verstraaten
--   https://github.com/HeinrichApfelmus/reactive-banana/issues/56
-- Minimization:
recursive4a :: Event Int -> Moment (Event (Bool, Int))
recursive4a eInput = mdo
    focus       <- stepperB False $ fst <$> resultE
    let resultE = resultB <@ eInput
    let resultB = (,) <$> focus <*> pureB 0
    return $ resultB <@ eInput

{-
-- Full example:
recursive4b :: Event Int -> Event (Bool, Int)
recursive4b eInput = result <@ eInput
    where
    focus     = stepperB False $ fst <$> result <@ eInput
    interface = (,) <$> focus <*> cntrVal
    (cntrVal, focusChange) = counter eInput focus
    result    = stepperB id ((***id) <$> focusChange) <*> interface

    filterApply :: Behavior (a -> Bool) -> Event a -> Event a
    filterApply b e = filterJust $ sat <$> b <@> e
        where sat p x = if p x then Just x else Nothing

    counter :: Event Int -> Behavior Bool -> (Behavior Int, Event (Bool -> Bool))
    counter input active = (result, not <$ eq)
        where
        result = accumB 0 $ (+) <$> neq
        eq     = filterApply ((==) <$> result) input
        neq    = filterApply ((/=) <$> result) input
-}

-- Test 'accumE' vs 'accumB'.
accumBvsE :: Event Dummy -> Moment (Event [Int])
accumBvsE e = mdo
    e1 <- accumE 0 ((+1) <$ e)

    b  <- accumB 0 ((+1) <$ e)
    let e2 = applyE (const <$> b) e

    return $ merge e1 e2

observeE_id = observeE . fmap return -- = id

observeE_stepper :: Event Int -> Event Int
observeE_stepper e = observeE $ (valueB =<< mb) <$ e
    where
    mb :: Moment (Behavior Int)
    mb = stepper 0 e

valueB_immediate e = do
    x <- valueB =<< stepper 0 e
    return $ x <$ e

{-- The following tests would need to use the  valueBLater  combinator

valueB_recursive1 e1 = mdo
    _ <- initialB b
    let b = stepper 0 e1
    return $ b <@ e1

valueB_recursive2 e1 = mdo
    x <- initialB b
    let bf = const x <$ stepper 0 e1
    let b  = stepper 0 $ (bf <*> b) <@ e1
    return $ b <@ e1
-}

dynamic_apply e = do
    b <- stepper 0 e
    return $ observeE $ (valueB b) <$ e
    -- = stepper 0 e <@ e

switchE1 e = switchE (e <$ e)

switchB1 e = do
    b0 <- stepper 0 e
    b1 <- stepper 0 e
    b  <- switchB b0 $ (\x -> if odd x then b1 else b0) <$> e
    return $ b <@ e

switchB2 e = do
    b0 <- stepper 0 $ filterE even e
    b1 <- stepper 1 $ filterE odd  e
    b  <- switchB b0 $ (\x -> if odd x then b1 else b0) <$> e
    return $ b <@ e

{-----------------------------------------------------------------------------
    Regression tests
------------------------------------------------------------------------------}
issue79 :: Event Dummy -> Moment (Event String)
issue79 inputEvent = mdo
    let
        appliedEvent  = (\_ _ -> 1) <$> lastValue <@> inputEvent
        filteredEvent = filterE (const True) appliedEvent
        fmappedEvent  = fmap id (filteredEvent)
    lastValue <- stepper 1 $ fmappedEvent

    let outputEvent = unionWith (++)
            (const "filtered event" <$> filteredEvent)
            (((" and " ++) . show) <$> unionWith (+) appliedEvent fmappedEvent)

    return $ outputEvent
