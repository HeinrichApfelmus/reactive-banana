{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE BangPatterns #-}
module Reactive.Banana.Model (
    -- * Synopsis
    -- | Model implementation for learning and testing.

    -- * Overview
    -- $model

    -- * Combinators
    -- ** Data types
    Event, Behavior,
    -- ** Basic
    never, filterJust, unionWith, mapE, accumE, applyE,
    stepperB, pureB, applyB, mapB,
    -- ** Dynamic event switching
    Moment,
    valueB, observeE, switchE, switchB,

    -- * Interpretation
    interpret,
    ) where

import Control.Applicative
import Control.Monad (join)

{-$model

This module reimplements the key FRP types and functions from the module
"Reactive.Banana.Combinators" in a simple that is easier to understand.
Thereby, this model also specifies the semantics of the library.
Of course, the real implementation is much more efficient than this model here.

To understand the model in detail, look at the source code!
(If there is no link to the source code at every type signature,
then you have to run cabal with --hyperlink-source flag.)

This model is /authoritative/: when observed with the 'interpretModel' function,
both the actual implementation and its model /must/ agree on the result.
Note that this must also hold for recursive and partial definitions
(at least in spirit, I'm not going to split hairs over @_|_@ vs @\\_ -> _|_@).

-}

{-----------------------------------------------------------------------------
    Basic Combinators
------------------------------------------------------------------------------}
type Event a    = [Maybe a]              -- should be abstract
data Behavior a = StepperB !a (Event a)  -- should be abstract

interpret :: (Event a -> Moment (Event b)) -> [Maybe a] -> [Maybe b]
interpret f e = f e 0

never :: Event a
never = repeat Nothing

filterJust :: Event (Maybe a) -> Event a
filterJust = map join

unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith f = zipWith g
    where
    g (Just x) (Just y) = Just $ f x y
    g (Just x) Nothing  = Just x
    g Nothing  (Just y) = Just y
    g Nothing  Nothing  = Nothing

mapE f  = applyE (pureB f)

applyE :: Behavior (a -> b) -> Event a -> Event b
applyE _               []     = []
applyE (StepperB f fe) (x:xs) = fmap f x : applyE (step f fe) xs
    where
    step a (Nothing:b) = StepperB a b
    step _ (Just a :b) = StepperB a b

-- applicative functor
pureB x = StepperB x never

applyB :: Behavior (a -> b) -> Behavior a -> Behavior b
applyB (StepperB f fe) (StepperB x xe) =
    StepperB (f x) $ mapE (uncurry ($)) (pair 0)
    where
    pair = accumE (f,x) $ unionWith (.) (mapE changeL fe) (mapE changeR xe)
    changeL f (_,x) = (f,x)
    changeR x (f,_) = (f,x)

mapB f = applyB (pureB f)

{-----------------------------------------------------------------------------
    Accumulation
------------------------------------------------------------------------------}
accumE :: a -> Event (a -> a) -> Moment (Event a)
accumE x e = \time -> go x $ drop (time - 0) e
    where
    go x []           = []
    go x (Nothing:fs) = Nothing : go x fs
    go x (Just f :fs) = let y = f x in y `seq` (Just y:go y fs)

stepperB :: a -> Event a -> Moment (Behavior a)
stepperB x e = \time -> StepperB x $ drop (time - 0) e

{-----------------------------------------------------------------------------
    Dynamic Event Switching
------------------------------------------------------------------------------}
type Time     = Int
type Moment a = Time -> a     -- should be abstract

{-
instance Monad Moment where
    return  = const
    m >>= g = \time -> g (m time) time
-}

trimB 0    b                           = b
trimB time (StepperB x []            ) = StepperB x []
trimB time (StepperB x (Nothing : xs)) = trimB (time-1) $ StepperB x xs
trimB time (StepperB x (Just y  : xs)) = trimB (time-1) $ StepperB y xs

valueB :: Behavior a -> Moment a
valueB b = \time -> let StepperB x _ = trimB time b in x

observeE :: Event (Moment a) -> Event a
observeE = zipWith (\time -> fmap ($ time)) [0..]

switchE :: Event (Event a) -> Event a
switchE = step 0 never
    where
    step time ys     []           = ys
    step time (y:ys) (Nothing:xs) = y : step (time+1) ys xs
    step time (y:ys) (Just zs:xs) = y : step (time+1) (drop (time+1) zs) xs
    -- assume that the dynamic events are at least as long as the
    -- switching event

switchB :: Behavior a -> Event (Behavior a) -> Behavior a
switchB (StepperB x e) = StepperB x . step 0 e
    where
    step t ys     []             = ys
    step t (y:ys) (Nothing : xs) =          y : step (t+1) ys xs
    step t (y:ys) (Just b  : xs) = Just value : step (t+1) (drop 1 zs) xs
        where
        StepperB z zs = trimB t b
        value         = case zs of
            Just z : _ -> z -- new behavior changes right away
            _          -> z -- new behavior stays constant for a while
