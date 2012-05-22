{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Model (
    -- * Synopsis
    -- | Model implementation of the abstract syntax tree.
    
    -- * Description
    -- $model

    -- * Combinators
    -- ** Data types
    Event, Behavior,
    -- ** Basic
    never, filterJust, unionWith, mapE, accumE, applyE,
    stepperB, pureB, applyB, mapB,
    -- ** Dynamic event switching
    Moment, returnM, bindM,
    trimE, observeE, switchE,
        
    -- * Interpretation
    interpretModel,
    ) where

import Control.Applicative
import Control.Monad (join)

{-$model

This module contains the model implementation for the primitive combinators
defined "Reactive.Banana.Internal.AST"
which in turn are the basis for the official combinators
documented in "Reactive.Banana.Combinators".

Look at the source code to make maximal use of this module.
(If there is no link to the source code at every type signature,
then you have to run cabal with --hyperlink-source flag.)

This model is /authoritative/: when observed with the 'interpretModel' function,
both the actual implementation and its model /must/ agree on the result.
Note that this must also hold for recursive and partial definitions
(at least in spirit, I'm not going to split hairs over @_|_@ vs @\\_ -> _|_@).

Concerning time and space complexity, the model is not authoritative, however.
Implementations are free to be much more efficient.
-}

{-----------------------------------------------------------------------------
    Basic Combinators
------------------------------------------------------------------------------}
type Event a    = [Maybe a]             -- should be abstract
data Behavior a = StepperB a (Event a)  -- should be abstract

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
    step a (Nothing:b) = stepperB a b
    step _ (Just a :b) = stepperB a b

accumE :: a -> Event (a -> a) -> Event a
accumE x []           = []
accumE x (Nothing:fs) = Nothing : accumE x fs
accumE x (Just f :fs) = let y = f x in y `seq` (Just y:accumE y fs) 

stepperB :: a -> Event a -> Behavior a
stepperB = StepperB

-- applicative functor
pureB x = stepperB x never

applyB :: Behavior (a -> b) -> Behavior a -> Behavior b
applyB (StepperB f fe) (StepperB x xe) =
    stepperB (f x) $ mapE (uncurry ($)) pair
    where
    pair = accumE (f,x) $ unionWith (.) (mapE changeL fe) (mapE changeR xe)
    changeL f (_,x) = (f,x)
    changeR x (f,_) = (f,x)

mapB f = applyB (pureB f)

interpretModel :: (Event a -> Event b) -> Event a -> IO (Event b)
interpretModel = (return .)

{-----------------------------------------------------------------------------
    Dynamic Event Switching
------------------------------------------------------------------------------}
type Time     = Int
type Moment a = Time -> a     -- should be abstract

returnM :: a -> Moment a
returnM = const

bindM :: Moment a -> (a -> Moment b) -> Moment b
bindM m g = \time -> g (m time) time


trimE :: Event a -> Moment (Moment (Event a))
trimE e = \now -> \later -> drop (later - now) e

observeE :: Event (Moment a) -> Event a
observeE = zipWith (\time -> fmap ($ time)) [0..]

switchE :: Event (Moment (Event a)) -> Event a
switchE = step never . observeE
    where
    step _      []           = []
    step (y:ys) (Nothing:xs) = y : step ys xs 
    step (y:ys) (Just zs:xs) = y : step zs xs
    -- assume that the dynamic events are at least as long as the
    -- switching event


