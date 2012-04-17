{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Model (
    -- * Synopsis
    -- | Model implementation of the abstract syntax tree.
    
    -- * Description
    -- $model

    -- * Combinators
    Event(..), Behavior(..),
    never, filterE, unionWith, applyE, accumE, stepperB,
    mapE, pureB, applyB, mapB,
    
    -- * Interpretation
    interpretModel,
    ) where

import Control.Applicative

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
    Combinators
------------------------------------------------------------------------------}
type Event a    = [Maybe a]
data Behavior a = StepperB a (Event a)

never :: Event a
never = repeat Nothing

filterE :: (a -> Bool) -> Event a -> Event a
filterE p = map (>>= \x -> if p x then Just x else Nothing)

unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith f = zipWith g
    where
    g (Just x) (Just y) = Just $ f x y
    g (Just x) Nothing  = Just x
    g Nothing  (Just y) = Just y
    g Nothing  Nothing  = Nothing

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

stepperB :: a -> [Maybe a] -> Behavior a
stepperB = StepperB

-- functor
mapE f  = applyE (pureB f)

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
