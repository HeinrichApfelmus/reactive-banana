{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo #-}
module Reactive.Banana.Model (
    -- * Synopsis
    -- | Model implementation for learning and testing.

    -- * Overview
    -- $overview

    -- * Core Combinators
    -- ** Event and Behavior
    Nat, Time,
    Event(..), Behavior(..),
    interpret,
    -- ** First-order
    module Control.Applicative,
    never, unionWith, mergeWith, filterJust, apply,
    -- ** Moment and accumulation
    Moment(..), accumE, stepper,
    -- ** Higher-order
    valueB, observeE, switchE, switchB,
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.These (These(..), these)

{-$overview

This module reimplements the key FRP types and functions from the module
"Reactive.Banana.Combinators" in a way that is hopefully easier to understand.
Thereby, this model also specifies the semantics of the library.
Of course, the real implementation is much more efficient than this model here.

To understand the model in detail, look at the source code!
(If there is no link to the source code at every type signature,
then you have to run cabal with --hyperlink-source flag.)

This model is /authoritative/:
Event functions that have been constructed using the same combinators
/must/ give the same results when run with the @interpret@ function
from either the module "Reactive.Banana.Combinators"
or the module "Reactive.Banana.Model".
This must also hold for recursive and partial definitions
(at least in spirit, I'm not going to split hairs over @_|_@ vs @\\_ -> _|_@).

-}

{-----------------------------------------------------------------------------
    Event and Behavior
------------------------------------------------------------------------------}
-- | Natural numbers (poorly represented).
type Nat = Int

-- | The FRP model used in this library is actually a model with continuous time.
--
-- However, it can be shown that this model is observationally
-- equivalent to a particular model with (seemingly) discrete time steps,
-- which is implemented here.
-- The main reason for doing this is to be able to handle recursion correctly.
-- Details will be explained elsewhere.
type Time = Nat -- begins at t = 0

-- | Event is modeled by an /infinite/ list of 'Maybe' values.
-- It is isomorphic to @Time -> Maybe a@.
--
-- 'Nothing' indicates that no occurrence happens,
-- while 'Just' indicates that an occurrence happens.
newtype Event a = E { unE :: [Maybe a] } deriving (Show)

-- | Behavior is modeled by an /infinite/ list of values.
-- It is isomorphic to @Time -> a@.
newtype Behavior a = B { unB :: [a] } deriving (Show)

interpret :: (Event a -> Moment (Event b)) -> [Maybe a] -> [Maybe b]
interpret f as =
    take (length as) . unE . (\m -> unM m 0) . f . E $ (as ++ repeat Nothing)

{-----------------------------------------------------------------------------
    First-order
------------------------------------------------------------------------------}
instance Functor Event where
    fmap f (E xs) = E (fmap (fmap f) xs)

instance Functor Behavior where
    fmap f (B xs) = B (fmap f xs)

instance Applicative Behavior where
    pure x          = B $ repeat x
    (B f) <*> (B x) = B $ zipWith ($) f x

never :: Event a
never = E $ repeat Nothing

unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith = mergeWith id id

mergeWith
  :: (a -> c)
  -> (b -> c)
  -> (a -> b -> c)
  -> Event a
  -> Event b
  -> Event c
mergeWith f g h xs ys = these f g h <$> merge xs ys

merge :: Event a -> Event b -> Event (These a b)
merge (E xs) (E ys) = E $ zipWith combine xs ys
    where
    combine Nothing  Nothing  = Nothing
    combine (Just x) Nothing  = Just (This x)
    combine Nothing  (Just y) = Just (That y)
    combine (Just x) (Just y) = Just (These x y)

filterJust :: Event (Maybe a) -> Event a
filterJust = E . fmap join . unE

apply :: Behavior (a -> b) -> Event a -> Event b
apply (B fs) = E . zipWith (\f mx -> fmap f mx) fs . unE

{-----------------------------------------------------------------------------
    Moment and accumulation
------------------------------------------------------------------------------}
newtype Moment a = M { unM :: Time -> a }

instance Functor     Moment where fmap f = M . fmap f . unM
instance Applicative Moment where
    pure   = M . const
    (<*>)  = ap
instance Monad Moment where
    return = pure
    (M m) >>= k = M $ \time -> unM (k $ m time) time

instance MonadFix Moment where
    mfix f = M $ mfix (unM . f)

-- Forget all event occurences before a particular time
forgetE :: Time -> Event a -> [Maybe a]
forgetE time (E xs) = drop time xs

stepper :: a -> Event a -> Moment (Behavior a)
stepper i e = M $ \time -> B $ replicate time i ++ step i (forgetE time e)
    where
    step i ~(x:xs) = i : step next xs
        where next = case x of
                        Just i  -> i
                        Nothing -> i

-- Expressed using recursion and the other primitives
-- FIXME: Strictness!
accumE :: a -> Event (a -> a) -> Moment (Event a)
accumE a e1 = mdo
    let e2 = ((\a f -> f a) <$> b) `apply` e1
    b <- stepper a e2
    return e2

{-----------------------------------------------------------------------------
    Higher-order
------------------------------------------------------------------------------}
valueB :: Behavior a -> Moment a
valueB (B b) = M $ \time -> b !! time

observeE :: Event (Moment a) -> Event a
observeE = E . zipWith (\time -> fmap (\m -> unM m time)) [0..] . unE

switchE :: Event (Event a) -> Moment (Event a)
switchE es = M $ \t -> E $
    replicate t Nothing ++ switch (unE never) (forgetE t (forgetDiagonalE es))
    where
    switch (x:xs) (Nothing : ys) = x : switch xs ys
    switch (x: _) (Just xs : ys) = x : switch (tail xs) ys

forgetDiagonalE :: Event (Event a) -> Event [Maybe a]
forgetDiagonalE = E . zipWith (\time -> fmap (forgetE time)) [0..] . unE

switchB :: Behavior a -> Event (Behavior a) -> Moment (Behavior a)
switchB b e = diagonalB <$> stepper b e

diagonalB :: Behavior (Behavior a) -> Behavior a
diagonalB = B . zipWith (\time xs -> xs !! time) [0..] . map unB . unB
