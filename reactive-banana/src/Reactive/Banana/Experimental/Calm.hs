{-----------------------------------------------------------------------------
    Reactive Banana
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types, MultiParamTypeClasses,
    TypeSynonymInstances, FlexibleInstances #-}

module Reactive.Banana.Experimental.Calm (
    -- * Synopsis
    -- | Experimental module: API change very likely.
    --
    -- 'Event' type that disallows simultaneous event occurrences.
    --
    -- The combinators behave essentially as their counterparts
    -- in "Reactive.Banana.Combinators".
    
    -- * Main types
    Event, Behavior, collect, fromCalm,
    interpret,
    
    -- * Core Combinators
    module Control.Applicative,
    never, unionWith, filterE, accumE,
    apply, stepper,
    -- $classes
    
    -- * Derived Combinators
    -- ** Filtering
    filterJust,
    -- ** Accumulation
    -- $Accumulation.
    accumB, mapAccum,
    -- ** Apply class
    Reactive.Banana.Combinators.Apply(..),
    ) where

import Control.Applicative
import Control.Monad

import Data.Maybe (listToMaybe)

import qualified Reactive.Banana.Combinators as Prim
import qualified Reactive.Banana.Combinators

{-----------------------------------------------------------------------------
    Main types
------------------------------------------------------------------------------}
newtype Event t a = E { unE :: Prim.Event t a }

type Behavior t = Reactive.Banana.Combinators.Behavior t

-- | Convert event with possible simultaneous occurrences
-- into an 'Event' with a single occurrence.
collect :: Reactive.Banana.Combinators.Event t a -> Event t [a]
collect = E . Prim.collect

-- | Convert event with single occurrences into
-- event with possible simultaneous occurrences
fromCalm :: Event t a -> Reactive.Banana.Combinators.Event t a
fromCalm = unE

singleton x = [x]

-- | Interpretation function.
-- Useful for testing.
interpret :: (forall t. Event t a -> Event t b) -> [a] -> IO [Maybe b]
interpret f xs =
    map listToMaybe <$> Prim.interpret (unE . f . E) (map singleton xs)

{-----------------------------------------------------------------------------
    Core Combinators
------------------------------------------------------------------------------}
-- | Event that never occurs.
-- Think of it as @never = []@.
never    :: Event t a
never = E $ Prim.never

-- | Merge two event streams of the same type.
-- Combine simultaneous values if necessary.
unionWith    :: (a -> a -> a) -> Event t a -> Event t a -> Event t a
unionWith f e1 e2 = E $ Prim.unionWith f (unE e1) (unE e2)

-- | Allow all events that fulfill the predicate, discard the rest.
filterE   :: (a -> Bool) -> Event t a -> Event t a
filterE p = E . Prim.filterE p . unE

-- | Construct a time-varying function from an initial value and 
-- a stream of new values.
stepper :: a -> Event t a -> Behavior t a
stepper x e = Prim.stepper x (unE e)

-- | The 'accumE' function accumulates a stream of events.
accumE   :: a -> Event t (a -> a) -> Event t a
accumE acc = E . Prim.accumE acc . unE

-- | Apply a time-varying function to a stream of events.
apply    :: Behavior t (a -> b) -> Event t a -> Event t b
apply b = E . Prim.apply b . unE

instance Functor (Event t) where
    fmap f = E . fmap f . unE

{-----------------------------------------------------------------------------
    Derived Combinators
------------------------------------------------------------------------------}
-- | Keep only the 'Just' values.
-- Variant of 'filterE'.
filterJust :: Event t (Maybe a) -> Event t a
filterJust = E . Prim.filterJust . unE

-- | The 'accumB' function is similar to a /strict/ left fold, 'foldl''.
-- It starts with an initial value and combines it with incoming events.
accumB :: a -> Event t (a -> a) -> Behavior t a
accumB acc = Prim.accumB acc . unE

-- $Accumulation.
-- Note: all accumulation functions are strict in the accumulated value!
-- acc -> (x,acc) is the order used by 'unfoldr' and 'State'.

-- | Efficient combination of 'accumE' and 'accumB'.
mapAccum :: acc -> Event t (acc -> (x,acc)) -> (Event t x, Behavior t acc)
mapAccum acc ef = let (e,b) = Prim.mapAccum acc (unE ef) in (E e, b)

instance Reactive.Banana.Combinators.Apply (Behavior t) (Event t) where
    (<@>) = apply


