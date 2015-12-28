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
    Time, Event, Behavior, Moment,
    -- ** Basic
    never, filterJust, unionWith, mapE, accumE, applyE,
    stepperB, pureB, applyB, mapB,
    -- ** Dynamic event switching
    valueB, observeE, switchE, switchB,

    -- * Interpretation
    interpret,
    ) where

import Control.Applicative
import Control.Monad (join)
import Data.List     (splitAt)

{-$model

This module reimplements the key FRP types and functions from the module
"Reactive.Banana.Combinators" in a way that is hopefully easier to understand.
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
    Types
------------------------------------------------------------------------------}
-- | The FRP model used in this library is actually a model with continuous time.
--
-- However, it can be shown that this model is observationally
-- equivalent to a particular model with (seemingly) discrete time steps,
-- which is implemented here.
-- Details will be explained elsewhere.
type Time = Int

data Event    a = E Time [Maybe a]  -- starting time, event values (always infinite)
    deriving (Show)
data Behavior a = B Time a [a]      -- starting time, old value, new values (always infinite)
    deriving (Show)
type Moment   a = Time -> a         -- should be abstract

epoch :: Time
epoch = 0

-- | Set the starting time of an Event.
trimE :: Event a -> Moment (Event a)
trimE (E t xs) s
    | s <= t = E s $ replicate (t-s) Nothing ++ xs
    | s >  t = E s $ drop (s-t) xs

-- | Set the starting time of a Behavior.
trimB :: Behavior a -> Moment (Behavior a)
trimB (B t x xs) s
    | s <= t = B s x $ replicate (t-s) x ++ xs
    | s >  t = B s (last ys) zs
        where
        (ys,zs) = splitAt (s-t) xs

-- Synchronize two entities
syncEE ~ex@(E tx _)   ~ey@(E ty _)   = (ex `trimE` t, ey `trimE` t)
    where t = min tx ty
syncBE ~bx@(B tx _ _) ~ey@(E ty _)   = (bx `trimB` t, ey `trimE` t)
    where t = min tx ty
syncBB ~bx@(B tx _ _) ~by@(B ty _ _) = (bx `trimB` t, by `trimB` t)
    where t = min tx ty

{-----------------------------------------------------------------------------
    Basic Combinators
------------------------------------------------------------------------------}
interpret :: (Event a -> Moment (Event b)) -> [Maybe a] -> [Maybe b]
interpret f as = zipWith const bs as
    where
    input  = E epoch (as ++ repeat Nothing)
    output = f input (epoch-7) `trimE` epoch
    E _ bs = output
    -- build network before epoch, but start external event at epoch

never :: Event a
never = E epoch (repeat Nothing)

filterJust :: Event (Maybe a) -> Event a
filterJust (E t xs) = E t (map join xs)

unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith f ex ey = E t $ zipWith g xs ys
    where
    (E t xs, E _ ys) = syncEE ex ey

    g (Just x) (Just y) = Just $ f x y
    g (Just x) Nothing  = Just x
    g Nothing  (Just y) = Just y
    g Nothing  Nothing  = Nothing

mapE f = applyE (pureB f)

applyE :: Behavior (a -> b) -> Event a -> Event b
applyE bf ex = E t $ zipWith (\f x -> fmap f x) (f:fs) xs
    where
    (B t f fs, E _ xs) = syncBE bf ex

-- applicative functor
pureB x = B epoch x (repeat x)

applyB :: Behavior (a -> b) -> Behavior a -> Behavior b
applyB bf bx = B t (f x) $ zipWith ($) fs xs
    where
    (B t f fs, B _ x xs) = syncBB bf bx

mapB f = applyB (pureB f)

{-----------------------------------------------------------------------------
    Accumulation
------------------------------------------------------------------------------}
-- Turn first occurence into `Nothing`.
smotherFirst :: Event a -> Event a
smotherFirst (E t (_:xs)) = E t (Nothing:xs)

accumE :: a -> Event (a -> a) -> Moment (Event a)
accumE x e time = E time $ go x xs
    where
    E _ xs = smotherFirst $ e `trimE` time

    go x (Nothing:fs) = Nothing : go x fs
    go x (Just f :fs) = let y = f x in y `seq` (Just y:go y fs)

stepperB :: a -> Event a -> Moment (Behavior a)
stepperB x e time = B time x $ go x xs
    where
    E _ xs = smotherFirst $ e `trimE` time

    go x (Nothing:ys) = x : go x ys
    go x (Just y :ys) = y : go y ys

{-----------------------------------------------------------------------------
    Dynamic Event Switching
------------------------------------------------------------------------------}
{-
instance Monad Moment where
    return  = const
    m >>= g = \time -> g (m time) time
-}

valueB :: Behavior a -> Moment a
valueB b time = x
    where B _ x _ = b `trimB` time

observeE :: Event (Moment a) -> Event a
observeE (E t xs) = E t $ zipWith (\time -> fmap ($ time)) [t..] xs

switchE :: Event (Event a) -> Event a
switchE (E t xs) = E t $ go t (repeat Nothing) xs
    where
    go time (y:ys) (Nothing:es) = y : go (time+1) ys es
    go time (y:ys) (Just e :es) = y : go (time+1) zs es
        where
        E _ zs = e `trimE` (time + 1)

switchB :: Behavior a -> Event (Behavior a) -> Moment (Behavior a)
switchB b x time0 = B t y $ go t ys es
    where
    (B t y ys, E _ es) = syncBE (trimB b time0) x

    go time (y:ys) (Nothing:es) = y : go (time+1) ys es
    go time _      (Just b :es) = z : go (time+1) zs es
        where B _ z zs = b `trimB` (time+1)
