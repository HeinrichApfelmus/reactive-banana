{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Reactive.Banana.Combinators (
    -- * Synopsis
    -- $synopsis

    -- * Core Combinators
    -- ** Event and Behavior
    Event, Behavior,
    interpret,

    -- ** First-order
    module Control.Applicative,
    module Data.Monoid,
    never, unionWith, filterE,
    apply,
    -- $classes

    -- ** Moment and accumulation
    Moment, MonadMoment,
    accumE, stepper,

    -- ** Higher-order
    valueB, valueBLater, observeE, switchE, switchB,

    -- * Derived Combinators
    -- ** Infix operators
    (<@>), (<@),
    -- ** Filtering
    filterJust, filterApply, whenE, split,
    -- ** Accumulation
    -- $Accumulation.
    accumB, mapAccum,
    ) where

import Control.Applicative
import Control.Monad
import Data.Maybe          (isJust, catMaybes)
import Data.Monoid         (Monoid(..))

import qualified Reactive.Banana.Internal.Combinators as Prim
import           Reactive.Banana.Types

{-----------------------------------------------------------------------------
    Introduction
------------------------------------------------------------------------------}
{-$synopsis

The main types and combinators of Functional Reactive Programming (FRP).

At its core, FRP is about two data types 'Event' and 'Behavior'
and the various ways to combine them.
There is also a third type 'Moment',
which is necessary for the higher-order combinators.

-}

-- Event
-- Behavior

{-----------------------------------------------------------------------------
    Interpetation
------------------------------------------------------------------------------}
-- | Interpret an event processing function.
-- Useful for testing.
interpret :: (Event a -> Event b) -> [Maybe a] -> IO [Maybe b]
interpret f = Prim.interpret (return . unE . f . E)

{-----------------------------------------------------------------------------
    Core combinators
------------------------------------------------------------------------------}
-- | Event that never occurs.
-- Think of it as @never = []@.
never    :: Event a
never = E Prim.never

-- | Merge two event streams of the same type.
-- The function argument specifies how event values are to be combined
-- in case of a simultaneous occurrence.
--
-- > unionWith f ((timex,x):xs) ((timey,y):ys)
-- >    | timex <  timey = (timex,x)     : unionWith f xs ((timey,y):ys)
-- >    | timex >  timey = (timey,y)     : unionWith f ((timex,x):xs) ys
-- >    | timex == timey = (timex,f x y) : unionWith f xs ys
unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith f e1 e2 = E $ Prim.unionWith f (unE e1) (unE e2)

-- | Allow all event occurrences that are 'Just' values, discard the rest.
-- Variant of 'filterE'.
filterJust :: Event (Maybe a) -> Event a
filterJust = E . Prim.filterJust . unE

-- | Allow all events that fulfill the predicate, discard the rest.
-- Think of it as
--
-- > filterE p es = [(time,a) | (time,a) <- es, p a]
filterE   :: (a -> Bool) -> Event a -> Event a
filterE p = filterJust . fmap (\x -> if p x then Just x else Nothing)

-- | Apply a time-varying function to a stream of events.
-- Think of it as
--
-- > apply bf ex = [(time, bf time x) | (time, x) <- ex]
--
-- This function is generally used in its infix variant '<@>'.
apply :: Behavior (a -> b) -> Event a -> Event b
apply bf ex = E $ Prim.applyE (unB bf) (unE ex)

{-$classes

/Further combinators that Haddock can't document properly./

> instance Applicative Behavior

'Behavior' is an applicative functor. In particular, we have the following functions.

> pure :: a -> Behavior a

The constant time-varying value. Think of it as @pure x = \\time -> x@.

> (<*>) :: Behavior (a -> b) -> Behavior a -> Behavior b

Combine behaviors in applicative style.
Think of it as @bf \<*\> bx = \\time -> bf time $ bx time@.

-}

{- No monoid instance, sorry.

instance Monoid (Event t (a -> a)) where
    mempty  = never
    mappend = unionWith (flip (.))
-}

instance Functor Event where
    fmap f = E . Prim.mapE f . unE

instance Applicative Behavior where
    pure x    = B $ Prim.pureB x
    bf <*> bx = B $ Prim.applyB (unB bf) (unB bx)

instance Functor Behavior where
    fmap = liftA

-- | Construct a time-varying function from an initial value and
-- a stream of new values. Think of it as
--
-- > stepper x0 ex = \time1 -> \time2 ->
-- >     last (x0 : [x | (timex,x) <- ex, time1 <= timex, timex < time2])
--
-- Note that the smaller-than-sign in the comparision @timex < time2@ means
-- that the value of the behavior changes \"slightly after\"
-- the event occurrences. This allows for recursive definitions.
stepper :: MonadMoment m => a -> Event a -> m (Behavior a)
stepper a = liftMoment . M . fmap B . Prim.stepperB a . unE

-- | The 'accumE' function accumulates a stream of events.
-- Example:
--
-- > accumE "x" [(time1,(++"y")),(time2,(++"z"))]
-- >    = \time0 -> filter (\(time,_) -> time >= time0)
-- >                [(time1,"xy"),(time2,"xyz")]
accumE :: MonadMoment m => a -> Event (a -> a) -> m (Event a)
accumE acc = liftMoment . M . fmap E . Prim.accumE acc . unE

-- | Obtain the value of the 'Behavior' at a given moment in time.
--
-- NOTE: The value is immediately available for pattern matching.
-- Unfortunately, this means that @valueB@ is unsuitable for use
-- with value recursion in the 'Moment' monad.
-- If you need recursion, please use 'valueBLater' instead.
valueB :: MonadMoment m => Behavior a -> m a
valueB = liftMoment . M . Prim.valueB . unB

-- | Obtain the value of the 'Behavior' at a given moment in time.
--
-- NOTE: To allow for more recursion, the value is returned /lazily/
-- and not available for pattern matching immediately.
-- It can be used safely with most combinators like 'stepper'.
-- If that doesn't work for you, please use 'valueB' instead.
valueBLater :: MonadMoment m => Behavior a -> m a
valueBLater = liftMoment . M . Prim.initialBLater . unB


-- | Observe a value at those moments in time where
-- event occurrences happen. Think of it as
--
-- > observeE e = [(time, m time) | (time, m) <- ex]
observeE :: Event (Moment a) -> Event a
observeE = E . Prim.observeE . Prim.mapE unM . unE

-- | Dynamically switch between 'Event'.
switchE :: Event (Event a) -> Event a
switchE = E . Prim.switchE . Prim.mapE (unE) . unE

-- | Dynamically switch between 'Behavior'.
switchB :: Behavior a -> Event (Behavior a) -> Behavior a
switchB b = B . Prim.switchB (unB b) . Prim.mapE (unB) . unE

{-----------------------------------------------------------------------------
    Derived Combinators
------------------------------------------------------------------------------}
{-

Unfortunately, we can't make a  Num  instance because that would
require  Eq  and  Show .

instance Num a => Num (Behavior t a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger
-}
infixl 4 <@>, <@

-- | Infix synonym for the 'apply' combinator. Similar to '<*>'.
--
-- > infixl 4 <@>
(<@>) :: Behavior (a -> b) -> Event a -> Event b
(<@>) = apply

-- | Tag all event occurrences with a time-varying value. Similar to '<*'.
--
-- > infixl 4 <@
(<@)  :: Behavior b -> Event a -> Event b
f <@ g = (const <$> f) <@> g

-- | Allow all events that fulfill the time-varying predicate, discard the rest.
-- Generalization of 'filterE'.
filterApply :: Behavior (a -> Bool) -> Event a -> Event a
filterApply bp = fmap snd . filterE fst . apply ((\p a-> (p a,a)) <$> bp)

-- | Allow events only when the behavior is 'True'.
-- Variant of 'filterApply'.
whenE :: Behavior Bool -> Event a -> Event a
whenE bf = filterApply (const <$> bf)

-- | Split event occurrences according to a tag.
-- The 'Left' values go into the left component while the 'Right' values
-- go into the right component of the result.
split :: Event (Either a b) -> (Event a, Event b)
split e = (filterJust $ fromLeft <$> e, filterJust $ fromRight <$> e)
    where
    fromLeft  (Left  a) = Just a
    fromLeft  (Right b) = Nothing
    fromRight (Left  a) = Nothing
    fromRight (Right b) = Just b


-- $Accumulation.
-- Note: All accumulation functions are strict in the accumulated value!
--
-- Note: The order of arguments is @acc -> (x,acc)@
-- which is also the convention used by 'unfoldr' and 'State'.

-- | The 'accumB' function is similar to a /strict/ left fold, 'foldl''.
-- It starts with an initial value and combines it with incoming events.
-- For example, think
--
-- > accumB "x" [(time1,(++"y")),(time2,(++"z"))]
-- >    = stepper "x" [(time1,"xy"),(time2,"xyz")]
--
-- Note that the value of the behavior changes \"slightly after\"
-- the events occur. This allows for recursive definitions.
accumB :: a -> Event (a -> a) -> Moment (Behavior a)
accumB acc e = stepper acc =<< accumE acc e

-- | Efficient combination of 'accumE' and 'accumB'.
mapAccum :: acc -> Event (acc -> (x,acc)) -> Moment (Event x, Behavior acc)
mapAccum acc ef = do
        e <- accumE  (undefined,acc) (lift <$> ef)
        b <- stepper acc (snd <$> e)
        return (fst <$> e, b)
    where
    lift f (_,acc) = acc `seq` f acc
