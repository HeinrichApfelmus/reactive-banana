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
    Moment, MonadMoment(..),
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
    unions, accumB, mapAccum,
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
-- Semantically, @never = []@.
never    :: Event a
never = E Prim.never

-- | Merge two event streams of the same type.
-- The function argument specifies how event values are to be combined
-- in case of a simultaneous occurrence. The semantics are
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
-- Semantically,
--
-- > filterE p es = [(time,a) | (time,a) <- es, p a]
filterE   :: (a -> Bool) -> Event a -> Event a
filterE p = filterJust . fmap (\x -> if p x then Just x else Nothing)

-- | Apply a time-varying function to a stream of events.
-- Semantically,
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

The constant time-varying value. Semantically, @pure x = \\time -> x@.

> (<*>) :: Behavior (a -> b) -> Behavior a -> Behavior b

Combine behaviors in applicative style.
The semantics are: @bf \<*\> bx = \\time -> bf time $ bx time@.

-}

instance Functor Event where
    fmap f = E . Prim.mapE f . unE

instance Applicative Behavior where
    pure x    = B $ Prim.pureB x
    bf <*> bx = B $ Prim.applyB (unB bf) (unB bx)

instance Functor Behavior where
    fmap = liftA

-- | Construct a time-varying function from an initial value and
-- a stream of new values. The result will be a step function.
-- Semantically,
--
-- > stepper x0 ex = \time1 -> \time2 ->
-- >     last (x0 : [x | (timex,x) <- ex, time1 <= timex, timex < time2])
--
-- Here is an illustration of the result Behavior at a particular time:
--
-- <<doc/frp-stepper.png>>
--
-- Note: The smaller-than-sign in the comparison @timex < time2@ means
-- that at time @time2 == timex@, the value of the Behavior will
-- still be the previous value.
-- In the illustration, this is indicated by the dots at the end
-- of each step.
-- This allows for recursive definitions.
stepper :: MonadMoment m => a -> Event a -> m (Behavior a)
stepper a = liftMoment . M . fmap B . Prim.stepperB a . unE

-- | The 'accumE' function accumulates a stream of event values,
-- similar to a /strict/ left scan, 'scanl''.
-- It starts with an initial value and emits a new value
-- whenever an event occurrence happens.
-- The new value is calculated by applying the function in the event
-- to the old value.
--
-- Example:
--
-- > accumE "x" [(time1,(++"y")),(time2,(++"z"))]
-- >     = trimE [(time1,"xy"),(time2,"xyz")]
-- >     where
-- >     trimE e start = [(time,x) | (time,x) <- e, start <= time]
--
--
-- Note: It makes sense to list the 'accumE' function as a primitive
-- combinator, but keep in mind that it can actually be expressed
-- in terms of 'stepper' and 'apply' by using recursion:
--
-- > accumE a e1 = mdo
-- >    let e2 = (\a f -> f a) <$> b <@> e1
-- >    b <- stepper a e2
-- >    return e2
--
accumE :: MonadMoment m => a -> Event (a -> a) -> m (Event a)
accumE acc = liftMoment . M . fmap E . Prim.accumE acc . unE

-- | Obtain the value of the 'Behavior' at a given moment in time.
-- Semantically, it corresponds to
--
-- > valueB b = \time -> b time
--
-- Note: The value is immediately available for pattern matching.
-- Unfortunately, this means that @valueB@ is unsuitable for use
-- with value recursion in the 'Moment' monad.
-- If you need recursion, please use 'valueBLater' instead.
valueB :: MonadMoment m => Behavior a -> m a
valueB = liftMoment . M . Prim.valueB . unB

-- | Obtain the value of the 'Behavior' at a given moment in time.
-- Semantically, it corresponds to
--
-- > valueBLater b = \time -> b time
--
-- Note: To allow for more recursion, the value is returned /lazily/
-- and not available for pattern matching immediately.
-- It can be used safely with most combinators like 'stepper'.
-- If that doesn't work for you, please use 'valueB' instead.
valueBLater :: MonadMoment m => Behavior a -> m a
valueBLater = liftMoment . M . Prim.initialBLater . unB


-- | Observe a value at those moments in time where
-- event occurrences happen. Semantically,
--
-- > observeE e = [(time, m time) | (time, m) <- e]
observeE :: Event (Moment a) -> Event a
observeE = E . Prim.observeE . Prim.mapE unM . unE

-- | Dynamically switch between 'Event'.
-- Semantically,
--
-- > switchE ee = concat [trim t1 t2 e | (t1,t2,e) <- intervals ee]
-- >     where
-- >     intervals e        = [(time1, time2, x) | ((time1,x),(time2,_)) <- zip e (tail e)]
-- >     trim time1 time2 e = [x | (timex,x) <- e, time1 < timex, timex <= time2]

switchE :: Event (Event a) -> Event a
switchE = E . Prim.switchE . Prim.mapE (unE) . unE

-- | Dynamically switch between 'Behavior'.
-- Semantically,
--
-- >  switchB b0 eb = \time ->
-- >     last (b0 : [b | (time2,b) <- eb, time2 < time]) time

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

-- | Merge event streams whose values are functions.
-- In case of simultaneous occurrences, the functions at the beginning
-- of the list are applied /after/ the functions at the end.
--
-- > unions [] = never
-- > unions xs = foldr1 (unionWith (.)) xs
--
-- Very useful in conjunction with accumulation functions like 'accumB'
-- and 'accumE'.
unions :: [Event (a -> a)] -> Event (a -> a)
unions [] = never
unions xs = foldr1 (unionWith (.)) xs

-- | The 'accumB' function accumulates event occurrences into a 'Behavior'.
--
-- The value is accumulated using 'accumE' and converted
-- into a time-varying value using 'stepper'.
--
-- Example:
--
-- > accumB "x" [(time1,(++"y")),(time2,(++"z"))]
-- >    = stepper "x" [(time1,"xy"),(time2,"xyz")]
--
-- Note: As with 'stepper', the value of the behavior changes \"slightly after\"
-- the events occur. This allows for recursive definitions.
accumB :: MonadMoment m => a -> Event (a -> a) -> m (Behavior a)
accumB acc e = stepper acc =<< accumE acc e

-- | Efficient combination of 'accumE' and 'accumB'.
mapAccum :: MonadMoment m => acc -> Event (acc -> (x,acc)) -> m (Event x, Behavior acc)
mapAccum acc ef = do
        e <- accumE  (undefined,acc) (lift <$> ef)
        b <- stepper acc (snd <$> e)
        return (fst <$> e, b)
    where
    lift f (_,acc) = acc `seq` f acc
