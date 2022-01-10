{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Reactive.Banana.Combinators (
    -- * Synopsis
    -- $synopsis

    -- * Core Combinators
    -- ** Event and Behavior
    Event, Behavior,
    interpret,

    -- ** First-order
    -- | This subsections lists the primitive first-order combinators for FRP.
    -- The 'Functor', 'Applicative' and 'Monoid' instances are also part of this,
    -- but they are documented at the types 'Event' and 'Behavior'.
    module Control.Applicative,
    module Data.Semigroup,
    never, unionWith, filterE,
    apply,

    -- ** Moment and accumulation
    Moment, MonadMoment(..),
    accumE, stepper,

    -- ** Recursion
    -- $recursion

    -- ** Higher-order
    valueB, valueBLater, observeE, switchE, switchB,

    -- * Derived Combinators
    -- ** Infix operators
    (<@>), (<@), (@>),
    -- ** Filtering
    filterJust, filterApply, whenE, split, once,
    -- ** Accumulation
    -- $Accumulation.
    unions, accumB, mapAccum,
    -- ** Merging events
    merge, mergeWith
    ) where

import Control.Applicative
import Data.Semigroup
import Data.These (These(..))

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
--
-- Note: You can safely assume that this function is pure,
-- even though the type seems to suggest otherwise.
-- I'm really sorry about the extra 'IO', but it can't be helped.
-- See source code for the sordid details.
interpret :: (Event a -> Moment (Event b)) -> [Maybe a] -> IO [Maybe b]
interpret f xs = Prim.interpret (fmap unE . unM . f . E) xs
-- FIXME: I would love to remove the 'IO' from the type signature,
-- but unfortunately, it is possible that the argument to interpret
-- returns an Event that was created in the context of an existing network, e.g.
--
-- >   eBad <- fromAddHandler ...
-- >   ...
-- >   let ys = interpret (\_ -> return eBad ) xs
--
-- Doing this is a big no-no and will break a lot of things,
-- but if we remove the 'IO' here, then we will also break referential
-- transparency, and I think that takes it too far.

{-----------------------------------------------------------------------------
    Core combinators
------------------------------------------------------------------------------}
-- | Event that never occurs.
-- Semantically,
--
-- > never = []
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
unionWith f = mergeWith id id f

-- | Merge two event streams of any type.
merge :: Event a -> Event b -> Event (These a b)
merge = mergeWith This That These

-- | Merge two event streams of any type.
--
-- This function generalizes 'unionWith'.
mergeWith
  :: (a -> c) -- ^ The function called when only the first event emits a value.
  -> (b -> c) -- ^ The function called when only the second event emits a value.
  -> (a -> b -> c) -- ^ The function called when both events emit values simultaneously.
  -> Event a
  -> Event b
  -> Event c
mergeWith f g h e1 e2 = E $ Prim.mergeWith f g h (unE e1) (unE e2)

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
-- See the discussion below for more on recursion.
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
accumE :: MonadMoment m => a -> Event (a -> a) -> m (Event a)
accumE acc = liftMoment . M . fmap E . Prim.accumE acc . unE

{-$recursion

/Recursion/ is a very important technique in FRP that is not apparent
from the type signatures.

Here is a prototypical example. It shows how the 'accumE' can be expressed
in terms of the 'stepper' and 'apply' functions by using recursion:

> accumE a e1 = mdo
>    let e2 = (\a f -> f a) <$> b <@> e1
>    b <- stepper a e2
>    return e2

(The @mdo@ notation refers to /value recursion/ in a monad.
The 'MonadFix' instance for the 'Moment' class enables this kind of recursive code.)
(Strictly speaking, this also means that 'accumE' is not a primitive,
because it can be expressed in terms of other combinators.)

This general pattern appears very often in practice:
A Behavior (here @b@) controls what value is put into an Event (here @e2@),
but at the same time, the Event contributes to changes in this Behavior.
Modeling this situation requires recursion.

For another example, consider a vending machine that sells banana juice.
The amount that the customer still has to pay for a juice
is modeled by a Behavior @bAmount@.
Whenever the customer inserts a coin into the machine,
an Event @eCoin@ occurs, and the amount will be reduced.
Whenver the amount goes below zero, an Event @eSold@ will occur,
indicating the release of a bottle of fresh banana juice,
and the amount to be paid will be reset to the original price.
The model requires recursion, and can be expressed in code as follows:

> mdo
>     let price = 50 :: Int
>     bAmount  <- accumB price $ unions
>                   [ subtract 10 <$ eCoin
>                   , const price <$ eSold ]
>     let eSold = whenE ((<= 0) <$> bAmount) eCoin

On one hand, the Behavior @bAmount@ controls whether the Event @eSold@
occcurs at all; the bottle of banana juice is unavailable to penniless customers.
But at the same time, the Event @eSold@ will cause a reset
of the Behavior @bAmount@, so both depend on each other.

Recursive code like this examples works thanks to the semantics of 'stepper'.
In general, /mutual recursion/ between several 'Event's and 'Behavior's
is always well-defined,
as long as an Event depends on itself only /via/ a Behavior,
and vice versa.

-}

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
-- > switchE e0 ee0 time0 =
-- >     concat [ trim t1 t2 e | (t1,t2,e) <- intervals ee ]
-- >   where
-- >     laterThan e time0  = [(timex,x) | (timex,x) <- e, time0 < timex ]
-- >     ee                 = [(time0, e0)] ++ (ee0 `laterThan` time0)
-- >     intervals ee       = [(time1, time2, e) | ((time1,e),(time2,_)) <- zip ee (tail ee)]
-- >     trim time1 time2 e = [x | (timex,x) <- e, time1 < timex, timex <= time2]
switchE :: MonadMoment m => Event a -> Event (Event a) -> m (Event a)
switchE e ee = liftMoment (M (fmap E (Prim.switchE (unE e) (Prim.mapE unE (unE ee)))))

-- | Dynamically switch between 'Behavior'.
-- Semantically,
--
-- >  switchB b0 eb = \time0 -> \time1 ->
-- >     last (b0 : [b | (timeb,b) <- eb, time0 <= timeb, timeb < time1]) time1
switchB :: MonadMoment m => Behavior a -> Event (Behavior a) -> m (Behavior a)
switchB b = liftMoment . M . fmap B . Prim.switchB (unB b) . Prim.mapE unB . unE

{-----------------------------------------------------------------------------
    Derived Combinators
------------------------------------------------------------------------------}
infixl 4 <@>, <@, @>

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

-- | Tag all event occurences with a time-varying value. Similar to '*>'.
--
-- This is the flipped version of '<@', but can be useful when combined with
-- @ApplicativeDo@ to sample from multiple 'Behavior's:
--
-- @
-- reactimate $ onEvent @> do
--   x <- behavior1
--   y <- behavior2
--   return (print (x + y))
-- @
(@>) :: Event a -> Behavior b -> Event b
g @> f = (const <$> f) <@> g

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
    fromLeft :: Either a b -> Maybe a
    fromLeft  (Left  a) = Just a
    fromLeft  (Right _) = Nothing

    fromRight :: Either a b -> Maybe b
    fromRight (Left  _) = Nothing
    fromRight (Right b) = Just b


-- | Keep only the next occurence of an event.
--
-- > once e = \time0 -> take 1 [(t, a) | (t, a) <- e, time0 < t]
once :: Event a -> MomentIO (Event a)
once e = mdo
    e1 <- switchE e (never <$ e1)
    return e1


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
