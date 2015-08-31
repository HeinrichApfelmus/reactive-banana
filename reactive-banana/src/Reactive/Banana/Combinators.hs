{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Reactive.Banana.Combinators (
    -- * Synopsis
    -- | Combinators for building event graphs.
    
    -- * Introduction
    -- $intro1
    Event, Behavior,
    -- $intro2
    interpret,
    
    -- * Core Combinators
    module Control.Applicative,
    module Data.Monoid,
    never, unionWith, filterE, accumE,
    apply, stepper,
    -- $classes
    
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
{-$intro1

At its core, Functional Reactive Programming (FRP) is about two
data types 'Event' and 'Behavior' and the various ways to combine them.

-}

-- Event
-- Behavior

{-$intro2

As you can see, both types seem to have a superfluous parameter @t@.
The library uses it to rule out certain gross inefficiencies,
in particular in connection with dynamic event switching.
For basic stuff, you can completely ignore it,
except of course for the fact that it will annoy you in your type signatures.

While the type synonyms mentioned above are the way you should think about
'Behavior' and 'Event', they are a bit vague for formal manipulation.
To remedy this, the library provides a very simple but authoritative
model implementation. See "Reactive.Banana.Model" for more.

-}

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

-- | Construct a time-varying function from an initial value and 
-- a stream of new values. Think of it as
--
-- > stepper x0 ex = \time1 -> \time2 ->
--      last (x0 : [x | (timex,x) <- ex, time1 < timex, timex < time2])
-- 
-- Note that the smaller-than-sign in the comparision @timex < time@ means 
-- that the value of the behavior changes \"slightly after\"
-- the event occurrences. This allows for recursive definitions.
-- 
-- Also note that in the case of simultaneous occurrences,
-- only the last one is kept.
stepper :: a -> Event a -> Moment (Behavior a)
stepper a = M . fmap B . Prim.stepperB a . unE

-- | The 'accumE' function accumulates a stream of events.
-- Example:
--
-- > accumE "x" [(time1,(++"y")),(time2,(++"z"))]
-- >    = [(time1,"xy"),(time2,"xyz")]
--
-- Note that the output events are simultaneous with the input events,
-- there is no \"delay\" like in the case of 'accumB'.
accumE   :: a -> Event (a -> a) -> Moment (Event a)
accumE acc = M . fmap E . Prim.accumE acc . unE

-- strict version of scanl
scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f x ys = x : case ys of
    []   -> []
    y:ys -> let z = f x y in z `seq` scanl' f z ys

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
