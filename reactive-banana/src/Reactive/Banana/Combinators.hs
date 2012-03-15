{-----------------------------------------------------------------------------
    Reactive Banana
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types, MultiParamTypeClasses, TupleSections, FlexibleInstances #-}

module Reactive.Banana.Combinators (
    -- * Synopsis
    -- | Combinators for building event graphs.
    
    -- * Introduction
    -- $intro1
    Event, Behavior,
    -- $intro2
    interpretModel, interpretPushGraph,
    
    -- * Core Combinators
    module Control.Applicative,
    never, union, unionWith, filterE, collect, spill, accumE,
    apply, stepper, accumB,
    -- $classes
    
    -- * Derived Combinators
    filterJust, filterApply, whenE,
    mapAccum, Apply(..),
    
    -- * Internal
    Event(..), Behavior(..),
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix (mfix)
import Control.Monad.Trans.State

import qualified Data.List
import Data.Maybe (isJust)
import Data.Monoid (Monoid(..))
import qualified Data.Vault as Vault
import Prelude hiding (filter)


import Reactive.Banana.Internal.InputOutput
import qualified Reactive.Banana.Internal.AST as AST
import qualified Reactive.Banana.Internal.Model as Model
import Reactive.Banana.Internal.PushGraph

{-----------------------------------------------------------------------------
    Introduction
------------------------------------------------------------------------------}
{-$intro1

At its core, Functional Reactive Programming (FRP) is about two
data types 'Event' and 'Behavior' and the various ways to combine them.

-}

{-| @Event t a@ represents a stream of events as they occur in time.
Semantically, you can think of @Event t a@ as an infinite list of values
that are tagged with their corresponding time of occurence,

> type Event t a = [(Time,a)]
-}
newtype Event t a = E { unE :: AST.Event AST.Expr [a] } -- ^ (Internal use.)

{-| @Behavior t a@ represents a value that varies in time. Think of it as

> type Behavior t a = Time -> a
-}
newtype Behavior t a = B { unB :: AST.Behavior AST.Expr a }

{-$intro2

As you can see, both types seem to have a superfluous parameter @t@.
The library uses it to rule out certain gross inefficiencies,
in particular in connection with dynamic event switching.
For basic stuff, you can completely ignore it,
except of course for the fact that it will annoy you in your type signatures.

While the type synonyms mentioned above are the way you should think about
'Behavior' and 'Event', they are a bit vague for formal manipulation.
To remedy this, the library provides a very simple but authoritative
model implementation. See 'Reactive.Banana.Model' for more.

-}

{-----------------------------------------------------------------------------
    Interpetation
------------------------------------------------------------------------------}
-- | Interpret with model implementation.
-- Useful for testing.
interpretModel :: (forall t. Event t a -> Event t b) -> [[a]] -> IO [[b]]
interpretModel f xs =
    map toList <$> Model.interpretModel (unE . f . E) (map Just xs)

-- | Interpret with push-based implementation.
-- Useful for testing.
interpretPushGraph :: (forall t. Event t a -> Event t b) -> [[a]] -> IO [[b]]
interpretPushGraph f xs = do
    i <- newInputChannel
    let automaton = compileToAutomaton (unE . f . E $ AST.inputE i)
    map toList <$> unfoldAutomaton automaton i xs

toList :: Maybe [a] -> [a]
toList Nothing   = []
toList (Just xs) = xs

{-----------------------------------------------------------------------------
    Basic combinators

    Implemented in terms of Reactive.Banana.Internal.AST
------------------------------------------------------------------------------}
singleton :: a -> [a]
singleton x = [x]

-- | Event that never occurs.
-- Think of it as @never = []@.
never    :: Event t a
never = E $ singleton <$> AST.never

-- | Merge two event streams of the same type.
-- In case of simultaneous occurrences, the left argument comes first.
-- Think of it as
--
-- > union ((timex,x):xs) ((timey,y):ys)
-- >    | timex <= timey = (timex,x) : union xs ((timey,y):ys)
-- >    | timex >  timey = (timey,y) : union ((timex,x):xs) ys
union    :: Event t a -> Event t a -> Event t a
union e1 e2 = E $ AST.unionWith (++) (unE e1) (unE e2)

-- | TODO
unionWith :: (a -> a -> a) -> Event t a -> Event t a -> Event t a
unionWith f e1 e2 = E $ AST.unionWith g (unE e1) (unE e2)
    where g xs ys = singleton $ foldr1 f (xs ++ ys)

-- | Apply a time-varying function to a stream of events.
-- Think of it as
-- 
-- > apply bf ex = [(time, bf time x) | (time, x) <- ex]
apply    :: Behavior t (a -> b) -> Event t a -> Event t b
apply bf ex = E $ AST.applyE (map <$> unB bf) (unE ex)

-- | Allow all events that fulfill the predicate, discard the rest.
-- Think of it as
-- 
-- > filterE p es = [(time,a) | (time,a) <- es, p a]
filterE   :: (a -> Bool) -> Event t a -> Event t a
filterE p = E . AST.filterE (not . null) . (Data.List.filter p <$>) . unE

-- | Collect simultaneous event occurences.
-- The result will never contain an empty list.
-- Example:
--
-- > collect [(time1, e1), (time1, e2)] = [(time1, [e1,e2])]
collect   :: Event t a -> Event t [a]
collect e = E $ singleton <$> unE e

-- | Emit simultaneous event occurrences.
-- Up to strictness, we have
--
-- > spill . collect = id
spill :: Event t [a] -> Event t a
spill e = E $ concat <$> unE e

-- | Accumulation.
-- Note: all accumulation functions are strict in the accumulated value!
-- acc -> (x,acc) is the order used by  unfoldr  and  State

-- | Construct a time-varying function from an initial value and 
-- a stream of new values. Think of it as
--
-- > stepper x0 ex = \time -> last (x0 : [x | (timex,x) <- ex, timex < time])
-- 
-- Note that the smaller-than-sign in the comparision @timex < time@ means 
-- that the value of the behavior changes \"slightly after\"
-- the event occurrences. This allows for recursive definitions.
-- 
-- Also note that in the case of simultaneous occurrences,
-- only the last one is kept.
stepper :: a -> Event t a -> Behavior t a
stepper x e = B $ AST.stepperB x (last <$> unE e)
-- stepper acc = accumB acc . fmap const

-- | The 'accumB' function is similar to a /strict/ left fold, 'foldl''.
-- It starts with an initial value and combines it with incoming events.
-- For example, think
--
-- > accumB "x" [(time1,(++"y")),(time2,(++"z"))]
-- >    = stepper "x" [(time1,"xy"),(time2,"xyz")]
-- 
-- Note that the value of the behavior changes \"slightly after\"
-- the events occur. This allows for recursive definitions.
accumB   :: a -> Event t (a -> a) -> Behavior t a
-- accumB x (Event e) = behavior $ AccumB x e
accumB  acc = stepper acc . accumE acc

-- | The 'accumE' function accumulates a stream of events.
-- Example:
--
-- > accumE "x" [(time1,(++"y")),(time2,(++"z"))]
-- >    = [(time1,"xy"),(time2,"xyz")]
--
-- Note that the output events are simultaneous with the input events,
-- there is no \"delay\" like in the case of 'accumB'.
accumE   :: a -> Event t (a -> a) -> Event t a
accumE acc = E . mapAccumE acc . fmap concatenate . unE
    where
    concatenate :: [a -> a] -> a -> ([a],a)
    concatenate fs acc = (tail values, last values)
        where values = scanl' (flip ($)) acc fs

    mapAccumE :: s -> AST.Event AST.Expr (s -> (a,s)) -> AST.Event AST.Expr a
    mapAccumE acc = fmap fst . AST.accumE (undefined,acc) . fmap (. snd)

-- strict version of scanl
scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f x ys = x : case ys of
    []   -> []
    y:ys -> let z = f x y in z `seq` scanl' f z ys

{-$classes

/Further combinators that Haddock can't document properly./

> instance Monoid (Event t (a -> a))

This monoid instance is /not/ the straightforward instance
that you would obtain from 'never' and 'union'.
Instead of just merging event streams, we use 'unionWith' to compose
the functions. This is very useful in the context of 'accumE' and 'accumB'
where simultaneous event occurrences are best avoided.

> instance Applicative (Behavior t)

'Behavior' is an applicative functor. In particular, we have the following functions.

> pure :: a -> Behavior t a

The constant time-varying value. Think of it as @pure x = \\time -> x@.

> (<*>) :: Behavior t (a -> b) -> Behavior t a -> Behavior t b

Combine behaviors in applicative style.
Think of it as @bf \<*\> bx = \\time -> bf time $ bx time@.

-}

instance Monoid (Event t (a -> a)) where
    mempty  = never
    mappend = unionWith (flip (.))

instance Functor (Event t) where
    fmap f e = E $ (map f) <$> (unE e)

instance Applicative (Behavior t) where
    pure x = B $ AST.pureB x
    bf <*> bx = B $ AST.applyB (unB bf) (unB bx)

instance Functor (Behavior t) where
    fmap = liftA

{-----------------------------------------------------------------------------
    Derived Combinators
------------------------------------------------------------------------------}
-- | Keep only the 'Just' values.
-- Variant of 'filterE'.
filterJust :: Event t (Maybe a) -> Event t a
filterJust = fmap (maybe err id) . filterE isJust
    where err = error "Reactive.Banana.Model.filterJust: Internal error. :("

-- | Allow all events that fulfill the time-varying predicate, discard the rest.
-- Generalization of 'filterE'.
filterApply :: Behavior t (a -> Bool) -> Event t a -> Event t a
filterApply bp = fmap snd . filterE fst . apply ((\p a-> (p a,a)) <$> bp)

-- | Allow events only when the behavior is 'True'.
-- Variant of 'filterApply'.
whenE :: Behavior t Bool -> Event t a -> Event t a
whenE bf = filterApply (const <$> bf)

-- | Efficient combination of 'accumE' and 'accumB'.
mapAccum :: acc -> Event t (acc -> (x,acc)) -> (Event t x, Behavior t acc)
mapAccum acc ef = (fst <$> e, stepper acc (snd <$> e))
    where e = accumE (undefined,acc) ((. snd) <$> ef)


infixl 4 <@>, <@

-- | Class for overloading the 'apply' function.
class (Functor f, Functor g) => Apply f g where
    -- | Infix operation for the 'apply' function, similar to '<*>'
    (<@>) :: f (a -> b) -> g a -> g b
    -- | Convenience function, similar to '<*'
    (<@)  :: f a -> g b -> g a
    
    f <@ g = (const <$> f) <@> g 

instance Apply (Behavior t) (Event t) where
    (<@>) = apply


