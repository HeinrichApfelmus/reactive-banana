{-----------------------------------------------------------------------------
    Reactive Banana
    
    Class interface + Semantic model
------------------------------------------------------------------------------}
{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, EmptyDataDecls,
  MultiParamTypeClasses #-}
module Reactive.Banana.Model (
    -- * Synopsis
    -- | Combinators for building event networks and their semantics.
    
    -- * Core Combinators
    module Control.Applicative,
    FRP(..),
    Event, Behavior,
    -- $classes
    
    -- * Derived Combinators
    whenE, filterJust, mapAccum, Apply(..),
    
    -- * Model implementation
    Model,
    Time, interpretTime, interpret,
    ) where

import Control.Applicative
import qualified Data.List
import Data.Maybe
import Prelude hiding (filter)
import Data.Monoid

{-----------------------------------------------------------------------------
    Class interface
------------------------------------------------------------------------------}
{- | @Event f a@ represents a stream of events as they occur in time.
Semantically, you can think of @Event f a@ as an infinite list of values
that are tagged with their corresponding time of occurence,

> type Event f a = [(Time,a)]

See the typeclass 'FRP' for more details.
-}
data family Event f    :: * -> *

{- | Behavior f a represents a value that varies in time. Think of it as

> type Behavior f a = Time -> a

See the typeclass 'FRP' for more details.
-}
data family Behavior f :: * -> *

{- | The 'FRP' class defines the primitive API for functional reactive programming.
Each instance 'f' defines two type constructors @Event f@ and @Behavior f@
and corresponding combinators.

@Event f a@ represents a stream of events as they occur in time.
Semantically, you can think of @Event f a@ as an infinite list of values
that are tagged with their corresponding time of occurence,

> type Event f a = [(Time,a)]

@Behavior f a@ represents a value that varies in time. Think of it as

> type Behavior f a = Time -> a

While these type synonyms are the way you should think about
'Behavior' and 'Event', they are a bit vague for formal manipulation.
To remedy this, the library provides a very simple model implementation,
called 'Model'.
This model is /authoritative/: every instance of the 'FRP' class /must/
give the same results as the model when observed with the 'interpret' function.
Note that this must also hold for recursive and partial definitions
(at least in spirit, I'm not going to split hairs over @_|_@ vs @\\_ -> _|_@).

Concerning time and space complexity, the model is not authoritative, however.
Implementations are free to be much more efficient.

Minimal complete definition of the 'FRP' class: One of 'filter' or 'filterApply'
and one of 'accumB' or 'stepper'.

-}

class (Functor (Event f),
       Functor (Behavior f), Applicative (Behavior f)) => FRP f where
    
    -- | Event that never occurs.
    -- Think of it as @never = []@.
    never    :: Event f a
    
    -- | Merge two event streams of the same type.
    -- In case of simultaneous occurrences, the left argument comes first.
    -- Think of it as
    --
    -- > union ((timex,x):xs) ((timey,y):ys)
    -- >    | timex <= timey = (timex,x) : union xs ((timey,y):ys)
    -- >    | timex >  timey = (timey,y) : union ((timex,x):xs) ys
    union    :: Event f a -> Event f a -> Event f a
    
    -- | Apply a time-varying function to a stream of events.
    -- Think of it as
    -- 
    -- > apply bf ex = [(time, bf time x) | (time, x) <- ex]
    apply    :: Behavior f (a -> b) -> Event f a -> Event f b


    -- | Allow all events that fulfill the predicate, discard the rest.
    -- Think of it as
    -- 
    -- > filterE p es = [(time,a) | (time,a) <- es, p a]
    filterE   :: (a -> Bool) -> Event f a -> Event f a
    
    -- | Allow all events that fulfill the time-varying predicate, discard the rest.
    -- It's a slight generalization of 'filterE'.
    filterApply :: Behavior f (a -> Bool) -> Event f a -> Event f a
    
    
    -- Accumulation.
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
    stepper :: a -> Event f a -> Behavior f a

    -- | The 'accumB' function is similar to a /strict/ left fold, 'foldl''.
    -- It starts with an initial value and combines it with incoming events.
    -- For example, think
    --
    -- > accumB "x" [(time1,(++"y")),(time2,(++"z"))]
    -- >    = stepper "x" [(time1,"xy"),(time2,"xyz")]
    -- 
    -- Note that the value of the behavior changes \"slightly after\"
    -- the events occur. This allows for recursive definitions.
    accumB   :: a -> Event f (a -> a) -> Behavior f a
    
    -- | The 'accumE' function accumulates a stream of events.
    -- Example:
    --
    -- > accumE "x" [(time1,(++"y")),(time2,(++"z"))]
    -- >    = [(time1,"xy"),(time2,"xyz")]
    --
    -- Note that the output events are simultaneous with the input events,
    -- there is no \"delay\" like in the case of 'accumB'.
    accumE   :: a -> Event f (a -> a) -> Event f a
    
    
    -- implementation filter
    filterE p = filterApply (pure p)
    filterApply bp = fmap snd . filterE fst . apply ((\p a-> (p a,a)) <$> bp)    
    
    -- implementation accumulation
    accumB  acc = stepper acc . accumE acc
    stepper acc = accumB acc . fmap const

{-$classes

/Further combinators that Haddock can't document properly./

> instance FRP f => Monoid (Event f a)

The combinators 'never' and 'union' turn 'Event' into a monoid.

> instance FPR f => Applicative (Behavior f)

'Behavior' is an applicative functor. In particular, we have the following functions.

> pure :: FRP f => a -> Behavior f a

The constant time-varying value. Think of it as @pure x = \\time -> x@.

> (<*>) :: FRP f => Behavior f (a -> b) -> Behavior f a -> Behavior f b

Combine behaviors in applicative style.
Think of it as @bf \<*\> bx = \\time -> bf time $ bx time@.

-}

instance FRP f => Monoid (Event f a) where
    mempty  = never
    mappend = union

{-----------------------------------------------------------------------------
    Derived Combinators
------------------------------------------------------------------------------}
-- | Variant of 'filterApply'.
whenE :: FRP f => Behavior f Bool -> Event f a -> Event f a
whenE bf = filterApply (const <$> bf)

-- | Variant of 'filterE'. Keep only the 'Just' values.
filterJust :: FRP f => Event f (Maybe a) -> Event f a
filterJust = fmap (maybe err id) . filterE isJust
    where err = error "Reactive.Banana.Model.filterJust: Internal error. :("

-- | Efficient combination of 'accumE' and 'accumB'.
mapAccum :: FRP f => acc -> Event f (acc -> (x,acc)) -> (Event f x, Behavior f acc)
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

instance FRP f => Apply (Behavior f) (Event f) where
    (<@>) = apply

{-----------------------------------------------------------------------------
    Semantic model
------------------------------------------------------------------------------}
-- | The type index 'Model' represents the model implementation.
-- You are encouraged to look at the source code!
-- (If there is no link to the source code at every type signature,
-- then you have to run @cabal@ with @--hyperlink-source@ flag.)
data Model

-- Stream of events. Simultaneous events are grouped into lists.
newtype instance Event Model a = E { unE :: [[a]] }
-- Stream of values that the behavior takes.
newtype instance Behavior Model a = B { unB :: [a] }


instance Functor (Event Model) where
    fmap f = E . map (map f) . unE

instance Applicative (Behavior Model) where
    pure x    = B $ repeat x
    bf <*> bx = B $ zipWith ($) (unB bf) (unB bx)

instance Functor (Behavior Model) where
    fmap = liftA

instance FRP Model where
    never       = E $ repeat []
    union e1 e2 = E $ zipWith (++) (unE e1) (unE e2)
    
    filterApply bp = E . zipWith (\p xs-> Data.List.filter p xs) (unB bp) . unE
    apply b    = E . zipWith (\f xs -> map f xs) (unB b) . unE

    stepper x  = B . scanl go x . unE
        where go x e = last (x:e)

    accumE acc = E . accumE' acc . unE
        where
        accumE' acc []     = []
        accumE' acc (e:es) = e' : accumE' acc' es
            where
            e'   = tail $ scanl' (flip ($)) acc e
            acc' = last e'

-- strict version of scanl
scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f x ys = x : case ys of
    []   -> []
    y:ys -> let z = f x y in z `seq` scanl' f z ys

-- | Slightly simpler interpreter that does not mention 'Time'.
-- Returns lists of event values that occur simultaneously.
interpret :: (Event Model a -> Event Model b) -> [a] -> [[b]]
interpret f = unE . f . E . map (:[])

type Time = Double
-- | Interpreter that corresponds to your mental model.
interpretTime :: (Event Model a -> Event Model b) -> [(Time,a)] -> [(Time,b)]
interpretTime f xs =
    concat . zipWith tag times . interpret f . map snd $ xs
    where
    times = map fst xs
    tag t xs = map (\x -> (t,x)) xs

{-----------------------------------------------------------------------------
    Example: Counter that can be decreased
------------------------------------------------------------------------------}
example :: FRP f => Event f () -> Event f Int
example edec = apply ((\c _ -> c) <$> bcounter) ecandecrease
    where
    bcounter     = accumB 10 $ (subtract 1) <$ ecandecrease
    ecandecrease = whenE ((>0) <$> bcounter) edec

testModel = interpret example $ replicate 15 ()
-- > testModel
-- [[10],[9],[8],[7],[6],[5],[4],[3],[2],[1],[],[],[],[],[]]

example2 :: FRP f => Event f () -> Event f Int
example2 e = apply (const <$> b) e
    where
    b = accumB 0 ((+1) <$ e)

