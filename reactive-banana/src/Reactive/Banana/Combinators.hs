{-----------------------------------------------------------------------------
    Reactive Banana
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types, MultiParamTypeClasses, TupleSections #-}

module Reactive.Banana.Combinators (
    -- * Synopsis
    -- | Combinators for building event graphs.
    
    -- * Introduction
    -- $intro1
    Event, Behavior,
    -- $intro2
    interpretModel,
    
    -- * Core Combinators
    module Control.Applicative,
    never, union, apply, filterE, stepper, accumB, accumE,
    -- $classes
    
    -- * Derived Combinators
    filterJust, filterApply, whenE,
    mapAccum, Apply(..),
    
    -- * Internal
    event, behavior, Event(..)
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

import Reactive.Banana.Input
import qualified Reactive.Banana.Model as Model

import Reactive.Banana.PushIO hiding (Event, Behavior)
import qualified Reactive.Banana.PushIO as Implementation

import System.IO.Unsafe (unsafePerformIO) -- for observable sharing

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
newtype Event t a = Event (Implementation.Event Accum a) -- ^ (Internal use.)

-- smart constructor
event :: Implementation.EventD Accum a -> Event t a
event e = Event pair
    where
    {-# NOINLINE pair #-}
    -- mention argument to prevent let-floating
    pair = unsafePerformIO (fmap (,e) newEventNode)

{-| @Behavior t a@ represents a value that varies in time. Think of it as

> type Behavior t a = Time -> a
-}
newtype Behavior t a = Behavior (Implementation.Behavior Accum a)

-- smart constructor
behavior :: Implementation.BehaviorD Accum a -> Behavior t a
behavior b = Behavior pair
    where
    {-# NOINLINE pair #-}
    -- mention argument to prevent let-floating  
    pair = unsafePerformIO (fmap (,b) newBehaviorNode)

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
    Basic combinators
------------------------------------------------------------------------------}
-- | Event that never occurs.
-- Think of it as @never = []@.
never    :: Event t a
never = event $ Never

-- | Merge two event streams of the same type.
-- In case of simultaneous occurrences, the left argument comes first.
-- Think of it as
--
-- > union ((timex,x):xs) ((timey,y):ys)
-- >    | timex <= timey = (timex,x) : union xs ((timey,y):ys)
-- >    | timex >  timey = (timey,y) : union ((timex,x):xs) ys
union    :: Event t a -> Event t a -> Event t a
union (Event e1) (Event e2) = event $ Union e1 e2

-- | Apply a time-varying function to a stream of events.
-- Think of it as
-- 
-- > apply bf ex = [(time, bf time x) | (time, x) <- ex]
apply    :: Behavior t (a -> b) -> Event t a -> Event t b
apply (Behavior bf) (Event ex) = event $ ApplyE bf ex

-- | Allow all events that fulfill the predicate, discard the rest.
-- Think of it as
-- 
-- > filterE p es = [(time,a) | (time,a) <- es, p a]
filterE   :: (a -> Bool) -> Event t a -> Event t a
filterE p (Event e) = event $ Filter p e

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
stepper acc = accumB acc . fmap const

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
accumB x (Event e) = behavior $ AccumB x e
-- accumB  acc = stepper acc . accumE acc

-- | The 'accumE' function accumulates a stream of events.
-- Example:
--
-- > accumE "x" [(time1,(++"y")),(time2,(++"z"))]
-- >    = [(time1,"xy"),(time2,"xyz")]
--
-- Note that the output events are simultaneous with the input events,
-- there is no \"delay\" like in the case of 'accumB'.
accumE   :: a -> Event t (a -> a) -> Event t a
accumE x (Event e) = event $ AccumE x e


{-$classes

/Further combinators that Haddock can't document properly./

> instance Monoid (Event t a)

The combinators 'never' and 'union' turn 'Event' into a monoid.

> instance Applicative (Behavior t)

'Behavior' is an applicative functor. In particular, we have the following functions.

> pure :: a -> Behavior t a

The constant time-varying value. Think of it as @pure x = \\time -> x@.

> (<*>) :: Behavior t (a -> b) -> Behavior t a -> Behavior t b

Combine behaviors in applicative style.
Think of it as @bf \<*\> bx = \\time -> bf time $ bx time@.

-}

instance Monoid (Event t a) where
    mempty  = never
    mappend = union

instance Functor (Event t) where
    fmap f e = apply (pure f) e

instance Applicative (Behavior t) where
    pure x = behavior $ Pure x
    (Behavior bf) <*> (Behavior bx) = behavior $ ApplyB bf bx

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


{-----------------------------------------------------------------------------
    Model implementation
------------------------------------------------------------------------------}
type Eval = State Vault.Vault

-- | Interpret an event graph with the model implementation.
-- Mainly useful for testing library internals.
interpretModel :: (forall t. Event t a -> Event t b) -> [[a]] -> IO [[b]]
interpretModel f input = do
    i0 <- newInputChannel
    
    let
        evalE :: EventD Accum a -> Eval (Model.Event t a)
        evalE (Filter p e )  = Model.filterE p <$> goE e
        evalE (Union e1 e2)  = Model.union     <$> goE e1 <*> goE e2
        evalE (ApplyE b e )  = Model.apply     <$> goB b  <*> goE e
        evalE (AccumE x e )  = Model.accumE x  <$> goE e
        evalE (Never)        = return $ Model.never
        evalE (InputPure i)  =
            return $ maybe err Model.E $ fromValue i (toValue i0 input)
            where err = error "Reactive.Banana.PushIO.interpretModel: internal error: Input"
        evalE _              =
            error "Reactive.Banana.PushIO.interpretModel: internal error: E"

        evalB :: BehaviorD Accum a -> Eval (Model.Behavior t a)
        evalB (Pure x      ) = return $ pure x
        evalB (ApplyB bf bx) = (<*>) <$> goB bf <*> goB bx
        evalB (AccumB x  e ) = Model.accumB x <$> goE e   
        evalB _              =
            error "Reactive.Banana.PushIO.interpretModel: internal error: B"
    
    
        goE :: Implementation.Event Accum a -> Eval (Model.Event t a)
        goE (node, e) = do
            values <- get
            Model.E <$> case Vault.lookup (valueE node) values of
                Nothing -> mfix $ \v -> do
                    modify $ Vault.insert (valueE node) v
                    Model.unE <$> evalE e
                Just v  -> return v

        goB :: Implementation.Behavior Accum a -> Eval (Model.Behavior t a)
        goB (node, b) = do
            values <- get
            Model.B <$> case Vault.lookup (valueB node) values of
                Nothing -> mfix $ \v -> do
                    modify $ Vault.insert (valueB node) v
                    Model.unB <$> evalB b
                Just v  -> return v
    
        unEvent (Event e) = e
    
    return
        $ Model.unE
        $ evalState (goE . unEvent . f . event $ InputPure i0) Vault.empty


-- TODO: test observable sharing (though that should probably work by now)





