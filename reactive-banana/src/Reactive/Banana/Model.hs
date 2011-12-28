{-----------------------------------------------------------------------------
    Reactive Banana
    
    Class interface + Semantic model
------------------------------------------------------------------------------}
{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, EmptyDataDecls,
  MultiParamTypeClasses #-}
module Reactive.Banana.Model (
    -- * Synopsis
    -- | Model implementation. Inspect the source code!
    
    -- * Description
    -- $model
    ) where

import Control.Applicative
import qualified Data.List
import Data.Maybe
import Prelude hiding (filter)
import Data.Monoid

{-----------------------------------------------------------------------------
    Description
------------------------------------------------------------------------------}

{-$model

This module contains the model implementation for the combinators
defined and documented in 'Reactive.Banana.Combinators'.
It does not export any functions,
you have to look at the source code to make use of it.
(If there is no link to the source code at every type signature,
then you have to run cabal with --hyperlink-source flag.)

This model is /authoritative/: when observed with the 'interpretModel' function,
both the actual implementation and its model /must/ agree on the result.
Note that this must also hold for recursive and partial definitions
(at least in spirit, I'm not going to split hairs over @_|_@ vs @\\_ -> _|_@).

Concerning time and space complexity, the model is not authoritative, however.
Implementations are free to be much more efficient.
-}

{-----------------------------------------------------------------------------
    Semantic model
------------------------------------------------------------------------------}
-- Stream of discrete occurences of a single value.
type Discrete = []

-- instance Functor Discrete
tailsD :: Discrete a -> Discrete (Discrete a)
tailsD = Data.List.tails


-- Stream of events. Simultaneous events are grouped into lists.
--   e !! i = list of simultaneous event occurences happening at time == i
newtype EventModel    t a = E { unE :: Discrete [a] } deriving Show

-- Stream of values that the behavior has taken.
--   b !! i = value of the behavior in the time interval  i-1 < time <= i
newtype BehaviorModel t a = B { unB :: Discrete a   } deriving Show


-- events
instance Functor (Event t) where
    fmap f = E . fmap (map f) . unE

never       = E $ pure []
union e1 e2 = E $ zipWith (++) (unE e1) (unE e2)

filterE p = E . fmap (Data.List.filter p) . unE

accumE acc = E . accumE' acc . unE
    where
    accumE' !acc []      = []
    accumE' !acc ([]:es) = [] : accumE' acc  es
    accumE' !acc ( e:es) = e' : accumE' acc' es
        where
        e'   = tail $ scanl' (flip ($)) acc e
        acc' = last e'

-- strict version of scanl
scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f x ys = x : case ys of
    []   -> []
    y:ys -> let z = f x y in z `seq` scanl' f z ys

collect  = E . fmap collect1 . unE
    where
    collect1 [] = []
    collect1 e  = [e] -- TODO: This has to be strict in the spine of the list!

-- behaviors
instance Functor (Behavior t) where
    fmap f = B . fmap f . unB

instance Applicative (Behavior t) where
    pure      = B . pure
    bf <*> bx = B $ zipWith ($) (unB bf) (unB bx)

stepper x = B . scanl go x . unE
    where go x es = last (x:es)

-- apply
apply b e = E $ zipWith map (unB b) (unE e)



type Time = Integer

-- | Interpreter that corresponds to your mental model.
run :: (forall t. Event t a -> Event t b) -> [(Time,[a])] -> [(Time,[b])]
run f input = zip times (f' events) 
    where
    (times,events) = unzip input
    f' = unE . f . E

-- | Slightly simpler interpreter that does not mention 'Time'.
-- Returns lists of event values that occur simultaneously.
interpret :: (Event Model a -> Event Model b) -> [a] -> [[b]]
interpret f = unE . f . E . map (:[])

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

