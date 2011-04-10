{-----------------------------------------------------------------------------
    Reactive Banana
    
    Class interface + Semantic model
------------------------------------------------------------------------------}
{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, EmptyDataDecls #-}
module Reactive.Banana.Model where

import Control.Applicative
import qualified Data.List
import Prelude hiding (filter)
import Data.Monoid

{-----------------------------------------------------------------------------
    Class interface
------------------------------------------------------------------------------}
data family Event f    :: * -> *
data family Behavior f :: * -> *

class (Functor (Event f),
       Functor (Behavior f), Applicative (Behavior f)) => FRP f where
    
    never    :: Event f a
    union    :: Event f a -> Event f a -> Event f a
    filter   :: Behavior f (a -> Bool) -> Event f a -> Event f a
    apply    :: Behavior f (a -> b) -> Event f a -> Event f b
    
    -- accumulation.
    -- minimal complete definition: either  accumB  or  mapAccum
    accumB   :: a -> Event f (a -> a) -> Behavior f a
    accumE   :: a -> Event f (a -> a) -> Event f a    
    mapAccum :: acc -> Event f (acc -> (x,acc)) -> (Event f x, Behavior f acc)
    
    accumB acc = snd . mapAccum acc . fmap ((\acc -> (undefined,acc)) .)
    accumE acc = fst . mapAccum acc . fmap ((\acc -> (acc,acc)) .)
    mapAccum acc ef = (ex,bacc)
        where
        ex   = apply ((\acc f -> fst (f acc)) <$> bacc) ef
        bacc = accumB acc (fmap (snd .) ef)
   
instance FRP f => Monoid (Event f a) where
    mempty  = never
    mappend = union

{-----------------------------------------------------------------------------
    Derived Combinators
------------------------------------------------------------------------------}
whenE :: FRP f => Behavior f Bool -> Event f a -> Event f a
whenE bf = filter (const <$> bf)

{-----------------------------------------------------------------------------
    Semantic model
------------------------------------------------------------------------------}
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
    
    filter bp = E . zipWith (\p xs-> Data.List.filter p xs) (unB bp) . unE
    apply b   = E . zipWith (\f xs -> map f xs) (unB b) . unE

    accumB acc = B . accumB' acc . unE
        where
        accumB' z es = z : case es of
            []   -> []
            e:es -> let z' = concatenate e z in accumB' z' es
        concatenate = foldl (.) id

-- interpreter
run :: (Event Model a -> Event Model b) -> [a] -> [[b]]
run f = unE . f . E . map (:[])

type Time = Double
interpret :: (Event Model a -> Event Model b) -> [(Time,a)] -> [(Time,b)]
interpret f xs =
    concat . zipWith tag times . run f . map snd $ xs
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

testModel = run example $ replicate 15 ()
-- > testModel
-- [[10],[9],[8],[7],[6],[5],[4],[3],[2],[1],[],[],[],[],[]]

example2 :: FRP f => Event f () -> Event f Int
example2 e = apply (const <$> b) e
    where
    b = accumB 0 ((+1) <$ e)

