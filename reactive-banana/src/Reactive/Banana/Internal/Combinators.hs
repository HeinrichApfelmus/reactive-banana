{-----------------------------------------------------------------------------
    Reactive Banana
------------------------------------------------------------------------------}

module Reactive.Banana.Internal.Combinators where

import Data.List (filter)
import Reactive.Banana.Internal.AST as AST

{-----------------------------------------------------------------------------
    Observe sharing
------------------------------------------------------------------------------}
newtype Event t a    = E { unE :: AST.Event [a] }
newtype Behavior t a = B { unB :: AST.Behavior a }

{-----------------------------------------------------------------------------
    Full combinators from primitive combinators
------------------------------------------------------------------------------}
singleton x = [x]

-- ---- Events

instance Functor (Event t) where
    fmap f = fmap (AST.mapE f)

never       = singleton <$> never
union e1 e2 = unionWith (++) e1 e2
filterE p e = E . AST.filterE (not . null) . unE $ Data.List.filter p <$> e

collect e   = singleton <$> e   -- the result list is never empty
spill e     = concat    <$> e

unionWith f = AST.unionWith g
    where g xs ys = singleton $ foldr1 f (xs ++ ys)

accumE acc = mapAccumE . fmap concatenate
    where
    concatenate :: [a -> a] -> a -> ([a],a)
    concatenate fs acc = (tail values, last values)
        where values = scanl' (flip ($)) acc fs

    mapAccumE :: s -> Event (s -> (a,s)) -> Event a
    mapAccumE acc = fmap fst . AST.accumE (undefined,acc) . fmap (. snd)

-- ---- Behaviors

instance Functor (Behavior t) where
    fmap f = fmap f

instance Applicative (Behavior t) where
    pure  = AST.pureB
    (<*>) = AST.applyB

stepper x  = AST.stepper x . fmap last
accumB acc = stepper acc . accumE acc

apply b    = AST.applyE (map <$> b)

-- ---- Helpers

-- strict version of scanl
scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f x ys = x : case ys of
    []   -> []
    y:ys -> let z = f x y in z `seq` scanl' f z ys

