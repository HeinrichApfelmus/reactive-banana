{-----------------------------------------------------------------------------
    reactive-banana
    
    Implementation of a bag whose elements are ordered by arrival time.
------------------------------------------------------------------------------}
{-# LANGUAGE TupleSections #-}
module Reactive.Banana.Prim.OrderedBag where

import           Data.Functor
import qualified Data.HashMap.Strict as Map
import           Data.Hashable
import           Data.List
import           Data.Maybe
import           Data.Ord

{-----------------------------------------------------------------------------
    Ordered Bag
------------------------------------------------------------------------------}
type Position = Integer

data OrderedBag a = OB (Map.HashMap a Position) !Position

empty :: OrderedBag a
empty = OB Map.empty 0

-- | Add an element to an ordered bag after all the others.
-- Does nothing if the element is already in the bag.
insert :: (Eq a, Hashable a) => a -> OrderedBag a -> OrderedBag a
insert x (OB xs n) = OB (Map.insertWith (\new old -> old) x n xs) (n+1)

-- | Reorder a list of elements to appear as they were inserted into the bag.
-- Remove any elements from the list that do not appear in the bag.
inOrder :: (Eq a, Hashable a) => [(a,b)] -> OrderedBag a -> [(a,b)]
inOrder xs (OB bag _) = map snd $ sortBy (comparing fst) $
    mapMaybe (\x -> (,x) <$> Map.lookup (fst x) bag) xs
