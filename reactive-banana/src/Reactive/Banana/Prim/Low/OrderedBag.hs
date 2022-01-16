{-----------------------------------------------------------------------------
    reactive-banana

    Implementation of a bag whose elements are ordered by arrival time.
------------------------------------------------------------------------------}
{-# LANGUAGE TupleSections #-}
module Reactive.Banana.Prim.Low.OrderedBag where

import qualified Data.HashMap.Strict as Map
import           Data.Hashable
import           Data.List ( foldl', sortBy )
import           Data.Maybe
import           Data.Ord

{-----------------------------------------------------------------------------
    Ordered Bag
------------------------------------------------------------------------------}
type Position = Integer

data OrderedBag a = OB !(Map.HashMap a Position) !Position

empty :: OrderedBag a
empty = OB Map.empty 0

-- | Add an element to an ordered bag after all the others.
-- Does nothing if the element is already in the bag.
insert :: (Eq a, Hashable a) => OrderedBag a -> a -> OrderedBag a
insert (OB xs n) x = OB (Map.insertWith (\_new old -> old) x n xs) (n+1)

-- | Add a sequence of elements to an ordered bag.
--
-- The ordering is left-to-right. For example, the head of the sequence
-- comes after all elements in the bag,
-- but before the other elements in the sequence.
inserts :: (Eq a, Hashable a) => OrderedBag a -> [a] -> OrderedBag a
inserts = foldl' insert

-- | Reorder a list of elements to appear as they were inserted into the bag.
-- Remove any elements from the list that do not appear in the bag.
inOrder :: (Eq a, Hashable a) => [(a,b)] -> OrderedBag a -> [(a,b)]
inOrder xs (OB bag _) = map snd $ sortBy (comparing fst) $
    mapMaybe (\x -> (,x) <$> Map.lookup (fst x) bag) xs
