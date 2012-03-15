{-----------------------------------------------------------------------------
    Reactive Banana
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types, BangPatterns #-}
module Reactive.Banana.Internal.TotalOrder (
    -- * Synopsis
    -- | Data structure that represents a total order.
    
    -- * TotalOrder
    TotalOrder, TotalOrderZipper,
    open, close, fromAscList, ascend, descend, insertBeforeFocus, delete,
    withTotalOrder,
    
    -- * Queue
    Queue(..), insertList, isEmpty,
    ) where


import Control.Applicative
import Control.Arrow (second)

import qualified Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Ord

{-----------------------------------------------------------------------------
    Total Order implementation
------------------------------------------------------------------------------}
-- Data type representing a total order between elements
-- It's simply an ordered list of elements
newtype TotalOrder a = TO { unTO :: Map.Map a Int }

-- Zipper variant of a total order.
data TotalOrderZipper a = TOZ { down :: [a], up :: [a] }

open  :: TotalOrder a -> TotalOrderZipper a
open (TO order) = TOZ { down = [], up = Map.keys order }

close :: Ord a => TotalOrderZipper a -> TotalOrder a
close order = TO $ Map.fromList $ zip (reverse (down order) ++ up order) [1..]

fromAscList :: Ord a => [a] -> TotalOrder a
fromAscList xs = close $ TOZ { down = [], up = xs }


-- move to the next larger element
ascend       :: TotalOrderZipper a -> TotalOrderZipper a
ascend (TOZ xs []    ) = TOZ xs     []
ascend (TOZ xs (y:ys)) = TOZ (y:xs) ys

-- move to the next smaller element
descend      :: TotalOrderZipper a -> TotalOrderZipper a
descend (TOZ []     ys) = TOZ [] ys
descend (TOZ (x:xs) ys) = TOZ xs (x:ys)

-- insert an element before the current one
insertBeforeFocus :: a -> TotalOrderZipper a -> TotalOrderZipper a
insertBeforeFocus x (TOZ xs ys) = TOZ (x:xs) ys

-- delete an element from a total order
delete       :: Ord a => a -> TotalOrderZipper a -> TotalOrderZipper a
delete x (TOZ xs ys) = TOZ (delete' x xs) (delete' x ys)
    where delete' = Data.List.delete

{-----------------------------------------------------------------------------
   Queue based on a total order
------------------------------------------------------------------------------}
-- | Obtain a queue based on a particular total order.
--
-- The type system ensures that the queue is only used temporarily.
-- The argument passed to the function is the empty queue.
withTotalOrder :: TotalOrder a -> (forall q. Queue q => q a -> b) -> b
withTotalOrder order f = f empty
    where empty = Q { order = order, queue = Set.empty }

-- public interface
class Queue q where
    insert  :: Ord a => a -> q a -> q a
    minView :: q a -> Maybe (a, q a)
    size    :: q a -> Int

-- | Check whether a queue is empty.
isEmpty :: Queue q => q a -> Bool
isEmpty = isNothing . minView

-- | Insert a collection of elements
insertList :: (Queue q, Ord a) => [a] -> q a -> q a
insertList xs q = foldl (flip insert) q xs

-- concrete implementation
data MyQueue a = Q { order :: TotalOrder a, queue :: Set.Set (Pair Int a) }

data Pair a b = Pair !a b
fstPair (Pair a _) = a
instance Eq a => Eq (Pair a b) where
    x == y = fstPair x == fstPair y
instance Ord a => Ord (Pair a b) where
    compare = comparing fstPair

-- set the queue field
setQueue :: MyQueue a -> Set.Set (Pair Int a) -> MyQueue a
setQueue q b = q { queue = b }

-- find the index of a particular element in a Total Order
position :: Ord a => TotalOrder a -> a -> Int
position (TO order) x = pos
    where Just pos = Map.lookup x order

instance Queue MyQueue where
    insert x q = q { queue = Set.insert (Pair pos x) (queue q) }
        where pos = position (order q) x
    minView  q = f <$> Set.minView (queue q)
        where f (Pair _ a,set) = (a, setQueue q set)
    size     q = Set.size (queue q)


