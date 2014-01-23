{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types, BangPatterns, RecordWildCards #-}
module Reactive.Banana.Prim.Order (
    -- * Synopsis
    -- | Data structure that represents a partial ordering by levels.
    
    -- * Order
    Order, flat,
    ensureAbove, recalculateParent,
    
    -- * Queue
    Queue(..), withOrder, insertList,
    ) where

import Data.Functor
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import           Data.Hashable
import qualified Data.IntMap.Strict  as IntMap

type IntMap = IntMap.IntMap
type Map    = Map.HashMap
type Set    = Set.HashSet

{-----------------------------------------------------------------------------
    Order by levels
------------------------------------------------------------------------------}
-- | Each element is assigned a /level/.
-- Elements in lower levels come before elements in higher levels.
-- There is no order on elements within the same level.
type Order a = Map a Level

-- | FIXME: Level should be an 'Integer' to avoid overflow.
--
-- FIXME: The algorithms in this module currently do not try to
-- shrink the number or width of levels.
type Level   = Int

-- | The flat order where every element is at 'ground' level.
flat :: Order a
flat = Map.empty

-- | Ground level.
ground :: Level
ground = 0

-- | Look up the level of an element. Default level is 'ground'.
level :: (Eq a, Hashable a) => a -> Order a -> Level
level x = maybe ground id . Map.lookup x

-- | Make sure that the first argument is at least one level
-- above the second argument.
ensureAbove :: (Eq a, Hashable a) => a -> a -> Order a -> Order a
ensureAbove child parent order =
    Map.insertWith max child (level parent order + 1) order

-- | Reassign the parent for a child and recalculate the levels
-- for the new parents and grandparents.
recalculateParent :: (Eq a, Hashable a)
    => a       -- Child.
    -> a       -- Parent.
    -> Graph a -- Query parents of a node. 
    -> Order a -> Order a
recalculateParent child parent parents order
    | d <= 0    = order
    | otherwise = concatenate
        [ Map.insertWith (+) node (-d) | node <- dfs parent parents ]
        order
    where
    d = level parent order - level child order + 1
    -- level parent - d = level child - 1
    concatenate = foldr (.) id

{-----------------------------------------------------------------------------
    Graph traversal
------------------------------------------------------------------------------}
-- | Graph represented as map of successors.
type Graph a = a -> [a]

-- | Depth-first search. List all transitive successors of a node.
dfs :: (Eq a, Hashable a) => a -> Graph a -> [a]
dfs x succs = go [x] Set.empty
    where
    go []     _               = []
    go (x:xs) seen
        | x `Set.member` seen = go xs seen
        | otherwise           = x : go (ys ++ xs) (Set.insert x seen)
        where
        ys = succs x

{-----------------------------------------------------------------------------
    Queue
------------------------------------------------------------------------------}
-- | Public API for a queue.
class Queue q where
    insert  :: (Hashable a, Eq a) => a -> q a -> q a
    minView :: (Hashable a, Eq a) => q a -> Maybe (a, q a)

-- | Insert a collection of elements into a 'Queue'.
insertList :: (Queue q, Hashable a, Eq a) => [a] -> q a -> q a
insertList xs q = {-# SCC insertList #-} foldl (flip insert) q xs

-- | Obtain a queue based on a particular 'Order'.
--
-- The type system ensures that the queue is only used temporarily.
-- The argument passed to the function is the empty queue.
withOrder :: Order a -> (forall q. Queue q => q a -> b) -> b
withOrder order f = f empty
    where empty = Q { order = order, queue = IntMap.empty }

-- | Concrete queue implementation.
data Q a = Q
    { order :: Order  a
    , queue :: IntMap (Set a)   -- inveriant: all sets have size >= 1
    } deriving (Eq, Show)

instance Queue Q where
    insert  = insertQ
    minView = minViewQ

insertQ a q@(Q{..}) =
    {-# SCC insertQ #-} q { queue = IntMap.alter (add a) (level a order) queue }
    where
    add x Nothing   = Just $ Set.singleton a
    add x (Just xs) = Just $ Set.insert x xs

minViewQ q@(Q{..}) =
    {-# SCC minViewQ #-} mkQ <$> case IntMap.minViewWithKey queue of
        Nothing                   -> Nothing
        Just ((level, xs), queue) -> case Set.toList xs of
            [x]    -> Just (x,queue)
            (x:_)  -> Just (x,IntMap.insert level (Set.delete x xs) queue)
  where
  mkQ (a,q) = (a, Q order q)

