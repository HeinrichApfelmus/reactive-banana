{-----------------------------------------------------------------------------
    reactive-banana

    Implementation of graph-related functionality
------------------------------------------------------------------------------}
{-# language ScopedTypeVariables#-}

module Reactive.Banana.Prim.Graph
  ( Graph
  , emptyGraph
  , insertEdge
  , getChildren
  , listParents
  , dfs
  ) where

import           Data.Functor.Identity
import qualified Data.HashMap.Strict   as Map
import qualified Data.HashSet          as Set
import           Data.Hashable
import           Data.Maybe

{-----------------------------------------------------------------------------
    Graphs and topological sorting
------------------------------------------------------------------------------}
data Graph a = Graph
    { -- | The mapping from each node to the set of nodes reachable by an out-edge. If a node has no out-edges, it is
      -- not a member of this map.
      --
      -- Invariant: the values are non-empty lists.
      children :: Map.HashMap a [a]
      -- | The Mapping from each node to the set of nodes reachable by an in-edge. If a node has no in-edges, it is not
      -- a member of this map.
      --
      -- Invariant: the values are non-empty lists.
    , parents  :: Map.HashMap a [a]
      -- | The set of nodes.
      --
      -- Invariant: equals (key children `union` keys parents)
    , nodes    :: Set.HashSet a
    }

-- | The graph with no edges and no nodes.
emptyGraph :: Graph a
emptyGraph = Graph Map.empty Map.empty Set.empty

-- | Insert an edge from the first node to the second node into the graph.
insertEdge :: (Eq a, Hashable a) => (a,a) -> Graph a -> Graph a
insertEdge (x,y) gr = gr
    { children = Map.insertWith (\new old -> new ++ old) x [y] (children gr)
    , parents  = Map.insertWith (\new old -> new ++ old) y [x] (parents  gr)
    , nodes    = Set.insert x $ Set.insert y $ nodes gr
    }

-- | Get all immediate children of a node in a graph.
getChildren :: (Eq a, Hashable a) => Graph a -> a -> [a]
getChildren gr x = fromMaybe [] . Map.lookup x . children $ gr

-- | List all nodes such that each parent is listed before all of its children.
listParents :: forall a. (Eq a, Hashable a) => Graph a -> [a]
listParents gr = list
    where
    -- all nodes without parents
    ancestors :: [a]
    -- We can filter from `children`, because a node without incoming edges can only be in the graph if it has outgoing edges.
    ancestors    = [x | x <- Map.keys (children gr), not (hasParents x)]
    hasParents x = Map.member x (parents gr)
    -- all nodes in topological order "parents before children"
    list :: [a]
    list = runIdentity $ dfs' ancestors (Identity . getChildren gr)

{-----------------------------------------------------------------------------
    Graph traversal
------------------------------------------------------------------------------}
-- | Graph represented as map of successors.
type GraphM m a = a -> m [a]

-- | Depth-first search. List all transitive successors of a node.
-- A node is listed *before* all its successors have been listed.
dfs :: (Eq a, Hashable a, Monad m) => a -> GraphM m a -> m [a]
dfs x = dfs' [x]

-- | Depth-first serach, refined version.
-- INVARIANT: None of the nodes in the initial list have a predecessor.
dfs' :: forall a m. (Eq a, Hashable a, Monad m) => [a] -> GraphM m a -> m [a]
dfs' xs succs = fst <$> go xs [] Set.empty
    where
    go :: [a] -> [a] -> Set.HashSet a -> m ([a], Set.HashSet a)
    go []     ys seen            = return (ys, seen)    -- all nodes seen
    go (x:xs) ys seen
        | x `Set.member` seen    = go xs ys seen
        | otherwise              = do
            xs' <- succs x
            -- visit all children
            (ys', seen') <- go xs' ys (Set.insert x seen)
            -- list this node as all successors have been seen
            go xs (x:ys') seen'
