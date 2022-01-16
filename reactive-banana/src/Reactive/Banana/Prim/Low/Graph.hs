{-----------------------------------------------------------------------------
    reactive-banana

    Implementation of graph-related functionality
------------------------------------------------------------------------------}
{-# language ScopedTypeVariables#-}

module Reactive.Banana.Prim.Low.Graph
  ( Graph
  , emptyGraph
  , insertEdge
  , getChildren
  , getParents
  , listParents
  , reversePostOrder
  ) where

import           Data.Functor.Identity
import           Data.Hashable
import qualified Data.HashMap.Strict   as Map
import qualified Data.HashSet          as Set
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

-- | Get all immediate parents of a node in a graph.
getParents :: (Eq a, Hashable a) => Graph a -> a -> [a]
getParents gr x = fromMaybe [] . Map.lookup x . parents $ gr

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
    list = runIdentity $ reversePostOrder' ancestors (Identity . getChildren gr)

{-----------------------------------------------------------------------------
    Graph traversal
------------------------------------------------------------------------------}
-- | Graph represented as map of immediate children.
type GraphM m a = a -> m [a]

-- | Computes the reverse post-order,
-- listing all transitive children of a node.
-- Each node is listed *before* all its children have been listed.
reversePostOrder :: (Eq a, Hashable a, Monad m) => a -> GraphM m a -> m [a]
reversePostOrder x = reversePostOrder' [x]

-- | Reverse post-order from multiple nodes.
-- INVARIANT: For this to be a valid topological order,
-- none of the nodes may have a parent.
reversePostOrder' :: (Eq a, Hashable a, Monad m) => [a] -> GraphM m a -> m [a]
reversePostOrder' xs children = fst <$> go xs [] Set.empty
    where
    go []     rpo visited        = return (rpo, visited)
    go (x:xs) rpo visited
        | x `Set.member` visited = go xs rpo visited
        | otherwise              = do
            xs' <- children x
            -- visit all children
            (rpo', visited') <- go xs' rpo (Set.insert x visited)
            -- prepend this node as all children have been visited
            go xs (x:rpo') visited'
