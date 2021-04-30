{-----------------------------------------------------------------------------
    reactive-banana

    Implementation of graph-related functionality
------------------------------------------------------------------------------}
{-# language ScopedTypeVariables#-}

module Reactive.Banana.Prim.Graph where

import           Control.Monad
import           Data.Functor.Identity
import qualified Data.HashMap.Strict   as Map
import qualified Data.HashSet          as Set
import           Data.Hashable
import           Data.Maybe

{-----------------------------------------------------------------------------
    Graphs and topological sorting
------------------------------------------------------------------------------}
data Graph a = Graph
    { children :: Map.HashMap a [a]
    , parents  :: Map.HashMap a [a]
    , nodes    :: Set.HashSet a
    }

-- | The graph with no edges and no nodes.
emptyGraph :: Graph a
emptyGraph = Graph Map.empty Map.empty Set.empty

-- | Insert an edge from the first node to the second node into the graph.
insertEdge :: (Eq a, Hashable a) => (a,a) -> Graph a -> Graph a
insertEdge (x,y) gr = gr
    { children = Map.insertWith (++) x [y] (children gr)
    , parents  = Map.insertWith (++) y [x] (parents  gr)
    , nodes    = Set.insert x $ Set.insert y $ nodes gr
    }

-- | Get all immediate children of a node in a graph.
getChildren :: (Eq a, Hashable a) => Graph a -> a -> [a]
getChildren gr x = maybe [] id . Map.lookup x . children $ gr

-- | Get all immediate parents of a node in a graph.
getParents :: (Eq a, Hashable a) => Graph a -> a -> [a]
getParents gr x = maybe [] id . Map.lookup x . parents $ gr

-- | List all nodes such that each parent is listed before all of its children.
listParents :: forall a. (Eq a, Hashable a) => Graph a -> [a]
listParents gr = list
    where
    -- all nodes without children
    ancestors :: [a]
    ancestors = [x | x <- Set.toList $ nodes gr, null (getParents gr x)]
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
dfs' xs succs = liftM fst $ go xs [] Set.empty
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
