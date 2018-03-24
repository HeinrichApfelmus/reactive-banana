{-----------------------------------------------------------------------------
    reactive-banana

    Implementation of graph-related functionality
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.Graph where

import           Control.Monad
import           Data.Functor.Identity
import           Data.Hashable
import qualified Data.HashMap.Strict   as Map
import qualified Data.HashSet          as Set
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
    { children = Map.insertWith (flip (++)) x [y] (children gr)
    , parents  = Map.insertWith (flip (++)) y [x] (parents  gr)
    , nodes    = Set.insert x $ Set.insert y $ nodes gr
    }

-- | Get all immediate children of a node in a graph.
getChildren :: (Eq a, Hashable a) => Graph a -> a -> [a]
getChildren gr x = fromMaybe [] . Map.lookup x . children $ gr

-- | Get all immediate parents of a node in a graph.
getParents :: (Eq a, Hashable a) => Graph a -> a -> [a]
getParents gr x = fromMaybe [] . Map.lookup x . parents $ gr

-- | List all nodes such that each parent is listed before all of its children.
listParents :: (Eq a, Hashable a) => Graph a -> [a]
listParents gr = list
    where
    -- all nodes without children
    ancestors = [x | x <- Set.toList $ nodes gr, null (getParents gr x)]
    -- all nodes in topological order "parents before children"
    list      = runIdentity $ reversePostOrder' ancestors (Identity . getChildren gr)

{-----------------------------------------------------------------------------
    Graph traversal
------------------------------------------------------------------------------}
-- | Graph represented as map of successors.
type GraphM m a = a -> m [a]

-- | Computes the reverse post-order by listing all transitive successors of a node.
-- A node is listed *before* all its successors have been listed.
reversePostOrder :: (Eq a, Hashable a, Monad m) => a -> GraphM m a -> m [a]
reversePostOrder seed = reversePostOrder' [seed]

-- | Reverse post-order from multiple seeds.
-- INVARIANT: For this to be a valid topological order, none of the seeds
-- may have a predecessor.
reversePostOrder' :: (Eq a, Hashable a, Monad m) => [a] -> GraphM m a -> m [a]
reversePostOrder' seeds successors = liftM fst $ go seeds [] Set.empty
    where
    go []           rpo visited     = return (rpo, visited)
    go (seed:seeds) rpo visited
        | seed `Set.member` visited = go seeds rpo visited
        | otherwise                 = do
            succs <- successors seed
            -- visit all successors
            (rpo', visited') <- go succs rpo (Set.insert seed visited)
            -- prepend this node as all successors have been visited
            go seeds (seed:rpo') visited'
