{-# language BangPatterns #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.Low.Graph
    ( Graph
    , empty
    , getOutgoing
    , getIncoming
    , size
    , edgeCount
    , listConnectedVertices

    , deleteVertex
    , insertEdge
    , deleteEdge
    , clearPredecessors
    , collectGarbage

    , topologicalSort
    , Step (..)
    , walkSuccessors
    , walkSuccessors_

    -- * Internal
    , Level
    , getLevel

    -- * Debugging
    , showDot
    ) where

import Data.Functor.Identity
    ( Identity (..) )
import Data.Hashable
    ( Hashable )
import Data.Maybe
    ( fromMaybe )
import Reactive.Banana.Prim.Low.GraphTraversal
    ( reversePostOrder )
import Reactive.Banana.Prim.Low.HasUnique
    ( HasUnique )

import qualified Data.List as L
import qualified Reactive.Banana.Prim.Low.UniqueMap as Map
import qualified Reactive.Banana.Prim.Low.UniqueSet as Set
import qualified Data.PQueue.Prio.Min as Q

type Queue = Q.MinPQueue
type Map = Map.UniqueMap
type Set = Set.UniqueSet

{-----------------------------------------------------------------------------
    Levels
------------------------------------------------------------------------------}
-- | 'Level's are used to keep track of the order of vertices —
-- Lower levels come first.
type Level = Int

ground :: Level
ground = 0

{-----------------------------------------------------------------------------
    Graph
------------------------------------------------------------------------------}
{- | A directed graph
whose set of vertices is the set of all values of the type @v@
and whose edges are associated with data of type @e@.

Note that a 'Graph' does not have a notion of vertex membership
— by design, /all/ values of the type @v@ are vertices of the 'Graph'.
The main purpose of 'Graph' is to keep track of directed edges between
vertices; a vertex with at least one edge incident on it is called
a /connected vertex/.
For efficiency, only the connected vertices are stored.
-}
data Graph v e = Graph
    { -- | Mapping from each vertex to its direct successors
      -- (possibly empty).
      outgoing :: !(Map v (Map v e))

      -- | Mapping from each vertex to its direct predecessors
      -- (possibly empty).
    , incoming :: !(Map v (Map v e))

      -- | Mapping from each vertex to its 'Level'.
      -- Invariant: If x precedes y, then x has a lower level than y.
    , levels :: !(Map v Level)
    } deriving (Eq, Show)

-- | The graph with no edges.
empty :: Graph v e
empty = Graph
    { outgoing = Map.empty
    , incoming = Map.empty
    , levels = Map.empty
    }

-- | Get all direct successors of a vertex in a 'Graph'.
getOutgoing :: HasUnique v => Graph v e -> v -> [(e,v)]
getOutgoing Graph{outgoing} x =
    map shuffle $ Map.toList $ fromMaybe Map.empty $ Map.lookup x outgoing
  where
      shuffle (x,y) = (y,x)

-- | Like 'getOutgoing', but returns only the vertices.
getOutgoingVertices :: HasUnique v => Graph v e -> v -> [v]
getOutgoingVertices Graph{outgoing} x =
  maybe [] Map.keys (Map.lookup x outgoing)

-- | Get all direct predecessors of a vertex in a 'Graph'.
getIncoming :: HasUnique v => Graph v e -> v -> [(v,e)]
getIncoming Graph{incoming} x =
    Map.toList $ fromMaybe Map.empty $ Map.lookup x incoming

-- | Get the 'Level' of a vertex in a 'Graph'.
getLevel :: HasUnique v => Graph v e -> v -> Level
getLevel Graph{levels} x = fromMaybe ground $ Map.lookup x levels

-- | List all connected vertices,
-- i.e. vertices on which at least one edge is incident.
listConnectedVertices :: HasUnique v => Graph v e -> [v]
listConnectedVertices Graph{incoming,outgoing} =
    -- value combining function doesn't matter since we only care about keys
    Map.keys $ Map.union (\_ _ -> id) outgoing incoming

-- | Number of connected vertices,
-- i.e. vertices on which at least one edge is incident.
size :: HasUnique v => Graph v e -> Int
size Graph{incoming,outgoing} =
    Map.size $ Map.union (\_ _ -> id) outgoing incoming

-- | Number of edges.
edgeCount :: (Eq v, Hashable v) => Graph v e -> Int
edgeCount Graph{incoming,outgoing} =
    (count incoming + count outgoing) `div` 2
  where
    count = Map.foldl' (\a v -> Map.size v + a) 0

{-----------------------------------------------------------------------------
    Insertion
------------------------------------------------------------------------------}
-- | Insert an edge from the first to the second vertex into the 'Graph'.
insertEdge :: HasUnique v => (v,v) -> e -> Graph v e -> Graph v e
insertEdge (x,y) exy g0@Graph{..} = Graph
    { outgoing
        = Map.upsert x (Map.singleton y exy) (Map.insert y exy)
        $ insertDefaultIfNotMember y Map.empty
        $ outgoing
    , incoming
        = Map.upsert y (Map.singleton x exy) (Map.insert x exy)
        . insertDefaultIfNotMember x Map.empty
        $ incoming
    , levels
        = adjustLevels
        $ levels0
    }
  where
    getLevel z = fromMaybe ground . Map.lookup z
    levels0
        = insertDefaultIfNotMember x (ground-1)
        . insertDefaultIfNotMember y ground
        $ levels

    levelDifference = getLevel y levels0 - 1 - getLevel x levels0
    adjustLevel g x = Map.adjust x (+ levelDifference) g
    adjustLevels ls
        | levelDifference >= 0 = ls
        | otherwise            = L.foldl' adjustLevel ls predecessors
      where
        Identity predecessors =
            reversePostOrder [x] (Identity . map fst . getIncoming g0)

-- Helper function: Insert a default value if the key is not a member yet
insertDefaultIfNotMember
    :: HasUnique k
    => k -> a -> Map k a -> Map k a
insertDefaultIfNotMember x def = Map.upsert x def id

{-----------------------------------------------------------------------------
    Deletion
------------------------------------------------------------------------------}
-- | TODO: Not implemented.
deleteEdge :: (Eq v, Hashable v) => (v,v) -> Graph v e -> Graph v e
deleteEdge (x,y) g = Graph
    { outgoing = undefined x g
    , incoming = undefined y g
    , levels = undefined
    }

-- | Remove all edges incident on this vertex from the 'Graph'.
deleteVertex :: HasUnique v => v -> Graph v e -> Graph v e
deleteVertex x = clearLevels . clearPredecessors x . clearSuccessors x
  where
    clearLevels g@Graph{levels} = g{levels = Map.delete x levels}

-- | Remove all the edges that connect the given vertex to its predecessors.
clearPredecessors :: HasUnique v => v -> Graph v e -> Graph v e
clearPredecessors x g@Graph{..} = g
    { outgoing = foldr ($) outgoing
        [ Map.adjust z (Map.delete x) | (z,_) <- getIncoming g x ]
    , incoming = Map.delete x incoming
    }

-- | Remove all the edges that connect the given vertex to its successors.
clearSuccessors :: HasUnique v => v -> Graph v e -> Graph v e
clearSuccessors x g@Graph{..} = g
    { outgoing = Map.delete x outgoing
    , incoming = foldr ($) incoming
        [ Map.adjust z (Map.delete x) | z <- getOutgoingVertices g x ]
    }

-- | Apply `deleteVertex` to all vertices which are not predecessors
-- of any of the vertices in the given list.
collectGarbage :: HasUnique v => [v] -> Graph v e -> Graph v e
collectGarbage roots g@Graph{incoming,outgoing} = g
    { incoming = Map.restrictKeys reachables incoming
        -- incoming edges of reachable members are reachable by definition
    , outgoing
        = Map.map (Map.restrictKeys reachables)
        $ Map.restrictKeys reachables outgoing
    }
  where
    reachables
        = Set.fromList . runIdentity
        $ reversePostOrder roots
        $ Identity . map fst . getIncoming g

{-----------------------------------------------------------------------------
    Topological sort
------------------------------------------------------------------------------}
-- | If the 'Graph' is acyclic, return a topological sort,
-- that is a linear ordering of its connected vertices such that
-- each vertex occurs before its successors.
--
-- (Vertices that are not connected are not listed in the topological sort.)
--
-- https://en.wikipedia.org/wiki/Topological_sorting
topologicalSort :: HasUnique v => Graph v e -> [v]
topologicalSort g@Graph{incoming} =
    runIdentity $ reversePostOrder roots (Identity . getOutgoingVertices g)
  where
    -- all vertices that have no (direct) predecessors
    roots = [ x | (x,preds) <- Map.toList incoming, Map.isEmpty preds ]

data Step = Next | Stop

-- | Starting from a list of vertices without predecessors,
-- walk through all successors, but in such a way that every vertex
-- is visited before its predecessors.
-- For every vertex, if the function returns `Next`, then
-- the successors are visited, otherwise the walk at the vertex
-- stops prematurely.
--
-- > topologicalSort g =
-- >     runIdentity $ walkSuccessors (roots g) (pure Next) g
--
walkSuccessors
    :: forall v e m. (Monad m, HasUnique v)
    => [v] -> (v -> m Step) -> Graph v e -> m [v]
walkSuccessors xs step g = go (Q.fromList $ zipLevels xs) Set.empty []
  where
    zipLevels vs = [(getLevel g v, v) | v <- vs]

    insertList :: Queue Level v -> [v] -> Queue Level v
    insertList = L.foldl' (\q v -> Q.insert (getLevel g v) v q)

    go :: Queue Level v -> Set v -> [v] -> m [v]
    go q0 seen visits = case Q.minView q0 of
        Nothing -> pure $ reverse visits
        Just (v,q1)
            | v `Set.member` seen -> go q1 seen visits
            | otherwise -> do
                next <- step v
                let q2 = case next of
                      Stop -> q1
                      Next -> insertList q1 (getOutgoingVertices g v)
                go q2 (Set.insert v seen) (v:visits)


walkSuccessors_
    :: (Monad m, HasUnique v)
    => [v] -> (v -> m Step) -> Graph v e -> m ()
walkSuccessors_ xs step g = walkSuccessors xs step g >> pure ()

{-----------------------------------------------------------------------------
    Debugging
------------------------------------------------------------------------------}
-- | Map to a string in @graphviz@ dot file format.
showDot
    :: HasUnique v
    => (v -> String) -> Graph v e -> String
showDot fv g = unlines $
    [ "digraph mygraph {"
    , "  node [shape=box];"
    ] <> map showVertex (listConnectedVertices g)
    <> ["}"]
  where
    showVertex x =
        concat [ "  " <> showEdge x y <> "; " | y <- getOutgoingVertices g x ]
    showEdge x y = escape x <> " -> " <> escape y
    escape = show . fv
