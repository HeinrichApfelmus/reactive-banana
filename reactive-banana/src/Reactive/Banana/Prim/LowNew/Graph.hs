{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.LowNew.Graph
    ( Graph
    , empty
    , getOutgoing
    , getIncoming
    , size
    , listConnectedVertices

    , deleteVertex
    , insertEdge
    , deleteEdge
    , clearPredecessors
    , collectGarbage

    , topologicalSort
    ) where

import Data.Functor.Identity
    ( Identity (..) )
import Data.Hashable
    ( Hashable )
import Data.Maybe
    ( fromMaybe )
import Reactive.Banana.Prim.LowNew.GraphTraversal
    ( reversePostOrder )

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

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
      outgoing :: Map.HashMap v [(e,v)]

      -- | Mapping from each vertex to its direct predecessors
      -- (possibly empty).
    , incoming :: Map.HashMap v [(v,e)]
    } deriving (Eq, Show)

-- | The graph with no edges.
empty :: Graph v e
empty = Graph
    { outgoing = Map.empty
    , incoming = Map.empty
    }

-- | Get all direct successors of a vertex in a 'Graph'.
getOutgoing :: (Eq v, Hashable v) => Graph v e -> v -> [(e,v)]
getOutgoing Graph{outgoing} x = fromMaybe [] $ Map.lookup x outgoing

-- | Get all direct predecessors of a vertex in a 'Graph'.
getIncoming :: (Eq v, Hashable v) => Graph v e -> v -> [(v,e)]
getIncoming Graph{incoming} x = fromMaybe [] $ Map.lookup x incoming

-- | List all connected vertices,
-- i.e. vertices on which at least one edge is incident.
listConnectedVertices :: (Eq v, Hashable v) => Graph v e -> [v]
listConnectedVertices Graph{incoming,outgoing} = 
    Map.keys $ (() <$ outgoing) `Map.union` (() <$ incoming)

-- | Number of connected vertices,
-- i.e. vertices on which at least one edge is incident.
size :: (Eq v, Hashable v) => Graph v e -> Int
size Graph{incoming,outgoing} =
    Map.size $ (() <$ outgoing) `Map.union` (() <$ incoming)

{-----------------------------------------------------------------------------
    Insertion
------------------------------------------------------------------------------}
-- | Insert an edge from the first to the second vertex into the 'Graph'.
insertEdge :: (Eq v, Hashable v) => (v,v) -> e -> Graph v e -> Graph v e
insertEdge (x,y) exy Graph{..} = Graph
    { outgoing
        = Map.insertWith (\new old -> new <> old) x [(exy,y)]
        $ insertDefaultIfNotMember y []
        $ outgoing
    , incoming
        = Map.insertWith (\new old -> new <> old) y [(x,exy)]
        . insertDefaultIfNotMember x []
        $ incoming
    }

-- Helper function: Insert a default value if the key is not a member yet
insertDefaultIfNotMember
    :: (Eq k, Hashable k)
    => k -> a -> Map.HashMap k a -> Map.HashMap k a
insertDefaultIfNotMember x def = Map.insertWith (\_ old -> old) x def

{-----------------------------------------------------------------------------
    Deletion
------------------------------------------------------------------------------}
-- | TODO: Not implemented.
deleteEdge :: (Eq v, Hashable v) => (v,v) -> Graph v e -> Graph v e
deleteEdge (x,y) g = Graph
    { outgoing = undefined x g
    , incoming = undefined y g
    }

-- | Remove all edges incident on this vertex from the 'Graph'.
deleteVertex :: (Eq v, Hashable v) => v -> Graph v e -> Graph v e
deleteVertex x = clearPredecessors x . clearSuccessors x

-- | Remove all the edges that connect the given vertex to its predecessors.
clearPredecessors :: (Eq v, Hashable v) => v -> Graph v e -> Graph v e
clearPredecessors x g@Graph{..} = Graph
    { outgoing = foldr ($) outgoing
        [ Map.adjust (delete x) z | (z,_) <- getIncoming g x ]
    , incoming = Map.delete x incoming
    }
  where
    delete a = filter $ (a /=) . snd

-- | Remove all the edges that connect the given vertex to its successors.
clearSuccessors :: (Eq v, Hashable v) => v -> Graph v e -> Graph v e
clearSuccessors x g@Graph{..} = Graph
    { outgoing = Map.delete x outgoing
    , incoming = foldr ($) incoming
        [ Map.adjust (delete x) z | (_,z) <- getOutgoing g x ]
    }
  where
    delete a = filter $ (a /=) . fst

-- | Apply `deleteVertex` to all vertices which are not predecessors
-- of any of the vertices in the given list.
collectGarbage :: (Eq v, Hashable v) => [v] -> Graph v e -> Graph v e
collectGarbage roots g@Graph{incoming,outgoing} = Graph
    { incoming = Map.filterWithKey (\v _ -> isReachable v) incoming
        -- incoming edges of reachable members are reachable by definition
    , outgoing
        = Map.map (filter (isReachable . snd))
        $ Map.filterWithKey (\v _ -> isReachable v) outgoing
    }
  where
    isReachable x = x `Set.member` reachables
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
topologicalSort :: (Eq v, Hashable v) => Graph v e -> [v]
topologicalSort g@Graph{incoming} =
    runIdentity $ reversePostOrder roots (Identity . map snd . getOutgoing g)
  where
    -- all vertices that have no (direct) predecessors
    roots = [ x | (x,preds) <- Map.toList incoming, null preds ]

