{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.LowNew.GraphGC
    ( GraphGC
    , listReachableVertices
    , new
    , insertEdge
    , removeGarbage
    ) where

import Data.IORef
    ( IORef, atomicModifyIORef, newIORef, readIORef )
import Data.Unique.Really
    ( Unique )
import Reactive.Banana.Prim.LowNew.Graph 
    ( Graph )
import Reactive.Banana.Prim.LowNew.Ref
    ( Ref, WeakRef )

import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as Map
import qualified Reactive.Banana.Prim.LowNew.Ref as Ref
import qualified Reactive.Banana.Prim.LowNew.Graph as Graph

type Map = Map.HashMap

{-----------------------------------------------------------------------------
    GraphGC
------------------------------------------------------------------------------}
type WeakEdge v = WeakRef v

-- Graph data
data GraphD v = GraphD
    { graph :: Graph Unique (WeakEdge v)
    , references :: Map Unique (WeakRef v)
    }

{- | A directed graph whose edges are mutable
    and whose vertices are subject to garbage collection.

    The vertices of the graph are mutable references of type 'Ref v'.
    

    Generally, the vertices of the graph are not necessarily kept reachable
    by the 'GraphGC' data structure
    — they need to be kept reachable by other parts of your program.

    That said, the edges in the graph do introduce additional reachability
    between vertices:
    Specifically, when an edge (x,y) is present in the graph,
    then the head @y@ will keep the tail @x@ reachable.
    (But the liveness of @y@ needs to come from elsewhere, e.g. another edge.)
    Use 'insertEdge' to insert an edge.

    Moreover, when a vertex is removed because it is no longer reachable,
    then all edges to and from that vertex will also be removed.
    In turn, this may cause further vertices and edges to be removed.

    Concerning garbage collection:
    Note that vertices and edges will not be removed automatically
    when the Haskell garbage collector runs —
    they will be marked as garbage by the Haskell runtime,
    but the actual removal of garbage needs
    to be done explicitly by calling 'removeGarbage'.
    This procedure makes it easier to reason about the state of the 'GraphGC'
    during a call to e.g. 'walkSuccessors'.
-}
data GraphGC v = GraphGC
    { graphRef :: IORef (GraphD v)
    , deletions :: STM.TQueue Unique
    }

-- | Create a new 'GraphGC'.
new :: IO (GraphGC v)
new = GraphGC <$> newIORef newGraphD <*> STM.newTQueueIO
  where
    newGraphD = GraphD
        { graph = Graph.empty
        , references = Map.empty
        }

-- | List all vertices that are reachable and have at least
-- one edge incident on them.
-- TODO: Is that really what the function does?
listReachableVertices :: GraphGC v -> IO [Ref v]
listReachableVertices GraphGC{graphRef} = do
    GraphD{references} <- readIORef graphRef
    concat . Map.elems <$> traverse inspect references
  where
    inspect ref = do
        mv <- Ref.deRefWeak ref
        pure $ case mv of
            Nothing -> []
            Just r -> [r]

-- | Insert an edge from the first vertex to the second vertex.
insertEdge :: (Ref v, Ref v) -> GraphGC v -> IO ()
insertEdge (x,y) g@GraphGC{graphRef} = do
    -- TODO: Reduce the number of finalizers if the vertex is
    -- already in the graph
    Ref.addFinalizer x (finalizeVertex g ux)
    Ref.addFinalizer y (finalizeVertex g uy)
    insertTheEdge =<< makeWeakPointerThatRepresentsEdge
  where
    ux = Ref.getUnique x
    uy = Ref.getUnique y

    makeWeakPointerThatRepresentsEdge =
        Ref.mkWeak y x Nothing

    insertTheEdge we = atomicModifyIORef_ graphRef $
        \GraphD{graph,references} -> GraphD
            { graph
                = Graph.insertEdge (ux,uy) we
                $ graph
            , references
                = Map.insert ux (Ref.getWeakRef x)
                . Map.insert uy (Ref.getWeakRef y)
                $ references
            }

{-----------------------------------------------------------------------------
    Garbage Collection
------------------------------------------------------------------------------}
-- | Explicitly remove all vertices and edges that have been marked
-- as garbage by the Haskell garbage collector.
removeGarbage :: GraphGC v -> IO ()
removeGarbage g@GraphGC{deletions} = do
    xs <- STM.atomically $ STM.tryReadTQueue deletions
    mapM_ (deleteVertex g) xs

-- Delete all edges associated with a vertex from the 'GraphGC'.
--
-- TODO: Check whether using an IORef is thread-safe.
-- I think it's fine because we have a single thread that performs deletions.
deleteVertex :: GraphGC v -> Unique -> IO ()
deleteVertex GraphGC{graphRef} x =
    atomicModifyIORef_ graphRef $ \GraphD{graph,references} -> GraphD
        { graph = Graph.deleteVertex x graph
        , references = Map.delete x references
        }

-- Finalize a vertex
finalizeVertex :: GraphGC v -> Unique -> IO ()
finalizeVertex GraphGC{deletions} =
    STM.atomically . STM.writeTQueue deletions

{-----------------------------------------------------------------------------
    Helper functions
------------------------------------------------------------------------------}
-- | Atomically modify an 'IORef' without returning a result.
atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f = atomicModifyIORef ref $ \x -> (f x, ())
