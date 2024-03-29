{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.Low.GraphGC
    ( GraphGC
    , listReachableVertices
    , getSize
    , new
    , insertEdge
    , clearPredecessors

    , Step (..)
    , walkSuccessors
    , walkSuccessors_

    , removeGarbage
    
    -- * Debugging
    , printDot
    ) where

import Control.Applicative
    ( (<|>) )
import Control.Monad
    ( unless )
import Data.IORef
    ( IORef, atomicModifyIORef', newIORef, readIORef )
import Data.Maybe
    ( fromJust )
import Data.Unique.Really
    ( Unique )
import Reactive.Banana.Prim.Low.Graph 
    ( Graph, Step )
import Reactive.Banana.Prim.Low.Ref
    ( Ref, WeakRef )

import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as Map
import qualified Reactive.Banana.Prim.Low.Graph as Graph
import qualified Reactive.Banana.Prim.Low.Ref as Ref

type Map = Map.HashMap

{-----------------------------------------------------------------------------
    GraphGC
------------------------------------------------------------------------------}
type WeakEdge v = WeakRef v

-- Graph data
data GraphD v = GraphD
    { graph :: !(Graph Unique (WeakEdge v))
    , references :: !(Map Unique (WeakRef v))
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

getSize :: GraphGC v -> IO Int
getSize GraphGC{graphRef} = Graph.size . graph <$> readIORef graphRef

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
    (xKnown, yKnown) <-
        insertTheEdge =<< makeWeakPointerThatRepresentsEdge
    unless xKnown $ Ref.addFinalizer x (finalizeVertex g ux)
    unless yKnown $ Ref.addFinalizer y (finalizeVertex g uy)
  where
    ux = Ref.getUnique x
    uy = Ref.getUnique y

    makeWeakPointerThatRepresentsEdge =
        Ref.mkWeak y x Nothing

    insertTheEdge we = atomicModifyIORef' graphRef $
        \GraphD{graph,references} ->
            ( GraphD
                { graph
                    = Graph.insertEdge (ux,uy) we
                    $ graph
                , references
                    = Map.insert ux (Ref.getWeakRef x)
                    . Map.insert uy (Ref.getWeakRef y)
                    $ references
                }
            ,   ( ux `Map.member` references
                , uy `Map.member` references
                ) 
            )

-- | Remove all the edges that connect the vertex to its predecessors.
clearPredecessors :: Ref v -> GraphGC v -> IO ()
clearPredecessors x GraphGC{graphRef} = do
    g <- atomicModifyIORef' graphRef $ \g -> (removeIncomingEdges g, g)
    finalizeIncomingEdges g
  where
    removeIncomingEdges g@GraphD{graph} =
        g{ graph = Graph.clearPredecessors (Ref.getUnique x) graph }
    finalizeIncomingEdges GraphD{graph} =
        mapM_ (Ref.finalize . snd) . Graph.getIncoming graph $ Ref.getUnique x

-- | Walk through all successors. See 'Graph.walkSuccessors'.
walkSuccessors
    :: Monad m
    => [Ref v] -> (WeakRef v -> m Step) -> GraphGC v -> IO (m [WeakRef v])
walkSuccessors roots step GraphGC{..} = do
    GraphD{graph,references} <- readIORef graphRef
    let rootsMap = Map.fromList
            [ (Ref.getUnique r, Ref.getWeakRef r) | r <- roots ]
        fromUnique u = fromJust $
            Map.lookup u references <|> Map.lookup u rootsMap
    pure
        . fmap (map fromUnique)
        . Graph.walkSuccessors (map Ref.getUnique roots) (step . fromUnique)
        $ graph

-- | Walk through all successors. See 'Graph.walkSuccessors_'.
walkSuccessors_ ::
    Monad m => [Ref v] -> (WeakRef v -> m Step) -> GraphGC v -> IO (m ())
walkSuccessors_ roots step g = do
    action <- walkSuccessors roots step g
    pure $ action >> pure ()

{-----------------------------------------------------------------------------
    Garbage Collection
------------------------------------------------------------------------------}
-- | Explicitly remove all vertices and edges that have been marked
-- as garbage by the Haskell garbage collector.
removeGarbage :: GraphGC v -> IO ()
removeGarbage g@GraphGC{deletions} = do
    xs <- STM.atomically $ STM.flushTQueue deletions
    mapM_ (deleteVertex g) xs

-- Delete all edges associated with a vertex from the 'GraphGC'.
--
-- TODO: Check whether using an IORef is thread-safe.
-- I think it's fine because we have a single thread that performs deletions.
deleteVertex :: GraphGC v -> Unique -> IO ()
deleteVertex GraphGC{graphRef} x =
    atomicModifyIORef'_ graphRef $ \GraphD{graph,references} -> GraphD
        { graph = Graph.deleteVertex x graph
        , references = Map.delete x references
        }

-- Finalize a vertex
finalizeVertex :: GraphGC v -> Unique -> IO ()
finalizeVertex GraphGC{deletions} =
    STM.atomically . STM.writeTQueue deletions

{-----------------------------------------------------------------------------
    Debugging
------------------------------------------------------------------------------}
-- | Show the underlying graph in @graphviz@ dot file format.
printDot :: (Unique -> WeakRef v -> IO String) -> GraphGC v -> IO String
printDot format GraphGC{graphRef} = do
    GraphD{graph,references} <- readIORef graphRef
    strings <- Map.traverseWithKey format references
    pure $ Graph.showDot (strings Map.!) graph

{-----------------------------------------------------------------------------
    Helper functions
------------------------------------------------------------------------------}
-- | Atomically modify an 'IORef' without returning a result.
atomicModifyIORef'_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef'_ ref f = atomicModifyIORef' ref $ \x -> (f x, ())
