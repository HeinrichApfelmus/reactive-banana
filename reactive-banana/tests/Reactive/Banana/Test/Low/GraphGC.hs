{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
-- | Property tests for 'GraphGC'.
module Reactive.Banana.Test.Low.GraphGC
    ( tests
    ) where

import Control.Monad
    ( when )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Map.Strict
    ( Map )
import Data.Unique.Really
    ( Unique )
import Reactive.Banana.Prim.LowNew.Graph 
    ( Graph )
import Reactive.Banana.Prim.LowNew.GraphGC
    ( GraphGC )
import Reactive.Banana.Test.Low.Gen
    ( DeltaGraph (..), TestGraph (..), Vertex )
import Test.QuickCheck
    ( Gen, Property, (===), (=/=) )
import Test.Tasty
    ( testGroup, TestTree )
import Test.Tasty.QuickCheck
    ( testProperty )

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Control.DeepSeq as Memory
import qualified Control.Exception as Memory
import qualified System.Mem as System
import qualified Control.Concurrent as System

import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Monadic as Q
import qualified Reactive.Banana.Test.Low.Graph as Q
import qualified Reactive.Banana.Test.Low.Gen as Q

import qualified Reactive.Banana.Prim.LowNew.Graph as Graph
import qualified Reactive.Banana.Prim.LowNew.GraphGC as GraphGC
import qualified Reactive.Banana.Prim.LowNew.Ref as Ref


tests :: TestTree
tests = testGroup "GraphGC"
    [ testGroup "Garbage collection (GC)"
        [ testProperty "retains the reachable vertices" prop_performGC
        , testProperty "not doing GC retains all vertices" prop_notPerformGC
        ]
    ]

{-----------------------------------------------------------------------------
    Properties
------------------------------------------------------------------------------}
prop_performGC :: Property
prop_performGC =
    Q.forAll Q.genTestGraph
    $ \g0 -> Q.forAll (genGarbageCollectionRoots g0)
    $ \roots ->
    let g = Q.mkGraph g0
        expected = Graph.collectGarbage roots g
    in  Q.cover 10 (Graph.size g == Graph.size expected)
            "no vertices unreachable"
        $ Q.cover 75 (Graph.size g > Graph.size expected)
            "some vertices unreachable"
        $ Q.cover 15 (Graph.size g > 2*Graph.size expected)
            "many vertices unreachable"
        $ Q.monadicIO $ liftIO $ do
            (actual, vertices) <- mkGraphGC g0
            let rootRefs = map (vertices Map.!) roots
            Memory.evaluate $ Memory.rnf rootRefs

            performSufficientGC
            GraphGC.removeGarbage actual
            reachables <- traverse Ref.read =<<
                GraphGC.listReachableVertices actual

            -- keep rootsRef reachable until this point
            rootsFromRef <- traverse Ref.read rootRefs

            pure $
                ( roots === rootsFromRef )
                Q..&&.
                ( Set.fromList (Graph.listConnectedVertices expected)
                    === Set.fromList reachables
                )

prop_notPerformGC :: Property
prop_notPerformGC =
    Q.forAll Q.genSquareSide
    $ \n -> Q.monadicIO $ liftIO $ do
        -- Trigger a garbage collection now so that it is
        -- highly unlikely to happen in the subsequent lines
        performSufficientGC

        let g = Q.mkLinearChain n

        (actual, _) <- mkGraphGC g
        GraphGC.removeGarbage actual
        reachables <- traverse Ref.read =<<
            GraphGC.listReachableVertices actual

        pure $
            Set.fromList reachables === Set.fromList [1..n]

performSufficientGC :: IO ()
performSufficientGC = System.performMinorGC

{-----------------------------------------------------------------------------
    Test graphs
------------------------------------------------------------------------------}
-- | Generate a 'GraphGC' from a 'TestGraph'.
mkGraphGC :: TestGraph -> IO (GraphGC Vertex, Map Vertex (Ref.Ref Vertex))
mkGraphGC TestGraph{vertices,edges} = do
    g <- GraphGC.new
    refMap <- Map.fromList . zip vertices <$> traverse Ref.new vertices
    let insertEdge (InsertEdge x y) = do
            GraphGC.insertEdge (refMap Map.! x, refMap Map.! y) g
    traverse insertEdge edges
    pure (g, refMap)

-- | Randomly generate a set of garbage collection roots.
genGarbageCollectionRoots :: TestGraph -> Gen [Vertex]
genGarbageCollectionRoots TestGraph{vertices} = Q.sized $ \n ->
    sequence . replicate (n `mod` 10) $ Q.elements vertices
