{-# LANGUAGE NamedFieldPuns #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
-- | Property tests for 'Graph'.
module Reactive.Banana.Test.Low.Graph
    ( mkGraph
    ) where

import Reactive.Banana.Prim.LowNew.Graph 
    ( Graph )
import Reactive.Banana.Test.Low.Gen
    ( DeltaGraph (..), TestGraph (..), Vertex )
import Test.QuickCheck
    ( Gen, Property, (===), (=/=) )
import Test.Tasty
    ( testGroup, TestTree )
import Test.Tasty.QuickCheck
    ( testProperty )

import qualified Data.List as List
import qualified Test.QuickCheck as Q
import qualified Reactive.Banana.Test.Low.Gen as Q

import qualified Reactive.Banana.Prim.LowNew.Graph as Graph

{-----------------------------------------------------------------------------
    Test graphs
------------------------------------------------------------------------------}
-- | Generate a 'Graph' from a 'TestGraph'.
mkGraph :: TestGraph -> Graph Vertex ()
mkGraph TestGraph{edges} = List.foldl' insertEdge Graph.empty edges
  where
    insertEdge g (InsertEdge x y) = Graph.insertEdge (x,y) () g
