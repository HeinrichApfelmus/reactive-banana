{-# LANGUAGE NamedFieldPuns #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
-- | Property tests for 'Graph'.
module Reactive.Banana.Test.Low.Graph
    ( tests
    , mkGraph
    ) where

import Reactive.Banana.Prim.Low.Graph 
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

import qualified Reactive.Banana.Prim.Low.Graph as Graph

tests :: TestTree
tests = testGroup "Graph"
    [ testGroup "walkSuccessors"
        [ testProperty "Predecessors have lower levels" prop_levelsInvariant
        , testProperty "succeeds on a square" prop_walkSquare
        ]
    ]

{-----------------------------------------------------------------------------
    Properties
------------------------------------------------------------------------------}
prop_levelsInvariant :: Property
prop_levelsInvariant = Q.forAll Q.genTestGraph $ \g0 ->
    let g = mkGraph g0
        level x = Graph.getLevel g x
    in
        Q.conjoin [ level x < level y | InsertEdge x y <- edges g0 ]

-- | Run 'walkSuccessors' on a square (with edges inserted randomly).
walkSquare :: Int -> Gen [Vertex]
walkSquare n = do
    g <- mkGraph <$> Q.shuffleEdges (Q.mkSquare n)
    Graph.walkSuccessors [1] (const step) g
  where
    step = Q.frequency [(10,pure Graph.Next), (1,pure Graph.Stop)]

prop_walkSquare :: Property
prop_walkSquare =
    Q.forAll Q.genSquareSide
    $ \n -> Q.cover 10 (n >= 10) "large square"
    $ Q.forAll (walkSquare n)
    $ \walk ->
    let correctOrder (x,y) =
            Q.counterexample (show y <> " precedes " <> show x)
                $ not $ (fromInt n y) `before` (fromInt n x)

        checkOrder = Q.conjoin $ replicate 10 $ do
            m <- Q.chooseInt (1, length walk - 1)
            pure
                $ Q.conjoin
                $ map correctOrder
                $ pairsFromPivot m walk

    in  Q.counterexample ("Walk result: " <> show walk)
        $ length walk >= 1
  where
    fromInt :: Int -> Vertex -> (Int, Int)
    fromInt n x = ((x-1) `mod` n, (x-1) `div` n)

    (x1,y1) `before` (x2,y2) = x1 <= x2 && y1 <= y2

pairsFromPivot :: Int -> [a] -> [(a,a)]
pairsFromPivot n [] = []
pairsFromPivot n xs = [(a,b) | a <- as] ++ [(b,c) | c <- cs]
  where
    (as, b:cs) = splitAt m xs
    m = max (length xs - 1) $ min 0 $ n

{-----------------------------------------------------------------------------
    Test graphs
------------------------------------------------------------------------------}
-- | Generate a 'Graph' from a 'TestGraph'.
mkGraph :: TestGraph -> Graph Vertex ()
mkGraph TestGraph{edges} = List.foldl' insertEdge Graph.empty edges
  where
    insertEdge g (InsertEdge x y) = Graph.insertEdge (x,y) () g
