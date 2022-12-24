{-# LANGUAGE NamedFieldPuns #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
-- | Generation of intereseting example graphs.
module Reactive.Banana.Test.Low.Gen
    (
    -- * Simple graph types for testing
      TestGraph (..)
    , DeltaGraph (..)
    , Vertex

    -- * Example graphs
    , mkLinearChain
    , mkSquare
    
    -- * Generators
    , genTestGraph
    , genLinearChain
    , genSquare
    , genSquareSide
    , shuffleEdges
    ) where

import Test.QuickCheck
    ( Gen )
import qualified Test.QuickCheck as Q

{-----------------------------------------------------------------------------
    Graphs for testing
------------------------------------------------------------------------------}
type Vertex = Int

data DeltaGraph
    = InsertEdge Vertex Vertex
    deriving (Eq, Show)

data TestGraph = TestGraph
    { vertices :: [Vertex]
    , edges :: [DeltaGraph]
    } deriving (Eq, Show)

{-----------------------------------------------------------------------------
    Interesting example graphs
------------------------------------------------------------------------------}
-- | A linear chain   1 -> 2 -> 3 -> … -> n .
mkLinearChain :: Int -> TestGraph
mkLinearChain n = TestGraph{vertices,edges}
  where
    vertices = [1..n]
    edges = zipWith InsertEdge vertices (drop 1 vertices)

-- | A cartesian product of linear chains
mkSquare :: Int -> TestGraph
mkSquare n = TestGraph{vertices,edges}
  where
    toInt (x,y) = (x-1) + n*(y-1) + 1
    vertices = [ toInt (x,y) | y <- [1..n], x <- [1..n]]
    edges =
        [ InsertEdge (toInt (x,y)) (toInt (x+1,y))
        | y <- [1..n]
        , x <- [1..n-1]
        ]
        ++ 
        [ InsertEdge (toInt (x,y)) (toInt (x,y+1))
        | y <- [1..n-1]
        , x <- [1..n]
        ]

{-----------------------------------------------------------------------------
    Generating various graphs
------------------------------------------------------------------------------}
-- | Interesting generator for 'TestGraph'.
genTestGraph :: Gen TestGraph
genTestGraph = shuffleEdges =<< Q.frequency
    [ (1, genLinearChain)
    , (1, genSquare)
    ]

shuffleEdges :: TestGraph -> Gen TestGraph
shuffleEdges g@TestGraph{edges} = (\e -> g{edges = e})<$> Q.shuffle edges

genLinearChain :: Gen TestGraph
genLinearChain = Q.sized $ pure . mkLinearChain

genSquare :: Gen TestGraph
genSquare = mkSquare <$> genSquareSide

genSquareSide :: Gen Int
genSquareSide = Q.sized $ \n -> Q.chooseInt (2,floorSqrt (2*n) + 2)

floorSqrt :: Int -> Int
floorSqrt = floor . sqrt . fromIntegral
