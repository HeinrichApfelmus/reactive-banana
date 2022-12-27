{-# LANGUAGE NamedFieldPuns #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.Mid.Dependencies
    ( -- | Utilities for operating on node dependencies.
      addChild
    , changeParent
    , applyChanges
    ) where

import qualified Reactive.Banana.Prim.Low.GraphGC as GraphGC
import           Reactive.Banana.Prim.Mid.Types

{-----------------------------------------------------------------------------
    Accumulate dependency information for nodes
------------------------------------------------------------------------------}
-- | Add a new child node to a parent node.
addChild :: SomeNode -> SomeNode -> DependencyChanges
addChild parent child = [InsertEdge parent child]

-- | Assign a new parent to a child node.
-- INVARIANT: The child may have only one parent node.
changeParent :: Pulse a -> Pulse b -> DependencyChanges
changeParent child parent = [ChangeParentTo (_nodeP child) (_nodeP parent)]

-- | Execute the information in the dependency builder
-- to change network topology.
applyChanges :: DependencyChanges -> Dependencies -> IO ()
applyChanges changes g = do
    sequence_ [applyChange c g | c@(InsertEdge _ _) <- changes]
    sequence_ [applyChange c g | c@(ChangeParentTo _ _) <- changes]

applyChange
    :: DependencyChange SomeNode SomeNode -> Dependencies -> IO ()
applyChange (InsertEdge parent child) =
    GraphGC.insertEdge (parent, child)
applyChange (ChangeParentTo child parent) = do
    _ <- GraphGC.clearPredecessors child
    GraphGC.insertEdge (parent, child)
