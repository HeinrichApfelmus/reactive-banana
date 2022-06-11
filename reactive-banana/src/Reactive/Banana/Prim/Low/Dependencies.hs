{-# language BangPatterns #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Reactive.Banana.Prim.Low.Dependencies (
    -- | Utilities for operating on node dependencies.
    addChild, changeParent, buildDependencies,
    ) where

import           Control.Monad
import           Data.Monoid
import           System.Mem.Weak

import qualified Reactive.Banana.Prim.Low.Graph as Graph
import           Reactive.Banana.Prim.Low.Types
import           Reactive.Banana.Prim.Low.Util

{-----------------------------------------------------------------------------
    Accumulate dependency information for nodes
------------------------------------------------------------------------------}
-- | Add a new child node to a parent node.
addChild :: SomeNode -> SomeNode -> DependencyBuilder
addChild parent child = (Endo $ Graph.insertEdge (parent,child), mempty)

-- | Assign a new parent to a child node.
-- INVARIANT: The child may have only one parent node.
changeParent :: Pulse a -> Pulse b -> DependencyBuilder
changeParent child parent = (mempty, [(P child, P parent)])

-- | Execute the information in the dependency builder
-- to change network topology.
buildDependencies :: DependencyBuilder -> IO ()
buildDependencies (Endo f, parents) = do
    sequence_ [{-# SCC doAddChild #-} (x `doAddChild` y) | x <- Graph.listParents gr, y <- Graph.getChildren gr x]
    sequence_ [{-# SCC doChangeParent #-} (x `doChangeParent` y) | (P x, P y) <- parents]
    where
    gr :: Graph.Graph SomeNode
    gr = f Graph.emptyGraph

{-----------------------------------------------------------------------------
    Set dependencies of individual notes
------------------------------------------------------------------------------}
-- | Add a child node to the children of a parent 'Pulse'.
connectChild
    :: Pulse a  -- ^ Parent node whose '_childP' field is to be updated.
    -> SomeNode -- ^ Child node to add.
    -> IO (Weak SomeNode)
                -- ^ Weak reference with the child as key and the parent as value.
connectChild parent child = do
    putStrLn "1"
    w <- {-# SCC childChild #-} mkWeakNodeValue child child
    modify' parent $ update childrenP $ const [w]

    putStrLn "2"
    mkWeakNodeValue child (P parent)        -- child keeps parent alive

-- | Add a child node to a parent node and update evaluation order.
doAddChild :: SomeNode -> SomeNode -> IO ()
doAddChild (P parent) (P child) = do
    level1 <- _levelP <$> readRef child
    level2 <- _levelP <$> readRef parent
    let level = level1 `max` (level2 + 1)
    w <- parent `connectChild` P child
    modify' child $ set levelP level . update parentsP (w:)
doAddChild (P parent) node = void $ parent `connectChild` node
doAddChild (L _) _ = error "doAddChild: Cannot add children to LatchWrite"
doAddChild (O _) _ = error "doAddChild: Cannot add children to Output"

-- | Remove a node from its parents and all parents from this node.
removeParents :: Pulse a -> IO ()
removeParents child = do
    c@Pulse{_parentsP} <- readRef child
    -- delete this child (and dead children) from all parent nodes
    forM_ _parentsP $ \w -> do
        Just (P parent) <- deRefWeak w  -- get parent node
        finalize w                      -- severe connection in garbage collector
        let isGoodChild w = not . maybe True (== P child) <$> deRefWeak w
        !new <- filterM isGoodChild . _childrenP =<< readRef parent
        modify' parent $ set childrenP new
    -- replace parents by empty list
    put child $ c{_parentsP = []}

-- | Set the parent of a pulse to a different pulse.
doChangeParent :: Pulse a -> Pulse b -> IO ()
doChangeParent child parent = do
    w <- {-# SCC connectChildX #-} do 
      parent `connectChild` P child

    -- remove all previous parents and connect to new parent
    {-# SCC removeParents #-} removeParents child

    modify' child $ update parentsP (w:)

    -- calculate level difference between parent and node
    levelParent <- _levelP <$> readRef parent
    levelChild  <- _levelP <$> readRef child
    let d = levelParent - levelChild + 1
    -- level parent - d = level child - 1

    -- lower all parents of the node if the parent was higher than the node
    when (d > 0) $ do
        parents <- Graph.reversePostOrder (P parent) getParents
        forM_ parents $ \case
            P node -> modify' node $ update levelP (subtract d)
            L _    -> error "doChangeParent: Cannot change parent of LatchWrite"
            O _    -> error "doChangeParent: Cannot change parent of Output"

{-----------------------------------------------------------------------------
    Helper functions
------------------------------------------------------------------------------}
getParents :: SomeNode -> IO [SomeNode]
getParents (P p) = deRefWeaks . _parentsP =<< readRef p
getParents _     = return []
