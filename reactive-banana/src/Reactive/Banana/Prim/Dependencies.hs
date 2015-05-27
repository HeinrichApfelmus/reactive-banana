{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Reactive.Banana.Prim.Dependencies (
    -- | Utilities for operating on node dependencies.
    addChild, changeParent,
    ) where

import           Control.Monad
import qualified Data.HashSet        as Set
import           Data.Hashable
import           Data.Functor
import           Reactive.Banana.Prim.Types
import           Reactive.Banana.Prim.Util
import           System.Mem.Weak

{-----------------------------------------------------------------------------
    Dependencies
------------------------------------------------------------------------------}
-- | Add a child node to the children of a parent 'Pulse'.
connectChild
    :: Pulse a  -- ^ Parent node whose '_childP' field is to be updated.
    -> SomeNode -- ^ Child node to add.
    -> IO (Weak SomeNode)
                -- ^ Weak reference with the child as key and the parent as value.
connectChild parent child = do
    w <- mkWeakNodeValue child child
    modify' parent $ update childrenP (w:)
    mkWeakNodeValue child (P parent)        -- child keeps parent alive

-- | Add a child node to a parent node and update evaluation order.
addChild :: SomeNode -> SomeNode -> IO ()
addChild (P parent) (P child) = do
    level1 <- _levelP <$> readRef child
    level2 <- _levelP <$> readRef parent
    let level = level1 `max` (level2 + 1)
    w <- parent `connectChild` (P child)
    modify' child $ set levelP level . update parentsP (w:)
addChild (P parent) node = void $ parent `connectChild` node

-- | Remove a node from its parents and all parents from this node.
removeParents :: Pulse a -> IO ()
removeParents child = do
    c@Pulse{_parentsP} <- readRef child
    -- delete this child (and dead children) from all parent nodes
    forM_ _parentsP $ \w -> do
        Just (P parent) <- deRefWeak w  -- get parent node
        finalize w                      -- severe connection in garbage collector
        let isGoodChild w = not . maybe True (== P child) <$> deRefWeak w
        new <- filterM isGoodChild . _childrenP =<< readRef parent
        modify' parent $ set childrenP new
    -- replace parents by empty list
    put child $ c{_parentsP = []}

-- | Set the parent of a pulse to a different pulse.
changeParent :: Pulse a -> Pulse b -> IO ()
changeParent child parent = do
    -- remove all previous parents and connect to new parent
    removeParents child
    w <- parent `connectChild` (P child)
    modify' child $ update parentsP (w:)

    -- calculate level difference between parent and node
    levelParent <- _levelP <$> readRef parent
    levelChild  <- _levelP <$> readRef child
    let d = levelParent - levelChild + 1
    -- level parent - d = level child - 1

    -- lower all parents of the node if the parent was higher than the node
    when (d > 0) $ do
        parents <- dfs (P parent) getParents
        forM_ parents $ \(P node) -> do
            modify' node $ update levelP (subtract d)

{-----------------------------------------------------------------------------
    Graph traversal
------------------------------------------------------------------------------}
-- | Graph represented as map of successors.
type Graph m a = a -> m [a]

-- | Depth-first search. List all transitive successors of a node.
dfs :: (Eq a, Hashable a, Monad m) => a -> Graph m a -> m [a]
dfs x succs = go [x] [] Set.empty
    where
    go []     ys _               = return $ reverse ys
    go (x:xs) ys seen
        | x `Set.member` seen    = go xs ys seen
        | otherwise              = do
            xs' <- succs x
            go (xs' ++ xs) (x:ys) (Set.insert x seen)

{-----------------------------------------------------------------------------
    Helper functions
------------------------------------------------------------------------------}
getChildren :: SomeNode -> IO [SomeNode]
getChildren (P p) = deRefWeaks =<< fmap _childrenP (readRef p)
getChildren _     = return []

getParents :: SomeNode -> IO [SomeNode]
getParents (P p) = deRefWeaks =<< fmap _parentsP (readRef p)
getParents _     = return []
