{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-}
module Reactive.Banana.Prim.Dependencies (
    -- | Utilities for operating with dependency graphs.
    Deps, children, parents, ancestorOrder,
    
    empty, addChild, changeParent,
    
    Continue(..), maybeContinue, traverseDependencies,
    ) where

import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet      as Set
import           Data.Hashable

import           Reactive.Banana.Prim.TotalOrder
import qualified Reactive.Banana.Prim.TotalOrder as TO

type Map = Map.HashMap
type Set = Set.HashSet

{-----------------------------------------------------------------------------
    Dependency graph data type
------------------------------------------------------------------------------}
-- | A dependency graph.
data Deps a = Deps
    { dChildren :: Map a [a] -- children depend on their parents
    , dParents  :: Map a [a]
    , dRoots    :: Set a
    } deriving (Show)

-- | Convenient queries.
children deps x = maybe [] id . Map.lookup x $ dChildren deps
parents  deps x = maybe [] id . Map.lookup x $ dParents  deps

-- | The empty dependency graph.
empty :: Hashable a => Deps a
empty = Deps
    { dChildren = Map.empty
    , dParents  = Map.empty
    , dRoots    = Set.empty
    }

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}
-- | Add a new dependency.
addChild :: (Eq a, Hashable a) => a -> a -> Deps a -> Deps a
addChild parent child deps0 = deps1
    where
    deps1 = deps0
        { dChildren = Map.insertWith (++) parent [child] $ dChildren deps0
        , dParents  = Map.insertWith (++) child [parent] $ dParents  deps0
        , dRoots    = roots
        }
    
    roots = when (null $ parents deps0 child ) (Set.delete child )
          . when (null $ parents deps1 parent) (Set.insert parent)
          $ dRoots deps0
    
    when b f = if b then f else id


-- | Change the parent of a given node.
--
-- FIXME: Remove old dependency.
changeParent :: (Eq a, Hashable a) => a -> a -> Deps a -> Deps a
changeParent child parent = addChild parent child

-- | Data type for signaling whether to continue a traversal or not.
data Continue = Children | Done
    deriving (Eq, Ord, Show, Read)

maybeContinue :: Maybe a -> Continue
maybeContinue Nothing  = Done
maybeContinue (Just _) = Children

-- | Starting with a set of root nodes, peform a monadic action
-- for each node. If the action returns 'Children', its children will also
-- be traversed at some point.
-- However, all nodes are traversed in dependency order:
-- A child node is only traversed when all its parent nodes have been traversed.
traverseDependencies :: forall a m. (Eq a, Hashable a, Monad m)
    => (a -> m Continue) -> Deps a -> [a] -> m ()
traverseDependencies f deps roots =
    withTotalOrder (ancestorOrder deps) $ go . insertList roots
    where
    go :: Queue q => q a -> m ()
    go q1 = case minView q1 of
        Nothing      -> return ()
        Just (a, q2) -> do
            continue <- f a
            case continue of
                Done     -> go q2
                Children -> go $ insertList (children deps a) q2

-- | Order the nodes in a way such that no children comes before its parent.
topologicalSort :: (Eq a, Hashable a) => Deps a -> [a]
topologicalSort deps = go (Set.toList $ dRoots deps) Set.empty
    where
    go []     _     = []
    go (x:xs) seen1 = x : go (adultChildren ++ xs) seen2
        where
        seen2         = Set.insert x seen1
        adultChildren = filter isAdult (children deps x)
        isAdult y     = all (`Set.member` seen2) (parents deps y)

-- | Order the nodes in a way such that no child comes before its parent.
ancestorOrder :: (Eq a, Hashable a) => Deps a -> TO.TotalOrder a
ancestorOrder = TO.fromAscList . topologicalSort

{-----------------------------------------------------------------------------
    Small tests
------------------------------------------------------------------------------}
test = id
    . addChild 'C' 'D'
    . addChild 'B' 'D'
    . addChild 'B' 'C'
    . addChild 'A' 'B'
    . addChild 'a' 'B'
    $ empty


