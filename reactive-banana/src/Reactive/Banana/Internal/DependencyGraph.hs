{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Internal.DependencyGraph (
    -- | Utilities for operating with dependency graphs.
    Deps,
    empty, dependOn, topologicalSort, 
    ) where

import Reactive.Banana.Compat.Data.Hashable
import qualified Reactive.Banana.Compat.Data.HashMap.Lazy as Map
import qualified Reactive.Banana.Compat.Data.HashSet as Set

type Map = Map.HashMap
type Set = Set.HashSet

{-----------------------------------------------------------------------------
    Dependency graph data type
------------------------------------------------------------------------------}
-- dependency graph
data Deps a = Deps
    { dChildren :: Map a [a] -- children depend on their parents
    , dParents  :: Map a [a]
    , dRoots    :: Set a
    } deriving (Show)

-- convenient queries
children deps x = maybe [] id . Map.lookup x $ dChildren deps
parents  deps x = maybe [] id . Map.lookup x $ dParents  deps

-- the empty dependency graph
empty :: Hashable a => Deps a
empty = Deps
    { dChildren = Map.empty
    , dParents  = Map.empty
    , dRoots    = Set.empty
    }

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}
-- add a dependency to the graph
dependOn :: (Eq a, Hashable a) => a -> a -> Deps a -> Deps a
dependOn x y deps0 = deps1
    where
    deps1 = deps0
        { dChildren = Map.insertWith (++) y [x] $ dChildren deps0
        , dParents  = Map.insertWith (++) x [y] $ dParents  deps0
        , dRoots    = roots
        }
    
    roots = when (null $ parents deps0 x) (Set.delete x)
          . when (null $ parents deps1 y) (Set.insert y)
          $ dRoots deps0
    
    when b f = if b then f else id

-- order the nodes in a way such that no children comes before its parent
topologicalSort :: (Eq a, Hashable a) => Deps a -> [a]
topologicalSort deps = go (Set.toList $ dRoots deps) Set.empty
    where
    go []     _     = []
    go (x:xs) seen1 = x : go (adultChildren ++ xs) seen2
        where
        seen2         = Set.insert x seen1
        adultChildren = filter isAdult (children deps x)
        isAdult y     = all (`Set.member` seen2) (parents deps y)

{-----------------------------------------------------------------------------
    Small tests
------------------------------------------------------------------------------}
test = id
    . dependOn 'D' 'C'
    . dependOn 'D' 'B'
    . dependOn 'C' 'B'
    . dependOn 'B' 'A'
    . dependOn 'B' 'a'
    $ empty


