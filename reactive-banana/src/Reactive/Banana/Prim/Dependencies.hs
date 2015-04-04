{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Reactive.Banana.Prim.Dependencies (
    -- | Utilities for operating with dependency graphs.
    Deps, dOrder, empty, allChildren, children, parents,
    addChild, changeParent,
    
    Continue(..), maybeContinue, traverseDependencies,
    
    DepsQueue, emptyQ, insert, minView,
    ) where

import           Control.Monad.Trans.Writer
import qualified Data.HashMap.Strict        as Map
import qualified Data.HashSet               as Set
import           Data.Hashable
import qualified Data.PQueue.Prio.Min       as Q

import           Reactive.Banana.Prim.Order
import qualified Reactive.Banana.Prim.Order as Order

type Map = Map.HashMap
type Set = Set.HashSet

{-----------------------------------------------------------------------------
    Dependency graph
------------------------------------------------------------------------------}
-- | A dependency graph.
data Deps a = Deps
    { dChildren :: Map a [a]     -- children depend on their parents
    , dParents  :: Map a [a]
    , dOrder    :: Order a
    } deriving (Show)

-- | Representation of the depencencies as an association list of nodes
-- to children.
allChildren :: Deps a -> [(a, [a])]
allChildren = Map.toList . dChildren

-- | Children of a node.
children deps x =
    {-# SCC children #-} maybe [] id . Map.lookup x $ dChildren deps

-- | Parents of a node.
parents  deps x = maybe [] id . Map.lookup x $ dParents  deps

-- | The empty dependency graph.
empty :: Hashable a => Deps a
empty = Deps
    { dChildren = Map.empty
    , dParents  = Map.empty
    , dOrder    = Order.flat
    }

-- | Add a new dependency.
addChild :: (Eq a, Hashable a) => a -> a -> Deps a -> Deps a
addChild parent child deps1@(Deps{..}) = deps2
    where
    deps2 = Deps
        { dChildren = Map.insertWith (++) parent [child] dChildren
        , dParents  = Map.insertWith (++) child [parent] dParents
        , dOrder    = ensureAbove child parent dOrder
        }
    when b f = if b then f else id

-- | Change the parent of the first argument to be the second one.
changeParent :: (Eq a, Hashable a) => a -> a -> Deps a -> Deps a
changeParent child parent deps1@(Deps{..}) = deps2
    where
    deps2 = Deps
        { dChildren = Map.insertWith (++) parent [child]
                    $ removeChild parentsOld dChildren
        , dParents  = Map.insert child [parent] dParents
        , dOrder    = recalculateParent child parent (parents deps2) dOrder
        }
    parentsOld   = parents deps1 child
    removeChild1 = Map.adjust (filter (/= child))
    removeChild  = concatenate . map removeChild1
    concatenate  = foldr (.) id

{-----------------------------------------------------------------------------
    Traversal
------------------------------------------------------------------------------}
-- | Data type for signaling whether to continue a traversal or not.
data Continue = Children | Done
    deriving (Eq, Ord, Show, Read)

-- | Convert a 'Maybe' value into a 'Continue' decision.
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
traverseDependencies f deps roots = go $ insertList roots emptyQ
    where
    order = dOrder deps
    insertList xs q = foldr (\x -> insert (level x order) x) q xs

    go q1 = case minView q1 of
        Nothing      -> return ()
        Just (a, q2) -> do
            continue <- f a
            case continue of
                Done     -> go q2
                Children -> go $ insertList (children deps a) q2

-- | Queue for traversing dependencies.
data DepsQueue a = DQ !(Q.MinPQueue Level a) !(Set a)

emptyQ :: DepsQueue a
emptyQ = DQ Q.empty Set.empty

insert :: (Eq a, Hashable a) => Level -> a -> DepsQueue a -> DepsQueue a
insert k a q@(DQ queue seen) = {-# SCC insert #-}
    if a `Set.member` seen
        then q
        else DQ (Q.insert k a queue) (Set.insert a seen)

minView :: (Eq a, Hashable a) => DepsQueue a -> Maybe (a, DepsQueue a)
minView (DQ queue seen) = {-# SCC minView #-} case Q.minView queue of
    Nothing          -> Nothing
    Just (a, queue2) -> Just (a, DQ queue2 (Set.delete a seen))

{-----------------------------------------------------------------------------
    Small tests
------------------------------------------------------------------------------}
test1 = id
    . changeParent 'C' 'A'
    . addChild 'C' 'D'
    . addChild 'B' 'C'
    . addChild 'B' 'D'
    . addChild 'A' 'B'
    . addChild 'a' 'B'
    $ empty

{- test2 =
        a
       / \
      b   d   A
      |   |   |
      c   e   B
       \ / \ /
        f   g
         \ /
          h

-}
test2 = id
    . addChild 'g' 'h' . addChild 'e' 'g'
    . addChild 'B' 'g' . addChild 'A' 'B'
    . addChild 'f' 'h'
    . addChild 'e' 'f' . addChild 'd' 'e' . addChild 'a' 'd'
    . addChild 'c' 'f' . addChild 'b' 'c' . addChild 'a' 'b'
    $ empty

test3 = changeParent 'A' 'f' $ test2

listChildren :: (Eq a, Hashable a) => Deps a -> a -> [a]
listChildren deps x = snd $ runWriter $ traverseDependencies f deps [x]
    where f x = tell [x] >> return Children
    