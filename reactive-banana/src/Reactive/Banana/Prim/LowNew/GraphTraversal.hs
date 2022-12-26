{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.LowNew.GraphTraversal
    ( GraphM
    , reversePostOrder1
    , reversePostOrder
    ) where

import Data.Hashable
import qualified Data.HashSet as Set

{-----------------------------------------------------------------------------
    Graph traversal
------------------------------------------------------------------------------}
-- | Graph represented as map from a vertex to its direct successors.
type GraphM m a = a -> m [a]

-- | Computes the reverse post-order,
-- listing all (transitive) successor of a node.
--
-- Each vertex is listed *before* all its direct successors have been listed.
reversePostOrder1 :: (Eq a, Hashable a, Monad m) => a -> GraphM m a -> m [a]
reversePostOrder1 x = reversePostOrder [x]

-- | Reverse post-order from multiple vertices.
--
-- INVARIANT: For this to be a valid topological order,
-- none of the vertices may have a direct predecessor.
reversePostOrder :: (Eq a, Hashable a, Monad m) => [a] -> GraphM m a -> m [a]
reversePostOrder xs successors = fst <$> go xs [] Set.empty
    where
    go []     rpo visited        = return (rpo, visited)
    go (x:xs) rpo visited
        | x `Set.member` visited = go xs rpo visited
        | otherwise              = do
            xs' <- successors x
            -- visit all direct successors
            (rpo', visited') <- go xs' rpo (Set.insert x visited)
            -- prepend this vertex as all direct successors have been visited
            go xs (x:rpo') visited'
