-- dummy implementation for the haste compiler
module Data.HashMap.Lazy where

import Data.Hashable

import qualified Data.List as List
import qualified Data.IntMap as Map

newtype HashMap k v = M { unM :: Map.IntMap [(k,v)] } deriving (Show)

empty :: HashMap k v
empty = M Map.empty

lookup :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
lookup k m = List.lookup k =<< Map.lookup (hash k) (unM m)

insertWith :: (Eq k, Hashable k) =>
    (v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v
insertWith f k new = M . Map.insertWith helper (hash k) [(k,new)] . unM
    where
    helper list _ = [(k,f new old) | (k1,old) <- list, k1 == k]
