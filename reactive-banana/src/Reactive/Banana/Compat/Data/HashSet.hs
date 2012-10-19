-- dummy implementation for the haste compiler
{-# LANGUAGE CPP #-}
#if UseGHC

module Reactive.Banana.Compat.Data.HashSet (module Data.HashSet) where
import Data.HashSet

#else

module Reactive.Banana.Compat.Data.HashSet where

import Reactive.Banana.Compat.Data.Hashable

import qualified Data.List as List
import qualified Data.IntMap as Map

newtype HashSet k = S { unS :: Map.IntMap [k] } deriving (Show)

empty :: HashSet k
empty = S Map.empty

member :: (Eq k, Hashable k) => k -> HashSet k -> Bool
member k = (== Just True) . fmap (List.elem k) . Map.lookup (hash k) . unS

insert :: (Eq k, Hashable k) => k -> HashSet k -> HashSet k
insert k = S . Map.insertWith helper (hash k) [k] . unS
    where
    helper old _ = k : filter (/= k) old

delete :: (Eq k, Hashable k) => k -> HashSet k -> HashSet k
delete k = S . Map.delete (hash k) . unS

toList :: HashSet k -> [k]
toList = concat . map snd . Map.toList . unS

#endif
