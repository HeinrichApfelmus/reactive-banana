{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Analogous to @IntMap@, a @UniqueMap@ is a collection of values, each indexed by a key with a unique identity.
module Reactive.Banana.Prim.Low.UniqueMap
  ( UniqueMap,

    -- * Basic construction
    empty,
    singleton,

    -- * Mapping
    map,

    -- * Querying
    size,
    isEmpty,
    lookup,
    keys,
    toList,

    -- * Folding
    foldl',

    -- * Deleting
    delete,

    -- * Filtering
    restrictKeys,

    -- * Inserting
    insert,
    adjust,
    upsert,

    -- * Combining
    union,
  )
where

import Data.Coerce (coerce)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Reactive.Banana.Prim.Low.HasUnique (HasUnique, U (..))
import Reactive.Banana.Prim.Low.UniqueSet (UniqueSet (..))
import Prelude hiding (lookup, map)

-- | A unique map.
newtype UniqueMap k v
  = UniqueMap (HashMap (U k) v)
  deriving newtype (Eq, Show)

-- | O(n). Get the number of elements in a unique map.
size :: forall k v. UniqueMap k v -> Int
size =
  coerce @(HashMap (U k) v -> Int) HashMap.size

-- | O(1). Get whether a unique map is empty.
isEmpty :: forall k v. UniqueMap k v -> Bool
isEmpty =
  coerce @(HashMap (U k) v -> Bool) HashMap.null

-- | O(1). An empty unique map.
empty :: UniqueMap k v
empty =
  UniqueMap HashMap.empty

singleton :: forall k v. (HasUnique k) => k -> v -> UniqueMap k v
singleton =
  coerce @(U k -> v -> HashMap (U k) v) HashMap.singleton

map :: forall k v w. (v -> w) -> UniqueMap k v -> UniqueMap k w
map =
  coerce @((v -> w) -> HashMap (U k) v -> HashMap (U k) w) HashMap.map

-- | Look up a value in a unique map by key.
lookup :: forall k v. (HasUnique k) => k -> UniqueMap k v -> Maybe v
lookup =
  coerce @(U k -> HashMap (U k) v -> Maybe v) HashMap.lookup

-- | O(n). Get the keys of a unique map in arbitrary order.
keys :: forall k v. UniqueMap k v -> [k]
keys =
  coerce @(HashMap (U k) v -> [U k]) HashMap.keys

-- | O(n). Get the keys and values of a unique map in arbitrary order.
toList :: forall k v. UniqueMap k v -> [(k, v)]
toList =
  coerce @(HashMap (U k) v -> [(U k, v)]) HashMap.toList

foldl' :: forall k v x. (x -> v -> x) -> x -> UniqueMap k v -> x
foldl' =
  coerce @((x -> v -> x) -> x -> HashMap (U k) v -> x) HashMap.foldl'

-- | Delete a key-value mapping in a unique map.
delete :: forall k v. (HasUnique k) => k -> UniqueMap k v -> UniqueMap k v
delete =
  coerce @(U k -> HashMap (U k) v -> HashMap (U k) v) HashMap.delete

-- | Restrict a unique map's keys to only those in a set.
restrictKeys :: (HasUnique k) => UniqueSet k -> UniqueMap k v -> UniqueMap k v
restrictKeys (UniqueSet ks) (UniqueMap m) =
  UniqueMap (HashMap.intersection m (HashSet.toMap ks))

-- | Insert a key-value mapping in a unique map, overwriting whatever might already be there.
insert :: forall k v. (HasUnique k) => k -> v -> UniqueMap k v -> UniqueMap k v
insert =
  coerce @(U k -> v -> HashMap (U k) v -> HashMap (U k) v) HashMap.insert

-- | Adjust an existing key-value mapping in a unique map.
adjust :: forall k v. (HasUnique k) => k -> (v -> v) -> UniqueMap k v -> UniqueMap k v
adjust k f =
  coerce @(HashMap (U k) v -> HashMap (U k) v) (HashMap.adjust f (U k))

-- | Insert a new key-value mapping in a unique map, or adjust the one that's already there.
upsert :: (HasUnique k) => k -> v -> (v -> v) -> UniqueMap k v -> UniqueMap k v
upsert k v f (UniqueMap m) =
  UniqueMap (HashMap.alter g (U k) m)
  where
    g = \case
      Nothing -> Just v
      Just w -> Just $! f w

-- | Union two unique maps together.
union :: forall k v. (HasUnique k) => (k -> v -> v -> v) -> UniqueMap k v -> UniqueMap k v -> UniqueMap k v
union =
  coerce @((U k -> v -> v -> v) -> HashMap (U k) v -> HashMap (U k) v -> HashMap (U k) v) HashMap.unionWithKey
