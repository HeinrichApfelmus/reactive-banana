{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Analogous to @IntMap@, a @UniqueMap@ is a collection of values, each indexed by a key with a unique identity.
module Reactive.Banana.Prim.Low.UniqueMap
  ( -- * Unique map
    UniqueMap,
    empty,

    -- * Querying
    size,
    lookup,
    keys,

    -- * Inserting
    adjust,
    upsert,

    -- * Deleting
    delete,

    -- * Combining
    union,
  )
where

import Data.Coerce (coerce)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Reactive.Banana.Prim.Low.HasUnique (HasUnique, U (..))
import Prelude hiding (lookup)

-- | A unique map.
newtype UniqueMap k v
  = UniqueMap (HashMap (U k) v)

-- | O(n). Get the number of elements in a unique map.
size :: forall k v. UniqueMap k v -> Int
size =
  coerce @(HashMap (U k) v -> Int) HashMap.size

-- | O(1). An empty unique map.
empty :: UniqueMap k v
empty =
  UniqueMap HashMap.empty

-- | Look up a value in a unique map by key.
lookup :: (HasUnique k) => k -> UniqueMap k v -> Maybe v
lookup k (UniqueMap m) =
  HashMap.lookup (U k) m

-- | O(n). Get the keys of a unique map in arbitrary order.
keys :: forall k v. UniqueMap k v -> [k]
keys =
  coerce @(HashMap (U k) v -> [U k]) HashMap.keys

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

delete :: forall k v. (HasUnique k) => k -> UniqueMap k v -> UniqueMap k v
delete =
  coerce @(U k -> HashMap (U k) v -> HashMap (U k) v) HashMap.delete

-- | Union two unique maps together.
union :: forall k v. (HasUnique k) => (k -> v -> v -> v) -> UniqueMap k v -> UniqueMap k v -> UniqueMap k v
union =
  coerce @((U k -> v -> v -> v) -> HashMap (U k) v -> HashMap (U k) v -> HashMap (U k) v) HashMap.unionWithKey
