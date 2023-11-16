module Reactive.Banana.Prim.Low.HasUnique
  ( HasUnique (getUnique),
    U (..),
  )
where

import Data.Hashable (Hashable (hashWithSalt))
import Data.Unique (Unique)

-- | The class of values with a unique identity.
class HasUnique k where
  getUnique :: k -> Unique

-- An internal newtype whose purpose is to translates external HasUnique instances to internal Eq+Hashable instances,
-- since the internal representation of a UniqueMap is a HashMap
--
-- This is all we use this type for; the rest is boilerplate newtype wrapping an unwrapping:
--
--     instance HasUnique k => Eq (U k)
--     instance HasUnique k => Hashable (U k)
newtype U k
  = U k

instance (HasUnique k) => Eq (U k) where
  U x == U y =
    getUnique x == getUnique y

instance (HasUnique k) => Hashable (U k) where
  hashWithSalt salt (U x) =
    hashWithSalt salt (getUnique x)
