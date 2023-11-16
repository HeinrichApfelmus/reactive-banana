{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Analogous to @IntSet@, a @UniqueSet@ is a collection of values, each with a unique identity.
module Reactive.Banana.Prim.Low.UniqueSet
  ( UniqueSet (..), -- Constructor exposed for use in UniqueMap

    -- * Basic construction
    empty,
    fromList,

    -- * Querying
    member,

    -- * Inserting
    insert,
  )
where

import Data.Coerce (coerce)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Reactive.Banana.Prim.Low.HasUnique (HasUnique, U (..))

-- | A unique set.
newtype UniqueSet v
  = UniqueSet (HashSet (U v))

empty :: UniqueSet v
empty =
  UniqueSet HashSet.empty

fromList :: forall v. (HasUnique v) => [v] -> UniqueSet v
fromList =
  coerce @([U v] -> HashSet (U v)) HashSet.fromList

member :: forall v. (HasUnique v) => v -> UniqueSet v -> Bool
member =
  coerce @(U v -> HashSet (U v) -> Bool) HashSet.member

insert :: forall v. (HasUnique v) => v -> UniqueSet v -> UniqueSet v
insert =
  coerce @(U v -> HashSet (U v) -> HashSet (U v)) HashSet.insert
