{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Analogous to @IntSet@, a @UniqueSet@ is a collection of values, each with a unique identity.
module Reactive.Banana.Prim.Low.UniqueSet
  ( UniqueSet (..), -- Constructor exposed for use in UniqueMap
    fromList,
  )
where

import Data.Coerce (coerce)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Reactive.Banana.Prim.Low.HasUnique (HasUnique, U (..))

-- | A unique set.
newtype UniqueSet v
  = UniqueSet (HashSet (U v))

fromList :: forall v. (HasUnique v) => [v] -> UniqueSet v
fromList =
  coerce @([U v] -> HashSet (U v)) HashSet.fromList
