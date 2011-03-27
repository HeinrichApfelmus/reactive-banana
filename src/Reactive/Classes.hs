{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-----------------------------------------------------------------------------
    Reactive-Banana
------------------------------------------------------------------------------}
module Reactive.Classes (
    -- $doc
    ReactiveSyntax(..)
    ) where

import Reactive.Core

{-$doc
This module provides a syntactically convenient 'accumulate' function.
This is an extra module because it uses type class extensions.
-}

-- | Convenient type class for automatically
-- selecting the right 'accumulate' function by type.
class ReactiveSyntax b t where
    accumulate :: (a -> b -> t) -> b -> Event a -> Behavior b

instance ReactiveSyntax b b where
    accumulate = accumulate'
instance ReactiveSyntax b (Change b) where
    accumulate = accumulateChange
instance ReactiveSyntax b (IO b) where
    accumulate = accumulateIO
instance ReactiveSyntax b (IO (Change b)) where
    accumulate = accumulateIOChange
