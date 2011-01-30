{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-----------------------------------------------------------------------------
    Reactive Banana
    A tiny library for functional reactive programming.
    
    Class instances for convenience.
    This is an extra module because it uses extensions.

------------------------------------------------------------------------------}
module Reactive.Classes where

import Reactive.Core

    -- convenient type class for automatically
    -- selecting the right  accumulation  and  map  function by type
class EventSyntax b t where
    accumulate :: (b -> a -> t) -> b -> Event a -> Behavior b
    map        :: (a -> t) -> Event a -> Event b

instance EventSyntax b b where
    accumulate = accumulate'
    map        = fmap
instance EventSyntax b (Change b) where
    accumulate = accumulateChange
    map        = mapChange
instance EventSyntax b (IO b) where
    accumulate = accumulateIO
    map        = mapIO
instance EventSyntax b (IO (Change b)) where
    accumulate = accumulateIOChange
    map        = mapIOChange

{- Design Note:
    Should all the  map  functions really get the same name?
    The variants involving the  Change  type are more akin
    to a  filter , and thus different from lists,
    but I think the similarity to  accumulate  justifies that.
-}
