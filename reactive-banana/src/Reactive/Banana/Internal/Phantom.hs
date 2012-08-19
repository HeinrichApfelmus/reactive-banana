{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE EmptyDataDecls, FlexibleInstances #-}
module Reactive.Banana.Internal.Phantom (
    -- * Synopsis
    -- | Classes used to constrain the phantom type @t@ in the 'Moment' type.
    
    -- * Documentation
    Frameworks, FrameworksD,
    ) where

import Reactive.Banana.Internal.Types2

-- | Constraint indicating that we can set up connections
-- with external event-based frameworks.
class Frameworks t

-- | Data type for discharging the 'Frameworks' constraint.
data FrameworksD

instance Frameworks (FrameworksD,t)