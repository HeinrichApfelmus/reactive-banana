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

-- | Class constraint on the type parameter @t@ of the 'Moment' monad.
-- 
-- Indicates that we can add input and output to an event network.
class Frameworks t

-- | Data type for discharging the 'Frameworks' constraint.
data FrameworksD

instance Frameworks (FrameworksD,t)