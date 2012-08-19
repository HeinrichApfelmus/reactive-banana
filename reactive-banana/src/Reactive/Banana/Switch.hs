{-----------------------------------------------------------------------------
    Reactive Banana
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types, ScopedTypeVariables, FlexibleInstances #-}

module Reactive.Banana.Switch (
    -- * Synopsis
    -- | Dynamic event switching.
    
    -- * Moment monad
    Moment, AnyMoment, anyMoment, now,
    
    -- * Dynamic event switching
    trimE, trimB,
    switchE, switchB,
    observeE, valueB,
    
    -- * Identity Functor
    Identity(..),
    ) where

import qualified Reactive.Banana.Internal.EventBehavior1 as Prim
import Reactive.Banana.Internal.Types2

{-----------------------------------------------------------------------------
    Constant
------------------------------------------------------------------------------}
-- | Identity functor with a dummy argument.
-- Unlike 'Data.Functor.Constant',
-- this functor is constant in the /second/ argument.

data Identity t a = Identity { getIdentity :: a }

instance Functor (Identity t) where
    fmap f (Identity a) = Identity (f a)

{-----------------------------------------------------------------------------
    Moment
------------------------------------------------------------------------------}
-- | Value present at any/every moment in time.
newtype AnyMoment f a = AnyMoment { now :: forall t. Moment t (f t a) }

instance Monad (AnyMoment Identity) where
    return x = AnyMoment $ return (Identity x)
    (AnyMoment m) >>= g = AnyMoment $ m >>= \(Identity x) -> now (g x)

anyMoment :: (forall t. Moment t (f t a)) -> AnyMoment f a
anyMoment = AnyMoment

{-----------------------------------------------------------------------------
    Dynamic event switching
------------------------------------------------------------------------------}
-- | Trim an 'Event' to a variable start time.
trimE :: Event t a -> Moment t (AnyMoment Event a)
trimE = M . fmap (\x -> AnyMoment (M $ fmap E x)) . Prim.trimE . unE

-- | Trim a 'Behavior' to a variable start time.
trimB :: Behavior t a -> Moment t (AnyMoment Behavior a)
trimB = M . fmap (\x -> AnyMoment (M $ fmap B x)) . Prim.trimB . unB

-- | Observe a value at those moments in time where
-- event occurrences happen.
observeE :: Event t (AnyMoment Identity a) -> Event t a
observeE = undefined

-- | Value of the 'Behavior' at moment @t@.
valueB :: Behavior t a -> Moment t a
valueB = M . Prim.initialB . unB

-- | Dynamically switch between 'Event'.
switchE
    :: forall t a. Event t (AnyMoment Event a)
    -> Event t a
switchE = undefined

-- | Dynamically switch between 'Behavior'.
switchB
    :: forall t a. Behavior t a
    -> Event t (AnyMoment Behavior a)
    -> Behavior t a
switchB = undefined
