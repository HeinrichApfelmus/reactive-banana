{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Types (
    -- | Primitive types.
    Event (..), Behavior (..), Moment (..), Future(..)
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix

import qualified Reactive.Banana.Internal.Combinators as Prim
import           Reactive.Banana.Internal.Phantom

{-| @Event a@ represents a stream of events as they occur in time.
Semantically, you can think of @Event a@ as an infinite list of values
that are tagged with their corresponding time of occurence,

> type Event a = [(Time,a)]

Each pair is called an /event occurrence/.
Note that within a single event stream,
no two event occurrences may happen at the same time.
-}
newtype Event a = E { unE :: Prim.Event a }
-- Invariant: The empty list `[]` never occurs as event value.

{-| @Behavior a@ represents a value that varies in time. Think of it as

> type Behavior a = Time -> a
-}
newtype Behavior a = B { unB :: Prim.Behavior a }

-- | The 'Future' monad is just a helper type for the 'changes' function.
-- 
-- A value of type @Future a@ is only available in the context
-- of a 'reactimate' but not during event processing.
newtype Future a = F { unF :: Prim.Future a }

-- boilerplate class instances
instance Functor Future where fmap f = F . fmap f . unF

instance Monad Future where
    return  = F . return
    m >>= g = F $ unF m >>= unF . g

instance Applicative Future where
    pure    = F . pure
    f <*> a = F $ unF f <*> unF a

{-| The 'Moment' monad denotes a value at a particular /moment in time/.
Think of it as a reader monad

> type Moment a = Time -> a

This monad is also used to describe event networks
in the "Reactive.Banana.Frameworks" module.
This only happens when the type parameter @t@
is constrained by the 'Frameworks' class.

To be precise, an expression of type @Moment t a@ denotes
a value of type @a@ that is observed at a moment in time
which is indicated by the type parameter @t@.

-}
newtype Moment a = M { unM :: Prim.Moment a }

-- boilerplate class instances
instance Functor Moment where fmap f = M . fmap f . unM

instance Monad Moment where
    return  = M . return
    m >>= g = M $ unM m >>= unM . g

instance Applicative Moment where
    pure    = M . pure
    f <*> a = M $ unM f <*> unM a

instance MonadFix Moment where mfix f = M $ mfix (unM . f)

{-
instance Frameworks t => MonadIO Moment where
    liftIO = M . Prim.liftIONow
-}