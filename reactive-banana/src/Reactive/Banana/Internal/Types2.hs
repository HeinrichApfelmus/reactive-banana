{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Internal.Types2 (
    -- | Primitive types.
    Event (..), Behavior (..), Moment (..)
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix

import qualified Reactive.Banana.Internal.EventBehavior1 as Prim


{-| @Event t a@ represents a stream of events as they occur in time.
Semantically, you can think of @Event t a@ as an infinite list of values
that are tagged with their corresponding time of occurence,

> type Event t a = [(Time,a)]
-}
newtype Event t a = E { unE :: Prim.Event [a] }

{-| @Behavior t a@ represents a value that varies in time. Think of it as

> type Behavior t a = Time -> a
-}
newtype Behavior t a = B { unB :: Prim.Behavior a }

{-| The 'Moment' monad denotes a value at a particular /moment in time/.

This monad is not very interesting, it is mainly used for book-keeping.
In particular, the type parameter @t@ is used
to disallow various unhealthy programs.

This monad is also used to describe event networks
in the "Reactive.Banana.Frameworks" module.
This only happens when the type parameter @t@
is constrained by the 'Frameworks' class.

To be precise, an expression of type @Moment t a@ denotes
a value of type @a@ that is observed at a moment in time
which is indicated by the type parameter @t@.

-}
newtype Moment t a = M { unM :: Prim.Moment a }


-- boilerplate class instances
instance Monad (Moment t) where
    return  = M . return
    m >>= g = M $ unM m >>= unM . g

instance Applicative (Moment t) where
    pure    = M . pure
    f <*> a = M $ unM f <*> unM a

instance MonadFix (Moment t) where   mfix f  = M $ mfix (unM . f)
instance Functor  (Moment t) where   fmap f  = M . fmap f . unM
