{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Internal.Types2 (
    -- | Primitive types.
    Event (..), Behavior (..), Moment (..)
    ) where

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

This monad is almost the trivial moment,
but the type parameter allows for some clever internal bookkeeping.

In particular, an expression of type @Moment t a@ denotes
a value of type @a@ that is observed at a moment in time
which is indicated by the type parameter @t@.
-}
newtype Moment t a = M { unM :: Prim.Moment a }

