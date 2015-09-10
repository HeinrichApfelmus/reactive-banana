{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Types (
    -- | Primitive types.
    Event(..), Behavior(..),
    Moment(..), MomentIO(..), MonadMoment(..),
    Future(..),
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix

import qualified Reactive.Banana.Internal.Combinators as Prim

{-| @Event a@ represents a stream of events as they occur in time.
Semantically, you can think of @Event a@ as an infinite list of values
that are tagged with their corresponding time of occurence,

> type Event a = [(Time,a)]

Each pair is called an /event occurrence/.
Note that within a single event stream,
no two event occurrences may happen at the same time.

<<doc/frp-event.png>>
-}
newtype Event a = E { unE :: Prim.Event a }
-- Invariant: The empty list `[]` never occurs as event value.

{-| @Behavior a@ represents a value that varies in time.
Think of it as

> type Behavior a = Time -> a

<<doc/frp-behavior.png>>
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

{-| The 'Moment' monad denotes a /pure/ computation that happens
at one particular moment in time. Think of it as a reader monad

> type Moment a = Time -> a

When run, the argument tells the time at which this computation happens.

Note that in this context, /time/ really means to /logical time/.
Of course, every calculation on a computer takes some
amount of wall-clock time to complete.
Instead, what is meant here is the time as it relates to
'Event's and 'Behavior's.
We use the fiction that every calculation within the 'Moment'
monad takes zero /logical time/ to perform.
-}
newtype Moment a = M { unM :: Prim.Moment a }

{-| The 'MomentIO' monad is used to add inputs and outputs
to an event network.
-}
newtype MomentIO a = MIO { unMIO :: Prim.Moment a }

instance MonadIO MomentIO where liftIO = MIO . liftIO

{-| An instance of the 'MonadMoment' class denotes a computation
that happens at one particular moment in time.

Unlike the 'Moment' monad, it need not be pure anymore.
-}
class Monad m => MonadMoment m where
    liftMoment :: Moment a -> m a

instance MonadMoment Moment   where liftMoment = id
instance MonadMoment MomentIO where liftMoment = MIO . unM

-- boilerplate class instances
instance Functor Moment where fmap f = M . fmap f . unM
instance Monad Moment where
    return  = M . return
    m >>= g = M $ unM m >>= unM . g
instance Applicative Moment where
    pure    = M . pure
    f <*> a = M $ unM f <*> unM a
instance MonadFix Moment where mfix f = M $ mfix (unM . f)

instance Functor MomentIO where fmap f = MIO . fmap f . unMIO
instance Monad MomentIO where
    return  = MIO . return
    m >>= g = MIO $ unMIO m >>= unMIO . g
instance Applicative MomentIO where
    pure    = MIO . pure
    f <*> a = MIO $ unMIO f <*> unMIO a
instance MonadFix MomentIO where mfix f = MIO $ mfix (unMIO . f)


{-
instance Frameworks t => MonadIO Moment where
    liftIO = M . Prim.liftIONow
-}