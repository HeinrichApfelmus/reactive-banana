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
import Control.Monad.IO.Class
import Control.Monad.Fix
import Data.String (IsString(..))

import qualified Reactive.Banana.Internal.Combinators as Prim

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}

{-| @Event a@ represents a stream of events as they occur in time.
Semantically, you can think of @Event a@ as an infinite list of values
that are tagged with their corresponding time of occurrence,

> type Event a = [(Time,a)]

Each pair is called an /event occurrence/.
Note that within a single event stream,
no two event occurrences may happen at the same time.

<<doc/frp-event.png>>
-}
newtype Event a = E { unE :: Prim.Event a }
-- Invariant: The empty list `[]` never occurs as event value.

-- | The function 'fmap' applies a function @f@ to every value.
-- Semantically,
--
-- > fmap :: (a -> b) -> Event a -> Event b
-- > fmap f e = [(time, f a) | (time, a) <- e]
instance Functor Event where
    fmap f = E . Prim.mapE f . unE

-- | The combinator '<>' merges two event streams of the same type.
-- In case of simultaneous occurrences,
-- the events are combined with the underlying 'Semigroup' operation.
-- Semantically,
--
-- > (<>) :: Event a -> Event a -> Event a
-- > (<>) ex ey = unionWith (<>) ex ey
instance Semigroup a => Semigroup (Event a) where
    x <> y = E $ Prim.mergeWith id id (<>) (unE x) (unE y)

-- | The combinator 'mempty' represents an event that never occurs.
-- It is a synonym,
--
-- > mempty :: Event a
-- > mempty = never
instance Semigroup a => Monoid (Event a) where
    mempty  = E Prim.never
    mappend = (<>)


{-| @Behavior a@ represents a value that varies in time.
Semantically, you can think of it as a function

> type Behavior a = Time -> a

<<doc/frp-behavior.png>>
-}
newtype Behavior a = B { unB :: Prim.Behavior a }

-- | The function 'pure' returns a value that is constant in time. Semantically,
--
-- > pure     :: a -> Behavior a
-- > pure x    = \time -> x
--
-- The combinator '<*>' applies a time-varying function to a time-varying value.
--
-- > (<*>)    :: Behavior (a -> b) -> Behavior a -> Behavior b
-- > fx <*> bx = \time -> fx time $ bx time
instance Applicative Behavior where
    pure x    = B $ Prim.pureB x
    bf <*> bx = B $ Prim.applyB (unB bf) (unB bx)

-- | The function 'fmap' applies a function @f@ at every point in time.
-- Semantically,
--
-- > fmap :: (a -> b) -> Behavior a -> Behavior b
-- > fmap f b = \time -> f (b time)
instance Functor Behavior where
    fmap = liftA

instance Semigroup a => Semigroup (Behavior a) where
  (<>) = liftA2 (<>)

instance (Semigroup a, Monoid a) => Monoid (Behavior a) where
  mempty = pure mempty
  mappend = (<>)

instance Num a => Num (Behavior a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger
    negate = fmap negate

instance Fractional a => Fractional (Behavior a) where
    (/) = liftA2 (/)
    fromRational = pure . fromRational
    recip = fmap recip

instance Floating a => Floating (Behavior a) where
    (**) = liftA2 (**)
    acos = fmap acos
    acosh = fmap acosh
    asin = fmap asin
    asinh = fmap asinh
    atan = fmap atan
    atanh = fmap atanh
    cos = fmap cos
    cosh = fmap cosh
    exp = fmap exp
    log = fmap log
    logBase = liftA2 logBase
    pi = pure pi
    sin = fmap sin
    sinh = fmap sinh
    sqrt = fmap sqrt

instance IsString a => IsString (Behavior a) where
    fromString = pure . fromString

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
at one particular moment in time. Semantically, it is a reader monad

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
class MonadFix m => MonadMoment m where
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

instance Semigroup a => Semigroup (Moment a) where
    (<>) = liftA2 (<>)
instance Monoid a => Monoid (Moment a) where
    mempty = pure mempty


instance Functor MomentIO where fmap f = MIO . fmap f . unMIO
instance Monad MomentIO where
    return  = MIO . return
    m >>= g = MIO $ unMIO m >>= unMIO . g
instance Applicative MomentIO where
    pure    = MIO . pure
    f <*> a = MIO $ unMIO f <*> unMIO a
instance MonadFix MomentIO where mfix f = MIO $ mfix (unMIO . f)

instance Semigroup a => Semigroup (MomentIO a) where
    (<>) = liftA2 (<>)
instance Monoid a => Monoid (MomentIO a) where
    mempty = pure mempty
