{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE ExistentialQuantification, NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Reactive.Banana.Prim.Types where

import           Control.Monad.Trans.RWSIO
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.ReaderWriterIO
import           Data.Functor
import           Data.Hashable
import           Data.Monoid
import qualified Data.Vault.Lazy                    as Lazy
import           Reactive.Banana.Prim.Graph                 (Graph)
import           Reactive.Banana.Prim.Util
import           System.Mem.Weak
import           System.IO.Unsafe

{-----------------------------------------------------------------------------
    Network
------------------------------------------------------------------------------}
-- | A 'Network' represents the state of a pulse/latch network,
data Network = Network
    { nTime    :: !Time         -- Current time.
    , nOutputs :: ![Output]     -- Remember outputs to prevent garbage collection.
    , nAlwaysP :: Maybe (Pulse ()) -- Pulse that always fires.
    }

instance Show Network where show = error "instance Show Network not implemented."

type Inputs        = ([SomeNode], Lazy.Vault)
type EvalNetwork a = Network -> IO (a, Network)
type Step          = EvalNetwork (IO ())

emptyNetwork = Network
    { nTime    = next beginning
    , nOutputs = []
    , nAlwaysP = Nothing
    }

type Build  = ReaderWriterIOT BuildR BuildW IO
type BuildR = (Time, Pulse ())
    -- (current time, pulse that always fires)
type BuildW = (DependencyBuilder, Action, [Output])
    -- reader : current timestamp
    -- writer : (actions that change the network topology
    --          ,late IO actions
    --          ,outputs to be added to the network)
type BuildIO = Build

type DependencyBuilder = (Endo (Graph SomeNode), [(SomeNode, SomeNode)])

{-----------------------------------------------------------------------------
    Synonyms
------------------------------------------------------------------------------}
-- | Priority used to keep track of declaration order for outputs.
type Position = Int
-- | Priority used to determine evaluation order for pulses.
type Level = Int

ground :: Level
ground = 0

-- | 'IO' actions as a monoid with respect to sequencing.
newtype Action = Action { doit :: IO () }
instance Monoid Action where
    mempty = Action $ return ()
    (Action x) `mappend` (Action y) = Action (x >> y)

-- | Lens-like functionality.
data Lens s a = Lens (s -> a) (a -> s -> s)
set    (Lens _   set)   = set
update (Lens get set) f = \s -> set (f $ get s) s

{-----------------------------------------------------------------------------
    Pulse and Latch
------------------------------------------------------------------------------}
type Pulse  a = Ref (Pulse' a)
data Pulse' a = Pulse
    { _keyP      :: Lazy.Key (Maybe a) -- Key to retrieve pulse from cache.
    , _seenP     :: !Time              -- See note [Timestamp].
    , _evalP     :: EvalP (Maybe a)    -- Calculate current value.
    , _childrenP :: [Weak SomeNode]    -- Weak references to child nodes.
    , _parentsP  :: [Weak SomeNode]    -- Weak reference to parent nodes.
    , _levelP    :: !Level             -- Priority in evaluation order.
    , _nameP     :: String             -- Name for debugging.
    }

instance Show (Pulse a) where
    show p = _nameP (unsafePerformIO $ readRef p) ++ " " ++ show (hashWithSalt 0 p)

type Latch  a = Ref (Latch' a)
data Latch' a = Latch
    { _seenL  :: !Time               -- Timestamp for the current value.
    , _valueL :: a                   -- Current value.
    , _evalL  :: EvalL a             -- Recalculate current latch value.
    }
type LatchWrite = Ref LatchWrite'
data LatchWrite' = forall a. LatchWrite
    { _evalLW  :: EvalP a            -- Calculate value to write.
    , _latchLW :: Weak (Latch a)     -- Destination 'Latch' to write to.
    }

type Output  = Ref Output'
data Output' = Output
    { _positionO :: !Position
    , _evalO     :: EvalP EvalO
    }

data SomeNode
    = forall a. P (Pulse a)
    | L LatchWrite
    | O Output

instance Hashable SomeNode where
    hashWithSalt s (P x) = hashWithSalt s x
    hashWithSalt s (L x) = hashWithSalt s x
    hashWithSalt s (O x) = hashWithSalt s x

instance Eq SomeNode where
    (P x) == (P y) = equalRef x y
    (L x) == (L y) = equalRef x y
    (O x) == (O y) = equalRef x y

{-# INLINE mkWeakNodeValue #-}
mkWeakNodeValue :: SomeNode -> v -> IO (Weak v)
mkWeakNodeValue (P x) = mkWeakRefValue x
mkWeakNodeValue (L x) = mkWeakRefValue x
mkWeakNodeValue (O x) = mkWeakRefValue x

-- Lenses for various parameters
seenP  = Lens _seenP  (\a s -> s { _seenP = a })
seenL  = Lens _seenL  (\a s -> s { _seenL = a })
valueL = Lens _valueL (\a s -> s { _valueL = a })
parentsP  = Lens _parentsP (\a s -> s { _parentsP = a })
childrenP = Lens _childrenP (\a s -> s { _childrenP = a })
levelP = Lens _levelP (\a s -> s { _levelP = a })

-- | Evaluation monads.
type EvalPW   = (EvalLW, [(Position, EvalO)])
type EvalLW   = Action

type EvalO    = Future (IO ())
type Future   = IO

-- Note: For efficiency reasons, we unroll the monad transformer stack.
-- type EvalP = RWST () Lazy.Vault EvalPW Build
type EvalP    = RWSIOT BuildR (EvalPW,BuildW) Lazy.Vault IO
    -- writer : (latch updates, IO action)
    -- state  : current pulse values

-- Computation with a timestamp that indicates the last time it was performed.
type EvalL    = ReaderWriterIOT () Time IO

{-----------------------------------------------------------------------------
    Show functions for debugging
------------------------------------------------------------------------------}
printNode :: SomeNode -> IO String
printNode (P p) = _nameP <$> readRef p
printNode (L l) = return "L"
printNode (O o) = return "O"

{-----------------------------------------------------------------------------
    Time monoid
------------------------------------------------------------------------------}
-- | A timestamp local to this program run.
--
-- Useful e.g. for controlling cache validity.
newtype Time = T Integer deriving (Eq, Ord, Show, Read)

-- | Before the beginning of time. See Note [TimeStamp]
agesAgo :: Time
agesAgo = T (-1)

beginning :: Time
beginning = T 0

next :: Time -> Time
next (T n) = T (n+1)

instance Monoid Time where
    mappend (T x) (T y) = T (max x y)
    mempty              = beginning

{-----------------------------------------------------------------------------
    Notes
------------------------------------------------------------------------------}
{- Note [Timestamp]

The time stamp indicates how recent the current value is.

For Pulse:
During pulse evaluation, a time stamp equal to the current
time indicates that the pulse has already been evaluated in this phase.

For Latch:
The timestamp indicates the last time at which the latch has been written to.

    agesAgo   = The latch has never been written to.
    beginning = The latch has been written to before everything starts.

The second description is ensured by the fact that the network
writes timestamps that begin at time `next beginning`.

-}
