{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE ExistentialQuantification, NamedFieldPuns #-}
module Reactive.Banana.Prim.Types where

import           Control.Monad.Trans.RWSIO
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.ReaderWriterIO
import           Data.Functor
import           Data.Hashable
import           Data.Monoid
import qualified Data.Vault.Lazy                    as Lazy
import           Reactive.Banana.Prim.Util
import           System.Mem.Weak

{-----------------------------------------------------------------------------
    Network
------------------------------------------------------------------------------}
-- | A 'Network' represents the state of a pulse/latch network,
data Network = Network
    { nTime    :: !Time         -- Current time.
    , nOutputs :: ![Output]     -- Remember outputs to prevent garbage collection.
    }

instance Show Network where show = error "instance Show Network not implemented."

type Inputs        = ([SomeNode], Lazy.Vault)
type EvalNetwork a = Network -> IO (a, Network)
type Step          = EvalNetwork (IO ())

emptyNetwork = Network
    { nTime    = 0
    , nOutputs = []
    }

type Build  = ReaderWriterIOT BuildR BuildW IO
type BuildR = Time
type BuildW = (Action, Action, [Output])
    -- reader : current timestamp
    -- writer : (actions that change the network topology
    --          ,late IO actions
    --          ,outputs to be added to the network)
type BuildIO = Build

{-----------------------------------------------------------------------------
    Synonyms
------------------------------------------------------------------------------}
-- | Timestamp used to determine cache validity.
type Time  = Int
-- | Priority used to keep track of declaration order for outputs.
type Position = Int
-- | Priority used to determine evaluation order for pulses.
type Level = Int

next :: Time -> Time
next = (+1)

agesAgo :: Time
agesAgo = 0

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

-- Note: For efficiency reasons, we unroll the monad transformer stack.
-- type EvalP = RWST () Lazy.Vault EvalPW Build
type EvalP    = RWSIOT BuildR (EvalPW,BuildW) Lazy.Vault IO
    -- writer : (latch updates, IO action)
    -- state  : current pulse values

type EvalL    = ReaderT Time IO
type EvalO    = Future (IO ())
type Future   = IO
type EvalLW   = Action

{-----------------------------------------------------------------------------
    Show functions for debugging
------------------------------------------------------------------------------}
printNode :: SomeNode -> IO String
printNode (P p) = _nameP <$> readRef p
printNode (L l) = return "L"
printNode (O o) = return "O"

{-----------------------------------------------------------------------------
    Notes
------------------------------------------------------------------------------}
{- Note [Timestamp]

The time stamp indicates how recent the current value is.

During pulse evaluation, a time stamp equal to the current
time indicates that the pulse has already been evaluated in this phase.
For Latches, however, a time stamp equal to the current time indicates
that the latch needs to be updated after all pulses have been evaluated.

-}
