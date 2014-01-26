{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE ExistentialQuantification, NamedFieldPuns #-}
module Reactive.Banana.Prim.Types where

import Control.Monad.Trans.RWS
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.Functor
import Data.IORef
import Data.Monoid
import System.Mem.Weak
import qualified Data.Vault.Lazy as Lazy

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

type Build = RWST Time (Action,Action,[Output]) () IO
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
type Pulse  a = IORef (Pulse' a)
data Pulse' a = Pulse
    { _keyP      :: Lazy.Key (Maybe a) -- Key to retrieve pulse from cache.
    , _seenP     :: !Time              -- See note [Timestamp].
    , _evalP     :: EvalP (Maybe a)    -- Calculate current value.
    , _childrenP :: [Weak SomeNode]    -- Weak references to child nodes.
    , _parentsP  :: [Weak SomeNode]    -- Weak reference to parent nodes.
    , _levelP    :: !Level             -- Priority in evaluation order.
    , _nameP     :: String             -- Name for debugging.
    }

type Latch  a = IORef (Latch' a)
data Latch' a = Latch
    { _seenL  :: !Time               -- Timestamp for the current value.
    , _valueL :: a                   -- Current value.
    , _evalL  :: EvalL a             -- Recalculate current latch value.
    }
type LatchWrite = IORef LatchWrite'
data LatchWrite' = forall a. LatchWrite
    { _evalLW  :: EvalP a            -- Calculate value to write.
    , _latchLW :: Weak (Latch a)     -- Destination 'Latch' to write to.
    }

type Output  = IORef Output'
data Output' = Output
    { _positionO :: !Position
    , _evalO     :: EvalP EvalO
    }

data SomeNode
    = forall a. P (Pulse a)
    | L LatchWrite
    | O Output

-- Lenses for various parameters
seenP  = Lens _seenP  (\a s -> s { _seenP = a })
seenL  = Lens _seenL  (\a s -> s { _seenL = a })
valueL = Lens _valueL (\a s -> s { _valueL = a })
parentsP  = Lens _parentsP (\a s -> s { _parentsP = a })
childrenP = Lens _childrenP (\a s -> s { _childrenP = a })
levelP = Lens _levelP (\a s -> s { _levelP = a })

-- | Evaluation monads.
type EvalP    = RWST () (EvalLW,[(Position, EvalO)]) Lazy.Vault Build
    -- writer : (latch updates, IO action)
    -- state  : pulse values
type EvalL    = ReaderT Time IO
type EvalO    = Future (IO ())
type Future   = IO
type EvalLW   = Action

{-----------------------------------------------------------------------------
    Show functions for debugging
------------------------------------------------------------------------------}
printNode :: SomeNode -> IO String
printNode (P p) = _nameP <$> readIORef p
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
