{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE ExistentialQuantification #-}
module Reactive.Banana.Prim.Types where

import Control.Monad.Trans.RWS
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.IORef
import Data.Monoid
import System.Mem.Weak

{-----------------------------------------------------------------------------
    Network
------------------------------------------------------------------------------}
-- | A 'Network' represents the state of a pulse/latch network,
data Network = Network
    { nTime    :: Time         -- Current time.
    , nOutputs :: [Output]     -- Remember outputs to prevent garbage collection.
    }

type Inputs        = [SomeNode]
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
type Time  = Integer
-- | Priority used to keep track of declaration order for outputs.
type Position = Integer
-- | Priority used to determine evaluation order for pulses.
type Level = Integer

next :: Time -> Time
next = (+1)

ground :: Level
ground = 0

-- | 'IO' actions as a monoid with respect to sequencing.
newtype Action = Action { doit :: IO () }
instance Monoid Action where
    mempty = Action $ return ()
    (Action x) `mappend` (Action y) = Action (x >> y)

-- | Lens-like functionality.
newtype Setter b a = Setter { update :: (a -> a) -> (b -> b) }
set f x = update f (const x)

{-----------------------------------------------------------------------------
    Pulse and Latch
------------------------------------------------------------------------------}
type Pulse  a = IORef (Pulse' a)
data Pulse' a = Pulse
    { _seenP     :: Time             -- Timestamp of the current value.
    , _valueP    :: Maybe a          -- Current value.
    , _evalP     :: EvalP (Maybe a)  -- Calculate current value.
    , _childrenP :: [Weak SomeNode]  -- Weak references to child nodes.
    , _parentsP  :: [Weak SomeNode]  -- Weak reference to parent nodes.
    , _levelP    :: Level            -- Priority in evaluation order.
    }

type Latch  a = IORef (Latch' a)
data Latch' a = Latch
    { _seenL  :: Time                -- Timestamp for the current value.
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
    { _positionO :: Position
    , _evalO     :: EvalP EvalO
    }

data SomeNode
    = forall a. P (Pulse a)
    | L LatchWrite
    | O Output

-- Lenses for various parameters
seenP  = Setter $ \f s -> s { _seenP = f (_seenP s) }
seenL  = Setter $ \f s -> s { _seenL = f (_seenL s) }
valueP = Setter $ \f s -> s { _valueP = f (_valueP s) }
valueL = Setter $ \f s -> s { _valueL = f (_valueL s) }
parentsP = Setter $ \f s -> s { _parentsP = f (_parentsP s) }
childrenP = Setter $ \f s -> s { _childrenP = f (_childrenP s) }
levelP = Setter $ \f s -> s { _levelP = f (_levelP s) }

-- | Evaluation monads.
type EvalP = WriterT (EvalLW,[(Position, EvalO)]) Build
    -- writer : (latch updates, IO action)
type EvalL = ReaderT Time IO
type EvalO    = Future (IO ())
type Future   = IO
type EvalLW   = Action
