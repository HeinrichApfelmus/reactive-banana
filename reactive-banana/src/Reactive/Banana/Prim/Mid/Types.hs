{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.Mid.Types where

import Data.Hashable
    ( hashWithSalt )
import Data.Unique.Really
    ( Unique )
import Control.Monad.Trans.RWSIO
    ( RWSIOT )
import Control.Monad.Trans.ReaderWriterIO
    ( ReaderWriterIOT )
import Reactive.Banana.Prim.Low.OrderedBag
    ( OrderedBag )
import System.IO.Unsafe
    ( unsafePerformIO )
import System.Mem.Weak
    ( Weak )

import qualified Data.Vault.Lazy as Lazy
import qualified Reactive.Banana.Prim.Low.Ref as Ref
import qualified Reactive.Banana.Prim.Low.GraphGC as GraphGC

{-----------------------------------------------------------------------------
    Network
------------------------------------------------------------------------------}
-- | A 'Network' represents the state of a pulse/latch network,
data Network = Network
    { nTime           :: !Time                 -- Current time.
    , nOutputs        :: !(OrderedBag Output)  -- Remember outputs to prevent garbage collection.
    , nAlwaysP        :: !(Pulse ())   -- Pulse that always fires.
    , nGraphGC        :: Dependencies
    }

getSize :: Network -> IO Int
getSize = GraphGC.getSize . nGraphGC

type Dependencies  = GraphGC.GraphGC SomeNodeD
type Inputs        = ([SomeNode], Lazy.Vault)
type EvalNetwork a = Network -> IO (a, Network)
type Step          = EvalNetwork (IO ())

type Build  = ReaderWriterIOT BuildR BuildW IO
type BuildR = (Time, Pulse ())
    -- ( current time
    -- , pulse that always fires)
newtype BuildW = BuildW (DependencyChanges, [Output], Action, Maybe (Build ()))
    -- reader : current timestamp
    -- writer : ( actions that change the network topology
    --          , outputs to be added to the network
    --          , late IO actions
    --          , late build actions
    --          )

instance Semigroup BuildW where
    BuildW x <> BuildW y = BuildW (x <> y)

instance Monoid BuildW where
    mempty  = BuildW mempty
    mappend = (<>)

type BuildIO = Build

data DependencyChange parent child
    = InsertEdge parent child
    | ChangeParentTo child parent
type DependencyChanges = [DependencyChange SomeNode SomeNode]

{-----------------------------------------------------------------------------
    Synonyms
------------------------------------------------------------------------------}
-- | 'IO' actions as a monoid with respect to sequencing.
newtype Action = Action { doit :: IO () }
instance Semigroup Action where
    Action x <> Action y = Action (x >> y)
instance Monoid Action where
    mempty = Action $ return ()
    mappend = (<>)

{-----------------------------------------------------------------------------
    Pulse and Latch
------------------------------------------------------------------------------}
data Pulse a = Pulse
    { _key :: Lazy.Key (Maybe a) -- Key to retrieve pulse value from cache.
    , _nodeP :: SomeNode         -- Reference to its own node
    }

data PulseD a = PulseD
    { _keyP      :: Lazy.Key (Maybe a) -- Key to retrieve pulse from cache.
    , _evalP     :: EvalP (Maybe a)    -- Calculate current value.
    , _nameP     :: String             -- Name for debugging.
    }

instance Show (Pulse a) where
    show p = name <> " " <> show (hashWithSalt 0 $ _nodeP p)
      where
        name = case unsafePerformIO $ Ref.read $ _nodeP p of
              P pulseD -> _nameP pulseD
              _ -> ""

showUnique :: Unique -> String
showUnique = show . hashWithSalt 0

type Latch  a = Ref.Ref (LatchD a)
data LatchD a = Latch
    { _seenL  :: !Time               -- Timestamp for the current value. See Note [Timestamp]
    , _valueL :: a                   -- Current value.
    , _evalL  :: EvalL a             -- Recalculate current latch value.
    }

type LatchWrite = SomeNode
data LatchWriteD = forall a. LatchWriteD
    { _evalLW  :: EvalP a            -- Calculate value to write.
    , _latchLW :: Weak (Latch a)     -- Destination 'Latch' to write to.
    }

type Output  = SomeNode
data OutputD = Output
    { _evalO     :: EvalP EvalO
    }

type SomeNode = Ref.Ref SomeNodeD
data SomeNodeD
    = forall a. P (PulseD a)
    | L LatchWriteD
    | O OutputD

{-# INLINE mkWeakNodeValue #-}
mkWeakNodeValue :: SomeNode -> v -> IO (Weak v)
mkWeakNodeValue x v = Ref.mkWeak x v Nothing

-- | Evaluation monads.
type EvalPW   = (EvalLW, [(Output, EvalO)])
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
printNode node = do
    someNode <- Ref.read node
    pure $ case someNode of
        P p -> _nameP p
        L _ -> "L"
        O _ -> "O"

-- | Show the graph of the 'Network' in @graphviz@ dot file format.
printDot :: Network -> IO String
printDot = GraphGC.printDot format . nGraphGC
  where
    format u weakref = do
         mnode <- Ref.deRefWeak weakref
         ((showUnique u <> ": ") <>) <$> case mnode of
             Nothing -> pure "(x_x)"
             Just node -> printNode node

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

instance Semigroup Time where
    T x <> T y = T (max x y)

instance Monoid Time where
    mappend = (<>)
    mempty  = beginning

{-----------------------------------------------------------------------------
    Notes
------------------------------------------------------------------------------}
{- Note [Timestamp]

The timestamp indicates the last time at which the latch has been written to.

    agesAgo   = The latch has never been written to.
    beginning = The latch has been written to before everything starts.

The second description is ensured by the fact that the network
writes timestamps that begin at time `next beginning`.

-}
