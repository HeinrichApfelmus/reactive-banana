{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE ExistentialQuantification #-}
module Reactive.Banana.Prim.Types where

import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.Lazy
import Data.Functor.Identity
import Data.Monoid

import           Data.Hashable
import           Data.Unique.Really
import qualified Data.Vault.Strict  as Strict
import qualified Data.Vault.Lazy    as Lazy

import           Reactive.Banana.Prim.Cached
import qualified Reactive.Banana.Prim.Dependencies as Deps

type Deps = Deps.Deps

{-----------------------------------------------------------------------------
    Graph
------------------------------------------------------------------------------}
-- | A 'Graph' represents the connections between pulses and events.
data Graph = Graph
    { grDeps    :: Deps SomeNode   -- dependency information
    , grCache   :: Lazy.Vault      -- cache for the monad
    }

-- | A 'Network' represents the state of a pulse/latch network,
-- which consists of a 'Graph' and the values of all accumulated latches
-- in the network.
data Network = Network
    { nGraph       :: Graph
    , nLatchValues :: Strict.Vault
    }

type Inputs        = (Strict.Vault, [SomeNode])
type EvalNetwork a = Network -> IO (a, Network)
type Step          = EvalNetwork (IO ())

-- | Lenses for the 'Graph' and the 'Network' type
updateGraph       f = \s -> s { nGraph       = f (nGraph s) }
updateLatchValues f = \s -> s { nLatchValues = f (nLatchValues s) }
updateDeps        f = \s -> s { grDeps       = f (grDeps s) }
updateCache       f = \s -> s { grCache      = f (grCache s) }

emptyGraph :: Graph
emptyGraph = Graph
    { grDeps    = Deps.empty
    , grCache   = Lazy.empty
    }

-- | The 'Network' that contains no pulses or latches.
emptyNetwork :: Network
emptyNetwork = Network emptyGraph Strict.empty


-- The 'Build' monad is used to change the graph, for example to
-- * add nodes
-- * change dependencies
-- * add inputs or outputs
type BuildT  = RWST () BuildConf Network
type Build   = BuildT Identity 
type BuildIO = BuildT IO

type BuildConf = [IO ()] -- liftIOLater

{- Note [BuildT]

It is very convenient to be able to perform some IO functions
while (re)building a network graph. At the same time,
we need a good  MonadFix  instance to build recursive networks.
These requirements clash, so the solution is to split the types
into a pure variant and IO variant, the former having a good
MonadFix  instance while the latter can do arbitrary IO.

-}

{-----------------------------------------------------------------------------
    Pulse and Latch
------------------------------------------------------------------------------}
{-
    evaluateL/P
        calculates the next value and makes sure that it's cached
    getValueL/P
        retrieves the current value
    uidL/P
        used for dependency tracking and evaluation order
-}

data Pulse a = Pulse
    { evaluateP :: EvalP Deps.Continue
    , getValueP :: Strict.Vault -> Maybe a
    , uidP      :: Unique
    }

data Latch a = Latch
    { getValueL   :: Strict.Vault -> a
    }

data LatchWrite = LatchWrite
    { evaluateL :: EvalP EvalL
    , uidL      :: Unique
    }

data Output = Output
    { evaluateO :: EvalP EvalO
    , uidO      :: Unique
    }

type EvalP = RWST () (EvalL, [EvalO]) Strict.Vault BuildIO
    -- state: current pulse values
    -- write: (update of latch values, output actions)

type EvalL = Endo Strict.Vault
type EvalO = Strict.Vault -> IO ()

-- | Existential quantification for dependency tracking
data SomeNode
    = forall a. P (Pulse a)
    | L LatchWrite
    | O Output

instance Eq SomeNode where
    (P x) == (P y)  =  uidP x == uidP y
    (L x) == (L y)  =  uidL x == uidL y
    (O x) == (O y)  =  uidO x == uidO y
    _     == _      =  False

instance Hashable SomeNode where
    hashWithSalt s (P p) = hashWithSalt s $ uidP p
    hashWithSalt s (L p) = hashWithSalt s $ uidL p
    hashWithSalt s (O p) = hashWithSalt s $ uidO p

