{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE ExistentialQuantification #-}
module Reactive.Banana.Internal0.Types where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.RWS.Lazy
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Functor.Identity

import Reactive.Banana.Frameworks.AddHandler

import Data.Hashable
import Data.Unique.Really
import qualified Data.Vault.Strict as Vault
import qualified Data.Vault.Lazy   as Vault.Lazy

import qualified Reactive.Banana.Internal0.DependencyGraph as Deps

import System.IO.Unsafe

import Debug.Trace

type Deps = Deps.Deps

debug s m = trace s m

{-----------------------------------------------------------------------------
    Graph data type
------------------------------------------------------------------------------}
-- A 'Graph' represents the connections between pulses and events.
data Graph = Graph
    { grOutputs :: [Output]             -- output actions
        
    , grCache   :: Vault.Lazy.Vault     -- cache for initialization
    
    , grDeps    :: Deps SomeNode        -- dependency information
    }

type Values = Vault.Vault
type Key    = Vault.Key
type Inputs = (Values, [SomeNode])
type Output     = Pulse (IO ())
type Reactimate = IO ()

emptyGraph :: Graph
emptyGraph = Graph
    { grCache   = Vault.Lazy.empty
    , grDeps    = Deps.empty
    , grOutputs = []
    }

-- | A 'GraphState' represents the state of a pulse/latch network,
-- which consists of a 'Graph' and the values of all latches in the graph.
data GraphState = GraphState
    { gsGraph       :: Graph
    , gsLatchValues :: Values
    }

emptyState :: GraphState
emptyState = GraphState emptyGraph Vault.empty

type EvalGraph a = GraphState -> IO (a, GraphState)

{-----------------------------------------------------------------------------
    Pulse and Latch types
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
    { evaluateP :: EvalP ()
    , getValueP :: Values -> Maybe a
    , uidP      :: Unique
    }

data Latch a = Latch
    { evaluateL :: EvalL ()
    , getValueL :: Values -> a
    , uidL      :: Unique
    }


-- | Event that always fires whenever the network processes events.
alwaysP :: Pulse ()
alwaysP = debug "alwaysP" $ unsafePerformIO $ do
    uid <- newUnique
    return $ Pulse
        { evaluateP = return ()
        , getValueP = return $ Just ()
        , uidP      = uid
        }


-- | Existential quantification over Pulse and Latch
-- for dependency tracking.
data SomeNode = forall a. P (Pulse a) | forall a. L (Latch a)

instance Eq SomeNode where
    (L x) == (L y)  =  uidL x == uidL y
    (P x) == (P y)  =  uidP x == uidP y
    _     == _      =  False

instance Hashable SomeNode where
    hashWithSalt s (P p) = hashWithSalt s $ uidP p
    hashWithSalt s (L l) = hashWithSalt s $ uidL l


{-----------------------------------------------------------------------------
    Monads
------------------------------------------------------------------------------}

-- | The 'EvalP' monad is used to evaluate pulses.
type EvalP = RWST Values () Values BuildIO
    -- read : future latch values
    -- state: current pulse values

-- | The 'EvalL' monad is used to evaluate latches.
type EvalL = RWS Values () Values
    -- read  : current pulse values
    -- state : current latch values


-- The 'Build' monad is used to change the graph, for example to
-- * add nodes
-- * change dependencies
-- * add inputs or outputs
type BuildT  = RWST () BuildConf GraphState
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




