{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecordWildCards #-}
module Reactive.Banana.Prim.Evaluation where

import qualified Control.Exception      as Strict (evaluate)
import           Control.Monad                    (foldM)
import           Control.Monad                    (join)
import           Control.Monad.IO.Class
import qualified Control.Monad.Trans.RWS    as RWS
import           Data.Maybe
import           Data.Functor
import qualified Data.PQueue.Prio.Min   as Q
import qualified Data.Vault.Lazy as Lazy
import           System.Mem.Weak

import           Reactive.Banana.Prim.Plumbing
import           Reactive.Banana.Prim.Types
import           Reactive.Banana.Prim.Util

type Queue = Q.MinPQueue Level

{-----------------------------------------------------------------------------
    Evaluation step
------------------------------------------------------------------------------}
-- | Evaluate all the pulses in the graph,
-- Rebuild the graph as necessary and update the latch values.
step :: Inputs -> Step
step (roots, pulses1) (Network time1 outputs1) = {-# SCC step #-} do
    -- evaluate pulses
    ((_, latchUpdates, output), topologyUpdates, os)
            <- runBuildIO time1
            $  runEvalP pulses1
            $  evaluatePulses roots
    
    doit latchUpdates           -- update latch values from pulses
    doit topologyUpdates        -- rearrange graph topology
    let actions = join output   -- output IO actions
        state2  = Network { nTime = next time1, nOutputs = os ++ outputs1 }
    return (actions, state2)

{-----------------------------------------------------------------------------
    Traversal in dependency order
------------------------------------------------------------------------------}
-- | Update all pulses in the graph, starting from a given set of nodes
evaluatePulses :: [SomeNode] -> EvalP ()
evaluatePulses roots = go =<< insertNodes roots Q.empty
    where
    go :: Queue SomeNode -> EvalP ()
    go q = {-# SCC go #-} case Q.minView q of
        Nothing         -> return ()
        Just (node, q) -> do
            children <- evaluateNode node
            q        <- insertNodes children q
            go q

-- | Recalculate a given node and return all children nodes
-- that need to evaluated subsequently.
evaluateNode :: SomeNode -> EvalP [SomeNode]
evaluateNode (P p) = {-# SCC evaluateNodeP #-} do
    Pulse{..} <- get p
    ma        <- _evalP
    writePulseP _keyP ma
    case ma of
        Nothing -> return []
        Just _  -> liftIO $ deRefWeaks _childrenP
evaluateNode (L lw) = {-# SCC evaluateNodeL #-} do
    time           <- getTime
    LatchWrite{..} <- get lw
    mlatch         <- liftIO $ deRefWeak _latchLW -- retrieve destination latch
    case mlatch of
        Nothing    -> return ()
        Just latch -> do
            a <- _evalLW                    -- calculate new latch value
            -- liftIO $ Strict.evaluate a      -- see Note [LatchStrictness]
            rememberLatchUpdate $           -- schedule value to be set later
                modify' latch $ \l ->
                    a `seq` l { _seenL = time, _valueL = a }
    return []
evaluateNode (O o) = {-# SCC evaluateNodeO #-}do
    Output{..} <- get o
    m          <- _evalO                    -- calculate output action
    rememberOutput $ (_positionO, m)
    return []

-- | Insert a node into the queue.
insertNode :: SomeNode -> Queue SomeNode -> EvalP (Queue SomeNode)
insertNode node@(P p) q = {-# SCC insertNode #-} do
    time      <- getTime
    Pulse{..} <- get p
    if time <= _seenP
        then return q       -- pulse has already been put into the queue once
        else do             -- pulse needs to be scheduled for evaluation
            -- the following code yields a more regular space profile
            -- and reduces entry count for  evaluateNodeP ??
            -- Apparently, that's because of garbage collection!
            put p $! (let p = Pulse{..} in p { _seenP = time })
            
            -- Compared to that, the following code does not work so well:
            -- What the heck?
            -- modify' p $ set seenP time
            return $ Q.insert _levelP node q
insertNode node q =         -- O and L nodes have only one parent, so
                            -- we can insert them at an arbitrary level
    return $ Q.insert ground node q

-- | Insert a list of children into the queue.
insertNodes :: [SomeNode] -> Queue SomeNode -> EvalP (Queue SomeNode)
insertNodes = flip $ foldM (flip insertNode)
