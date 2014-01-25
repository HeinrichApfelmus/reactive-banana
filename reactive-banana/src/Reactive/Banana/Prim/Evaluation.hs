{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecordWildCards #-}
module Reactive.Banana.Prim.Evaluation where

import qualified Control.Exception      as Strict (evaluate)
import           Control.Monad                    (foldM)
import           Control.Monad                    (join)
import           Control.Monad.IO.Class
import qualified Data.PQueue.Prio.Min   as Q
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
step roots state1 = {-# SCC step #-} do
    let time1    = nTime state1
        outputs1 = nOutputs state1
    
    -- evaluate pulses
    ((_, latchUpdates, output), topologyUpdates, os)
            <- runBuildIO time1
            $  runEvalP
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
    go q1 = {-# SCC go #-} case Q.minView q1 of
        Nothing         -> liftIO (putStrLn "done") >> return ()
        Just (node, q2) -> do
            children <- evaluateNode node
            q2       <- insertNodes children q1
            go q2

-- | Recalculate a given node and return all children nodes
-- that need to evaluated subsequently.
evaluateNode :: SomeNode -> EvalP [SomeNode]
evaluateNode (P p) = do
    Pulse{..} <- get p
    ma        <- _evalP
    modify p $ set valueP ma
    case ma of
        Nothing -> return []
        Just _  -> liftIO $ deRefWeaks _childrenP
evaluateNode (L lw) = do
    time           <- getTime
    LatchWrite{..} <- get lw
    mlatch         <- liftIO $ deRefWeak _latchLW -- retrieve destination latch
    case mlatch of
        Nothing    -> return ()
        Just latch -> do
            a <- _evalLW                    -- calculate new latch value
            liftIO $ Strict.evaluate a      -- force evaluation
            rememberLatchUpdate $           -- schedule value to be set later
                modify latch $ set seenL time . set valueL a
    return []
evaluateNode (O o) = do
    Output{..} <- get o
    m          <- _evalO                    -- calculate output action
    rememberOutput $ (_positionO, m)
    return []

-- | Insert a node into the queue.
insertNode :: SomeNode -> Queue SomeNode -> EvalP (Queue SomeNode)
insertNode node@(P p) q = do
    time      <- getTime
    Pulse{..} <- get p
    if time <= _seenP
        then return q       -- pulse has already been put into the queue once
        else do             -- pulse needs to be scheduled for evaluation
            modify p $ set seenP time
            return $ Q.insert _levelP node q
insertNode node q =         -- O and L nodes have only one parent, so
                            -- we can insert them at an arbitrary level
    return $ Q.insert ground node q

-- | Insert a list of children into the queue.
insertNodes :: [SomeNode] -> Queue SomeNode -> EvalP (Queue SomeNode)
insertNodes = flip $ foldM (flip insertNode)
