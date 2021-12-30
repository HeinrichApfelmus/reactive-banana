{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecordWildCards #-}
module Reactive.Banana.Prim.Evaluation (
    step
    ) where

import Control.Monad ( join )
import           Control.Monad.IO.Class
import qualified Control.Monad.Trans.RWSIO          as RWS
import qualified Data.PQueue.Prio.Min               as Q
import qualified Data.Vault.Lazy                    as Lazy
import           System.Mem.Weak

import qualified Reactive.Banana.Prim.OrderedBag as OB
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
step (inputs,pulses)
        Network{ nTime = time1
        , nOutputs = outputs1
        , nAlwaysP = Just alwaysP   -- we assume that this has been built already
        }
    = do

    -- evaluate pulses
    ((_, (latchUpdates, outputs)), topologyUpdates, os)
            <- runBuildIO (time1, alwaysP)
            $  runEvalP pulses
            $  evaluatePulses inputs

    doit latchUpdates                           -- update latch values from pulses
    doit topologyUpdates                        -- rearrange graph topology
    let actions :: [(Output, EvalO)]
        actions = OB.inOrder outputs outputs1   -- EvalO actions in proper order

        state2 :: Network
        state2  = Network
            { nTime    = next time1
            , nOutputs = OB.inserts outputs1 os
            , nAlwaysP = Just alwaysP
            }
    return (runEvalOs $ map snd actions, state2)
step _ Network{ nAlwaysP = Nothing } = error "step: step called when nAlwaysP is Nothing"

runEvalOs :: [EvalO] -> IO ()
runEvalOs = mapM_ join

{-----------------------------------------------------------------------------
    Traversal in dependency order
------------------------------------------------------------------------------}
-- | Update all pulses in the graph, starting from a given set of nodes
evaluatePulses :: [SomeNode] -> EvalP ()
evaluatePulses roots = wrapEvalP $ \r -> go r =<< insertNodes r roots Q.empty
    where
    go :: RWS.Tuple BuildR (EvalPW, BuildW) Lazy.Vault -> Queue SomeNode -> IO ()
    go r q =
        case ({-# SCC minView #-} Q.minView q) of
            Nothing         -> return ()
            Just (node, q)  -> do
                children <- unwrapEvalP r (evaluateNode node)
                q        <- insertNodes r children q
                go r q

-- | Recalculate a given node and return all children nodes
-- that need to evaluated subsequently.
evaluateNode :: SomeNode -> EvalP [SomeNode]
evaluateNode (P p) = {-# SCC evaluateNodeP #-} do
    Pulse{..} <- readRef p
    ma        <- _evalP
    writePulseP _keyP ma
    case ma of
        Nothing -> return []
        Just _  -> liftIO $ deRefWeaks _childrenP
evaluateNode (L lw) = {-# SCC evaluateNodeL #-} do
    time           <- askTime
    LatchWrite{..} <- readRef lw
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
evaluateNode (O o) = {-# SCC evaluateNodeO #-} do
    debug "evaluateNode O"
    Output{..} <- readRef o
    m          <- _evalO                    -- calculate output action
    rememberOutput (o,m)
    return []

-- | Insert nodes into the queue
insertNodes :: RWS.Tuple BuildR (EvalPW, BuildW) Lazy.Vault -> [SomeNode] -> Queue SomeNode -> IO (Queue SomeNode)
insertNodes (RWS.Tuple (time,_) _ _) = go
    where
    go :: [SomeNode] -> Queue SomeNode -> IO (Queue SomeNode)
    go []              q = return q
    go (node@(P p):xs) q = do
        Pulse{..} <- readRef p
        if time <= _seenP
            then go xs q        -- pulse has already been put into the queue once
            else do             -- pulse needs to be scheduled for evaluation
                put p $! (let p = Pulse{..} in p { _seenP = time })
                go xs (Q.insert _levelP node q)
    go (node:xs)      q = go xs (Q.insert ground node q)
            -- O and L nodes have only one parent, so
            -- we can insert them at an arbitrary level
