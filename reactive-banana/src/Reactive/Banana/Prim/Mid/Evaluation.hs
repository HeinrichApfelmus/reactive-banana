{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.Mid.Evaluation
    ( step
    , applyDependencyChanges
    ) where

import Control.Monad
    ( join )
import Control.Monad.IO.Class
    ( liftIO )

import qualified Reactive.Banana.Prim.Low.GraphGC as GraphGC
import qualified Reactive.Banana.Prim.Low.OrderedBag as OB
import qualified Reactive.Banana.Prim.Low.Ref as Ref
import           Reactive.Banana.Prim.Mid.Plumbing
import           Reactive.Banana.Prim.Mid.Types

{-----------------------------------------------------------------------------
    Evaluation step
------------------------------------------------------------------------------}
-- | Evaluate all the pulses in the graph,
-- Rebuild the graph as necessary and update the latch values.
step :: Inputs -> Step
step (inputs,pulses)
        Network{ nTime = time1
        , nOutputs = outputs1
        , nAlwaysP = alwaysP
        , nGraphGC
        }
    = do

    -- evaluate pulses
    ((_, (latchUpdates, outputs)), dependencyChanges, os)
            <- runBuildIO (time1, alwaysP)
            $  runEvalP pulses
            $  evaluatePulses inputs nGraphGC

    doit latchUpdates                          -- update latch values from pulses
    applyDependencyChanges dependencyChanges   -- rearrange graph topology
        nGraphGC
    GraphGC.removeGarbage nGraphGC             -- remove unreachable pulses
    let actions :: [(Output, EvalO)]
        actions = OB.inOrder outputs outputs1  -- EvalO actions in proper order

        state2 :: Network
        !state2 = Network
            { nTime    = next time1
            , nOutputs = OB.inserts outputs1 os
            , nAlwaysP = alwaysP
            , nGraphGC
            }
    return (runEvalOs $ map snd actions, state2)

runEvalOs :: [EvalO] -> IO ()
runEvalOs = mapM_ join

{-----------------------------------------------------------------------------
    Dependency changes
------------------------------------------------------------------------------}
-- | Apply all dependency changes to the 'GraphGC'.
applyDependencyChanges :: DependencyChanges -> Dependencies -> IO ()
applyDependencyChanges changes g = do
    sequence_ [applyDependencyChange c g | c@(InsertEdge _ _) <- changes]
    sequence_ [applyDependencyChange c g | c@(ChangeParentTo _ _) <- changes]

applyDependencyChange
    :: DependencyChange SomeNode SomeNode -> Dependencies -> IO ()
applyDependencyChange (InsertEdge parent child) g =
    GraphGC.insertEdge (parent, child) g
applyDependencyChange (ChangeParentTo child parent) g = do
    GraphGC.clearPredecessors child g
    GraphGC.insertEdge (parent, child) g

{-----------------------------------------------------------------------------
    Traversal in dependency order
------------------------------------------------------------------------------}
-- | Update all pulses in the graph, starting from a given set of nodes
evaluatePulses :: [SomeNode] -> Dependencies -> EvalP ()
evaluatePulses inputs g = do
    action <- liftIO $ GraphGC.walkSuccessors_ inputs evaluateWeakNode g
    action

evaluateWeakNode :: Ref.WeakRef SomeNodeD -> EvalP GraphGC.Step
evaluateWeakNode w = do
    mnode <- liftIO $ Ref.deRefWeak w
    case mnode of
        Nothing -> pure GraphGC.Stop
        Just node -> evaluateNode node

-- | Recalculate a given node and return all children nodes
-- that need to evaluated subsequently.
evaluateNode :: SomeNode -> EvalP GraphGC.Step
evaluateNode someNode = do
    node <- Ref.read someNode
    case node of
        P PulseD{_evalP,_keyP} -> {-# SCC evaluateNodeP #-} do
            ma <- _evalP
            writePulseP _keyP ma
            pure $ case ma of
                Nothing -> GraphGC.Stop
                Just _  -> GraphGC.Next
        L lw -> {-# SCC evaluateLatchWrite #-} do
            evaluateLatchWrite lw
            pure GraphGC.Stop
        O o -> {-# SCC evaluateNodeO #-} do
            m <- _evalO o -- calculate output action
            rememberOutput (someNode,m)
            pure GraphGC.Stop

evaluateLatchWrite :: LatchWriteD -> EvalP ()
evaluateLatchWrite LatchWriteD{_evalLW,_latchLW} = do
    time   <- askTime
    mlatch <- liftIO $ Ref.deRefWeak _latchLW -- retrieve destination latch
    case mlatch of
        Nothing    -> pure ()
        Just latch -> do
            a <- _evalLW                    -- calculate new latch value
            -- liftIO $ Strict.evaluate a   -- see Note [LatchStrictness]
            rememberLatchUpdate $           -- schedule value to be set later
                Ref.modify' latch $ \l ->
                    a `seq` l { _seenL = time, _valueL = a }
