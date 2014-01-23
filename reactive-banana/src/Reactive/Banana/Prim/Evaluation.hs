{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo #-}
module Reactive.Banana.Prim.Evaluation where

import qualified Control.Exception as Strict (evaluate)
import           Data.Monoid

import qualified Reactive.Banana.Prim.Dated        as Dated
import qualified Reactive.Banana.Prim.Dependencies as Deps
import           Reactive.Banana.Prim.Order
import           Reactive.Banana.Prim.Plumbing
import           Reactive.Banana.Prim.Types

{-----------------------------------------------------------------------------
    Graph evaluation
------------------------------------------------------------------------------}
-- | Evaluate all the pulses in the graph,
-- Rebuild the graph as necessary and update the latch values.
step :: Inputs -> Step
step (pulse1, roots) state1 = {-# SCC step #-} mdo
    let graph1 = nGraph state1
        latch1 = nLatchValues state1
        time1  = nTime state1

    -- evaluate pulses while recalculating some latch values
    ((_, latchUpdates, output), state2)
            <- runBuildIO state1
            $  runEvalP pulse1
            $  evaluatePulses graph1 roots
    
    let
        -- updated graph dependencies
        graph2 = nGraph state2
        -- update latch values from accumulations
        latch2 = appEndo latchUpdates $ nLatchValues state2
        -- calculate output actions, possibly recalculating more latch values
        (actions, latch3) = Dated.runDated output latch2

    -- make sure that the latch values are in WHNF
    Strict.evaluate $ {-# SCC evaluate #-} latch3
    return (actions, Network
            { nGraph       = graph2
            , nLatchValues = latch3
            , nTime        = Dated.next time1
            })

-- | Update all pulses in the graph, starting from a given set of nodes
evaluatePulses :: Graph -> [SomeNode] -> EvalP ()
evaluatePulses Graph { grDeps = deps } roots =
    withOrder (Deps.dOrder deps) $ go . insertList roots
    where
    go :: Q SomeNode -> EvalP ()
    go q1 = {-# SCC go #-} case minView q1 of
        Nothing      -> return ()
        Just (a, q2) -> do
            continue <- {-# SCC traverseDependencies_f #-} evaluatePulse a
            case continue of
                Deps.Done     -> go q2
                Deps.Children -> go $ insertList (Deps.children deps a) q2
    
    evaluatePulse (P p) = evaluateP p
    evaluatePulse (L l) =
        evaluateL l >>= rememberLatchUpdate >> return Deps.Done
    evaluatePulse (O o) =
        evaluateO o >>= rememberOutput (positionO o) >> return Deps.Done

