{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo #-}
module Reactive.Banana.Prim.Evaluation where

import qualified Control.Exception as Strict (evaluate)
import           Data.Monoid

import qualified Reactive.Banana.Prim.Dependencies as Deps
import           Reactive.Banana.Prim.Plumbing
import qualified Reactive.Banana.Prim.TotalOrder   as Deps
import           Reactive.Banana.Prim.Types

{-----------------------------------------------------------------------------
    Graph evaluation
------------------------------------------------------------------------------}
-- | Evaluate all the pulses in the graph,
-- Rebuild the graph as necessary and update the latch values.
step :: Inputs -> Step
step (pulse1, roots) state1 = mdo
    let graph1 = nGraph state1
        latch1 = nLatchValues state1
    
    ((pulse2, latchUpdates), state2)
            <- runBuildIO state1
            $  runEvalP latch2 pulse1
            $  evaluatePulses graph1 roots
    
    let graph2 = nGraph state2
        latch2 = appEndo latchUpdates $ nLatchValues state2
        output = readOutputs graph2 pulse2
        state3 = Network graph2 latch2

    Strict.evaluate latch2  -- make sure that the latch values are in WHNF
    return (output, state3)

-- | Update all pulses in the graph, starting from a given set of nodes
evaluatePulses :: Graph -> [SomeNode] -> EvalP ()
evaluatePulses graph _ = mapM_ evaluatePulse $ buildEvaluationOrder graph
    where
    evaluatePulse (P p) = evaluateP p
    evaluatePulse (L l) = evaluateL l >>= rememberLatchUpdate

-- | Read Output values.
readOutputs graph pulses =
    sequence_ [action | out         <- grOutputs graph
                      , Just action <- [getValueP out pulses]]

-- | List of all nodes in topological order.
buildEvaluationOrder :: Graph -> [SomeNode]
buildEvaluationOrder = Deps.topologicalSort . grDeps



{-
traverseDependencies :: Monoid m => (a -> m) -> Deps a -> [a] -> m


traverseOrder :: (Monad m, Hashable a, Eq a)
    => (a -> m [a])
    -> Deps.TotalOrder a -> [a] -> m ()
traverseOrder = Deps.
-}