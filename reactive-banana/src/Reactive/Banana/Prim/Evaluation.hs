{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE CPP, RecursiveDo #-}
module Reactive.Banana.Prim.Evaluation where

import qualified Control.Exception as Strict (evaluate)

import Reactive.Banana.Prim.Types
import Reactive.Banana.Prim.Monads

import qualified Reactive.Banana.Prim.DependencyGraph as Deps
import qualified Reactive.Banana.Prim.TotalOrder      as Deps

{-----------------------------------------------------------------------------
    Graph evaluation
------------------------------------------------------------------------------}

-- | Evaluate all the pulses in the graph,
-- Rebuild the graph as necessary and update the latch values.
step :: Inputs -> EvalGraph (IO ())
step (pulse1, nodes) state1 = {-# SCC step #-} mdo
    let graph1 = gsGraph state1
        latch1 = gsLatchValues state1
    
    (pulse2, state2) <- runBuildIO state1
            $ runEvalP latch2 pulse1
            $ evaluatePulses graph1
    
    let
        graph2 = gsGraph state2
        latch2 = evaluateLatches graph2 pulse2 $ gsLatchValues state2
        output = readOutputs graph2 pulse2
        state3 = GraphState graph2 latch2

    Strict.evaluate latch2  -- make sure that the latch values are in WHNF
    return (output, state3)

readOutputs graph pulses =
    sequence_ [action | out <- grOutputs graph
                      , Just action <- [getValueP out pulses]]


-- update all pulses in the graph
evaluatePulses  :: Graph -> EvalP ()
-- update all latches in the graph
evaluateLatches :: Graph -> Values -> Values -> Values

-- #define PUSH_BASED_IMPLEMENTATION 1
#ifdef PUSH_BASED_IMPLEMENTATION

evaluatePulses graph = {-# SCC evaluatePulses #-}
        Deps.withTotalOrder
            (Deps.ancestorOrder $ grDeps graph)
            (traverse . Deps.insertList inputs)
    where
    inputs         = map fst $ grInputs graph
    traverse queue = case Deps.minView queue of
        Nothing                   -> return ()
        Just (      L _ , queue2) -> return ()
        Just (node@(P p), queue2) -> do
            evaluateP p
            -- check whether event occurrence has happened
            mp <- readPulseP p
            case mp of
                Nothing -> return ()
                Just _  -> do
                    let children = Deps.children (grDeps graph) node
                    let queue3   = Deps.insertList children queue2
                    traverse queue3

evaluateLatches graph pulse latch = {-# SCC evaluateLatches #-}
    runEvalL pulse latch . mapM_ evaluateLatch
    . Deps.topologicalSort . grDeps
    $ graph
    where
    evaluateLatch (P _) = return ()
    evaluateLatch (L l) = trace "evalLatch" $ evaluateL l   

#else

evaluatePulses = mapM_ evaluatePulse . buildEvaluationOrder
    where
    evaluatePulse (P p) = evaluateP p
    evaluatePulse (L _) = return ()

evaluateLatches graph pulse latch = {-# SCC evaluateLatches #-}
    runEvalL pulse latch . mapM_ evaluateLatch $ buildEvaluationOrder graph
    where
    evaluateLatch (P _) = return ()
    evaluateLatch (L l) = evaluateL l   

-- Figure out which nodes need to be evaluated.
--
-- All nodes that are connected to current input nodes must be evaluated.
-- The other nodes don't have to be evaluated, because they yield
-- Nothing / don't change anyway.
buildEvaluationOrder :: Graph -> [SomeNode]
buildEvaluationOrder = Deps.topologicalSort . grDeps
#endif
