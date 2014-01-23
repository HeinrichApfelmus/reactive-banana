{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo #-}
module Reactive.Banana.Prim.Evaluation where

import qualified Control.Exception as Strict (evaluate)
import           Data.Monoid
import qualified Data.PQueue.Prio.Min as Q

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


type Result = (EvalL, [(Position, EvalO)])
type Q      = Q.MinPQueue Level

-- | Update all pulses in the graph, starting from a given set of nodes
evaluatePulses :: Graph -> [SomeNode] -> EvalP Result
evaluatePulses Graph { grDeps = deps } roots =
        go mempty [] $ insertList roots Q.empty
    where
    order = Deps.dOrder deps
    
    go :: EvalL -> [(Position,EvalO)] -> Q SomeNode -> EvalP Result
    go el eo q1 = {-# SCC go #-} case ({-# SCC minView #-} Q.minView q1) of
        Nothing      -> return (el, eo)
        Just (a, q2) -> case a of
            P p -> evaluateP p >>= \c -> case c of
                Deps.Children -> go el eo $ insertList (Deps.children deps a) q2
                Deps.Done     -> go el eo q2
            L l -> evaluateL l >>= \x -> go (el `mappend` x) eo      q2
            O o -> evaluateO o >>= \x -> go el ((positionO o, x):eo) q2

    insertList :: [SomeNode] -> Q SomeNode -> Q SomeNode
    insertList xs q = {-# SCC insertList #-}
        foldr (\node -> Q.insert (level node order) node) q xs


