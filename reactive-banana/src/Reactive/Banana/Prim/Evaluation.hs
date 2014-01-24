{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo, BangPatterns #-}
module Reactive.Banana.Prim.Evaluation where

import qualified Control.Exception as Strict (evaluate)
import           Data.Function
import           Data.Functor
import           Data.List                   (foldl', sortBy)
import           Data.Monoid
import qualified Data.Vault.Lazy   as Lazy

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
        time2  = Dated.next $ nTime state1

    -- evaluate pulses while recalculating some latch values
    ((latchUpdates, output), state2)
            <- runBuildIO state1
            $  evaluatePulses graph1 roots pulse1 time2
    
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
            , nTime        = time2
            })


type Result = (EvalL, EvalO)
type Q      = Deps.DepsQueue

-- | Update all pulses in the graph, starting from a given set of nodes
evaluatePulses :: Graph -> [SomeNode] -> Lazy.Vault -> Dated.Time -> BuildIO Result
evaluatePulses Graph { grDeps = deps } roots pulses time =
        go mempty [] pulses $ insertList roots Deps.emptyQ
    where
    order = Deps.dOrder deps
    
    go :: EvalL -> [(Position,EvalO)] -> Lazy.Vault -> Q SomeNode
       -> BuildIO Result
    go el eo pulses !q1 = {-# SCC go #-} case Deps.minView q1 of
        Nothing         -> return (el, sequenceOutputs eo)
        Just (node, q2) -> case node of
            P p ->
                let go' a = go el eo (writeP p a pulses)
                                     (insertList (Deps.children deps node) q2)
                in case evaluateP p pulses of
                    Done      -> go el eo pulses q2
                    Pure a    -> go' a
                    BuildIO m -> m >>= go'
            L l -> let x = evaluateL l pulses time
                in go (el `mappend` x) eo pulses q2
            O o -> let x = evaluateO o pulses
                in go el ((positionO o, x):eo) pulses q2

    insertList :: [SomeNode] -> Q SomeNode -> Q SomeNode
    insertList xs q = {-# SCC insertList #-}
        foldl' (\q node -> Deps.insert (level node order) node q) q xs


sequenceOutputs :: [(Position, EvalO)] -> EvalO
sequenceOutputs xs = sequence_ <$> sequence (sortOutputs xs)

sortOutputs = map snd . sortBy (compare `on` fst)
