{-----------------------------------------------------------------------------
    Reactive Banana
------------------------------------------------------------------------------}
module Reactive.Banana.Automaton (
    -- * Synopsis
    -- | Internal module.
    -- Stepwise execution of an event graph.
    
    Automaton(..), fromStateful

    ) where

{-----------------------------------------------------------------------------
    Stepwise execution
------------------------------------------------------------------------------}
-- Automaton that takes input values and produces a result
data Automaton a = Step { runStep :: [InputValue] -> IO (a, Automaton a) }

fromStateful :: ([InputValue] -> s -> IO (a,s)) -> s -> Automaton a
fromStateful f s = Step $ do
    (a,s') <- f s
    return (a, fromStateful f s')