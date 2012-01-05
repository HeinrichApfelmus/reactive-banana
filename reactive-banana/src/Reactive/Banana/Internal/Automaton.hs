{-----------------------------------------------------------------------------
    Reactive Banana
------------------------------------------------------------------------------}
module Reactive.Banana.Internal.Automaton (
    -- * Synopsis
    -- | Internal module.
    -- Stepwise execution of an event graph.
    
    Automaton(..), fromStateful

    ) where

import Reactive.Banana.Internal.Input

{-----------------------------------------------------------------------------
    Stepwise execution
------------------------------------------------------------------------------}
-- Automaton that takes input values and produces a result
data Automaton a = Step { runStep :: [InputValue] -> IO (a, Automaton a) }

fromStateful :: ([InputValue] -> s -> IO (a,s)) -> s -> Automaton a
fromStateful f s = Step $ \i -> do
    (a,s') <- f i s
    return (a, fromStateful f s')