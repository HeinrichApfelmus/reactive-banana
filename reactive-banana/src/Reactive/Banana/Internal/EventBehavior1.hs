{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Internal.EventBehavior1 where

import qualified Reactive.Banana.Internal.PulseLatch0 as Prim
import Reactive.Banana.Internal.Cached

type Latch   = Prim.Latch
type Network = Prim.Network
type Pulse   = Prim.Pulse

{-----------------------------------------------------------------------------
    Event and Behavior types
    Combinators - basic
------------------------------------------------------------------------------}
type Behavior a = Cached Network (Latch a, Pulse ())
type Event a    = Cached Network (Pulse a)

unionWith f = liftCached2 $ \p1 p2 -> Prim.unionWith f
stepperB a  = liftCached1 $ \p1 -> do
    l  <- Prim.stepperL a p1
    p2 <- Prim.mapP (const ()) p1
    return (l, p2)

accumE x = liftCached1 $ Prim.accumP x