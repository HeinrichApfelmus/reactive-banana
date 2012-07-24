{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Internal.EventBehavior1 where

import Control.Monad (join)

import qualified Reactive.Banana.Internal.PulseLatch0 as Prim
import Reactive.Banana.Internal.Cached

type Network = Prim.Network

{-----------------------------------------------------------------------------
    Event and Behavior types
------------------------------------------------------------------------------}
type Behavior a = Cached Network (Prim.Latch a, Prim.Pulse ())
type Event a    = Cached Network (Prim.Pulse a)
type Moment     = Network

{-----------------------------------------------------------------------------
    Combinators - basic
------------------------------------------------------------------------------}
never    = mkCached $ Prim.neverP
unionWith f = liftCached2 $ \p1 p2 -> Prim.unionWith f
accumE x = liftCached1 $ Prim.accumP x
mapE f   = liftCached1 $ Prim.mapP f

stepperB a  = liftCached1 $ \p1 -> do
    l  <- Prim.stepperL a p1
    p2 <- Prim.mapP (const ()) p1
    return (l, p2)

pureB a = stepperB a never
applyB = liftCached2 $ \(l1,p1) (l2,p2) ->
    p3 <- Prim.unionWith const p1 p2
    -- ????
    l3 <- observeP $ ($) <*> readLatch l1 <*> readLatch l2
    returN (l3,p3)

{-----------------------------------------------------------------------------
    Combinators - dynamic event switching
------------------------------------------------------------------------------}
observeE :: Event (Moment a) -> Event a 
observeE = liftCached1 $ Prim.observeP

switchE :: Event (Moment (Event a)) -> Event a
switchE = liftCached1 $ \p1 -> do
    p2 <- Prim.mapP (join . fmap runCached) p1
    p3 <- Prim.observeP p2
    Prim.switchP p3

trimE :: Event a -> Moment (Moment (Event a))
trimE e = do
    p <- runCached e                 -- add pulse to network
    -- NOTE: if the pulse is not connected to an input node,
    -- it will be garbage collected right away.
    -- TODO: Do we need to check for this?
    return $ return $ fromPure p     -- remember it henceforth

trimB :: Behavior a -> Moment (Moment (Behavior a))
trimB b = do
    (l,p) <- runCached b             -- add behavior to network
    return $ return $ fromPure (l,p) -- remember it henceforth


