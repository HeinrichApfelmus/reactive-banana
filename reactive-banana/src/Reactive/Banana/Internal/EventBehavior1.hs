{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Internal.EventBehavior1 (
    -- * Interpreter
    interpret, compile,
    
    -- * Basic combinators
    Event, Behavior,
    never, filterJust, unionWith, mapE, accumE, applyE,
    changesB, stepperB, pureB, applyB, mapB,
    
    -- * Dynamic event switching
    Moment,
    initialB, trimE, trimB, executeE, observeE, switchE, switchB,
    
    -- * Setup and IO
    addReactimate, fromAddHandler, liftIONow, liftIOLater,
    ) where

import Data.Functor
import Data.Functor.Identity
import Control.Monad (join, (<=<))
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)

import qualified Reactive.Banana.Internal.PulseLatch0 as Prim
import Reactive.Banana.Internal.Cached
import Reactive.Banana.Internal.InputOutput
import Reactive.Banana.AddHandler

type Network = Prim.Network
type Latch   = Prim.Latch
type Pulse   = Prim.Pulse

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
type Behavior a = Cached Network (Latch a, Pulse ())
type Event a    = Cached Network (Pulse a)
type Moment     = Prim.NetworkSetup

runCachedM :: Cached Network a -> Moment a
runCachedM = Prim.liftNetwork . runCached

{-----------------------------------------------------------------------------
    Interpretation
------------------------------------------------------------------------------}
inputE :: InputChannel a -> Event a
inputE = mkCached . Prim.inputP

interpret :: (Event a -> Moment (Event b)) -> [Maybe a] -> IO [Maybe b]
interpret f = Prim.interpret (\pulse -> runCachedM =<< f (fromPure pulse))

compile :: Moment () -> IO ()
compile m = Prim.compile m >> return ()

{-----------------------------------------------------------------------------
    Combinators - basic
------------------------------------------------------------------------------}
never       = mkCached $ Prim.neverP
unionWith f = liftCached2 $ Prim.unionWith f
filterJust  = liftCached1 $ Prim.filterJustP
accumE x    = liftCached1 $ Prim.accumP x
mapE f      = liftCached1 $ Prim.mapP f
applyE      = liftCached2 $ \(lf,_) px -> Prim.applyP lf px

changesB    = liftCached1 $ \(lx,px) -> Prim.tagFuture lx px
stepperB a  = liftCached1 $ \p1 -> do
    l  <- Prim.stepperL a p1
    p2 <- Prim.mapP (const ()) p1
    return (l, p2)
pureB a = stepperB a never
applyB = liftCached2 $ \(l1,p1) (l2,p2) -> do
    p3 <- Prim.unionWith const p1 p2
    l3 <- Prim.applyL l1 l2
    return (l3,p3)
mapB f = applyB (pureB f)

{-----------------------------------------------------------------------------
    Combinators - dynamic event switching
------------------------------------------------------------------------------}
initialB :: Behavior a -> Moment a
initialB b = do
    ~(l,_) <- runCachedM b
    Prim.liftNetwork $ Prim.valueL l

trimE :: Event a -> Moment (Moment (Event a))
trimE e = do
    p <- runCachedM e                  -- add pulse to network
    -- NOTE: if the pulse is not connected to an input node,
    -- it will be garbage collected right away.
    -- TODO: Do we need to check for this?
    return $ return $ fromPure p       -- remember it henceforth

trimB :: Behavior a -> Moment (Moment (Behavior a))
trimB b = do
    ~(l,p) <- runCachedM b             -- add behavior to network
    return $ return $ fromPure (l,p)   -- remember it henceforth


observeE :: Event (Moment a) -> Event a 
observeE = liftCached1 $ Prim.executeP

executeE :: Event (Moment a) -> Moment (Event a)
executeE e = Prim.liftNetwork $ do
    p <- runCached e
    result <- Prim.executeP p
    return $ fromPure result

switchE :: Event (Moment (Event a)) -> Event a
switchE = liftCached1 $ \p1 -> do
    p2 <- Prim.mapP (join . fmap runCachedM) p1
    p3 <- Prim.executeP p2
    Prim.switchP p3

switchB :: Behavior a -> Event (Moment (Behavior a)) -> Behavior a
switchB = liftCached2 $ \(l0,p0) p1 -> do
    p2 <- Prim.mapP (join . fmap runCachedM) p1
    p3 <- Prim.executeP p2
    lr <- Prim.switchL l0 =<< Prim.mapP fst p3
    pr <- Prim.unionWith (\_ _ -> ()) p0 =<< Prim.switchP =<< Prim.mapP snd p3
    return (lr, pr)

{-----------------------------------------------------------------------------
    Combinators - Setup and IO
------------------------------------------------------------------------------}
addReactimate :: Event (IO ()) -> Moment ()
addReactimate e = do
    p <- runCachedM e
    lift $ Prim.addReactimate p

liftIONow :: IO a -> Moment a
liftIONow = liftIO

liftIOLater :: IO () -> Moment ()
liftIOLater = lift . Prim.liftIOLater

fromAddHandler :: AddHandler a -> Moment (Event a)
fromAddHandler addHandler = do
    i <- liftIO newInputChannel
    p <- Prim.liftNetwork $ Prim.inputP i
    lift $ Prim.registerHandler $ mapIO (return . (:[]) . toValue i) addHandler
    return $ fromPure p

