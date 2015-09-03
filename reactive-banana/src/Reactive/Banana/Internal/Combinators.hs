{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo, FlexibleInstances, NoMonomorphismRestriction #-}
module Reactive.Banana.Internal.Combinators where

import           Control.Concurrent.MVar
import           Control.Event.Handler
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class           (lift)
import           Control.Monad.Trans.Reader
import           Data.Functor
import           Data.Functor.Identity
import           Data.IORef
import qualified Reactive.Banana.Prim        as Prim
import           Reactive.Banana.Prim.Cached

type Build   = Prim.Build
type Latch a = Prim.Latch a
type Pulse a = Prim.Pulse a
type Future  = Prim.Future

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
type Behavior a = Cached Moment (Latch a, Pulse ())
type Event a    = Cached Moment (Pulse a)
type Moment     = ReaderT EventNetwork Prim.Build

liftBuild :: Build a -> Moment a
liftBuild = lift

{-----------------------------------------------------------------------------
    Interpretation
------------------------------------------------------------------------------}
interpret :: (Event a -> Moment (Event b)) -> [Maybe a] -> IO [Maybe b]
interpret f = Prim.interpret $ \pulse -> runReaderT (g pulse) undefined
    where
    g pulse = runCached =<< f (Prim.fromPure pulse)
    -- Ignore any  addHandler  inside the  Moment

{-----------------------------------------------------------------------------
    IO
------------------------------------------------------------------------------}
-- | Data type representing an event network.
data EventNetwork = EventNetwork
    { runStep :: Prim.Step -> IO ()
    , actuate :: IO ()
    , pause   :: IO ()
    , showNetwork :: IO String
    }

-- | Compile to an event network.
compile :: Moment () -> IO EventNetwork
compile setup = do
    actuated <- newIORef False                   -- flag to set running status
    s        <- newEmptyMVar                     -- setup callback machinery
    let
        whenFlag flag action = readIORef flag >>= \b -> when b action
        runStep f            = whenFlag actuated $ do
            s1 <- takeMVar s                    -- read and take lock
            -- pollValues <- sequence polls     -- poll mutable data
            (output, s2) <- f s1                -- calculate new state
            putMVar s s2                        -- write state
            output                              -- run IO actions afterwards

        eventNetwork = EventNetwork
            { runStep = runStep
            , actuate = writeIORef actuated True
            , pause   = writeIORef actuated False
            , showNetwork = show <$> readMVar s
            }

    (output, s0) <-                             -- compile initial graph
        Prim.compile (runReaderT setup eventNetwork) Prim.emptyNetwork
    putMVar s s0                                -- set initial state
        
    return $ eventNetwork

fromAddHandler :: AddHandler a -> Moment (Event a)
fromAddHandler addHandler = do
    (p, fire) <- liftBuild $ Prim.newInput
    network   <- ask
    liftIO $ register addHandler $ runStep network . fire
    return $ Prim.fromPure p

addReactimate :: Event (Future (IO ())) -> Moment ()
addReactimate e = do
    network   <- ask
    liftBuild $ Prim.buildLater $ do
        -- Run cached computation later to allow more recursion with `Moment`
        p <- runReaderT (runCached e) network
        Prim.addHandler p id

fromPoll :: IO a -> Moment (Behavior a)
fromPoll poll = do
    a <- liftIO poll
    e <- liftBuild $ do
        p <- Prim.unsafeMapIOP (const poll) =<< Prim.alwaysP
        return $ Prim.fromPure p
    stepperB a e

liftIONow :: IO a -> Moment a
liftIONow = liftIO

liftIOLater :: IO () -> Moment ()
liftIOLater = lift . Prim.liftBuild . Prim.liftIOLater

imposeChanges :: Behavior a -> Event () -> Behavior a
imposeChanges = liftCached2 $ \(l1,_) p2 -> return (l1,p2)

{-----------------------------------------------------------------------------
    Combinators - basic
------------------------------------------------------------------------------}
never       = don'tCache  $ liftBuild $ Prim.neverP
unionWith f = liftCached2 $ (liftBuild .) . Prim.unionWithP f
filterJust  = liftCached1 $ liftBuild . Prim.filterJustP
mapE f      = liftCached1 $ liftBuild . Prim.mapP f
applyE      = liftCached2 $ \(~(lf,_)) px -> liftBuild $ Prim.applyP lf px

changesB    = liftCached1 $ \(~(lx,px)) -> liftBuild $ Prim.tagFuture lx px

pureB a = cache $ do
    p <- runCached never
    return (Prim.pureL a, p)
applyB  = liftCached2 $ \(~(l1,p1)) (~(l2,p2)) -> liftBuild $ do
    p3 <- Prim.unionWithP const p1 p2
    let l3 = Prim.applyL l1 l2
    return (l3,p3)
mapB f  = applyB (pureB f)

{-----------------------------------------------------------------------------
    Combinators - accumulation
------------------------------------------------------------------------------}
-- make sure that the event is added to the network eventually
trimE :: Event a -> Moment (Event a)
trimE e = do
    liftBuildFun Prim.buildLater $ void $ runCached e
    return e

-- make sure that the behavior is added to the network eventually
trimB :: Behavior a -> Moment (Behavior a)
trimB b = do
    liftBuildFun Prim.buildLater $ void $ runCached b
    return $ b

stepperB a = \e ->
    trimB $ cache $ do
        p0 <- runCached e
        liftBuild $ do
            p1    <- Prim.mapP const p0
            p2    <- Prim.mapP (const ()) p1
            (l,_) <- Prim.accumL a p1
            return (l,p2)

accumE a = \e1 ->
    trimE $ cache $ do
        p0 <- runCached e1
        liftBuild $ do
            (_,p1) <- Prim.accumL a p0
            return p1

{-----------------------------------------------------------------------------
    Combinators - dynamic event switching
------------------------------------------------------------------------------}
liftBuildFun :: (Build a -> Build b) -> Moment a -> Moment b
liftBuildFun f m = do
    r <- ask
    liftBuild $ f $ runReaderT m r

valueB :: Behavior a -> Moment a
valueB b = do
    ~(l,_) <- runCached b
    liftBuild $ Prim.readLatch l

initialBLater :: Behavior a -> Moment a
initialBLater = liftBuildFun Prim.buildLaterReadNow . valueB

executeP :: Pulse (Moment a) -> Moment (Pulse a)
executeP p1 = do
    r <- ask
    liftBuild $ do
        p2 <- Prim.mapP runReaderT p1
        Prim.executeP p2 r

observeE :: Event (Moment a) -> Event a 
observeE = liftCached1 $ executeP

executeE :: Event (Moment a) -> Moment (Event a)
executeE e = do
    -- Run cached computation later to allow more recursion with `Moment`
    p <- liftBuildFun Prim.buildLaterReadNow $ executeP =<< runCached e
    return $ fromPure p

switchE :: Event (Event a) -> Event a
switchE = liftCached1 $ \p1 -> do
    p2 <- liftBuild $ Prim.mapP (runCached) p1
    p3 <- executeP p2
    liftBuild $ Prim.switchP p3

switchB :: Behavior a -> Event (Behavior a) -> Behavior a
switchB = liftCached2 $ \(l0,p0) p1 -> do
    p2 <- liftBuild $ Prim.mapP (runCached) p1
    p3 <- executeP p2
    
    liftBuild $ do
        lr <- Prim.switchL l0 =<< Prim.mapP fst p3
        -- TODO: switch away the initial behavior
        let c1 = p0                              -- initial behavior changes
        c2 <- Prim.mapP (const ()) p3            -- or switch happens
        c3 <- Prim.switchP =<< Prim.mapP snd p3  -- or current behavior changes
        pr <- merge c1 =<< merge c2 c3
        return (lr, pr)

merge = Prim.unionWithP (\_ _ -> ())
