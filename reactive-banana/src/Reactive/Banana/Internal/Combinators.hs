{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo #-}
module Reactive.Banana.Internal.Combinators (
    -- * Interpreter
    interpret,
    
    -- * Basic combinators
    Event, Behavior,
    never, filterJust, unionWith, mapE, accumE, applyE,
    changesB, stepperB, pureB, applyB, mapB,
    
    -- * Dynamic event switching
    Moment,
    initialB, trimE, trimB, executeE, observeE, switchE, switchB,
    
    -- * Setup and IO
    newEvent, addHandler, liftIONow, liftIOLater,
    ) where

import Data.Functor
import Data.Functor.Identity
import Control.Monad (join, (<=<))
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)

import qualified Reactive.Banana.Prim as Prim
import Reactive.Banana.Prim.Cached

type Build = Prim.Build
type Latch = Prim.Latch
type Pulse = Prim.Pulse

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
type Behavior a = Cached Build (Latch a, Pulse ())
type Event a    = Cached Build (Pulse a)
type Moment     = Prim.BuildIO

runCachedM :: Cached Build a -> Moment a
runCachedM = Prim.liftBuild . runCached

{-----------------------------------------------------------------------------
    Interpretation
------------------------------------------------------------------------------}
interpret :: (Event a -> Moment (Event b)) -> [Maybe a] -> IO [Maybe b]
interpret f = Prim.interpret (\pulse -> runCachedM =<< f (fromPure pulse))

{-----------------------------------------------------------------------------
    Combinators - basic
------------------------------------------------------------------------------}
never       = don'tCache  $ Prim.neverP
unionWith f = liftCached2 $ Prim.unionWithP f
filterJust  = liftCached1 $ Prim.filterJustP
accumE x    = liftCached1 $ fmap snd . Prim.accumL x
mapE f      = liftCached1 $ Prim.mapP f
applyE      = liftCached2 $ \(lf,_) px -> Prim.applyP lf px

changesB = error "Reactive.Banana.Internal.Combinators: changesB not implemented."

-- FIXME: To allow more recursion, create the latch first and
-- build the pulse later.
stepperB a  = \c1 -> cache $ do
    p1    <- Prim.mapP const =<< runCached c1
    p2    <- Prim.mapP (const ()) p1
    (l,_) <- Prim.accumL a p1
    return (l,p2)

pureB a = stepperB a never
applyB = liftCached2 $ \(l1,p1) (l2,p2) -> do
    p3 <- Prim.unionWithP const p1 p2
    let l3 = Prim.applyL l1 l2
    return (l3,p3)
mapB f = applyB (pureB f)

{-----------------------------------------------------------------------------
    Combinators - dynamic event switching
------------------------------------------------------------------------------}
initialB :: Behavior a -> Moment a
initialB b = Prim.liftBuild $ do
    ~(l,_) <- runCached b
    Prim.readLatch l

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
executeE e = Prim.liftBuild $ do
    p <- runCached e
    result <- Prim.executeP p
    return $ fromPure result

switchE :: Event (Moment (Event a)) -> Event a
switchE = liftCached1 $ \p1 -> do
    p2 <- Prim.mapP (runCachedM =<<) p1
    p3 <- Prim.executeP p2
    Prim.switchP p3

switchB :: Behavior a -> Event (Moment (Behavior a)) -> Behavior a
switchB = liftCached2 $ \(l0,p0) p1 -> do
    p2 <- Prim.mapP (runCachedM =<<) p1
    p3 <- Prim.executeP p2
    lr <- Prim.switchL l0 =<< Prim.mapP fst p3

    -- TODO: switch away the initial behavior
    let c1 = p0                              -- initial behavior changes
    c2 <- Prim.mapP (const ()) p3            -- or switch happens
    c3 <- Prim.switchP =<< Prim.mapP snd p3  -- or current behavior changes
    pr <- merge c1 =<< merge c2 c3
    return (lr, pr)

merge = Prim.unionWithP (\_ _ -> ())

{-----------------------------------------------------------------------------
    Combinators - Setup and IO
------------------------------------------------------------------------------}
newEvent :: Moment (Event a, a -> Moment ())
newEvent = undefined

addHandler :: Event a -> (a -> IO ()) -> Moment ()
addHandler e f = Prim.liftBuild $ do
    p <- runCached e
    Prim.addHandler p f

liftIONow :: IO a -> Moment a
liftIONow = liftIO

liftIOLater :: IO () -> Moment ()
liftIOLater = Prim.liftBuild . Prim.liftIOLater

{-
fromPoll :: IO a -> Moment (Behavior a)
fromPoll poll = do
    a <- liftIO poll
    e <- Prim.liftBuild $ do
        pm <- Prim.mapP (const $ liftIO poll) Prim.alwaysP
        p  <- Prim.executeP pm
        return $ fromPure p
    return $ stepperB a e
-}