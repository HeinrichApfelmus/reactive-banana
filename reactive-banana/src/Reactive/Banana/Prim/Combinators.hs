{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}
module Reactive.Banana.Prim.Combinators where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Reactive.Banana.Prim.Dated (Box(..))
import Reactive.Banana.Prim.Plumbing
    ( neverP, newPulse, newPulseResultP, newLatch, cachedLatch
    , dependOn, changeParent
    , readPulseP, readLatchBIO, readLatchFutureP, liftBuild,
    )
import Reactive.Banana.Prim.Types
    (Latch(..), Future, Pulse, Build, BuildIO, ResultP(..), EvalP)

import Debug.Trace
-- debug s = trace s
debug s = id

{-----------------------------------------------------------------------------
    Combinators - basic
------------------------------------------------------------------------------}
mapP :: (a -> b) -> Pulse a -> Build (Pulse b)
mapP f p1 = do
    p2 <- newPulse "mapP" $ {-# SCC mapP #-} fmap f <$> readPulseP p1
    p2 `dependOn` p1
    return p2

-- | Tag a 'Pulse' with future values of a 'Latch'.
--
-- This is in contrast to 'applyP' which applies the current value
-- of a 'Latch' to a pulse.
tagFuture :: Latch a -> Pulse b -> Build (Pulse (Future a))
tagFuture x p1 = do
    p2 <- newPulse "tagFuture" $
        fmap . const <$> readLatchFutureP x <*> readPulseP p1
    p2 `dependOn` p1
    return p2

filterJustP :: Pulse (Maybe a) -> Build (Pulse a)
filterJustP p1 = do
    p2 <- newPulse "filterJustP" $ {-# SCC filterJustP #-} join <$> readPulseP p1
    p2 `dependOn` p1
    return p2

unsafeMapIOP :: (a -> IO b) -> Pulse a -> Build (Pulse b)
unsafeMapIOP f p1 = do
        p2 <- newPulseResultP "unsafeMapIOP" $
            {-# SCC unsafeMapIOP #-} eval <$> readPulseP p1
        p2 `dependOn` p1
        return p2
    where
    eval (Just x) = BuildIO $ liftIO (f x)
    eval Nothing  = Done

unionWithP :: (a -> a -> a) -> Pulse a -> Pulse a -> Build (Pulse a)
unionWithP f px py = do
        p <- newPulse "unionWithP" $
            {-# SCC unionWithP #-} eval <$> readPulseP px <*> readPulseP py
        p `dependOn` px
        p `dependOn` py
        return p
    where
    eval (Just x) (Just y) = Just (f x y)
    eval (Just x) Nothing  = Just x
    eval Nothing  (Just y) = Just y
    eval Nothing  Nothing  = Nothing

-- See note [LatchRecursion]
applyP :: Latch (a -> b) -> Pulse a -> Build (Pulse b)
applyP f x = do
        p <- newPulseResultP "applyP" $
            {-# SCC applyP #-} eval <$> readPulseP x
        p `dependOn` x
        return p
    where
    eval Nothing  = Done
    eval (Just a) = BuildIO $ readLatchBIO f <*> return a

pureL :: a -> Latch a
pureL a = Latch { getValueL = return (pure a) }

-- specialization of   mapL f = applyL (pureL f)
mapL :: (a -> b) -> Latch a -> Latch b
mapL f lx = cachedLatch $ {-# SCC mapL #-} fmap f <$> getValueL lx

applyL :: Latch (a -> b) -> Latch a -> Latch b
applyL lf lx = cachedLatch $
    {-# SCC applyL #-} (<*>) <$> getValueL lf <*> getValueL lx

accumL :: a -> Pulse (a -> a) -> Build (Latch a, Pulse a)
accumL a p1 = do
    (updateOn, x) <- newLatch a
    p2 <- applyP (mapL (\x f -> f x) x) p1
    updateOn p2
    return (x,p2)

-- specialization of accumL
stepperL :: a -> Pulse a -> Build (Latch a)
stepperL a p = do
    (updateOn, x) <- newLatch a
    updateOn p
    return x

{-----------------------------------------------------------------------------
    Combinators - dynamic event switching
------------------------------------------------------------------------------}
switchL :: Latch a -> Pulse (Latch a) -> Build (Latch a)
switchL l pl = mdo
    x <- stepperL l pl
    return $ Latch { getValueL = getValueL x >>= \(Box a) -> getValueL a }

executeP :: Pulse (x -> BuildIO a) -> x -> Build (Pulse a)
executeP p1 b = do
        p2 <- newPulseResultP "executeP" $
            {-# SCC executeP #-} eval <$> readPulseP p1
        p2 `dependOn` p1
        return p2
    where
    eval (Just a) = BuildIO $ a b
    eval Nothing  = Done

switchP :: forall a. Pulse (Pulse a) -> Build (Pulse a)
switchP pp = mdo
    never <- neverP
    lp    <- stepperL never pp
    let
        -- switch to a new parent
        switch = do
            mnew <- readPulseP pp
            return $ case mnew of
                Nothing  -> Done
                Just new -> BuildIO $ liftBuild $ p2 `changeParent` new
        -- fetch value from old parent
        eval :: EvalP (ResultP a)
        eval pulses = BuildIO $ readLatchBIO lp >>= \p ->
            return $ maybe Done Pure $ readPulseP p pulses
        
    p1 <- newPulseResultP "switchP_in" switch :: Build (Pulse ())
    p1 `dependOn` pp
    p2 <- newPulseResultP "switchP_out" eval
    return p2

{-----------------------------------------------------------------------------
    Notes
------------------------------------------------------------------------------}
{-

* Note [PulseCreation]

We assume that we do not have to calculate a pulse occurrence
at the moment we create the pulse. Otherwise, we would have
to recalculate the dependencies *while* doing evaluation;
this is a recipe for desaster.

* Note [unsafePerformIO]

We're using @unsafePerformIO@ only to get @Key@ and @Unique@.
It's not great, but it works.

Unfortunately, using @IO@ as the base of the @Network@ monad
transformer doens't work because it doesn't support recursion
and @mfix@ very well.

We could use the @ST@ monad, but this would add a type parameter
to everything. A refactoring of this scope is too annoying for
my taste right now.

* Note [LatchRecursion]

...

* Note [LatchStrictness]

Any value that is stored in the graph over a longer
period of time must be stored in WHNF.

This implies that the values in a latch must be forced to WHNF
when storing them. That doesn't have to be immediately
since we are tying a knot, but it definitely has to be done
before  evaluateGraph  is done.

It also implies that reading a value from a latch must
be forced to WHNF before storing it again, so that we don't
carry around the old collection of latch values.
This is particularly relevant for `applyL`.

Conversely, since latches are the only way to store values over time,
this is enough to guarantee that there are no space leaks in this regard.

-}


