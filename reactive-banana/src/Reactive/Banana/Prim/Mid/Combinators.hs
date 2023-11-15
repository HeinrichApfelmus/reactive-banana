{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.Mid.Combinators (
    -- * Pulse
    mapP,
    tagFuture,
    filterJustP,
    unsafeMapIOP,
    mergeWithP,
    applyP,

    -- * Latch
    Reactive.Banana.Prim.Mid.Plumbing.pureL,
    mapL,
    applyL,
    accumL,

    -- * Dynamic event switching
    switchL,
    executeP,
    switchP,
  ) where

import Control.Monad
    ( join )
import Control.Monad.IO.Class
    ( liftIO )

import Reactive.Banana.Prim.Mid.Plumbing
    ( newPulse, newLatch, cachedLatch
    , dependOn, keepAlive, changeParent
    , getValueL, getValueL'
    , readPulseP, readLatchP, readLatchP', readLatchFutureP, liftBuildP,
    )
import qualified Reactive.Banana.Prim.Mid.Plumbing
    ( pureL )
import Reactive.Banana.Prim.Mid.Types
    ( Latch(..), Latch', Future, Pulse, Build, EvalP )

{-----------------------------------------------------------------------------
    Combinators - basic
------------------------------------------------------------------------------}
mapP :: (a -> b) -> Pulse a -> Build (Pulse b)
mapP f p1 = do
    p2 <- newPulse "mapP" ({-# SCC mapP #-} fmap f <$> readPulseP p1)
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
    p2 <- newPulse "filterJustP" ({-# SCC filterJustP #-} join <$> readPulseP p1)
    p2 `dependOn` p1
    return p2

unsafeMapIOP :: forall a b. (a -> IO b) -> Pulse a -> Build (Pulse b)
unsafeMapIOP f p1 = do
        p2 <- newPulse "unsafeMapIOP"
            ({-# SCC unsafeMapIOP #-} eval =<< readPulseP p1)
        p2 `dependOn` p1
        return p2
    where
    eval :: Maybe a -> EvalP (Maybe b)
    eval (Just x) = Just <$> liftIO (f x)
    eval Nothing  = return Nothing

mergeWithP
  :: (a -> Maybe c)
  -> (b -> Maybe c)
  -> (a -> b -> Maybe c)
  -> Pulse a
  -> Pulse b
  -> Build (Pulse c)
mergeWithP f g h px py = do
  p <- newPulse "mergeWithP"
       ({-# SCC mergeWithP #-} eval <$> readPulseP px <*> readPulseP py)
  p `dependOn` px
  p `dependOn` py
  return p
  where
    eval Nothing  Nothing  = Nothing
    eval (Just x) Nothing  = f x
    eval Nothing  (Just y) = g y
    eval (Just x) (Just y) = h x y

-- See note [LatchRecursion]
applyP :: Latch (a -> b) -> Pulse a -> Build (Pulse b)
applyP f x = do
    p <- newPulse "applyP"
        ({-# SCC applyP #-} fmap <$> readLatchP f <*> readPulseP x)
    p `dependOn` x
    return p

-- specialization of   mapL f = applyL (pureL f)
mapL :: (a -> b) -> Latch a -> Latch b
mapL f = \case
  PureL x -> PureL (f x)
  ImpureL lx -> ImpureL (cachedLatch ({-# SCC mapL #-} f <$> getValueL' lx))

applyL :: Latch (a -> b) -> Latch a -> Latch b
applyL (PureL f) (PureL x) = PureL (f x)
applyL lf lx = ImpureL (cachedLatch ({-# SCC applyL #-} getValueL lf <*> getValueL lx))

accumL :: a -> Pulse (a -> a) -> Build (Latch a, Pulse a)
accumL a p1 = do
    (updateOn, x) <- newLatch a
    p2 <- newPulse "accumL" $ do
      a <- readLatchP' x
      f <- readPulseP p1
      return $ fmap (\g -> g a) f
    p2 `dependOn` p1
    updateOn p2
    return (ImpureL x,p2)

-- specialization of accumL
stepperL :: a -> Pulse a -> Build (Latch' a)
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
    return $ ImpureL $ cachedLatch $ getValueL' x >>= getValueL

executeP :: forall a b. Pulse (b -> Build a) -> b -> Build (Pulse a)
executeP p1 b = do
        p2 <- newPulse "executeP" ({-# SCC executeP #-} eval =<< readPulseP p1)
        p2 `dependOn` p1
        return p2
    where
    eval :: Maybe (b -> Build a) -> EvalP (Maybe a)
    eval (Just x) = Just <$> liftBuildP (x b)
    eval Nothing  = return Nothing

switchP :: Pulse a -> Pulse (Pulse a) -> Build (Pulse a)
switchP p pp = do
    -- track the latest Pulse in a Latch
    lp <- stepperL p pp

    -- fetch the latest Pulse value
    pout <- newPulse "switchP_out" (readPulseP =<< readLatchP' lp)

    let -- switch the Pulse `pout` to a new parent,
        -- keeping track of the new dependencies.
        switch = do
            mnew <- readPulseP pp
            case mnew of
                Nothing  -> pure ()
                Just new -> liftBuildP $ pout `changeParent` new
            pure Nothing

    pin <- newPulse "switchP_in" switch :: Build (Pulse ())
    pin  `dependOn` pp

    pout `dependOn` p       -- initial dependency
    pout `keepAlive` pin    -- keep switches happening
    pure pout
