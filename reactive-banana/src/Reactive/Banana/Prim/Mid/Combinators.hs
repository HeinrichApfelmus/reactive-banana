{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}
module Reactive.Banana.Prim.Mid.Combinators where

import Control.Monad
import Control.Monad.IO.Class

import Reactive.Banana.Prim.Low.Plumbing
    ( newPulse, newLatch, cachedLatch
    , dependOn, keepAlive, changeParent
    , getValueL
    , readPulseP, readLatchP, readLatchFutureP, liftBuildP,
    )
import qualified Reactive.Banana.Prim.Low.Plumbing (pureL)
import           Reactive.Banana.Prim.Low.Types    (Latch, Future, Pulse, Build, EvalP)

debug :: String -> a -> a
-- debug s = trace s
debug _ = id

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

pureL :: a -> Latch a
pureL = Reactive.Banana.Prim.Low.Plumbing.pureL

-- specialization of   mapL f = applyL (pureL f)
mapL :: (a -> b) -> Latch a -> Latch b
mapL f lx = cachedLatch ({-# SCC mapL #-} f <$> getValueL lx)

applyL :: Latch (a -> b) -> Latch a -> Latch b
applyL lf lx = cachedLatch
    ({-# SCC applyL #-} getValueL lf <*> getValueL lx)

accumL :: a -> Pulse (a -> a) -> Build (Latch a, Pulse a)
accumL a p1 = do
    (updateOn, x) <- newLatch a
    p2 <- newPulse "accumL" $ do
      a <- readLatchP x
      f <- readPulseP p1
      return $ fmap (\g -> g a) f
    p2 `dependOn` p1
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
    return $ cachedLatch $ getValueL x >>= getValueL

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
switchP p pp = mdo
    lp <- stepperL p pp
    let
        -- switch to a new parent
        switch = do
            mnew <- readPulseP pp
            case mnew of
                Nothing  -> return ()
                Just new -> liftBuildP $ p2 `changeParent` new
            return Nothing
        -- fetch value from old parent
        eval = readPulseP =<< readLatchP lp

    p1 <- newPulse "switchP_in" switch :: Build (Pulse ())
    p1 `dependOn` pp
    p2 <- newPulse "switchP_out" eval
    p2 `dependOn` p
    p2 `keepAlive` p1
    return p2
