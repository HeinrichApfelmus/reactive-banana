{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo #-}
module Reactive.Banana.Prim.Cached (
    -- | Utility for executing monadic actions once
    -- and then retrieving values from a cache.
    -- 
    -- Very useful for observable sharing.
    Cached, runCached, cache, fromPure, don'tCache,
    liftCached1, liftCached2,
    ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.IORef
import System.IO.Unsafe       (unsafePerformIO)

{-----------------------------------------------------------------------------
    Cache type
------------------------------------------------------------------------------}
data Cached m a = Cached (m a)

runCached :: Cached m a -> m a
runCached (Cached x) = x

-- | An action whose result will be cached.
-- Executing the action the first time in the monad will
-- execute the side effects. From then on,
-- only the generated value will be returned.
{-# NOINLINE cache #-}
cache :: (MonadFix m, MonadIO m) => m a -> Cached m a
cache m = unsafePerformIO $ do
    key <- liftIO $ newIORef Nothing
    return $ Cached $ do
        ma <- liftIO $ readIORef key    -- read the cached result
        case ma of
            Just a  -> return a         -- return the cached result.
            Nothing -> mdo
                liftIO $                -- write the result already
                    writeIORef key (Just a)
                a <- m                  -- evaluate
                return a

-- | Return a pure value. Doesn't make use of the cache.
fromPure :: Monad m => a -> Cached m a
fromPure = Cached . return

-- | Lift an action that is /not/ chached, for instance because it is idempotent.
don'tCache :: Monad m => m a -> Cached m a
don'tCache = Cached

liftCached1 :: (MonadFix m, MonadIO m) =>
    (a -> m b) -> Cached m a -> Cached m b
liftCached1 f ca = cache $ do
    a <- runCached ca
    f a

liftCached2 :: (MonadFix m, MonadIO m) =>
    (a -> b -> m c) -> Cached m a -> Cached m b -> Cached m c
liftCached2 f ca cb = cache $ do
    a <- runCached ca
    b <- runCached cb
    f a b

