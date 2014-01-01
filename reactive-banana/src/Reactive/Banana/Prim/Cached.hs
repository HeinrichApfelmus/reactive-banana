{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo #-}
module Reactive.Banana.Prim.Cached (
    -- | Utility for executing monadic actions once
    -- and then retrieving values from a cache.
    -- 
    -- Very useful for observable sharing.
    HasCache(..),
    Cached, runCached, cache, fromPure, don'tCache,
    liftCached1, liftCached2,
    ) where

import           Control.Monad
import           Control.Monad.Fix
import           Data.Unique.Really
import qualified Data.Vault.Lazy    as Lazy (Key, newKey)
import           System.IO.Unsafe           (unsafePerformIO)

{-----------------------------------------------------------------------------
    Cache type
------------------------------------------------------------------------------}
data Cached m a = Cached (m a)

runCached :: Cached m a -> m a
runCached (Cached x) = x

-- | Type class for monads that have a lazy 'Vault' that can be used as a cache.
--
-- The cache has to be lazy in the values in order to be useful for recursion.
class (Monad m, MonadFix m) => HasCache m where
    retrieve :: Lazy.Key a -> m (Maybe a)
    write    :: Lazy.Key a -> a -> m ()

-- | An action whose result will be cached.
-- Executing the action the first time in the monad will
-- execute the side effects. From then on,
-- only the generated value will be returned.
{-# NOINLINE cache #-}
cache :: HasCache m => m a -> Cached m a
cache m = unsafePerformIO $ do
    key <- Lazy.newKey
    return $ Cached $ do
        ma <- retrieve key      -- look up calculation result
        case ma of
            Nothing -> mdo
                write key a     -- black-hole result first
                a <- m          -- evaluate
                return a
            Just a  -> return a -- return cached result

-- | Return a pure value. Doesn't make use of the cache.
fromPure :: HasCache m => a -> Cached m a
fromPure = Cached . return

-- | Lift an action that is /not/ chached, for instance because it is idempotent.
don'tCache :: HasCache m => m a -> Cached m a
don'tCache = Cached

liftCached1 :: HasCache m => (a -> m b) -> Cached m a -> Cached m b
liftCached1 f ca = cache $ do
    a <- runCached ca
    f a

liftCached2 :: HasCache m =>
    (a -> b -> m c) -> Cached m a -> Cached m b -> Cached m c
liftCached2 f ca cb = cache $ do
    a <- runCached ca
    b <- runCached cb
    f a b

