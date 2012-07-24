{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Internal.Cached (
    -- | Utility for executing monadic actions once
    -- and then retrieving values from a cache.
    -- 
    -- Very useful for observable sharing.
    HasVault(..),
    Cached, runCached, mkCached, fromPure,
    liftCached1, liftCached2,
    ) where

import Control.Monad
import Data.Unique.Really
import qualified Data.Vault as Vault
import System.IO.Unsafe

{-----------------------------------------------------------------------------
    Cache type
------------------------------------------------------------------------------}
data Cached m a = Cached (m a)

runCached :: Cached m a -> m a
runCached (Cached x) = x

-- | Type class for monads that have a 'Vault' that can be used.
class Monad m => HasVault m where
    retrieve :: Vault.Key a -> m (Maybe a)
    write    :: Vault.Key a -> a -> m ()

-- | An action whose result will be cached.
-- Executing the action the first time in the monad will
-- execute the side effects. From then on,
-- only the generated value will be returned.
{-# NOINLINE mkCached #-}
mkCached :: HasVault m => m a -> Cached m a
mkCached m = unsafePerformIO $ do
    key <- Vault.newKey
    return $ Cached $ do
        ma <- retrieve key      -- look up calculation result
        case ma of
            Nothing -> do
                a <- m          -- evaluate
                write key a
                return a
            Just a  -> return a -- return cached result

-- | Return a pure value.
-- Doesn't make use of the cache 'Vault'.
fromPure :: HasVault m => a -> Cached m a
fromPure = Cached . return

liftCached1
    :: HasVault m
    => (a -> m b)
    -> Cached m a -> Cached m b
liftCached1 f ca = mkCached $ do
    a <- runCached ca
    f a

liftCached2
    :: HasVault m
    => (a -> b -> m c)
    -> Cached m a -> Cached m b -> Cached m c
liftCached2 f ca cb = mkCached $ do
    a <- runCached ca
    b <- runCached cb
    f a b

