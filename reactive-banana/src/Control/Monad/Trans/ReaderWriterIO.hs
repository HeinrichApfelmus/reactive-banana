{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Trans.ReaderWriterIO (
    -- * Synopsis
    -- | An implementation of the reader/writer monad transformer
    -- using an 'IORef' for the writer.

    -- * Documentation
    ReaderWriterIOT, readerWriterIOT, runReaderWriterIOT, tell, listen, ask, local,
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.IORef
import Data.Monoid
import Data.Semigroup

{-----------------------------------------------------------------------------
    Type and class instances
------------------------------------------------------------------------------}
newtype ReaderWriterIOT r w m a = ReaderWriterIOT { run :: r -> IORef w -> m a }

instance Functor m => Functor (ReaderWriterIOT r w m)   where fmap = fmapR

instance Applicative m => Applicative (ReaderWriterIOT r w m) where
    pure  = pureR
    (<*>) = apR

instance Monad m => Monad (ReaderWriterIOT r w m) where
    return = returnR
    (>>=)  = bindR

instance MonadFix m => MonadFix (ReaderWriterIOT r w m) where mfix = mfixR
instance MonadIO m => MonadIO (ReaderWriterIOT r w m)   where liftIO = liftIOR
instance MonadTrans (ReaderWriterIOT r w)               where lift = liftR

instance (Monad m, a ~ ()) => Semigroup (ReaderWriterIOT r w m a) where
    mx <> my = mx >> my

instance (Monad m, a ~ ()) => Monoid (ReaderWriterIOT r w m a) where
    mempty          = return ()
    mx `mappend` my = mx >> my

{-----------------------------------------------------------------------------
    Functions
------------------------------------------------------------------------------}
liftIOR :: MonadIO m => IO a -> ReaderWriterIOT r w m a
liftIOR m = ReaderWriterIOT $ \x y -> liftIO m
{-# INLINE liftIOR #-}

liftR :: m a -> ReaderWriterIOT r w m a
liftR m = ReaderWriterIOT $ \x y -> m
{-# INLINE liftR #-}

fmapR :: Functor m => (a -> b) -> ReaderWriterIOT r w m a -> ReaderWriterIOT r w m b
fmapR f m = ReaderWriterIOT $ \x y -> fmap f (run m x y)
{-# INLINE fmapR #-}

returnR :: Monad m => a -> ReaderWriterIOT r w m a
returnR a = ReaderWriterIOT $ \_ _ -> return a
{-# INLINE returnR #-}

bindR :: Monad m => ReaderWriterIOT r w m a -> (a -> ReaderWriterIOT r w m b) -> ReaderWriterIOT r w m b
bindR m k = ReaderWriterIOT $ \x y -> run m x y >>= \a -> run (k a) x y
{-# INLINE bindR #-}

mfixR :: MonadFix m => (a -> ReaderWriterIOT r w m a) -> ReaderWriterIOT r w m a
mfixR f = ReaderWriterIOT $ \x y -> mfix (\a -> run (f a) x y)
{-# INLINE mfixR #-}

pureR :: Applicative m => a -> ReaderWriterIOT r w m a
pureR a = ReaderWriterIOT $ \_ _ -> pure a
{-# INLINE pureR #-}

apR :: Applicative m => ReaderWriterIOT r w m (a -> b) -> ReaderWriterIOT r w m a -> ReaderWriterIOT r w m b
apR f a = ReaderWriterIOT $ \x y -> run f x y <*> run a x y
{-# INLINE apR #-}

readerWriterIOT :: (MonadIO m, Monoid w) =>
    (r -> IO (a, w)) -> ReaderWriterIOT r w m a
readerWriterIOT f = do
    r <- ask
    (a,w) <- liftIOR $ f r
    tell w
    return a
{-# INLINE readerWriterIOT #-}

runReaderWriterIOT :: (MonadIO m, Monoid w) => ReaderWriterIOT r w m a -> r -> m (a,w)
runReaderWriterIOT m r = do
    ref <- liftIO $ newIORef mempty
    a   <- run m r ref
    w   <- liftIO $ readIORef ref
    return (a,w)
{-# INLINE runReaderWriterIOT #-}

tell :: (MonadIO m, Monoid w) => w -> ReaderWriterIOT r w m ()
tell w = ReaderWriterIOT $ \_ ref -> liftIO $ modifyIORef ref (`mappend` w)
{-# INLINE tell #-}

listen :: (MonadIO m, Monoid w) => ReaderWriterIOT r w m a -> ReaderWriterIOT r w m (a, w)
listen m = ReaderWriterIOT $ \r ref -> do
    a <- run m r ref
    w <- liftIO $ readIORef ref
    return (a,w)
{-# INLINE listen #-}

local :: MonadIO m => (r -> r) -> ReaderWriterIOT r w m a -> ReaderWriterIOT r w m a
local f m = ReaderWriterIOT $ \r ref -> run m (f r) ref
{-# INLINE local #-}

ask :: Monad m => ReaderWriterIOT r w m r
ask = ReaderWriterIOT $ \r _ -> return r
{-# INLINE ask #-}

test :: ReaderWriterIOT String String IO ()
test = do
    c <- ask
    tell c
{-# INLINE test #-}
