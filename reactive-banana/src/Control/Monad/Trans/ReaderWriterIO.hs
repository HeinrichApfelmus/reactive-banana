{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Trans.ReaderWriterIO (
    -- * Synopsis
    -- | An implementation of the reader/writer monad transformer
    -- using an 'IORef' for the writer.

    -- * Documentation
    ReaderWriterIOT, readerWriterIOT, runReaderWriterIOT, tell, listen, ask, local,
    ) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.IORef

{-----------------------------------------------------------------------------
    Type and class instances
------------------------------------------------------------------------------}
newtype ReaderWriterIOT r w m a = ReaderWriterIOT { run :: r -> IORef w -> m a }

instance Functor m => Functor (ReaderWriterIOT r w m)   where fmap = fmapR

instance Applicative m => Applicative (ReaderWriterIOT r w m) where
    pure  = pureR
    (<*>) = apR

instance Monad m => Monad (ReaderWriterIOT r w m) where
    (>>=)  = bindR

instance MonadFix m => MonadFix (ReaderWriterIOT r w m) where mfix = mfixR
instance MonadIO m => MonadIO (ReaderWriterIOT r w m)   where liftIO = liftIOR
instance MonadTrans (ReaderWriterIOT r w)               where lift = liftR

instance (Monad m, a ~ ()) => Semigroup (ReaderWriterIOT r w m a) where
    mx <> my = mx >> my

instance (Monad m, a ~ ()) => Monoid (ReaderWriterIOT r w m a) where
    mempty  = return ()
    mappend = (<>)

{-----------------------------------------------------------------------------
    Functions
------------------------------------------------------------------------------}
liftIOR :: MonadIO m => IO a -> ReaderWriterIOT r w m a
liftIOR m = ReaderWriterIOT $ \_ _ -> liftIO m

liftR :: m a -> ReaderWriterIOT r w m a
liftR m = ReaderWriterIOT $ \_ _ -> m

fmapR :: Functor m => (a -> b) -> ReaderWriterIOT r w m a -> ReaderWriterIOT r w m b
fmapR f m = ReaderWriterIOT $ \x y -> fmap f (run m x y)

bindR :: Monad m => ReaderWriterIOT r w m a -> (a -> ReaderWriterIOT r w m b) -> ReaderWriterIOT r w m b
bindR m k = ReaderWriterIOT $ \x y -> run m x y >>= \a -> run (k a) x y

mfixR :: MonadFix m => (a -> ReaderWriterIOT r w m a) -> ReaderWriterIOT r w m a
mfixR f = ReaderWriterIOT $ \x y -> mfix (\a -> run (f a) x y)

pureR :: Applicative m => a -> ReaderWriterIOT r w m a
pureR a = ReaderWriterIOT $ \_ _ -> pure a

apR :: Applicative m => ReaderWriterIOT r w m (a -> b) -> ReaderWriterIOT r w m a -> ReaderWriterIOT r w m b
apR f a = ReaderWriterIOT $ \x y -> run f x y <*> run a x y

readerWriterIOT :: (MonadIO m, Monoid w) =>
    (r -> IO (a, w)) -> ReaderWriterIOT r w m a
readerWriterIOT f = do
    r <- ask
    (a,w) <- liftIOR $ f r
    tell w
    return a

runReaderWriterIOT :: (MonadIO m, Monoid w) => ReaderWriterIOT r w m a -> r -> m (a,w)
runReaderWriterIOT m r = do
    ref <- liftIO $ newIORef mempty
    a   <- run m r ref
    w   <- liftIO $ readIORef ref
    return (a,w)

tell :: (MonadIO m, Monoid w) => w -> ReaderWriterIOT r w m ()
tell w = ReaderWriterIOT $ \_ ref -> liftIO $ modifyIORef ref (`mappend` w)

listen :: (MonadIO m, Monoid w) => ReaderWriterIOT r w m a -> ReaderWriterIOT r w m (a, w)
listen m = ReaderWriterIOT $ \r ref -> do
    a <- run m r ref
    w <- liftIO $ readIORef ref
    return (a,w)

local :: MonadIO m => (r -> r) -> ReaderWriterIOT r w m a -> ReaderWriterIOT r w m a
local f m = ReaderWriterIOT $ \r ref -> run m (f r) ref

ask :: Monad m => ReaderWriterIOT r w m r
ask = ReaderWriterIOT $ \r _ -> return r
