{-# LANGUAGE FlexibleInstances #-}
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

instance Monad m => Monoid (ReaderWriterIOT r w m ()) where
    mempty          = return ()
    mx `mappend` my = mx >> my

{-----------------------------------------------------------------------------
    Functions
------------------------------------------------------------------------------}
liftIOR m = ReaderWriterIOT $ \x y -> liftIO m

liftR m = ReaderWriterIOT $ \x y -> m

fmapR f m = ReaderWriterIOT $ \x y -> fmap f (run m x y)

returnR a = ReaderWriterIOT $ \_ _ -> return a

bindR m k = ReaderWriterIOT $ \x y -> run m x y >>= \a -> run (k a) x y

mfixR f = ReaderWriterIOT $ \x y -> mfix (\a -> run (f a) x y)

pureR a = ReaderWriterIOT $ \_ _ -> pure a

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

test :: ReaderWriterIOT String String IO ()
test = do
    c <- ask
    tell c
