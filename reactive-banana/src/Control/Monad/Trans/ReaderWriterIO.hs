module Control.Monad.Trans.ReaderWriterIO (
    -- * Synopsis
    -- | An implementation of the reader/writer monad transformer
    -- using an 'IORef' for the writer.
    
    -- * Documentation
    ReaderWriterIOT, runReaderWriterIOT, tell, ask, local,
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.IORef
import Data.Monoid

newtype ReaderWriterIOT r w m a = ReaderWriterIOT { run :: r -> IORef w -> m a }

instance Functor m => Functor (ReaderWriterIOT r w m) where
    fmap f m = ReaderWriterIOT $ \x y -> fmap f (run m x y)

instance Applicative m => Applicative (ReaderWriterIOT r w m) where
    pure a  = ReaderWriterIOT $ \x y -> pure a
    f <*> a = ReaderWriterIOT $ \x y -> run f x y <*> run a x y

instance Monad m => Monad (ReaderWriterIOT r w m) where
    return a = ReaderWriterIOT $ \_ _ -> return a
    m >>= k  = ReaderWriterIOT $ \x y -> run m x y >>= \a -> run (k a) x y

instance MonadFix m => MonadFix (ReaderWriterIOT r w m) where
    mfix f = ReaderWriterIOT $ \x y -> mfix (\a -> run (f a) x y)

instance MonadIO m => MonadIO (ReaderWriterIOT r w m) where
    liftIO m = ReaderWriterIOT $ \x y -> liftIO m

instance MonadTrans (ReaderWriterIOT r w) where
    lift m = ReaderWriterIOT $ \x y -> m


runReaderWriterIOT :: (MonadIO m, Monoid w) => ReaderWriterIOT r w m a -> r -> m (a,w)
runReaderWriterIOT m r = do
    ref <- liftIO $ newIORef mempty
    a   <- run m r ref
    w   <- liftIO $ readIORef ref
    return (a,w)

tell :: (MonadIO m, Monoid w) => w -> ReaderWriterIOT r w m ()
tell w = ReaderWriterIOT $ \_ ref -> liftIO $ modifyIORef ref (`mappend` w)

local :: MonadIO m => (r -> r) -> ReaderWriterIOT r w m a -> ReaderWriterIOT r w m a
local f m = ReaderWriterIOT $ \r ref -> run m (f r) ref

ask :: Monad m => ReaderWriterIOT r w m r
ask = ReaderWriterIOT $ \r _ -> return r

test :: ReaderWriterIOT String String IO ()
test = do
    c <- ask
    tell c
