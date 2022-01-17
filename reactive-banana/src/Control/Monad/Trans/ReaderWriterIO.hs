{-# LANGUAGE TypeFamilies, DerivingVia, GeneralisedNewtypeDeriving #-}
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
import Control.Monad.Trans.Writer.CPS ( WriterT )
import qualified Control.Monad.Trans.Writer.CPS as W
import Control.Monad.Trans.Reader ( ReaderT )
import qualified Control.Monad.Trans.Reader as R
import Data.Monoid

newtype ReaderWriterIOT r w m a = ReaderWriterIOT { run :: ReaderT r (WriterT w m) a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO)
    deriving (Semigroup, Monoid) via Ap (ReaderT r (WriterT w m)) a

instance MonadTrans (ReaderWriterIOT r w) where 
    lift = ReaderWriterIOT . lift . lift

readerWriterIOT :: (MonadIO m, Monoid w) =>
    (r -> IO (a, w)) -> ReaderWriterIOT r w m a
readerWriterIOT f = do
    r <- ask
    (a,w) <- liftIO $ f r
    tell w
    return a

runReaderWriterIOT :: (MonadIO m, Monoid w) => ReaderWriterIOT r w m a -> r -> m (a,w)
runReaderWriterIOT m r = W.runWriterT (R.runReaderT (run m) r)

tell :: (Monoid w, Monad m) => w -> ReaderWriterIOT r w m ()
tell = ReaderWriterIOT . lift . W.tell

listen :: (MonadIO m, Monoid w) => ReaderWriterIOT r w m a -> ReaderWriterIOT r w m (a, w)
listen = ReaderWriterIOT . R.mapReaderT W.listen . run

local :: MonadIO m => (r -> r) -> ReaderWriterIOT r w m a -> ReaderWriterIOT r w m a
local f = ReaderWriterIOT . R.local f . run

ask :: Monad m => ReaderWriterIOT r w m r
ask = ReaderWriterIOT R.ask
