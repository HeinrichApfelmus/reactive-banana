module Control.Monad.Trans.RWSIO (
    -- * Synopsis
    -- | An implementation of the reader/writer/state monad transformer
    -- using an 'IORef'.

    -- * Documentation
    RWSIOT(..), Tuple(..), rwsT, runRWSIOT, tell, ask, get, put,
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
data Tuple r w s = Tuple !r !(IORef w) !(IORef s)

newtype RWSIOT r w s m a = R { run :: Tuple r w s -> m a }

instance Functor m => Functor (RWSIOT r w s m) where fmap = fmapR

instance Applicative m => Applicative (RWSIOT r w s m) where
    pure  = pureR
    (<*>) = apR

instance Monad m => Monad (RWSIOT r w s m) where
    return = returnR
    (>>=)  = bindR

instance MonadFix m => MonadFix (RWSIOT r w s m) where mfix = mfixR
instance MonadIO m => MonadIO (RWSIOT r w s m)   where liftIO = liftIOR
instance MonadTrans (RWSIOT r w s)               where lift = liftR

{-----------------------------------------------------------------------------
    Functions
------------------------------------------------------------------------------}
liftIOR :: MonadIO m => IO a -> RWSIOT r w s m a
liftIOR m = R $ \_ -> liftIO m
{-# INLINE liftIOR #-}

liftR :: m a -> RWSIOT r w s m a
liftR   m = R $ \_ -> m
{-# INLINE liftR #-}

fmapR :: Functor m => (a -> b) -> RWSIOT r w s m a -> RWSIOT r w s m b
fmapR f m = R $ \x -> fmap f (run m x)
{-# INLINE fmapR #-}

returnR :: Monad m => a -> RWSIOT r w s m a
returnR a = R $ \_ -> return a
{-# INLINE returnR #-}

bindR :: Monad m => RWSIOT r w s m a -> (a -> RWSIOT r w s m b) -> RWSIOT r w s m b
bindR m k = R $ \x -> run m x >>= \a -> run (k a) x
{-# INLINE bindR #-}

mfixR :: MonadFix m => (a -> RWSIOT r w s m a) -> RWSIOT r w s m a
mfixR f   = R $ \x -> mfix (\a -> run (f a) x)
{-# INLINE mfixR #-}

pureR :: Applicative m => a -> RWSIOT r w s m a
pureR a   = R $ \_ -> pure a
{-# INLINE pureR #-}

apR :: Applicative m => RWSIOT r w s m (a -> b) -> RWSIOT r w s m a -> RWSIOT r w s m b
apR f a   = R $ \x -> run f x <*> run a x
{-# INLINE apR #-}

rwsT :: (MonadIO m, Monoid w) => (r -> s -> IO (a, s, w)) -> RWSIOT r w s m a
rwsT f = do
    r <- ask
    s <- get
    (a,s,w) <- liftIOR $ f r s
    put  s
    tell w
    return a
{-# INLINE rwsT #-}

runRWSIOT :: (MonadIO m, Monoid w) => RWSIOT r w s m a -> (r -> s -> m (a,s,w))
runRWSIOT m r s = do
    w' <- liftIO $ newIORef mempty
    s' <- liftIO $ newIORef s
    a  <- run m (Tuple r w' s')
    s  <- liftIO $ readIORef s'
    w  <- liftIO $ readIORef w'
    return (a,s,w)
{-# INLINE runRWSIOT #-}

tell :: (MonadIO m, Monoid w) => w -> RWSIOT r w s m ()
tell w = R $ \(Tuple _ w' _) -> liftIO $ modifyIORef w' (`mappend` w)
{-# INLINE tell #-}

ask :: Monad m => RWSIOT r w s m r
ask = R $ \(Tuple r _ _) -> return r
{-# INLINE ask #-}

get :: MonadIO m => RWSIOT r w s m s
get = R $ \(Tuple _ _ s') -> liftIO $ readIORef s'
{-# INLINE get #-}

put :: MonadIO m => s -> RWSIOT r w s m ()
put s = R $ \(Tuple _ _ s') -> liftIO $ writeIORef s' s
{-# INLINE put #-}

test :: RWSIOT String String () IO ()
test = do
    c <- ask
    tell c
{-# INLINE test #-}
