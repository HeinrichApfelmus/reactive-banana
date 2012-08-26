{-----------------------------------------------------------------------------
    Reactive Banana
------------------------------------------------------------------------------}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, ImpredicativeTypes, FlexibleInstances #-}

module Reactive.Banana.Experimental.Switch (
    -- * Synopsis
    -- | Highly experimental combinators for dynamic event switching.
    -- They don't even work properly yet.
    -- Mainly used for API experiments.
    
    -- * Documentation
    Reactive,
    Trimmed, unTrim, trimE, trimB,
    switchE, switchB, Forall(..), compileNew,
    newChannel,
    
    ) where

import Control.Monad (join)
import Data.IORef

import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

type Reactive = NetworkDescription

{-----------------------------------------------------------------------------
    Trim
------------------------------------------------------------------------------}
-- | A trimmed 'Behavior' or 'Event'.
-- This means that the start time is variable.
data Trimmed f a = Trim { unTrim :: forall s. Reactive s (f s a) }

instance Functor (Trimmed Event) where
    fmap f (Trim x) = Trim (fmap (fmap f) x)
instance Functor (Trimmed Behavior) where
    fmap f (Trim x) = Trim (fmap (fmap f) x)

-- | Trim an 'Event' to a variable start time.
trimE :: Event t a -> Reactive t (Trimmed Event a)
trimE e = do
    (eout, ein) <- liftIO newChannel
    ein e
    return $ Trim eout

-- | Trim a 'Behavior' to a variable start time.
trimB :: Behavior t a -> Reactive t (Trimmed Behavior a)
trimB b = do
    x        <- initial b
    echanges <- changes b
    -- an IORef remembers the current value of the behavior
    value <- liftIO $ newIORef x
    e     <- trimE echanges
    -- write changes to the behavior *after* the event was fired
    reactimate $ writeIORef value <$> echanges
    return $ Trim (do
        x <- liftIO $ readIORef value
        e <- unTrim e
        return $ stepper x e
        )

{-----------------------------------------------------------------------------
    Switch
------------------------------------------------------------------------------}
-- Using 'Trimmed' is too restrictive, because we also
-- want to link to new networks

-- | Dynamic event switching.
-- Does not respect simultaneity.
switchE
    :: forall t a. Event t (Trimmed Event a)
    -> Reactive t (Event t a)
switchE e = do
    -- set up communication channel between event networks
    (eout, ein) <- liftIO newChannel

    -- remember event network
    refNetwork  <- liftIO $ newIORef Nothing
    let stopOldNetwork     = maybe (return ()) pause =<< readIORef refNetwork
        rememberNewNetwork = writeIORef refNetwork . Just
    
    -- switch event networks
    reactimate $ (<$> e) $ \m -> do
        stopOldNetwork
        network <- compile $ (unTrim m >>= ein)
        rememberNewNetwork network
        actuate network
    eout

-- | Dynamic behavior switching.
-- Does not respect simultaneity.
switchB :: Behavior t (Trimmed Behavior a) -> Reactive t (Behavior t a)
switchB b =  do
    -- set up communication channel between event networks
    (addHandler, fire) <- liftIO newAddHandler

    -- get initial values
    b0 <- join (unTrim <$> initial b)
    x  <- initial b0
    (e0out, e0in) <- liftIO $ newChannel
    e0 <- changes b0
    e0in e0
    e  <- changes b

    -- remember event network
    refNetwork  <- liftIO $ newIORef Nothing
    let stopOldNetwork     = maybe (return ()) pause =<< readIORef refNetwork
        rememberNewNetwork = writeIORef refNetwork . Just
    
    -- set event network for the initial value
    liftIO $ do
        network <- compile $ reactimate . fmap fire =<< e0out
        rememberNewNetwork network
        actuate network
    
    -- switch event networks dynamically
    reactimate $ (<$> e) $ \m -> do
        stopOldNetwork
        network <- compile $ do
            b  <- unTrim m
            b0 <- initial b
            b1 <- changes b
            liftIO $ fire b0
            reactimate $ fire <$> b1
        rememberNewNetwork network
        actuate network
    
    stepper x <$> fromAddHandler addHandler


instance Applicative (Trimmed Behavior) where
    pure x  = Trim (return $ pure x)
    f <*> x = Trim (liftA2 (<*>) (unTrim f) (unTrim x))

-- | The 'Monad' instance for trimmed 'Behavior's
-- needs a lot of switching, which is probably very inefficient.
instance Monad (Trimmed Behavior) where
    return x = Trim (return $ pure x)
    m >>= g  = join (fmap g m)
        where
        join m = Trim (switchB =<< unTrim m)



-- | Dummy type to deal with impredicative polymorphism.
newtype Forall a = F { unF :: forall s. Reactive s a }

-- | Compile a new event network on the fly.
compileNew :: Event t (Forall a) -> Reactive t (Event t a)
compileNew e = do
    (addHandler, fire) <- liftIO newAddHandler
    reactimate $ flip fmap e $ \m -> do
        network <- compile (unF m >>= liftIO . fire)
        actuate network
    fromAddHandler addHandler

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
-- | Allow two event networks to communicate with each other.
newChannel :: IO
    (forall s. Reactive s (Event s a)       -- outgoing
    ,forall t. Event t a -> Reactive t ())  -- ingoing
newChannel = do
    (addHandler, fire) <- newAddHandler
    return (fromAddHandler addHandler, reactimate . fmap fire)
