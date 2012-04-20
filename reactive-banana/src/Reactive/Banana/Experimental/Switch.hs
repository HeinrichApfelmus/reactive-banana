{-----------------------------------------------------------------------------
    Reactive Banana
------------------------------------------------------------------------------}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, ImpredicativeTypes #-}

module Reactive.Banana.Experimental.Switch (
    -- * Synopsis
    -- | Experimental combinators for dynamic event switching.
    
    -- * Documentation
    Reactive,
    Dummy(..), switchE, newChannel,
    
    ) where

import Data.IORef

import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

type Reactive = NetworkDescription

{-----------------------------------------------------------------------------
    Switching
------------------------------------------------------------------------------}
-- | Impredicative polymorphism doesn't work so well at the moment.
data Dummy a = D { unD :: forall s. Reactive s (Event s a) }

-- | Dynamic event switching.
-- Does not respect simultaneity.
switchE
    :: forall t a. Event t (Dummy a)
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
        network <- compile $ (unD m >>= ein)
        rememberNewNetwork network
        actuate network
    eout

-- | Allow two event networks to communicate with each other.
newChannel :: IO
    (forall s. Reactive s (Event s a)       -- outgoing
    ,forall t. Event t a -> Reactive t ())  -- ingoing
newChannel = do
    (addHandler, fire) <- newAddHandler
    return (fromAddHandler addHandler, reactimate . fmap fire)
