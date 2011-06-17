{-# LANGUAGE ExistentialQuantification #-}
{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Utility functions for interfacing with wxHaskell
------------------------------------------------------------------------------}

module Reactive.Banana.WX where

import Reactive.Banana
import qualified Graphics.UI.WX as WX
import Graphics.UI.WX (on, Prop(..))

{-----------------------------------------------------------------------------
    Wx
    
    Utilities for representing stuff from Wx as events and behaviors
------------------------------------------------------------------------------}

-- | Event with exactly one parameter.
event1 :: Typeable a => w -> WX.Event w (a -> IO ()) -> NetworkDescription (Event a)
event1 widget e = do
    (addHandler, runHandlers) <- liftIO $ newAddHandler
    liftIO $ WX.set widget [on e :~ \h x -> h x >> runHandlers x]
    fromAddHandler addHandler

-- | Event without parameters.
event0 :: w -> WX.Event w (IO ()) -> NetworkDescription (Event ())
event0 widget e = event1 widget $ WX.mapEvent const (\_ e -> e ()) e


data Prop' w = forall a. (WX.Attr w a) :== (a, Event a)

-- | "Animate" a property with a stream of events
sink :: w -> [Prop' w] -> NetworkDescription ()
sink widget props = mapM_ sink1 props
    where
    sink1 (attr :== (x,ex)) = do
        liftIO $ WX.set widget [attr := x]
        reactimate $ (\x -> WX.set widget [attr := x]) <$> ex



