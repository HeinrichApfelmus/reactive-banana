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

    -- NOTE: Some events don't work, for instance   leftKey  and  rightKey
    -- "user error (WX.Events: the key event is write-only.)"
    -- That's because they are actually just derived from the  key  event
    -- Not sure what to do with this.

-- | Event without parameters.
event0 :: w -> WX.Event w (IO ()) -> NetworkDescription (Event ())
event0 widget e = event1 widget $ WX.mapEvent const (\_ e -> e ()) e

-- | Behavior form an attribute
behavior :: w -> WX.Attr w a -> NetworkDescription (Behavior a)
behavior widget attr = fromPoll . liftIO $ WX.get widget attr


data Prop' w = forall a. (WX.Attr w a) :== Discrete a

infixr 0 :==

-- | "Animate" a property with a stream of events
sink :: w -> [Prop' w] -> NetworkDescription ()
sink widget props = mapM_ sink1 props
    where
    sink1 (attr :== x) = do
        liftIOLater $ WX.set widget [attr := initial x]
        reactimate $ (\x -> WX.set widget [attr := x]) <$> changes x

-- Typeable instances, yikes!
-- Also, these instances are wrong, but I don't care.
instance Typeable WX.EventKey where
    typeOf _ = mkTyConApp (mkTyCon "WX.EventKey") []
instance Typeable WX.EventMouse where
    typeOf _ = mkTyConApp (mkTyCon "WX.EventMouse") []




