{-# LANGUAGE ExistentialQuantification #-}
{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Utility functions for interfacing with wxHaskell
------------------------------------------------------------------------------}

module Reactive.Banana.WX where

import Reactive.Banana
import qualified Graphics.UI.WX as WX

{-----------------------------------------------------------------------------
    Wx
    
    Utilities for representing stuff from Wx as events and behaviors
------------------------------------------------------------------------------}

-- | Event with exactly one parameter.
event1 :: Typeable a => w -> WX.Event w (a -> IO ()) -> Prepare (Event a)
event1 widget e = fromAddHandler addHandler
    where
    addHandler k = WX.set widget [WX.on e WX.:~ \h x -> h x >> k x]

-- | Event without parameters.
event0 :: w -> WX.Event w (IO ()) -> Prepare (Event ())
event0 widget e = fromAddHandler addHandler
   where
   addHandler k = WX.set widget [WX.on e WX.:~ \h -> h >> k ()]

data Prop' w = forall a. (WX.Attr w a) :== (a, Event a)

-- | "Animate" a property with a stream of events
sink :: w -> [Prop' w] -> Prepare ()
sink widget props = mapM_ sink1 props
    where
    sink1 (attr :== (x,ex)) = do
        liftIO $ WX.set widget [attr  WX.:= x]
        reactimate $ (\x -> WX.set widget [attr WX.:= x]) <$> ex



