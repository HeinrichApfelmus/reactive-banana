{-# LANGUAGE ExistentialQuantification #-}
{-----------------------------------------------------------------------------
    Reactive Banana

    A tiny library for functional reactive programming.
    
    Interface to a GUI library
------------------------------------------------------------------------------}

module Reactive.WX where

import Reactive.Core
import Graphics as WX
import qualified Graphics.UI.WX as WX

{-----------------------------------------------------------------------------
    Wx
    
    Utilities for representing stuff from Wx as events and behaviors
------------------------------------------------------------------------------}

    -- event with exactly one parameter
event1 :: w -> WX.Event w (a -> IO ()) -> Event a
event1 widget e = fromSource $ EventSource
    { getHandlerS = WX.get widget (WX.on e)
    , setHandlerS = \h -> WX.set widget [WX.on e := h] }

    -- event without parameters
event0 :: w -> WX.Event w (IO ()) -> Event ()
event0 widget e = fromSource $ EventSource
    { getHandlerS = (\m -> \() -> m) `fmap` WX.get widget (WX.on e)
    , setHandlerS = \h -> WX.set widget [WX.on e := h ()] }

    -- "animate" a proper by passing an event to it
data Prop' w = forall a. (WX.Attr w a) :== Behavior a

sink :: w -> [Prop' w] -> Prepare ()
sink widget props = mapM_ sink1 props
    where
    sink1 (attr :== b) = do
        WX.set widget [attr  WX.:=  initial b]
        reactimate $ (\a -> WX.set widget [attr WX.:= a]) `fmap` changes b



