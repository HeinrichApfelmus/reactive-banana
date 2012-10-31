{-----------------------------------------------------------------------------
    reactive-banana-wx
------------------------------------------------------------------------------}
{-# LANGUAGE EmptyDataDecls, FlexibleInstances, Rank2Types #-}
module Reactive.Banana.Ji (
    -- * Synopsis
    -- | Utility functions for interfacing with Ji.
    --
    -- Note: This is a prototype only.
    
    -- * General
    module Reactive.Banana.Frameworks,
    runJiIO,
    
    -- * Specific widgets
    eventValue, behaviorValue, sinkValue,
    ) where

import Data.IORef
import Control.Monad
import System.IO.Unsafe

import Reactive.Banana
import Reactive.Banana.Frameworks

import Graphics.UI.Ji

{-----------------------------------------------------------------------------
    Deal with the Ji monad
------------------------------------------------------------------------------}
sessionRef :: IORef (Session IO)
sessionRef = unsafePerformIO $ newIORef undefined

instance MonadJi IO where
    askSession = readIORef sessionRef

runJiIO :: Session IO -> IO a -> IO a
runJiIO s m = writeIORef sessionRef s >> m

{-----------------------------------------------------------------------------
    Specific widgets
------------------------------------------------------------------------------}
-- | Event that occurs when the /user/ changed
-- the text in text edit widget.
eventValue :: Frameworks t =>
    Element -> Moment t (Event t String)
eventValue w = do
    (addHandler, fire) <- liftIO $ newAddHandler
    liftIO $ onKeyDown w $ \_ -> do
        s <- getValue w
        fire s
    fromAddHandler addHandler

onKeyDown = bind "keydown"

-- | Behavior corresponding to user input the text field.
behaviorValue :: Frameworks t =>
    Element -> String -> Moment t (Behavior t String)
behaviorValue w s = stepper s <$> eventValue w

-- | Set the value of an input field.
sinkValue :: Frameworks t =>
    Element -> Behavior t String -> Moment t ()
sinkValue w b = do
        x <- initial b
        e <- changes b
        liftIOLater $ setValue w x
        reactimate  $ setValue w <$> e
    where
    setValue w x = setAttr "value" x w >> return ()


