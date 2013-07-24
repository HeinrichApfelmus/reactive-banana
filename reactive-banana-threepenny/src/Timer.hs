{-# LANGUAGE RecordWildCards #-}

module Timer (
    Timer, newTimer,
    interval, running, tick, start, stop,
    ) where

import Control.Monad (when, forever, void)
import Control.Event
import Control.Concurrent
import Control.Concurrent.STM

import Graphics.UI.Threepenny.Core

data Timer = Timer
    { tRunning  :: TVar Bool
    , tInterval :: TVar Int      -- in ms
    , tTick     :: Event ()
    }

-- | Create a new timer
newTimer :: IO Timer
newTimer = do
    tRunning      <- newTVarIO False
    tInterval     <- newTVarIO 1000
    (tTick, fire) <- newEvent
    
    forkIO $ forever $ do
        wait <- atomically $ do
            b <- readTVar tRunning
            when (not b) retry
            readTVar tInterval
        threadDelay (wait * 1000)
        fire ()
        
    return $ Timer {..}

tick = tTick

-- | Timer interval in milliseconds.
interval :: Attr Timer Int
interval = fromTVar tInterval

-- | Whether the timer is running or not.
running :: Attr Timer Bool
running = fromTVar tRunning

-- | Start the timer.
start :: Timer -> IO ()
start = set' running True

-- | Stop the timer.
stop :: Timer -> IO ()
stop = set' running False

fromTVar :: (x -> TVar a) -> Attr x a
fromTVar f = mkReadWriteAttr
    (atomically . readTVar . f)
    (\i x -> atomically $ writeTVar (f x) i)


testTimer = do
    t <- newTimer
    void $ register (tick t) $ const $ putStr "Hello"
    return t
        # set interval 1000
        # set running True
    
    

