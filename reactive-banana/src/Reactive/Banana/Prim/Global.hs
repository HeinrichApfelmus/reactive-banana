{-----------------------------------------------------------------------------
    Compilation.
    State machine IO stuff.
------------------------------------------------------------------------------}
-- compile to an event network
compile :: BuildIO () -> IO EventNetwork
compile setup = do
    actuated <- newIORef False                   -- flag to set running status
    rstate   <- newEmptyMVar                     -- setup callback machinery
    let
        whenFlag flag action = readIORef flag >>= \b -> when b action
        callback inputs = whenFlag actuated $ do
            state1 <- takeMVar rstate            -- read and take lock
            -- pollValues <- sequence polls      -- poll mutable data
            (reactimates, state2)
                <- step callback inputs state1   -- calculate new state
            putMVar rstate state2                -- write state
            reactimates                          -- run IO actions afterwards

    (_, state) <-
        runBuildIO callback emptyState setup     -- compile initial graph
    putMVar rstate state                         -- set initial state
        
    return $ EventNetwork
        { actuate = writeIORef actuated True
        , pause   = writeIORef actuated False
        }
