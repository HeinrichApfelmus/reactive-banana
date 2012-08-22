{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Frameworks.AddHandler (
    -- * Synopsis
    -- | Various utility functions concerning event handlers.
    
    -- * Documentation
    AddHandler, newAddHandler,
    mapIO, filterAddHandler,
    ) where


import Data.IORef
import qualified Data.Unique -- ordinary uniques here, because they are Ord

import qualified Data.Map as Map

type Map = Map.Map

{-----------------------------------------------------------------------------
    AddHandler
------------------------------------------------------------------------------}
-- | A value of type @AddHandler a@ is just a facility for registering
-- callback functions, also known as event handlers.
-- 
-- The type is a bit mysterious, it works like this:
-- 
-- > do unregisterMyHandler <- addHandler myHandler
--
-- The argument is an event handler that will be registered.
-- The return value is an action that unregisters this very event handler again.
type AddHandler a = (a -> IO ()) -> IO (IO ())

-- | Apply a function with side effects to an 'AddHandler'
mapIO :: (a -> IO b) -> AddHandler a -> AddHandler b
mapIO f addHandler = \h -> addHandler $ \x -> f x >>= h 

-- | Filter event occurrences that don't return 'True'.
filterAddHandler :: (a -> IO Bool) -> AddHandler a -> AddHandler a
filterAddHandler f addHandler = \h ->
    addHandler $ \x -> f x >>= \b -> if b then h x else return ()

-- | Build a facility to register and unregister event handlers.
newAddHandler :: IO (AddHandler a, a -> IO ())
newAddHandler = do
    handlers <- newIORef Map.empty
    let addHandler k = do
            key <- Data.Unique.newUnique
            modifyIORef handlers $ Map.insert key k
            return $ modifyIORef handlers $ Map.delete key
        runHandlers x =
            mapM_ ($ x) . map snd . Map.toList =<< readIORef handlers
    return (addHandler, runHandlers)

