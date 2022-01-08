module Control.Event.Handler (
    -- * Synopsis
    -- | <http://en.wikipedia.org/wiki/Event-driven_programming Event-driven programming>
    -- in the traditional imperative style.

    -- * Documentation
    Handler, AddHandler(..), newAddHandler,
    mapIO, filterIO,
    ) where


import           Control.Monad ((>=>), when)
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map    as Map
import           Data.Monoid.Compat (Ap(Ap), getAp)
import qualified Data.Unique

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
-- | An /event handler/ is a function that takes an
-- /event value/ and performs some computation.
type Handler a = a -> IO ()

-- | The type 'AddHandler' represents a facility for registering
-- event handlers. These will be called whenever the event occurs.
--
-- When registering an event handler, you will also be given an action
-- that unregisters this handler again.
--
-- > do unregisterMyHandler <- register addHandler myHandler
--
newtype AddHandler a = AddHandler { register :: Handler a -> IO (IO ()) }

{-----------------------------------------------------------------------------
    Combinators
------------------------------------------------------------------------------}
instance Functor AddHandler where
    fmap f = mapIO (return . f)

-- | Map the event value with an 'IO' action.
mapIO :: (a -> IO b) -> AddHandler a -> AddHandler b
mapIO f e = AddHandler $ \h -> register e (f >=> h)

-- | Filter event values that don't return 'True'.
filterIO :: (a -> IO Bool) -> AddHandler a -> AddHandler a
filterIO f e = AddHandler $ \h ->
    register e $ \x -> f x >>= \b -> when b $ h x

{-----------------------------------------------------------------------------
    Construction
------------------------------------------------------------------------------}
-- | Build a facility to register and unregister event handlers.
-- Also yields a function that takes an event handler and runs all the registered
-- handlers.
--
-- Example:
--
-- > do
-- >     (addHandler, fire) <- newAddHandler
-- >     register addHandler putStrLn
-- >     fire "Hello!"
newAddHandler :: IO (AddHandler a, Handler a)
newAddHandler = do
    handlers <- newIORef Map.empty
    let register handler = do
            key <- Data.Unique.newUnique
            atomicModifyIORef_ handlers $ Map.insert key handler
            return $ atomicModifyIORef_ handlers $ Map.delete key
        runHandlers a =
            runAll a =<< readIORef handlers
    return (AddHandler register, runHandlers)
    where
        -- This function can also be seen as
        --
        --   runAll a fs = mapM_ ($ a) fs
        --
        -- The reason we write this using `Ap` and `ReaderT` is to produce code
        -- that doesn't allocate. See https://github.com/HeinrichApfelmus/reactive-banana/pull/237
        -- for more info.
        runAll :: Foldable f => a -> f (a -> IO ()) -> IO ()
        runAll a fs = runReaderT (getAp (foldMap (Ap . ReaderT) fs)) a

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f = atomicModifyIORef ref $ \x -> (f x, ())
