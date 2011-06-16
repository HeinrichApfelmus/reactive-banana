{-----------------------------------------------------------------------------
    Reactive Banana
    
    Linking any implementation to an event-based framework
------------------------------------------------------------------------------}
module Reactive.Banana.Implementation (
    -- * Synopsis
    -- | Build event networks using existing event-based frameworks
    --   and run them.
    
    -- * Implementation
    PushIO, interpret,

    -- * Building event networks with input and output
    -- $build
    NetworkDescription, compile,
    AddHandler, fromAddHandler, reactimate, liftIO,
    
    -- * Running event networks
    EventNetwork, run, pause,
    
    -- * Utilities
    newAddHandler,
    
    module Data.Dynamic,
    ) where

import Reactive.Banana.PushIO hiding (compile)
import qualified Reactive.Banana.PushIO as Implementation
-- import Reactive.Banana.Model hiding (Event, Behavior, run)
import qualified Reactive.Banana.Model as Model

import Control.Applicative
import Control.Monad.RWS

import Data.Dynamic
import Data.List (nub)
import Data.IORef
import qualified Data.Map as Map
import Data.Unique

-- debug = putStrLn

{-----------------------------------------------------------------------------
    PushIO specific functions
------------------------------------------------------------------------------}
type Flavor  = Implementation.PushIO

input :: Typeable a => Channel -> Model.Event Flavor a
input = event . Input

compileHandlers :: Model.Event Flavor (IO ()) -> IO [(Channel, Universe -> IO ())]
compileHandlers graph = do
    -- compile event graph
    let graph' = Implementation.unEvent graph
    (paths,cache) <- Implementation.compile (invalidRef, Reactimate graph')
    -- reduce to one path per channel
    let paths1 = groupChannelsBy (\p q x -> p x >> q x) paths

    -- prepare threading the cache as state
    rcache <- newIORef emptyCache
    writeIORef rcache cache
    let run m = do
            cache <- readIORef rcache
            (_,cache') <- runRun m cache
            writeIORef rcache cache'
        paths2 = map (\(i,p) -> (i, run . p)) $ paths1
    
    return paths2


-- FIXME: make this faster
groupChannelsBy :: (a -> a -> a) -> [(Channel, a)] -> [(Channel, a)]
groupChannelsBy f xs = [(i, foldr1 f [x | (j,x) <- xs, i == j]) | i <- channels]
    where channels = nub . map fst $ xs

{-----------------------------------------------------------------------------
    NetworkDescription, setting up event networks
------------------------------------------------------------------------------}
{-$build

    After having read all about 'Event's and 'Behavior's,
    you want to hook them up to an existing event-based framework,
    like @wxHaskell@ or @Gtk2Hs@.
    How do you do that?

    This "Reactive.Banana.Implementation" module allows you to obtain /input/ events
    from external sources
    and it allows you perform /output/ in reaction to events.
    
    In constrast, the functions from "Reactive.Banana.Model" allow you 
    to express the output events in terms of the input events.
    This expression is called an /event graph/.
    
    An /event network/ is an event graph together with inputs and outputs.
    To build an event network,
    describe the inputs, outputs and event graph in the 'NetworkDescription' monad 
    and use the 'compile' function to obtain an event network from that.

    To /run/ an event network, use the 'run' function.
    The network will register its input event handlers and start producing output.

    A typical setup looks like this:
    
> main = do
>   -- initialize your GUI framework
>   window <- newWindow
>   ...
>
>   -- build the event network
>   network <- compile $ do
>       -- input: obtain  Event  from functions that register event handlers
>       emouse    <- fromAddHandler (registerMouseEvent window)
>       ekeyboard <- fromAddHandler (registerKeyEvent window)
>   
>       -- express event graph
>       let
>           behavior1 = accumB ...
>           ...
>           event15 = union event13 event14
>   
>       -- output: animate some event occurences
>       reactimate $ fmap print event15
>       reactimate $ fmap drawCircle eventCircle
>
>   -- register handlers and start producing outputs
>   run network

    In short, you use 'fromAddHandler' to obtain /input/ events.
    The library uses this to register event handlers
    with your event-based framework.
    
    To animate /output/ events, use the 'reactimate' function.

-}

type AddHandler'  = (Channel, AddHandler Universe)
type Preparations = ([Model.Event Flavor (IO ())], [AddHandler'])

-- | Monad for describing event networks.
-- 
-- The 'NetworkDescription' monad is an instance of 'MonadIO',
-- so 'IO' is allowed inside.
-- 
-- Note: It is forbidden to smuggle values of types 'Event' or 'Behavior'
-- outside the 'NetworkDescription' monad. This shouldn't be possible by default,
-- but you might get clever and use 'IORef' to circumvent this.
-- Don't do that, it won't work and also has a 99,98% chance of 
-- destroying the earth by summoning time-traveling zygohistomorphisms.
newtype NetworkDescription a = Prepare { unPrepare :: RWST () Preparations Channel IO a }

instance Monad (NetworkDescription) where
    return  = Prepare . return
    m >>= k = Prepare $ unPrepare m >>= unPrepare . k
instance MonadIO (NetworkDescription) where
    liftIO  = Prepare . liftIO
instance Functor (NetworkDescription) where
    fmap f  = Prepare . fmap f . unPrepare
instance Applicative (NetworkDescription) where
    pure    = Prepare . pure
    f <*> a = Prepare $ unPrepare f <*> unPrepare a

-- | Output.
-- Execute the 'IO' action whenever the event occurs.
reactimate :: Model.Event PushIO (IO ()) -> NetworkDescription ()
reactimate e = Prepare $ tell ([e], [])

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

-- | Input,
-- obtain an 'Event' from an 'AddHandler'.
--
-- When the event network is run,
-- this will register a callback function such that
-- an event will occur whenever the callback function is called.
fromAddHandler :: Typeable a => AddHandler a -> NetworkDescription (Model.Event PushIO a)
fromAddHandler addHandler = Prepare $ do
        channel <- newChannel
        let addHandler' k = addHandler $ k . toUniverse channel
        tell ([], [(channel, addHandler')])
        return $ input channel
    where
    newChannel = do c <- get; put $! c+1; return c

-- | Compile a 'NetworkDescription' into an 'EventNetwork'
-- that you can 'run', 'pause' and so on.
compile :: NetworkDescription () -> IO EventNetwork
compile (Prepare m) = do
    (_,_,(outputs,inputs)) <- runRWST m () 0
    
    let -- union of all  reactimates
        graph = mconcat outputs :: Model.Event Flavor (IO ())
    paths <- compileHandlers graph
    
    let -- register event handlers
        register = fmap sequence_ . sequence . map snd . applyChannels inputs $ paths
    makeEventNetwork register

-- FIXME: make this faster
applyChannels :: [(Channel, a -> b)] -> [(Channel, a)] -> [(Channel, b)]
applyChannels fs xs =
    [(i, f x) | (i,f) <- fs, (j,x) <- xs, i == j]

{-----------------------------------------------------------------------------
    Running event networks
------------------------------------------------------------------------------}
-- | Data type that represents a compiled event network.
-- It may be paused or already running.
data EventNetwork = EventNetwork {
    -- | Run an event network.
    -- The inputs will register their event handlers, so that
    -- the networks starts to produce outputs in response to input events.
    run :: IO (),
    
    -- | Pause an event network.
    -- Immediately stop producing output and
    -- unregister all event handlers for inputs.
    -- Hence, the network stops responding to input events,
    -- but it's state will be preserved.
    --
    -- You can resume the network with 'run'.
    --
    -- Note: You can stop a network even while it is processing events,
    -- i.e. you can use 'pause' as an argument to 'reactimate'.
    -- The network will /not/ stop immediately though, only after
    -- the current event has been processed completely.
    pause :: IO ()
    }

-- Make an event network from a function that registers all event handlers
makeEventNetwork :: IO (IO ()) -> IO EventNetwork
makeEventNetwork register = do
    let nop = return ()
    unregister <- newIORef nop
    let
        run   = register >>= writeIORef unregister
        pause = readIORef unregister >>= id >> writeIORef unregister nop
    return $ EventNetwork run pause


{-----------------------------------------------------------------------------
    Interpreter for testing
------------------------------------------------------------------------------}
-- | Simple way to run an event graph. Very useful for testing.
interpret :: Typeable a
    => (Model.Event PushIO a -> Model.Event PushIO b) -> [a] -> IO [[b]]
interpret f xs = do
    output                    <- newIORef []
    (addHandler, runHandlers) <- newAddHandler
    network                   <- compile $ do
        e <- fromAddHandler addHandler
        reactimate $ fmap (\b -> modifyIORef output (++[b])) (f e)

    run network
    bs <- forM xs $ \x -> do
        runHandlers x
        bs <- readIORef output
        writeIORef output []
        return bs
    return bs


{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
-- | Build a facility to register and unregister event handlers.
-- 
-- This function is only useful if you want to hook up this library
-- to a poorly designed event-based framework, or roll your own.
newAddHandler :: IO (AddHandler a, a -> IO ())
newAddHandler = do
    handlers <- newIORef Map.empty
    let addHandler k = do
            key <- newUnique
            modifyIORef handlers $ Map.insert key k
            return $ modifyIORef handlers $ Map.delete key
        runHandlers x =
            mapM_ ($ x) . map snd . Map.toList =<< readIORef handlers
    return (addHandler, runHandlers)

