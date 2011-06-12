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

    -- * Build event networks with input and output
    -- $build
    NetworkDescription, compile, EventNetwork,
    AddHandler, fromAddHandler, reactimate, liftIO,
    
    -- * Run event networks
    run, pause, reset,
    
    module Data.Dynamic,
    ) where

import Reactive.Banana.PushIO hiding (compile)
import qualified Reactive.Banana.PushIO as Implementation
-- import Reactive.Banana.Model hiding (Event, Behavior, run)
import qualified Reactive.Banana.Model as Model
import Data.Dynamic

import Data.List (nub)
import Control.Applicative
import Control.Monad.RWS
import Data.IORef

-- debug = putStrLn

{-----------------------------------------------------------------------------
    PushIO specific functions
------------------------------------------------------------------------------}
type Flavor  = Implementation.PushIO

input :: Typeable a => Channel -> Model.Event Flavor a
input = event . Input

compileHandlers :: Model.Event Flavor (IO ()) -> IO [(Channel, Universe -> IO ())]
compileHandlers network = do
    -- compile network
    let network' = Implementation.unEvent network
    (paths,cache) <- Implementation.compile (invalidRef, Reactimate network')
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

type AddHandler'  = (Channel, (Universe -> IO ()) -> IO ())
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
-- destroying the earth with time-traveling zygohistomorphisms.
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
type AddHandler a = (a -> IO ()) -> IO ()

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
-- that you can 'run', 'pause' and 'reset'.
compile :: NetworkDescription () -> IO EventNetwork
compile (Prepare m) = do
    (_,_,(outputs,inputs)) <- runRWST m () 0
    let
        -- union of all  reactimates
        network = mconcat outputs :: Model.Event Flavor (IO ())

    -- compile network
    paths <- compileHandlers network
    -- register event handlers
    return $ sequence_ . map snd . applyChannels inputs $ paths

-- FIXME: make this faster
applyChannels :: [(Channel, a -> b)] -> [(Channel, a)] -> [(Channel, b)]
applyChannels fs xs =
    [(i, f x) | (i,f) <- fs, (j,x) <- xs, i == j]

{-----------------------------------------------------------------------------
    Running event networks
------------------------------------------------------------------------------}
type EventNetwork = IO ()

-- | Run an event network.
-- The inputs will register their event handlers, so that
-- the networks starts to produce outputs in response to input events.
run   :: EventNetwork -> IO ()
run = id

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
pause :: EventNetwork -> IO ()
pause network = return ()

-- | Reset an event network.
-- Immediately pauses the event network and also resets its state.
--
-- You can restart the network with 'run'.
reset :: EventNetwork -> IO ()
reset network = pause network >> return ()

{-----------------------------------------------------------------------------
    interpret
------------------------------------------------------------------------------}
-- | Simple way to run an event graph. Very useful for testing.
interpret :: Typeable a
    => (Model.Event PushIO a -> Model.Event PushIO b) -> [a] -> IO [[b]]
interpret f xs = do
    oref <- newIORef []

    href <- newIORef []
    let addHandler k = modifyIORef href (++[k])

    network <- compile $ do
        e <- fromAddHandler addHandler
        reactimate $ fmap (\b -> modifyIORef oref (++[b])) (f e)

    handler <- (\ks x -> mapM ($ x) ks) <$> readIORef href

    run network
    bs <- forM xs $ \x -> do
        handler x
        bs <- readIORef oref
        writeIORef oref []
        return bs
    reset network
    return bs
