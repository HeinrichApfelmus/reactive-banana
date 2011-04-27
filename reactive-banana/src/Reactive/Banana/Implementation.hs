{-----------------------------------------------------------------------------
    Reactive Banana
    
    Linking any implementation to an event-based framework
------------------------------------------------------------------------------}
module Reactive.Banana.Implementation (
    -- * Synopsis
    -- | Run event networks and hook them up to existing event-based frameworks.
    
    -- * Implementation
    PushIO, run,

    -- * Using existing event-based frameworks
    -- $Prepare
    Prepare, prepareEvents, reactimate, AddHandler, fromAddHandler, liftIO,
    
    module Data.Dynamic,
    ) where

import Reactive.Banana.PushIO as Implementation
-- import Reactive.Banana.Model hiding (Event, Behavior, run)
import qualified Reactive.Banana.Model as Model
import Data.Dynamic

import Data.List (nub)
import Control.Applicative
import Control.Monad.RWS
import Data.IORef

{-----------------------------------------------------------------------------
    PushIO specific functions
------------------------------------------------------------------------------}
type Flavor = PushIO

input :: Typeable a => Channel -> Model.Event PushIO a
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
    Setting up an event network
------------------------------------------------------------------------------}
{-$Prepare

    After having read all about 'Event's and 'Behavior's,
    you want to hook things up to an existing event-based framework,
    like @wxHaskell@ or @Gtk2Hs@.
    How do you do that?

    To do that, you have to use the 'Prepare' monad.
    The typical setup looks like this:
    
> main = do
>   ... -- other initialization
>
>   -- initialize event network
>   prepareEvents $ do
>       -- obtain  Event  from functions that register event handlers
>       emouse    <- fromAddHandler (registerMouseEvent window)
>       ekeyboard <- fromAddHandler (registerKeyEvent window)
>   
>       -- build event network
>       let
>           behavior1 = accumB ...
>           ...
>           event15 = union event13 event14
>   
>       -- animate relevant event occurences
>       reactimate $ fmap print event15
>       reactimate $ fmap drawCircle eventCircle
>
>   ... -- start the GUI framework here
    
    In short, you use 'fromAddHandler' to obtain /input events/;
    the library will register corresponding event handlers
    with your event-based framework.
    
    To animate /output events/, you use the 'reactimate' function.
    
    The whole setup has to be wrapped into a call to 'prepareEvents'.
    
    The 'Prepare' monad is an instance of 'MonadIO',
    so 'IO' is allowed inside. However, you can't pass anything
    of type @Event@ or @Behavior@ outside the 'prepareEvents' call;
    this is intentional.
    (You can probably circumvent this with mutable variables,
    but there is a 99,8% chance that earth will be suspended
    by time-traveling zygohistomorphisms
    if you do that; you have been warned.)

-}

type AddHandler'  = (Channel, (Universe -> IO ()) -> IO ())
type Preparations = ([Model.Event Flavor (IO ())], [AddHandler'])
newtype Prepare a = Prepare { unPrepare :: RWST () Preparations Channel IO a }

instance Monad (Prepare) where
    return  = Prepare . return
    m >>= k = Prepare $ unPrepare m >>= unPrepare . k
instance MonadIO Prepare where
    liftIO  = Prepare . liftIO

-- | Animate an output event.
-- Executes the 'IO' action whenever the event occurs.
reactimate :: Model.Event PushIO (IO ()) -> Prepare ()
reactimate e = Prepare $ tell ([e], [])

-- | Wrap around the 'Prepare' monad to set up an event network.
prepareEvents :: Prepare () -> IO ()
prepareEvents (Prepare m) = do
    (_,_,(outputs,inputs)) <- runRWST m () 0
    let
        -- union of all  reactimates
        network = mconcat outputs :: Model.Event PushIO (IO ())
    -- compile network
    paths <- compileHandlers network
    -- register event handlers
    sequence_ . map snd . applyChannels inputs $ paths

-- FIXME: make this faster
applyChannels :: [(Channel, a -> b)] -> [(Channel, a)] -> [(Channel, b)]
applyChannels fs xs =
    [(i, f x) | (i,f) <- fs, (j,x) <- xs, i == j]

-- | A value of type @AddHandler a@ is just an IO function that registers
-- callback functions, also known as event handlers. 
type AddHandler a = (a -> IO ()) -> IO ()

-- | Obtain an 'Event' from an 'AddHandler'.
-- This will register a callback function such that
-- an event will occur whenever the callback function is called.
fromAddHandler :: Typeable a => AddHandler a -> Prepare (Model.Event PushIO a)
fromAddHandler addHandler = Prepare $ do
        channel <- newChannel
        let addHandler' k = addHandler $ k . toUniverse channel
        tell ([], [(channel, addHandler')])
        return $ input channel
    where
    newChannel = do c <- get; put $! c+1; return c

{-----------------------------------------------------------------------------
    Run function for testing
------------------------------------------------------------------------------}
-- | Running an event network for the purpose of easy testing.
run :: Typeable a
    => (Model.Event PushIO a -> Model.Event PushIO b) -> [a] -> IO [[b]]
run f xs = do
    oref <- newIORef []

    href <- newIORef []
    let addHandler k = modifyIORef href (++[k])

    prepareEvents $ do
        e <- fromAddHandler addHandler
        reactimate $ fmap (\b -> modifyIORef oref (++[b])) (f e)

    handler <- (\ks x -> mapM ($ x) ks) <$> readIORef href

    forM xs $ \x -> do
        handler x
        bs <- readIORef oref
        writeIORef oref []
        return bs
