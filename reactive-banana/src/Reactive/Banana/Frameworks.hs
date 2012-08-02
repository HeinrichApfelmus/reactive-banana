{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE CPP, Rank2Types #-}
-- #define UseExtensions 1

module Reactive.Banana.Frameworks (
    -- * Synopsis
    -- | Build event networks using existing event-based frameworks
    -- and run them.
    
    -- * Simple use
    interpretAsHandler,

    -- * Building event networks with input/output
    -- $build
    NetworkDescription, compile,
    AddHandler, fromAddHandler, fromChanges, fromPoll,
    reactimate, initial, changes, now,
    liftIO, liftIOLater,
    
    -- * Running event networks
    EventNetwork, actuate, pause,
    
    -- * Utilities
    -- $utilities
    newAddHandler, newEvent,
    
    -- * Internal
    interpretFrameworks,
    ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fix       (MonadFix(..))
import Control.Monad.IO.Class  (MonadIO(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS

import Data.IORef
import Data.Monoid
import qualified Data.Unique -- ordinary uniques here, because they are Ord

import Reactive.Banana.Internal.InputOutput
import Reactive.Banana.Combinators

import qualified Reactive.Banana.Internal.EventBehavior1 as Prim
import Reactive.Banana.Internal.Types2

import qualified Data.Map as Map

type Map = Map.Map

{-----------------------------------------------------------------------------
    Compilation specific to the different backends
------------------------------------------------------------------------------}
data InputToEvent = InputToEvent (forall a. InputChannel a -> Prim.Event a)
type Compile a b
    =  (InputToEvent -> Prim.MomentT IO (Prim.Event a,b))
    -> IO (Automaton a,b)

compileWithGlobalInput :: Compile a b
compileWithGlobalInput f = do
    rx <- newIORef undefined
    e  <- Prim.compileToAutomatonT $ do
        (e,b) <- f $ InputToEvent Prim.inputE
        liftIO $ writeIORef rx b
        return e
    b <- readIORef rx
    return (e, b)

{-----------------------------------------------------------------------------
    NetworkDescription, setting up event networks
------------------------------------------------------------------------------}
{-$build

    After having read all about 'Event's and 'Behavior's,
    you want to hook them up to an existing event-based framework,
    like @wxHaskell@ or @Gtk2Hs@.
    How do you do that?

    The module presented here allows you to obtain /input/ events
    from external sources
    and it allows you perform /output/ in reaction to events.
    
    In constrast, the functions from "Reactive.Banana.Model" allow you 
    to express the output events in terms of the input events.
    This expression is called an /event graph/.
    
    An /event network/ is an event graph together with inputs and outputs.
    To build an event network,
    describe the inputs, outputs and event graph in the
    'NetworkDescription' monad 
    and use the 'compile' function to obtain an event network from that.

    To /activate/ an event network, use the 'actuate' function.
    The network will register its input event handlers and start 
    producing output.

    A typical setup looks like this:
    
> main = do
>   -- initialize your GUI framework
>   window <- newWindow
>   ...
>
>   -- describe the event network
>   let networkDescription :: forall t. NetworkDescription t ()
>       networkDescription = do
>           -- input: obtain  Event  from functions that register event handlers
>           emouse    <- fromAddHandler $ registerMouseEvent window
>           ekeyboard <- fromAddHandler $ registerKeyEvent window
>           -- input: obtain  Behavior  from changes
>           btext     <- fromChanges    "" $ registerTextChange editBox
>           -- input: obtain  Behavior  from mutable data by polling
>           bdie      <- fromPoll       $ randomRIO (1,6)
>
>           -- express event graph
>           let
>               behavior1 = accumB ...
>               ...
>               event15 = union event13 event14
>   
>           -- output: animate some event occurences
>           reactimate $ fmap print event15
>           reactimate $ fmap drawCircle eventCircle
>
>       -- compile network description into a network
>       network <- compile networkDescription
>       -- register handlers and start producing outputs
>       actuate network

    In short, you use 'fromAddHandler' to obtain /input/ events.
    The library uses this to register event handlers
    with your event-based framework.
    
    To animate /output/ events, use the 'reactimate' function.

-}

type AddHandler'     = AddHandler InputValue
type Preparations t  =
    ( [Event t (IO ())]     -- reactimate outputs
    , [AddHandler']         -- fromAddHandler events 
    , [IO InputValue]       -- fromPoll events
    , [IO ()]               -- liftIOLater
    )

-- | Monad for describing event networks.
-- 
-- The 'NetworkDescription' monad is an instance of 'MonadIO',
-- so 'IO' is allowed inside.
-- 
-- Note: The phantom type @t@ prevents you from smuggling
-- values of types 'Event' or 'Behavior'
-- outside the 'NetworkDescription' monad.
newtype NetworkDescription t a = Prepare
    { unPrepare :: RWST InputToEvent (Preparations t) () (Prim.MomentT IO) a
    }

-- boilerplate class instances
instance Monad (NetworkDescription t) where
    return  = Prepare . return
    m >>= k = Prepare $ unPrepare m >>= unPrepare . k
instance MonadIO (NetworkDescription t) where
    liftIO  = Prepare . liftIO
instance Functor (NetworkDescription t) where
    fmap f  = Prepare . fmap f . unPrepare
instance Applicative (NetworkDescription t) where
    pure    = Prepare . pure
    f <*> a = Prepare $ unPrepare f <*> unPrepare a
instance MonadFix (NetworkDescription t) where
    mfix f  = Prepare $ mfix (unPrepare . f)

{- | Output.
    Execute the 'IO' action whenever the event occurs.
    
    
    Note: If two events occur very close to each other,
    there is no guarantee that the @reactimate@s for one 
    event will have finished before the ones for the next event start executing.
    This does /not/ affect the values of events and behaviors,
    it only means that the @reactimate@ for different events may interleave.
    Fortuantely, this is a very rare occurrence, and only happens if
    
    * you call an event handler from inside 'reactimate',
    
    * or you use concurrency.
    
    In these cases, the @reactimate@s follow the control flow
    of your event-based framework.

-}
reactimate :: Event t (IO ()) -> NetworkDescription t ()
reactimate e = Prepare $ tell ([e],[],[],[])

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
-- When the event network is actuated,
-- this will register a callback function such that
-- an event will occur whenever the callback function is called.
fromAddHandler :: AddHandler a -> NetworkDescription t (Event t a)
fromAddHandler addHandler = do
    (i,e) <- newInput
    let addHandler' k = addHandler $ k . toValue i . (\x -> [x])
    Prepare $ tell ([],[addHandler'],[],[])
    return e

-- create a new input event from the global input event
newInput :: NetworkDescription t (InputChannel [a], Event t a)
newInput = Prepare $ do
    i <- liftIO newInputChannel
    (InputToEvent f) <- ask
    return (i, E $ f i)


-- | Input,
-- obtain a 'Behavior' by frequently polling mutable data, like the current time.
--
-- The resulting 'Behavior' will be updated on whenever the event
-- network processes an input event.
--
-- This function is occasionally useful, but
-- the recommended way to obtain 'Behaviors' is by using 'fromChanges'.
-- 
-- Ideally, the argument IO action just polls a mutable variable,
-- it should not perform expensive computations.
-- Neither should its side effects affect the event network significantly.
fromPoll :: IO a -> NetworkDescription t (Behavior t a)
fromPoll poll = do
    (i,e) <- newInput
    let poll' = toValue i . (:[]) <$> poll
    Prepare $ tell ([],[],[poll'],[])
    initial <- liftIO $ poll
    return $ stepper initial e

-- | Input,
-- obtain a 'Behavior' from an 'AddHandler' that notifies changes.
-- 
-- This is essentially just an application of the 'stepper' combinator.
fromChanges :: a -> AddHandler a -> NetworkDescription t (Behavior t a)
fromChanges initial changes = stepper initial <$> fromAddHandler changes

-- | Output,
-- observe when a 'Behavior' changes.
-- 
-- Strictly speaking, a 'Behavior' denotes a value that
-- varies *continuously* in time,
-- so there is no well-defined event which indicates when the behavior changes.
-- 
-- Still, for reasons of efficiency, the library provides a way to observe
-- changes when the behavior is a step function, for instance as 
-- created by 'stepper'. There are no formal guarantees,
-- but the idea is that
--
-- > changes (stepper x e) = return (calm e)
--
-- WARNING: The values of the event will not become available
-- until event processing is complete. Use them within 'reactimate'.
-- If you try to access them before that, the program
-- will be thrown into an infinite loop.
changes :: Behavior t a -> NetworkDescription t (Event t a)
changes = return . E . Prim.mapE (:[]) . Prim.changesB . unB

-- | Output,
-- observe the initial value contained in a 'Behavior'.
--
-- Similar to 'updates', this function is not well-defined,
-- but exists for reasons of efficiency.
--
-- WARNING: The value will actually only become available
-- at the end of compilation.
-- Use it within 'liftIOLater'.
-- If you try to access it before that, the program will be
-- thrown into an infinite loop.
initial :: Behavior t a -> NetworkDescription t a
initial = now . M . Prim.initialB . unB

-- | Observe a 'Moment' in the 'NetworkDescription' monad.
now :: Moment t a -> NetworkDescription t a
now = Prepare . lift . Prim.liftMoment . unM

-- | Lift an 'IO' action into the 'NetworkDescription' monad,
-- but defer its execution until compilation time.
-- This can be useful for recursive definitions using 'MonadFix'.
liftIOLater :: IO () -> NetworkDescription t ()
liftIOLater m = Prepare $ tell ([],[],[],[m])

-- | Compile a 'NetworkDescription' into an 'EventNetwork'
-- that you can 'actuate', 'pause' and so on.
compile :: (forall t. NetworkDescription t ()) -> IO EventNetwork
compile m = do

    -- compile network description into an automaton
    (automaton,(inputs,polls,liftIOs)) <-
        compileWithGlobalInput $ \inputToEvent -> do
            -- execute the NetworkDescription monad
            (_,_,(outputs,a,b,c))
                <- runRWST (unPrepare m) inputToEvent ()
            -- output event = union of all the reactimates
            let E e = foldr union never outputs
            -- compile the output event
            return (e, (a,b,c))

    -- execute the late IOs after compilation
    sequence_ liftIOs
        
    -- allocate new variable for the automaton
    rautomaton <- newEmptyMVar
    putMVar rautomaton automaton
    
    let -- run the automaton on a single input value
        run :: InputValue -> IO ()
        run input = do
            -- takeMVar  makes sure that event graph updates are atomic
            automaton  <- takeMVar rautomaton
            -- poll mutable data
            pollValues <- sequence polls
            -- percolate inputs through event graph
            (reactimates,automaton') <- runStep automaton $ input:pollValues
            putMVar rautomaton automaton'
            -- Run corresponding IO actions afterwards.
            -- Under certain circumstances, they can *interleave*.
            case reactimates of
                Just actions -> sequence_ actions
                Nothing      -> return ()
    
        -- register event handlers
        register :: IO (IO ())
        register = fmap sequence_ . sequence . map ($ run) $ inputs

    makeEventNetwork register


{-----------------------------------------------------------------------------
    Running event networks
------------------------------------------------------------------------------}
-- | Data type that represents a compiled event network.
-- It may be paused or already running.
data EventNetwork = EventNetwork {
    -- | Actuate an event network.
    -- The inputs will register their event handlers, so that
    -- the networks starts to produce outputs in response to input events.
    actuate :: IO (),
    
    -- | Pause an event network.
    -- Immediately stop producing output and
    -- unregister all event handlers for inputs.
    -- Hence, the network stops responding to input events,
    -- but it's state will be preserved.
    --
    -- You can resume the network with 'actuate'.
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
        actuate = register >>= writeIORef unregister
        pause   = readIORef unregister >>= id >> writeIORef unregister nop
    return $ EventNetwork actuate pause


{-----------------------------------------------------------------------------
    Simple use
------------------------------------------------------------------------------}
-- | Interpret by using a framework internally.
-- Only useful for testing library internals.
interpretFrameworks :: (forall t. Event t a -> Event t b) -> [a] -> IO [[b]]
interpretFrameworks f xs = do
    output                    <- newIORef []
    (addHandler, runHandlers) <- newAddHandler
    network                   <- compile $ do
        e <- fromAddHandler addHandler
        reactimate $ fmap (\b -> modifyIORef output (++[b])) (f e)

    actuate network
    bs <- forM xs $ \x -> do
        runHandlers x
        bs <- readIORef output
        writeIORef output []
        return bs
    return bs

-- | Simple way to write a single event handler with
-- functional reactive programming.
interpretAsHandler
    :: (forall t. Event t a -> Event t b)
    -> AddHandler a -> AddHandler b
interpretAsHandler f addHandlerA = \handlerB -> do
    network <- compile $ do
        e <- fromAddHandler addHandlerA
        reactimate $ handlerB <$> f e
    actuate network
    return (pause network)


{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
{-$utilities

    This section collects a few convenience functions
    for unusual use cases. For instance:
    
    * The event-based framework you want to hook into is poorly designed
    
    * You have to write your own event loop and roll a little event framework

-}

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


-- | Build an 'Event' together with an 'IO' action that can 
-- fire occurrences of this event. Variant of 'newAddHandler'.
-- 
-- This function is mainly useful for passing callback functions
-- inside a 'reactimate'.
newEvent :: NetworkDescription t (Event t a, a -> IO ())
newEvent = do
    (addHandler, fire) <- liftIO $ newAddHandler
    e <- fromAddHandler addHandler
    return (e,fire)
