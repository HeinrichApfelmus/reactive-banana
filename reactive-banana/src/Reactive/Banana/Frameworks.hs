{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types #-}

module Reactive.Banana.Frameworks (
    -- * Synopsis
    -- | Build event networks using existing event-based frameworks
    -- and run them.
    
    -- * Simple use
    interpretAsHandler,

    -- * Building event networks with input/output
    -- $build
    compile, Frameworks,
    AddHandler, fromAddHandler, fromChanges, fromPoll,
    reactimate, initial, changes, imposeChanges,
    FrameworksMoment(..), execute, liftIOLater,
    -- $liftIO
    module Control.Monad.IO.Class,
    
    -- * Running event networks
    EventNetwork, actuate, pause,
    
    -- * Utilities
    -- $utilities
    newAddHandler, newEvent,
    module Reactive.Banana.Frameworks.AddHandler,
    
    -- * Internal
    interpretFrameworks,
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef

import           Reactive.Banana.Combinators
import           Reactive.Banana.Frameworks.AddHandler
import qualified Reactive.Banana.Internal.Combinators  as Prim
import           Reactive.Banana.Internal.Phantom
import           Reactive.Banana.Types


{-----------------------------------------------------------------------------
    Documentation
------------------------------------------------------------------------------}
{-$build

After having read all about 'Event's and 'Behavior's,
you want to hook them up to an existing event-based framework,
like @wxHaskell@ or @Gtk2Hs@.
How do you do that?

The module presented here allows you to

* obtain /input/ events from external sources and to

* perform /output/ in reaction to events.

In constrast, the functions from "Reactive.Banana.Combinators" allow you 
to express the output events in terms of the input events.
This expression is called an /event graph/.

An /event network/ is an event graph together with inputs and outputs.
To build an event network,
describe the inputs, outputs and event graph in the
'Moment' monad 
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
>   let networkDescription :: forall t. Frameworks t => Moment t ()
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
>   -- compile network description into a network
>   network <- compile networkDescription
>   -- register handlers and start producing outputs
>   actuate network

In short,

* Use 'fromAddHandler' to obtain /input/ events.
The library uses this to register event handlers with your event-based framework.

* Use 'reactimate' to animate /output/ events.

-}

{-----------------------------------------------------------------------------
    Combinators
------------------------------------------------------------------------------}
singletonsE :: Prim.Event a -> Event t a
singletonsE = E . Prim.mapE (:[])

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
    
Note: An event network essentially behaves like a single,
huge callback function. The 'IO' action are not run in a separate thread.
The callback function will throw an exception if one of your 'IO' actions
does so as well.
Your event-based framework will have to handle this situation.

-}
reactimate :: Frameworks t => Event t (IO ()) -> Moment t ()
reactimate = M . Prim.addReactimate . Prim.mapE sequence_ . unE

-- | Input,
-- obtain an 'Event' from an 'AddHandler'.
--
-- When the event network is actuated,
-- this will register a callback function such that
-- an event will occur whenever the callback function is called.
fromAddHandler :: Frameworks t => AddHandler a -> Moment t (Event t a)
fromAddHandler = M . fmap singletonsE . Prim.fromAddHandler

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
fromPoll :: Frameworks t => IO a -> Moment t (Behavior t a)
fromPoll = M . fmap B . Prim.fromPoll

-- | Input,
-- obtain a 'Behavior' from an 'AddHandler' that notifies changes.
-- 
-- This is essentially just an application of the 'stepper' combinator.
fromChanges :: Frameworks t => a -> AddHandler a -> Moment t (Behavior t a)
fromChanges initial changes = stepper initial <$> fromAddHandler changes

-- | Output,
-- observe the initial value contained in a 'Behavior'.
initial :: Behavior t a -> Moment t a
initial = M . Prim.initialB . unB

-- | Output,
-- observe when a 'Behavior' changes.
-- 
-- Strictly speaking, a 'Behavior' denotes a value that
-- varies /continuously/ in time,
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
-- until event processing is complete.
-- It is ok to use this as a direct source to 'reactimate',
-- but most other uses will likely result in a runtime error.
-- For example, 'filterJust' will throw a runtime error
-- once applied to the result of 'changes'.
changes :: Frameworks t => Behavior t a -> Moment t (Event t a)
changes = return . singletonsE . Prim.changesB . unB

-- | Impose a different sampling event on a 'Behavior'.
--
-- The 'Behavior' will vary continuously as before, but the event returned
-- by the 'changes' function will now happen simultaneously with the
-- imposed event.
--
-- Note: This function is useful only in very specific circumstances.
imposeChanges :: Frameworks t => Behavior t a -> Event t () -> Behavior t a
imposeChanges b e = B $ Prim.imposeChanges (unB b) (Prim.mapE (const ()) (unE e))

-- | Dummy type needed to simulate impredicative polymorphism.
newtype FrameworksMoment a
    = FrameworksMoment
    { runFrameworksMoment :: forall t. Frameworks t => Moment t a }

unFM :: FrameworksMoment a -> Moment (FrameworksD,t) a
unFM = runFrameworksMoment

-- | Dynamically add input and output to an existing event network.
--
-- Note: You can even do 'IO' actions here, but there is no
-- guarantee about the order in which they are executed.
execute
    :: Frameworks t
    => Event t (FrameworksMoment a)
    -> Moment t (Event t a)
execute = M
    . fmap singletonsE . Prim.executeE
    . Prim.mapE (fmap last . sequence . map (unM . unFM) )
    . unE

-- $liftIO
-- 
-- > liftIO :: Frameworks t => IO a -> Moment t a
--
-- Lift an 'IO' action into the 'Moment' monad.

-- | Lift an 'IO' action into the 'Moment' monad,
-- but defer its execution until compilation time.
-- This can be useful for recursive definitions using 'MonadFix'.
liftIOLater :: Frameworks t => IO () -> Moment t ()
liftIOLater = M . Prim.liftIOLater

-- | Compile the description of an event network
-- into an 'EventNetwork'
-- that you can 'actuate', 'pause' and so on.
--
-- Event networks are described in the 'Moment' monad
-- and use the 'Frameworks' class constraint.
compile :: (forall t. Frameworks t => Moment t ()) -> IO EventNetwork
compile m = fmap EN $ Prim.compile $ unM (m :: Moment (FrameworksD, t) ())

{-----------------------------------------------------------------------------
    Running event networks
------------------------------------------------------------------------------}
-- | Data type that represents a compiled event network.
-- It may be paused or already running.
newtype EventNetwork = EN { unEN :: Prim.EventNetwork }

-- | Actuate an event network.
-- The inputs will register their event handlers, so that
-- the networks starts to produce outputs in response to input events.
actuate :: EventNetwork -> IO ()
actuate = Prim.actuate . unEN

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
pause :: EventNetwork -> IO ()
pause   = Prim.pause . unEN

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

-- | Build an 'Event' together with an 'IO' action that can 
-- fire occurrences of this event. Variant of 'newAddHandler'.
-- 
-- This function is mainly useful for passing callback functions
-- inside a 'reactimate'.
newEvent :: Frameworks t => Moment t (Event t a, a -> IO ())
newEvent = do
    (addHandler, fire) <- liftIO $ newAddHandler
    e <- fromAddHandler addHandler
    return (e,fire)
