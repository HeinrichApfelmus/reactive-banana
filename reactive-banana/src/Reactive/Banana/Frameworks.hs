{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types #-}

module Reactive.Banana.Frameworks (
    -- * Synopsis
    -- | Connect to the outside world by building 'EventNetwork's
    -- and running them.

    -- * Simple use
    interpretAsHandler,

    -- * Overview
    -- $build

    -- * Building event networks with input/output
    -- ** Core functions
    compile, MomentIO,
    module Control.Event.Handler,
    fromAddHandler, fromChanges, fromPoll,
    reactimate, Future, reactimate',
    changes,
    -- $changes
    imposeChanges,
    execute, liftIOLater,
    -- $liftIO
    module Control.Monad.IO.Class,

    -- ** Utility functions
    -- | This section collects a few convience functions
    -- built from the core functions.
    newEvent, mapEventIO, newBehavior,

    -- * Running event networks
    EventNetwork, actuate, pause,

    -- * Internal
    interpretFrameworks, showNetwork,
    ) where

import           Control.Event.Handler
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Reactive.Banana.Combinators
import qualified Reactive.Banana.Internal.Combinators as Prim
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

In contrast, the functions from "Reactive.Banana.Combinators" allow you
to express the output events in terms of the input events.
This expression is called an /event graph/.

An /event network/ is an event graph together with inputs and outputs.
To build an event network,
describe the inputs, outputs and event graph in the
'MomentIO' monad
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
>   let networkDescription :: MomentIO ()
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
>           behavior1 <- accumB ...
>           let
>               ...
>               event15 = union event13 event14
>
>           -- output: animate some event occurrences
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

* Use 'compile' to put everything together in an 'EventNetwork's
and use 'actuate' to start handling events.

-}

{-----------------------------------------------------------------------------
    Combinators
------------------------------------------------------------------------------}
{- | Output.
Execute the 'IO' action whenever the event occurs.


Note: If two events occur very close to each other,
there is no guarantee that the @reactimate@s for one
event will have finished before the ones for the next event start executing.
This does /not/ affect the values of events and behaviors,
it only means that the @reactimate@ for different events may interleave.
Fortunately, this is a very rare occurrence, and only happens if

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
reactimate :: Event (IO ()) -> MomentIO ()
reactimate = MIO . Prim.addReactimate . Prim.mapE return . unE

-- | Output.
-- Execute the 'IO' action whenever the event occurs.
--
-- This version of 'reactimate' can deal with values obtained
-- from the 'changes' function.
reactimate' :: Event (Future (IO ())) -> MomentIO ()
reactimate' = MIO . Prim.addReactimate . Prim.mapE unF . unE


-- | Input,
-- obtain an 'Event' from an 'AddHandler'.
--
-- When the event network is actuated,
-- this will register a callback function such that
-- an event will occur whenever the callback function is called.
fromAddHandler ::AddHandler a -> MomentIO (Event a)
fromAddHandler = MIO . fmap E . Prim.fromAddHandler

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
fromPoll :: IO a -> MomentIO (Behavior a)
fromPoll = MIO . fmap B . Prim.fromPoll

-- | Input,
-- obtain a 'Behavior' from an 'AddHandler' that notifies changes.
--
-- This is essentially just an application of the 'stepper' combinator.
fromChanges :: a -> AddHandler a -> MomentIO (Behavior a)
fromChanges initial changes = do
    e <- fromAddHandler changes
    stepper initial e

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
-- > changes =<< stepper x e = return e
--
-- Note: The values of the event will not become available
-- until event processing is complete.
-- It can be used only in the context of 'reactimate''.
changes :: Behavior a -> MomentIO (Event (Future a))
changes = return . E . Prim.mapE F . Prim.changesB . unB

{- $changes

Note: If you need a variant of the 'changes' function that does /not/
have the additional 'Future' type, then the following code snippet
may be useful:

> plainChanges :: Behavior a -> MomentIO (Event a)
> plainChanges b = do
>     (e, handle) <- newEvent
>     eb <- changes b
>     reactimate' $ (fmap handle) <$> eb
>     return e

However, this approach is not recommended, because the result 'Event'
will occur /slightly/ later than the event returned by 'changes'.
In fact, there is no guarantee whatsoever about what /slightly/ means
in this context. Still, it is useful in some cases.

-}

-- | Impose a different sampling event on a 'Behavior'.
--
-- The 'Behavior' will vary continuously as before, but the event returned
-- by the 'changes' function will now happen simultaneously with the
-- imposed event.
--
-- Note: This function is useful only in very specific circumstances.
imposeChanges :: Behavior a -> Event () -> Behavior a
imposeChanges b e = B $ Prim.imposeChanges (unB b) (Prim.mapE (const ()) (unE e))

-- | Dynamically add input and output to an existing event network.
--
-- Note: You can even do 'IO' actions here,
-- which is useful if you want to register additional event handlers
-- dynamically.
-- However, there is no
-- guarantee about the order in which the actions are executed.
-- If the result 'Event' of this function is garbage collected,
-- it may also happen that the actions are not executed at all.
-- If you want a reliable way to turn events into 'IO' actions
-- use the 'reactimate' and 'reactimate'' functions.
execute :: Event (MomentIO a) -> MomentIO (Event a)
execute = MIO . fmap E . Prim.executeE . Prim.mapE unMIO . unE

-- $liftIO
--
-- > liftIO :: Frameworks t => IO a -> Moment t a
--
-- Lift an 'IO' action into the 'Moment' monad.

-- | Lift an 'IO' action into the 'Moment' monad,
-- but defer its execution until compilation time.
-- This can be useful for recursive definitions using 'MonadFix'.
liftIOLater :: IO () -> MomentIO ()
liftIOLater = MIO . Prim.liftIOLater

-- | Compile the description of an event network
-- into an 'EventNetwork'
-- that you can 'actuate', 'pause' and so on.
compile :: MomentIO () -> IO EventNetwork
compile = fmap EN . Prim.compile . unMIO

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
-- Immediately stop producing output.
-- (In a future version, it will also unregister all event handlers for inputs.)
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

-- | A multiline description of the current 'Latch'es and 'Pulse's in
-- the 'EventNetwork'.
--
-- Incidentally, evaluation the returned string to normal
-- form will also force the 'EventNetwork' to some kind of normal form.
-- This may be useful for benchmarking purposes.
showNetwork :: EventNetwork -> IO String
showNetwork = Prim.showNetwork . unEN


{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
-- | Build an 'Event' together with an 'IO' action that can
-- fire occurrences of this event. Variant of 'newAddHandler'.
--
-- This function is mainly useful for passing callback functions
-- inside a 'reactimate'.
newEvent :: MomentIO (Event a, Handler a)
newEvent = do
    (addHandler, fire) <- liftIO $ newAddHandler
    e <- fromAddHandler addHandler
    return (e,fire)

-- | Build an 'Behavior' toget with an initial value and a 'IO'
--  action that can push changes to this 'Behavior'.
newBehavior :: a -> MomentIO (Behavior a, Handler a)
newBehavior s0 = do
  (e, fe) <- newEvent
  (,) <$> stepper s0 e <*> pure fe


-- | Build a new 'Event' that contains the result
-- of an IO computation.
-- The input and result events will /not/ be simultaneous anymore,
-- the latter will occur /later/ than the former.
--
-- Please use the 'fmap' for 'Event' if your computation is pure.
--
-- Implementation:
--
-- > mapEventIO f e1 = do
-- >     (e2, handler) <- newEvent
-- >     reactimate $ (\a -> f a >>= handler) <$> e1
-- >     return e2
mapEventIO :: (a -> IO b) -> Event a -> MomentIO (Event b)
mapEventIO f e1 = do
    (e2, handler) <- newEvent
    reactimate $ (\a -> f a >>= handler) <$> e1
    return e2

{-----------------------------------------------------------------------------
    Simple use
------------------------------------------------------------------------------}
-- | Interpret by using a framework internally.
-- Only useful for testing library internals.
interpretFrameworks :: (Event a -> Event b) -> [a] -> IO [[b]]
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
interpretAsHandler :: (Event a -> Event b) -> AddHandler a -> AddHandler b
interpretAsHandler f addHandlerA = AddHandler $ \handlerB -> do
    network <- compile $ do
        e <- fromAddHandler addHandlerA
        reactimate $ handlerB <$> f e
    actuate network
    return (pause network)
