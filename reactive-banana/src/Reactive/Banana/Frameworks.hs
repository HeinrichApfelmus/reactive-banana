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
    interpretFrameworks, newEvent, mapEventIO, newBehavior,

    -- * Running event networks
    EventNetwork, actuate, pause,

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
-- return an 'Event' that is adapted to the changes of a 'Behavior'.
--
-- Remember that semantically, a 'Behavior' is a function @Behavior a = Time -> a@.
-- This means that a Behavior does not have a notion of \"changes\" associated with it.
-- For instance, the following Behaviors are equal:
--
-- > stepper 0 []
-- > = stepper 0 [(time1, 0), (time2, 0)]
-- > = stepper 0 $ zip [time1,time2..] (repeat 0)
--
-- In principle, to perform IO actions with the value of a Behavior,
-- one has to sample it using an 'Event' and the 'apply' function.
--
-- However, in practice, Behaviors are usually step functions.
-- For reasons of efficiency, the library provides a way
-- to obtain an Event that /mostly/ coincides with the steps of a Behavior,
-- so that sampling is only done at a few select points in time.
-- The idea is that
--
-- > changes =<< stepper x e  =  return e
--
-- Please use 'changes' only in a ways that do /not/ distinguish
-- between the different expressions for the same Behavior above.
--
-- Note that the value of the event is actually the /new/ value,
-- i.e. that value slightly after this point in time. (See the documentation of 'stepper').
-- This is more convenient.
-- However, the value will not become available until after event processing is complete;
-- this is indicated by the type 'Future'.
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
-- The 'Behavior' will have the same values as before, but the event returned
-- by the 'changes' function will now happen simultaneously with the
-- imposed event.
--
-- Note: This function is useful only in very specific circumstances.
imposeChanges :: Behavior a -> Event () -> Behavior a
imposeChanges b e = B $ Prim.imposeChanges (unB b) (Prim.mapE (const ()) (unE e))

{- | Dynamically add input and output to an existing event network.


Note: You can perform 'IO' actions here, which is useful if you want
to register additional event handlers dynamically.

However, if two arguments to 'execute' occur simultaneously,
then the order in which the 'IO' therein are executed is unspecified.
For instance, in the following code

> example e = do
>       e1 <- execute (liftIO (putStrLn "A") <$ e)
>       e2 <- execute (liftIO (putStrLn "B") <$ e)
>       return (e1,e2)

it is unspecified whether @A@ or @B@ are printed first.

Moreover, if the result 'Event' of this function has been garbage collected,
it may also happen that the actions are not executed at all.
In the example above, if the events `e1` and `e2` are not used any further,
then it can be that neither @A@ nor @B@ will be printed.

If your main goal is to reliably turn events into 'IO' actions,
use the 'reactimate' and 'reactimate'' functions instead.
-}
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
compile :: MonadIO m => MomentIO () -> m EventNetwork
compile = liftIO . fmap EN . Prim.compile . unMIO

{-----------------------------------------------------------------------------
    Running event networks
------------------------------------------------------------------------------}
-- | Data type that represents a compiled event network.
-- It may be paused or already running.
newtype EventNetwork = EN { unEN :: Prim.EventNetwork }

-- | Actuate an event network.
-- The inputs will register their event handlers, so that
-- the networks starts to produce outputs in response to input events.
actuate :: MonadIO m => EventNetwork -> m ()
actuate = liftIO . Prim.actuate . unEN

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
pause :: MonadIO m => EventNetwork -> m ()
pause   = liftIO . Prim.pause . unEN

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

-- | Build a 'Behavior' together with an 'IO' action that can
-- update this behavior with new values.
--
-- Implementation:
--
-- > newBehavior a = do
-- >     (e, fire) <- newEvent
-- >     b         <- stepper a e
-- >     return (b, fire)
newBehavior :: a -> MomentIO (Behavior a, Handler a)
newBehavior a = do
    (e, fire) <- newEvent
    b         <- stepper a e
    return (b, fire)

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
-- | Interpret an event processing function by building an 'EventNetwork'
-- and running it. Useful for testing, but uses 'MomentIO'.
-- See 'interpret' for a plain variant.
interpretFrameworks :: (Event a -> MomentIO (Event b)) -> [Maybe a] -> IO [Maybe b]
interpretFrameworks f xs = do
    output                    <- newIORef Nothing
    (addHandler, runHandlers) <- newAddHandler
    network                   <- compile $ do
        e1 <- fromAddHandler addHandler
        e2 <- f e1
        reactimate $ writeIORef output . Just <$> e2

    actuate network
    bs <- forM xs $ \x -> do
        case x of
            Nothing -> return Nothing
            Just x  -> do
                runHandlers x
                b <- readIORef output
                writeIORef output Nothing
                return b
    return bs

-- | Simple way to write a single event handler with
-- functional reactive programming.
interpretAsHandler :: (Event a -> Moment (Event b)) -> AddHandler a -> AddHandler b
interpretAsHandler f addHandlerA = AddHandler $ \handlerB -> do
    network <- compile $ do
        e1 <- fromAddHandler addHandlerA
        e2 <- liftMoment (f e1)
        reactimate $ handlerB <$> e2
    actuate network
    return (pause network)
