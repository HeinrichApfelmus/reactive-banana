{-----------------------------------------------------------------------------
    Reactive Banana
    
    A demand-driven implementation
------------------------------------------------------------------------------}
{-# LANGUAGE TypeFamilies, FlexibleInstances, EmptyDataDecls, BangPatterns #-}
module Reactive.Banana.Implementation (
    module Reactive.Banana.Model,
    Demand, Event, Behavior,
    run,
    
    -- * Setting up a network of events
    Prepare, prepareEvents, reactimate, liftIO,
    
    -- $EventSource
    EventSource(..), fromEventSource, Handler,
        newEventSource, addEventHandler, fire,
    module Data.Dynamic,
    ) where

import Reactive.Banana.Model hiding (Event, Behavior, run)
import qualified Reactive.Banana.Model as Model

import Control.Applicative
import qualified Data.List
import Prelude hiding (filter)
import Data.Monoid
import Data.Maybe

import Control.Monad.Writer as Monad
import Control.Monad.State as Monad
import Control.Monad.Fix
import Data.IORef
import System.IO.Unsafe

import Data.Dynamic

{-----------------------------------------------------------------------------
    Infinite sum of data types
------------------------------------------------------------------------------}
type Unique = Integer
type Universe = (Unique, Dynamic)

type MonadUniqueT = StateT Unique

newUnique :: Monad m => MonadUniqueT m Unique
newUnique = do u <- get; put $! (u+1); return u

runUnique :: Monad m => MonadUniqueT m a -> m a
runUnique m = evalStateT m 0

project :: Typeable a => Unique -> Universe -> Maybe a
project u1 (u2,dx) = if u1 == u2 then fromDynamic dx else Nothing

{-----------------------------------------------------------------------------
    Implementation
------------------------------------------------------------------------------}
data Demand

-- data types
type Event = Model.Event Demand
newtype instance Model.Event Demand a
    = Event { unEvent :: Universe -> ResultE a }
data ResultE a = ResultE [a] (Event a)

type Behavior = Model.Behavior Demand
newtype instance Model.Behavior Demand a
    = Behavior { unBehavior :: Universe -> ResultB a }
data ResultB a = ResultB a (Behavior a)

-- instances
instance Functor (Model.Event Demand) where
    fmap f (Event e) = Event $ \i ->
        let !(ResultE as e') = e i
        in  ResultE (map f as) (fmap f e')

instance Applicative (Model.Behavior Demand) where
    pure x    = Behavior $ \_ -> ResultB x (pure x)
    (Behavior bf) <*> (Behavior bx) = Behavior $ \i ->
        let !(ResultB f bf') = bf i
            !(ResultB x bx') = bx i
        in  ResultB (f x) (bf' <*> bx')

instance Functor (Model.Behavior Demand) where
    fmap = liftA

instance FRP Demand where
    never = Event $ \_ -> ResultE [] never
    union (Event e1) (Event e2) = Event $ \i ->
        let !(ResultE as1 e1') = e1 i
            !(ResultE as2 e2') = e2 i
        in  ResultE (as1 ++ as2) (union e1' e2')

    filterApply (Behavior b) (Event e) = Event $ \i ->
        let !(ResultB p  b') = b i
            !(ResultE as e') = e i
        in  ResultE (Data.List.filter p as) (filterApply b' e')

    apply (Behavior b) (Event e) = Event $ \i ->
        let !(ResultB f  b') = b i
            !(ResultE as e') = e i
        in  ResultE (map f as) (apply b' e')

    accumB x (Event e) = Behavior $ \i ->
        ResultB x $
            let !(ResultE fs e') = e i
                -- compose functions from left to right. Left = earlier
                !x'              = Data.List.foldl' (flip ($)) x fs
                -- the accumulation function is strict by default!
            in  accumB x' e'

-- interpreter
run :: Typeable a => (Event a -> Event b) -> [a] -> [[b]]
run f = fst . myMapAccumL step network
    where
    u       = 0
    input   = Event $ \i -> ResultE (maybeToList $ project u i) input
    network = f input
    step (Event e) a = let ResultE bs e' = e (u, toDyn a) in (bs,e')
    
    switch (x,y) = (y,x)
    myMapAccumL f acc =
        switch . Data.List.mapAccumL (\acc -> switch . f acc) acc

{-----------------------------------------------------------------------------
    Setting up an event network
------------------------------------------------------------------------------}
type Handler a = a -> IO ()
type AddHandler a = Handler a -> IO ()

type Preparations = ([Event (IO ())] , [AddHandler Universe])
type Prepare a = MonadUniqueT (Monad.WriterT Preparations IO) a

reactimate :: Event (IO ()) -> Prepare ()
reactimate e = tell ([e], [])

-- | Set up an event network.
prepareEvents :: Prepare () -> IO ()
prepareEvents m = do
    (a,(outputs,inputs)) <- runWriterT $ runUnique m
    let -- union of all  reactimates    
        network = mconcat outputs :: Event (IO ())
    ref <- newIORef network
    let -- run one step of the network
        step i = do
            Event network <- readIORef ref
            let !(ResultE as network') = network i
            writeIORef ref network'
            sequence_ as

    -- Register event handlers.
    mapM_ ($ step) inputs
    return a

{-----------------------------------------------------------------------------  
    EventSource - "I'll call you back"
------------------------------------------------------------------------------}
{-$EventSource

* Event Sources
    
    After having read all about 'Event's and 'Behavior's,
    you want to hook things up to an existing event-based framework,
    like @wxHaskell@ or @Gtk2Hs@.
    How do you do that?
    
    'EventSource's are a small bookkeeping device that helps you with that.
    Basically, they store event handlers.
    Sometimes, you can just obtain them from
    corresponding bookkeeping devices from your framework,
    but often you have to create your own 'EventSource'
    and use the 'fire' function to hook it into the framework.
    
    After creating an 'EventSource',
    you can finally obtain an 'Event' via the `fromEventSource' function.
-}

-- | An 'EventSource' is a facility where you can register
-- callback functions, aka event handlers.
data EventSource a = EventSource {
                    -- | Replace all event handlers by this one.
                      setEventHandler :: Handler a -> IO ()
                    -- | Retrieve the currently registered event handler.
                    , getEventHandler :: IO (Handler a) }

-- | Add an additional event handler to the source
addEventHandler :: EventSource a -> Handler a -> IO ()
addEventHandler es f = do
    g <- getEventHandler es
    setEventHandler es (\a -> g a >> f a)

-- | Fire the event handler of an event source manually.
-- Useful for hooking into external event sources.
fire :: EventSource a -> Handler a
fire es a = getEventHandler es >>= ($ a)

-- | Create a new store for callback functions.
-- They have to be fired manually with the 'fire' function.
newEventSource :: IO (EventSource a)
newEventSource = do
    handlerRef <- newIORef (const $ return ())
    return $ EventSource
        { setEventHandler = writeIORef handlerRef
        , getEventHandler = readIORef handlerRef }

-- | Obtain an 'Event' from an 'EventSource'.
-- The event fires whenever the event source is fired.
fromEventSource :: Typeable a => EventSource a -> Prepare (Event a)
fromEventSource es = do
    u <- newUnique
    let
        -- Event that just projects the right value from the input
        input  = Event (\i -> ResultE (maybeToList $ project u i) input)
        source = \run -> addEventHandler es $ \a -> run (u, toDyn a)
    
    tell ([], [source])
    return input


