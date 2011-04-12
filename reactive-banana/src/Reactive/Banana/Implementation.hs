{-----------------------------------------------------------------------------
    Reactive Banana
    
    A demand-driven implementation
------------------------------------------------------------------------------}
{-# LANGUAGE TypeFamilies, FlexibleInstances, EmptyDataDecls #-}
module Reactive.Banana.Implementation (
    module Reactive.Banana.Model,
    Demand, Event, Behavior,
    Prepare, runPrepare, reactimate,
    EventSource(..), fromEventSource,
    module Data.Dynamic, run,
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
        let ResultE as e' = e i
        in  ResultE (map f as) (fmap f e')

instance Applicative (Model.Behavior Demand) where
    pure x    = Behavior $ \_ -> ResultB x (pure x)
    (Behavior bf) <*> (Behavior bx) = Behavior $ \i ->
        let ResultB f bf' = bf i
            ResultB x bx' = bx i
        in  ResultB (f x) (bf' <*> bx')

instance Functor (Model.Behavior Demand) where
    fmap = liftA

instance FRP Demand where
    never = Event $ \_ -> ResultE [] never
    union (Event e1) (Event e2) = Event $ \i ->
        let ResultE as1 e1' = e1 i
            ResultE as2 e2' = e2 i
        in  ResultE (as1 ++ as2) (union e1' e2')

    filter (Behavior b) (Event e) = Event $ \i ->
        let ResultE as e' = e i
            ResultB p  b' = b i
        in  ResultE (Data.List.filter p as) (filter b' e')

    apply (Behavior b) (Event e) = Event $ \i ->
        let ResultB f  b' = b i
            ResultE as e' = e i
        in  ResultE (map f as) (apply b' e')

    accumB x (Event e) = Behavior $ \i ->
        let ResultE fs e' = e i
            -- compose functions from left to right. Left = earlier
            x'            = Data.List.foldl' (flip ($)) x fs
            -- the accumulation function is strict by default!
        in  ResultB x (x' `seq` accumB x' e')

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
    Interpeter
------------------------------------------------------------------------------}
{-
-- Run a network of events for a single step
stepNetwork :: Event a -> IO [a]
stepNetwork e = do
    xs <- runOnce $ values e
    runOnce $ updateBehaviors e
    nextPhase
    return xs

run :: (Event a -> Event b) -> [a] -> IO [[b]]
run f xs = do
    ref <- newIORef []
    let
        network = f $ Event (Once $ readIORef ref) (Once $ return ())
        step x = do
            writeIORef ref [x]
            stepNetwork network
    
    mapM step xs -}

{-----------------------------------------------------------------------------
    Setting up an event network
------------------------------------------------------------------------------}
type Preparations = ([Event (IO ())] , [EventSource Universe])
type Prepare a = MonadUniqueT (Monad.WriterT Preparations IO) a

reactimate :: Event (IO ()) -> Prepare ()
reactimate e = tell ([e], [])

-- | Set up an event network.
runPrepare :: Prepare () -> IO ()
runPrepare m = do
    (_,(outputs,inputs)) <- runWriterT $ runUnique m
    let -- union of all  reactimates    
        network = mconcat outputs :: Event (IO ())
    ref <- newIORef network
    let -- run one step of the network
        step i = do
            Event network <- readIORef ref
            let ResultE as network' = network i
            writeIORef ref network'
            sequence_ as

    -- Register event handlers.
    mapM_ (\es -> addHandler es step) inputs


data EventSource a = EventSource { addHandler :: (a -> IO ()) -> IO () }

fromEventSource :: Typeable a => EventSource a -> Prepare (Event a)
fromEventSource es = do
    u <- newUnique
    let
        -- Event that just projects the right value from the input
        input  = Event (\i -> ResultE (maybeToList $ project u i) input)
        source = EventSource $
            \run -> addHandler es $ \a -> run (u, toDyn a)
    
    tell ([], [source])
    return input


