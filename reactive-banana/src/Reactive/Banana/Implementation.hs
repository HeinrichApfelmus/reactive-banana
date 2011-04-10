{-----------------------------------------------------------------------------
    Reactive Banana
    
    A demand-driven implementation (full of yucky IO)
    
FIXME: This implementation sucks big time! Because of the global state.
    Idea: use the automaton-like implementation, but with  Dynamic  as
          input variable, to avoid the extra type parameter.

------------------------------------------------------------------------------}
{-# LANGUAGE TypeFamilies, FlexibleInstances, EmptyDataDecls #-}
module Reactive.Banana.Implementation where

import Reactive.Banana.Model hiding (Event, Behavior)
import qualified Reactive.Banana.Model as Model

import Control.Applicative
import qualified Data.List
import Prelude hiding (filter)
import Data.Monoid

import Control.Monad.Writer as Monad
import Control.Monad.Fix
import Data.IORef
import System.IO.Unsafe

{-----------------------------------------------------------------------------
    Phases and IO actions that execute only once
------------------------------------------------------------------------------}
-- Using only two values for the phase requires that every element
-- is touched at least once. Otherwise, old phases may still linger around
data Phase = None | One | Two
        deriving (Eq, Ord, Show, Read)

-- TODO: The global variable is ugly. I think this can be backed into
-- the Once monad, Reader-style

phaseRef :: IORef Phase
phaseRef = unsafePerformIO $ newIORef One

currentPhase :: IO Phase
currentPhase = readIORef phaseRef

nextPhase :: IO ()
nextPhase = do
    modifyIORef phaseRef $ \phase -> case phase of
        None -> One
        One  -> Two
        Two  -> One


-- IO actions that yield their side effect only once per phase
-- We NEED to make this an algebraic data type because
-- IO actions are not shared by default, but algebraic data types are.
-- The  Once  constructor serves as an indirection that allows us to 
-- reserve a mutable variable
data Once a = Once { runOnce :: IO a }

{-# NOINLINE once #-}
once :: IO a -> Once a
once m = ref `seq` (Once $ do
        phase <- currentPhase
        (phase', x) <- readIORef ref    -- read saved value
        -- test if the saved value is from the current phase
        if phase == phase'
            then return x
            else mfix $ \x -> do
                writeIORef ref (phase, x)
                x' <- m
                return x')
    where
    ref = unsafePerformIO $ newIORef (None, undefined m)

-- Very important test. It should print "Hello" only once.
testOnce = runOnce x >> runOnce x
    where
    x = once $ print "Hello!" >> return 2

{-----------------------------------------------------------------------------
    Implementation
------------------------------------------------------------------------------}
data DemandIO

-- data types
type Event = Model.Event DemandIO
data instance Model.Event DemandIO a    = Event
    { values :: Once [a]
    , updateBehaviors :: Once () }

type Behavior = Model.Behavior DemandIO
data instance Model.Behavior DemandIO a = Behavior
    { value :: Once a
    , updateBehavior :: Once () }

-- instances
instance Functor (Model.Event DemandIO) where
    fmap f e = Event
        (once $ map f <$> (runOnce $ values e))
        (once $ runOnce $ updateBehaviors e)

instance Applicative (Model.Behavior DemandIO) where
    pure x    = Behavior (Once $ return x) (Once $ return ())
    bf <*> bx = Behavior
        (once $ (runOnce $ value bf) <*> (runOnce $ value bx) )
        (once $ (runOnce $ updateBehavior bf) >> (runOnce $ updateBehavior bx))

instance Functor (Model.Behavior DemandIO) where
    fmap = liftA

instance FRP DemandIO where
    never = Event (Once $ return []) (Once $ return ())
    union e1 e2 = Event
        (once $ (++) <$> (runOnce $ values e1) <*> (runOnce $ values e2))
        (once $ (runOnce $ updateBehaviors e1) >> (runOnce $ updateBehaviors e2))

    filter bp e = Event
        (once $ Data.List.filter <$> (runOnce $ value bp) <*> (runOnce $ values e))
        (once $ runOnce $ updateBehaviors e)

    apply bf ex = Event
        (once $ map <$> (runOnce $ value bf) <*> (runOnce $ values ex))
        (once $ (runOnce $ updateBehavior bf) >> (runOnce $ updateBehaviors ex))

    -- TODO: Implement  mapAccum  instead!
    accumB x e = unsafePerformIO $ do
        ref <- newIORef x
        return $ Behavior
            (once $ do
                        -- Demand the event values, but don't do anything with them.
                        -- We have to demand them here, before doing updates.
                        runOnce $ values e
                        readIORef ref)
            (once $ do
                        -- Read the prepared event values
                       fs <- runOnce $ values e; modifyIORef ref (concatenate fs)
                       runOnce $ updateBehaviors e;)
        where
        concatenate = foldl (.) id


{-----------------------------------------------------------------------------
    Interpeter
------------------------------------------------------------------------------}
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
    
    mapM step xs

{-----------------------------------------------------------------------------
    Setting up an event network
------------------------------------------------------------------------------}
type Preparations = ([Event (IO ())] , [IO () -> IO ()])
type Prepare a = Monad.WriterT Preparations IO a

reactimate :: Event (IO ()) -> Prepare ()
reactimate e = tell ([e], [])

-- | Set up an event network.
setup :: Prepare () -> IO ()
setup m = do
    (_,(outputs,inputs)) <- runWriterT m
    let
        -- union of all  reactimates    
        network = mconcat outputs :: Event (IO ())
        -- trigger the events and run the results
        run = stepNetwork network >>= sequence_
    -- Register event handlers.
    mapM_ ($ run) inputs

data EventSource a = EventSource { addHandler :: (a -> IO ()) -> IO () }

fromEventSource :: EventSource a -> Prepare (Event a)
fromEventSource es = do
    ref <- liftIO $ newIORef []
    let
        -- The event just reads from the reference
        event = Event (Once $ readIORef ref) (Once $ return ())
        register = \run -> addHandler es $ \a -> do
            writeIORef ref [a]  -- fill event value with reference
            run
            writeIORef ref []   -- remove event occurence again
    tell ([], [register])
    return event




