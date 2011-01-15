{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances #-}
{-----------------------------------------------------------------------------
    Reactive Banana


    A tiny library for functional reactive programming.
    
    It was written to make GUI programming easier
    and is quite flexible in that it can be hooked into any
    system that is based on callback functions. So, in a sense,
    it's another viewpoint on callbacks.

    In the spectrum of possible FRP implementations,
    this one features simple semantics but little expressivity.
    Predicting space & time usage should not be difficult.

------------------------------------------------------------------------------}

{-----------------------------------------------------------------------------

    TODO:
    What should we do with the variants involving time-varying functions?
    Should they get the same, or a different name?
    
    For example:
    
    map   ::          (a -> b) -> Event a -> Event b
    apply :: Behavior (a -> b) -> Event a -> Event b 
    
    filter  ::          (a -> Bool) -> Event a -> Event a
    filterB :: Behavior (a -> Bool) -> Event a -> Event a 


    TODO:
    At some point, we probably need a function to dynamically switch
    between events, something like this
    
        join :: Event (Event a) -> Event a

    Not sure about this particular functions,
    but the point is that event handlers are being registered,
    and also *unregisterered* while the program is running.
    At the moment, everything is set up statically.

------------------------------------------------------------------------------}

module Reactive.Core where

import Prelude hiding (map, filter)
import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe
import System.IO.Unsafe

import Debug.Trace

    -- The monad in which you prepare the event flow.
    -- It's just a type synonym for IO, but the idea is that all
    -- Prepare  actions should be called during the
    -- initialization of the program, but not while the event loop is running.
type Prepare a = IO a

{-----------------------------------------------------------------------------
    EventSource - "I'll call you back"
    
    An  EventSource  is the imperative predecessor of a proper  Event .
    It's a facility where you can register callback functions.
------------------------------------------------------------------------------}
data EventSource a = EventSource {
                    -- replace all event handlers by this one
                      setHandlerS :: (a -> IO ()) -> Prepare ()
                    -- retrieve current registered event handler
                    , getHandlerS :: Prepare (a -> IO ()) }

    -- add an additional event handler to the source
addHandlerS :: EventSource a -> (a -> IO ()) -> Prepare ()
addHandlerS es f = getHandlerS es >>= \g -> setHandlerS es (\a -> g a >> f a)


    -- fire the event handler of an event source manually
fire :: EventSource a -> a -> IO ()
fire es a = getHandlerS es >>= ($ a)
    -- here, the purpose of the prepare monad is intentionally violated

    -- make a new event source, which has to be fired manually
newEventSource :: Prepare (EventSource a)
newEventSource = do
    handlerRef <- newIORef (const $ return ())
    return $ EventSource
        { setHandlerS = writeIORef handlerRef
        , getHandlerS = readIORef handlerRef }

{-----------------------------------------------------------------------------
    Event
    
    The type  Event a  denotes a stream of events, which are values of type a.
    
    Note that duplicating an event stream will recalculate the 
    individual values, which is potentially expensive. Use the
    share  function to avoid that.
    
    
    FIXME: Implement extra constructor for events that never fire
            data Event a = Never | Event ...
        to optimize the applicative instance of  Behavior
------------------------------------------------------------------------------}

    -- who would have thought that the implementation is this simple
type AddHandler a = (a -> IO ()) -> Prepare ()
data Event a      = Event { addHandler :: AddHandler a }

    -- smart constructor, ensures proper sharing
mkEvent :: AddHandler a -> Event a
mkEvent =
    -- What happens when  unsafePerformIO  is accidentally exectued twice?
    -- In that case, work will be duplicated as there will be two
    -- buffers (event sources) for one and the same event.
    -- But this is the same as the situation without any sharing at all,
    -- so there's no harm done.
    \h -> unsafePerformIO $ share $ Event { addHandler = h }
    where
        -- Cache the value of an event,
        -- so that it's not recalculated for multiple consumers
    share :: Event a -> Prepare (Event a)
    share e1 = do
        es2 <- newEventSource
        addHandler e1 (fire es2) -- sharing happens through call-by-need
        return $ fromSource es2

    -- derive an  Event  from an event source
fromSource :: EventSource a -> Event a
fromSource s = Event { addHandler = addHandlerS s }

    -- schedule an IO event to be actually execute whenever it fires
reactimate :: Event (IO ()) -> Prepare ()
reactimate e = addHandler e id

    -- an Event that never fires
never :: Event a
never = Event { addHandler = const $ return () }

    -- mapping over events
    -- Functor instance
instance Functor Event where
    fmap f e = mkEvent addHandler'
        where addHandler' g = addHandler e (g . f)

    -- map version - IO, Change
mapIOChange :: (a -> IO (Change b)) -> Event a -> Event b
mapIOChange f ea = mkEvent addHandler'
    where
    addHandler' g = addHandler ea $ \a -> do
        mb <- f a
        case mb of
            Change b -> g b
            Keep     -> return ()

    -- map version - Change
    -- can be used to implement filter
mapChange :: (a -> Change b) -> Event a -> Event b
mapChange f = mapIOChange (return . f)

    -- map version - IO
    -- Note: IO is unsound without sharing!
mapIO :: (a -> IO b) -> Event a -> Event b
mapIO f e = mkEvent addHandler'
    where addHandler' g = addHandler e (g <=< f)

    -- merge two event streams of the same type
    -- When both events fire at the same time, the first argument comes first.
union :: Event a -> Event a -> Event a
union e1 e2 = mkEvent addHandler'
    where addHandler' g = addHandler e1 g >> addHandler e2 g
    

    -- merge two event streams of different types
merge :: Event a -> Event b -> Event (Either a b)
merge e1 e2 = fmap Left e1 `union` fmap Right e2

    -- filter event occurrences
filter :: (a -> Bool) -> Event a -> Event a
filter p e = mkEvent addHandler'
    where addHandler' g = addHandler e $ \a -> when (p a) (g a)

    -- keep only proper change
filterChanges :: Event (Change a) -> Event a
filterChanges = fmap (\(Change x) -> x) . filter isChange


    -- debugging
traceEvent :: Show a => Event a -> Event a
traceEvent = mapIO (\a -> print a >> return a)

{-----------------------------------------------------------------------------
    Behavior

    A  Behavior  represents a value that changes with time.
    This concept is a close cousin of  Event  , but the difference is that
    Event  is an occurence while the  Behavior  has a value even when nothing
    has occured yet.
------------------------------------------------------------------------------}
    -- FIXME: exporting  initial  to users might cause space leaks
    -- where the initial value is retained long beyond the point where
    -- it was consumed.
    -- However, if we want the user to implement optimized behaviors
    -- himself, like  TimeGraphic , we have to provide a mechanism
    -- similar to this one.
    -- Alternative: keep current value in a IORef. This will eliminate
    -- this particular space leak.
data Behavior a = Behavior { initial :: a, changes :: Event a }

    -- behavior that is always the same value and never changes
always :: a -> Behavior a
always a = Behavior { initial = a, changes = never }

    -- data type to indicate that a value has changed.
    -- It's basically the  Maybe  type with a different name.
    -- Using different names gives better documentation 
    -- and makes it easier to select the right  accumulate  function by type
data Change a = Keep | Change a deriving (Eq, Show, Read)

instance Functor Change where
    fmap _ Keep       = Keep
    fmap f (Change a) = Change (f a)

isChange :: Change a -> Bool
isChange (Change _) = True
isChange Keep       = False

    -- trigger an event whenever the value changes.
-- changes :: Behavior a -> Event a

    -- See below for a type class that manages the many different versions
    -- of accumulate.
    -- 
    -- accumulate a value through events
    -- very much like  foldl'
    -- for flexibility, the action that updates may have side effects
accumulateIOChange :: (a -> b -> IO (Change a)) -> a -> Event b -> Behavior a
accumulateIOChange f a eb =
    Behavior { initial = a , changes = mkEvent addHandler' }
    where
    addHandler' g = addHandler eb (handler g)
    
    -- we need a global state
    -- FIXME: NOINLINE pragma!
    ref = unsafePerformIO $ newIORef a
    handler g = \b -> do
        a   <- readIORef ref     -- read old value
        ma' <- f a b            -- accumulate
        case ma' of
            Keep      -> return ()
            Change a' -> do
                writeIORef ref $! a'    -- use new value
                g a'

    -- accumulate version - pure, strict
accumulate' :: (a -> b -> a) -> a -> Event b -> Behavior a
accumulate' f = accumulateIOChange (\a b -> return . Change $ f a b)

    -- accumulate version - incremental , strict 
accumulateChange :: (a -> b -> Change a) -> a -> Event b -> Behavior a
accumulateChange f = accumulateIOChange (\a b -> return $ f a b)

    -- accumulate version - IO, strict
    -- Note: IO is unsound without sharing!
accumulateIO :: (a -> b -> IO a) -> a -> Event b -> Behavior a
accumulateIO f = accumulateIOChange (\a b -> fmap Change $ f a b)


    -- Functor instance
instance Functor Behavior where
    fmap f b = Behavior
        { initial = f (initial b), changes = fmap f (changes b) }

    -- Applicative instance
instance Applicative Behavior where
    pure a    = always a
    bf <*> bx = fmap (uncurry ($)) $
        accumulate' go (initial bf, initial bx) (changes bf `merge` changes bx)
        where
        go (f,x) (Left  f') = (f',x)
        go (f,x) (Right x') = (f,x')

    -- store the occurences of an event in a behavior
-- latch :: Event a -> Behavior (Maybe a)
-- latch = accumulate' (\_ a -> Just a) Nothing

    -- map events with an external state
mapAccum :: (acc -> x -> (acc,y)) -> acc -> Event x -> (Behavior acc, Event y)
mapAccum f acc xs =
    (fmap fst result, fmap snd $ changes result)
    where
    result = accumulate (\(acc,_) x -> f acc x) (acc,undefined) xs

    -- very important primitive function:
    -- apply a time-varying functions to events as they come in
apply :: Behavior (a -> b) -> Event a -> Event b
apply bf ex =
    filterChanges . snd . mapAccum go (initial bf) $ changes bf `merge` ex
    where
    go _ (Left  f) = (f, Keep)
    go f (Right x) = (f, Change $ f x)

{-----------------------------------------------------------------------------
    Name overloading
------------------------------------------------------------------------------}
{-
    Question: Should all the  map  functions get the same name?
        I tend to say yes, and the same goes for accumulate.
        Adding the  Change  part to the map functions makes it
        kind of like filter, and thus different from lists,
        but I think the similarity to  accumulate  justifies that.

-}

    -- convenient type class for automatically
    -- selecting the right accumulation function by type
class Accumulate b t where
    accumulate :: (b -> a -> t) -> b -> Event a -> Behavior b
    map        :: (a -> t) -> Event a -> Event b

instance Accumulate b b where
    accumulate = accumulate'
    map        = fmap
instance Accumulate b (Change b) where
    accumulate = accumulateChange
    map        = mapChange
instance Accumulate b (IO b) where
    accumulate = accumulateIO
    map        = mapIO
instance Accumulate b (IO (Change b)) where
    accumulate = accumulateIOChange
    map        = mapIOChange


{-----------------------------------------------------------------------------
    Test examples
    
    The examples return event sources that you can fire.
------------------------------------------------------------------------------}
counter :: Prepare (EventSource Int)
counter = do
    es <- newEventSource
    let e = fromSource es
    reactimate . changes $ print <$> accumulate' (+) 0 e
    return es

    -- test the  apply  function
testApply :: Prepare (EventSource Int, EventSource Int)
testApply = do
    es1 <- newEventSource
    let e1 = fromSource es1
    
    es2 <- newEventSource
    let e2 = fromSource es2

    reactimate . fmap print $ apply (fmap (+) (Behavior 0 e1)) e1
    return (es1, es2)

