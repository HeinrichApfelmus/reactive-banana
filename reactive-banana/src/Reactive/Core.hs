{-----------------------------------------------------------------------------
    reactive-banana
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


    accumulate  doesn't need a  Behavior  variant!
    ->  accumulate ($) b $ apply behavior event

    TODO:
    At some point, we probably need a function to dynamically switch
    between events, something like this
    
        join :: Event (Event a) -> Event a

    Not sure about this particular functions,
    but the point is that event handlers are being registered,
    and also *unregisterered* while the program is running.
    At the moment, everything is set up statically.

------------------------------------------------------------------------------}

module Reactive.Core (
    -- * Events
    -- $Event
    Event, never, fromEventSource, reactimate,
    mapIO, filter, filterChanges,
    union, merge, orderedDuplicate,
    traceEvent,
    
    -- * Behaviors
    -- $Behavior
    Behavior, behavior, always, initial, changes, apply,
    accumulate', accumulateChange, accumulateIO, accumulateIOChange,
    mapAccum,
    
    -- * The @Change@ data type
    Change(..), isChange, isKeep,
    
    -- * Event Sources
    -- $EventSource
    EventSource(..), Prepare, newEventSource, fire,
    
    -- * Internal
    testCounter, testApply
    ) where

import Prelude hiding (map, filter)
import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Monoid
import System.IO.Unsafe
import System.IO

import Debug.Trace

{-----------------------------------------------------------------------------  
    Prepare
------------------------------------------------------------------------------}

-- | The 'Prepare' monad is just a type synonym for 'IO'.
-- The idea is that the event flow is set up in the 'Prepare' monad;
-- all 'Prepare' actions should be called
-- during the program initialization, but not while the event loop
-- is running.
type Prepare a = IO a

{-----------------------------------------------------------------------------  
    EventSource - "I'll call you back"
------------------------------------------------------------------------------}
{-$EventSource
    
    After having read all about 'Event's and 'Behavior's,
    you want to hook things up to an existing event-based framework,
    like @wxHaskell@ or @Gtk2Hs@.
    How do you do that?
    
    'EventSource's are a small bookkeeping device that helps you with that.
    Basically, they store event handlers. Often, you can just obtain them from
    corresponding bookkeeping devices from your framework,
    but sometimes you have to create your own 'EventSource'
    and use the 'fire' function to hook it into the framework.
    Event sources are also useful for testing.
    
    After creating an 'EventSource',
    you can finally obtain an 'Event' via the `fromEventSource' function.
-}


-- | An 'EventSource' is a facility where you can register
-- callback functions, aka event handlers.
-- 'EventSource's are the precursor of proper 'Event's.
data EventSource a = EventSource {
                    -- | Replace all event handlers by this one.
                      setEventHandler :: (a -> IO ()) -> Prepare ()
                    -- | Retrieve the currently registered event handler.
                    , getEventHandler :: Prepare (a -> IO ()) }

-- add an additional event handler to the source
addEventHandler :: EventSource a -> (a -> IO ()) -> Prepare ()
addEventHandler es f = do
    g <- getEventHandler es
    setEventHandler es (\a -> g a >> f a)


-- | Fire the event handler of an event source manually.
-- Useful for hooking into external event sources.
fire :: EventSource a -> a -> IO ()
fire es a = getEventHandler es >>= ($ a)
    -- here, the purpose of the Prepare monad is intentionally violated

-- | Create a new store for callback functions.
-- They have to be fired manually with the 'fire' function.
newEventSource :: Prepare (EventSource a)
newEventSource = do
    handlerRef <- newIORef (const $ return ())
    return $ EventSource
        { setEventHandler = writeIORef handlerRef
        , getEventHandler = readIORef handlerRef }

{-----------------------------------------------------------------------------
    Event
------------------------------------------------------------------------------}
{-$Event

The 'Event' type constructor is one of the cornerstones of the present
approach to functional reactive programmings.
It represents a stream of values as they occur in time.

-}


-- who would have thought that the implementation is this simple
type AddHandler a = (a -> IO ()) -> Prepare ()

{- | @Event a@ represents a stream of events as they occur in time.
Semantically, you can think of @Event a@ as an infinite list of values
that are tagged with their corresponding time of occurence,

> type Event a = [(Time,a)]

Note that this is a semantic model;
the type is not actually implement that way,
but you can often treat it as if it where.
In particular, most of the subsequent operations
will be explained in terms of this model.

-}
data Event a      = Never
                  | Event { addHandler :: AddHandler a }

-- smart constructor, ensures proper sharing
mkEvent :: AddHandler a -> Event a
mkEvent =
    -- What happens when  unsafePerformIO  is accidentally exectued twice?
    -- In that case, work will be duplicated as there will be two
    -- buffers (event sources) for one and the same event.
    -- But this is the same as the situation without any sharing at all,
    -- so there's no harm done.
    -- There might be a problem with executing IO actions twice, though.
    \h -> unsafePerformIO $ share $ Event { addHandler = h }
    where
    -- Cache the value of an event,
    -- so that it's not recalculated for multiple consumers
    share :: Event a -> Prepare (Event a)
    share e1 = do
        es2 <- newEventSource
        addHandler e1 (fire es2) -- sharing happens through call-by-need
        return $ fromEventSource es2

-- | Derive an 'Event' from an 'EventSource'.
-- Apart from 'never', this is the only way to construct events.
fromEventSource :: EventSource a -> Event a
fromEventSource s = Event { addHandler = addEventHandler s }

-- | Schedule an IO event to be executed whenever it happens.
-- This is the only way to observe events.
-- Semantically, you could write it as something like this
--
-- > reactimate ((time,action):es) = atTime time action >> reactimate es 
-- 
-- The 'Prepare' monad indicates that you should call this function
-- during program initialization only.
reactimate :: Event (IO ()) -> Prepare ()
reactimate Never = return ()
reactimate e     = addHandler e id

-- | The value 'never' denotes the event that never happens.
-- We can model it as the empty stream of events, @never = []@.
never :: Event a
never = Never

-- | The 'Functor' instance allows you to map the values of type 'a'.
-- Semantically,
-- 
-- > fmap f ((time,a):es) = (time, f a) : fmap f es
instance Functor Event where
    fmap f Never = Never
    fmap f e     = mkEvent addHandler'
        where addHandler' g = addHandler e (g . f)

-- | Version of 'fmap' that performs an 'IO' action for each event occurence.
mapIO :: (a -> IO b) -> Event a -> Event b
mapIO f Never = Never
mapIO f e     = mkEvent addHandler'
    where addHandler' g = addHandler e (g <=< f)


-- | Merge two event streams of the same type. Semantically, we have
-- 
-- > union ((time1,a1):es1) ((time2,a2):es2)
-- >    | time1 < time2 = (time1,a1) : union es1 ((time2,a2):es2)
-- >    | time1 > time2 = (time2,a2) : union ((time1,a1):es1) es2
-- >    | otherwise     = ... -- either of the previous two cases
-- 
-- Note that the order of events that happen simultaneously is /undefined/.
-- This is not a problem most of the time,
-- but sometimes you have to force a certain order.
-- In that case, you have to combine this with the 'orderedDuplicate' function. 
union :: Event a -> Event a -> Event a
union Never e2    = e2
union e1    Never = Event { addHandler = addHandler e1} -- need to be lazy here
union e1    e2    = mkEvent addHandler'
    where addHandler' g = addHandler e1 g >> addHandler e2 g
    -- FIXME: union and recursion
    -- Sometimes, events depend on themselves recursively.
    -- This is were things get hairy.
    -- Problem: Checking whether an event is Never may result in a black hole
    -- For now, union is left-biased. Maye it should always return
    -- Event anyway.

-- | The 'Monoid' instance allows you to merge event streams,
-- see the 'union' function below.
-- 
-- > mempty  = never
-- > mappend = union
instance Monoid (Event a) where
    mempty  = never
    mappend = union

-- | Merge two event streams that have differen types. Semantically, we have
-- 
-- > merge e1 e2 = fmap Left e1 `union` fmap Right e2
merge :: Event a -> Event b -> Event (Either a b)
merge e1 e2 = fmap Left e1 `union` fmap Right e2


-- | Duplicate an event stream while paying attention to ordering.
-- Events from the first duplicate (and anything derived from them)
-- will always happen
-- before the events from the second duplicate.
-- Use this function to fine-tune the order of events.
orderedDuplicate :: Event a -> (Event a, Event a)
orderedDuplicate Never = (never, never)
orderedDuplicate e     =
    unsafePerformIO $ do      -- should be safe, though, only for sharing
        es1 <- newEventSource
        es2 <- newEventSource
        addHandler e $ \a -> fire es1 a >> fire es2 a
        return (fromEventSource es1, fromEventSource es2)

-- | Pass all events that fulfill the predicate, discard the rest. Semantically,
-- 
-- > filter p es = [(time,a) | (time,a) <- es, p a]
filter :: (a -> Bool) -> Event a -> Event a
filter p Never = Never
filter p e     = mkEvent addHandler'
    where addHandler' g = addHandler e $ \a -> when (p a) (g a)

-- | Unpacks event values of the form @Change _@ and discards
-- everything else.
filterChanges :: Event (Change a) -> Event a
filterChanges = fmap (\(Change x) -> x) . filter isChange


-- | Debugging helper. Prints the first argument and the value of the event
-- whenever it happens to 'stderr'.
traceEvent :: Show a => String -> Event a -> Event a
traceEvent s = mapIO (\a -> hPutStrLn stderr (s ++ " : " ++ show a) >> return a)

{-----------------------------------------------------------------------------
    Behavior
------------------------------------------------------------------------------}
{-
FIXME: exporting  initial  to users might cause space leaks
where the initial value is retained long beyond the point where
it was consumed.
However, if we want the user to implement optimized behaviors
himself, like  TimeGraphic , we have to provide a mechanism
similar to this one.
Alternative: keep current value in a IORef. This will eliminate
this particular space leak? Probably not. I think it's fine the way it is.
-}

{-$Behavior

The 'Behavior' type constructor is the other cornerstone of the
present approach to functional reactive programming.
It represents a value that changes with time.

-}

{-| @Behavior a@ represents a value in time. Think of it as

> type Behavior a = Time -> a

However, note that this model misses an important point:
we only allow /piecewise constant/ functions.
Continuous behaviors like

> badbehavior = \time -> 2*time

cannot be implemented.

-}
data Behavior a = Behavior {
    initial :: a,       -- ^ The value that the behavior initially has.
    changes :: Event a
        -- ^ An event stream recording how the behavior changes
        -- Remember that behaviors are piecewise constant functions.
    }

-- | Smart constructor. Supply an initial value and a sequence of changes.
-- In particular,
-- 
-- > initial (behavior a es) = a
-- > changes (behavior a es) = es
behavior :: a -> Event a -> Behavior a
behavior = Behavior

-- | The constant behavior. Semantically,
-- 
-- > always a = \time -> a
always :: a -> Behavior a
always a = Behavior { initial = a, changes = never }

    -- trigger an event whenever the value changes.
-- changes :: Behavior a -> Event a

-- | Version of 'accumulate' that involves the 'Change' data type
-- and performs an 'IO' action to update the value.
-- 
-- It is recommended that you use the 'accumulate' function from
-- 'Reactive.Classes' to pick types automatically.
accumulateIOChange :: (b -> a -> IO (Change a)) -> a -> Event b -> Behavior a
accumulateIOChange f a eb    = Behavior { initial = a ,
        changes = case eb of
            Never -> Never
            _     -> mkEvent addHandler' }
    where
    addHandler' g = addHandler eb (handler g)
    
    -- we need a global state
    -- FIXME: NOINLINE pragma!
    ref = unsafePerformIO $ newIORef a
    handler g = \b -> do
        a   <- readIORef ref    -- read old value
        ma' <- f b a            -- accumulate
        case ma' of
            Keep      -> return ()
            Change a' -> do
                writeIORef ref $! a'    -- use new value
                g a'

{- | The most important way to create behaviors.
The 'accumulate'' function is similar to a strict left fold, 'foldl''.
It starts with an initial value and combines it with incoming events.
For example, semantically
 
> accumulate' (++) "x" [(time1,"y"),(time2,"z")]
>    = behavior "x" [(time1,"yx"),(time2,"zyx")]
 
Note that the accumulated value is evaluated /strictly/.
This prevents space leaks.

It is recommended that you use the 'accumulate' function from
'Reactive.Classes' to pick types automatically.
-}
accumulate' :: (b -> a -> a) -> a -> Event b -> Behavior a
accumulate' f = accumulateIOChange (\b a -> return . Change $ f b a)

-- | Version of 'accumulate' that involves the 'Change' data type.
-- Use the 'Keep' constructor to indicate that the incoming event 
-- hasn't changed the value. No change event will be propagated in that case.
-- 
-- It is recommended that you use the 'accumulate' function from
-- 'Reactive.Classes' to pick types automatically.
accumulateChange :: (b -> a -> Change a) -> a -> Event b -> Behavior a
accumulateChange f = accumulateIOChange (\b a -> return $ f b a)


-- | Version of 'accumulate' that performs an 'IO' action to update the value.
--     
-- It is recommended that you use the 'accumulate' function from
-- 'Reactive.Classes' to pick types automatically.
accumulateIO :: (b -> a -> IO a) -> a -> Event b -> Behavior a
accumulateIO f = accumulateIOChange (\b a -> fmap Change $ f b a)
    -- Note: IO would be unsound without sharing!


-- | The 'Functor' instance allows you to map the values of type @a@.
-- Semantically, 
-- 
-- > fmap f behavior = \time -> f (behavior time)
instance Functor Behavior where
    fmap f b = Behavior
        { initial = f (initial b), changes = fmap f (changes b) }

-- | The 'Applicative' instance is one most of the most important ways
-- to combine behaviors. Semantically,
-- 
-- > pure a    = always a
-- > bf <*> bx = \time -> bf time $ bx time 
instance Applicative Behavior where
    pure a    = always a
    
    -- optimize the cases where the event never fires
    (Behavior f Never) <*> bx = fmap (f $) bx
    bf <*> (Behavior x Never) = fmap ($ x) bf
    bf <*> bx                 = fmap (uncurry ($)) $
        accumulate' go (initial bf, initial bx) (changes bf `merge` changes bx)
        where
        go (Left  f') (f,x) = (f',x)
        go (Right x') (f,x) = (f,x')

    -- store the occurences of an event in a behavior
-- latch :: Event a -> Behavior (Maybe a)
-- latch = accumulate' (\a _ -> Just a) Nothing

-- | Map events while threading state.
-- Similar to the standard 'mapAccumL' function.
mapAccum :: (acc -> x -> (acc,y)) -> acc -> Event x -> (Behavior acc, Event y)
mapAccum f acc Never = (always acc, never) 
mapAccum f acc xs    =
    (fmap fst result, fmap snd $ changes result)
    where
    result = accumulate' (\x (acc,_) -> f acc x) (acc,undefined) xs

-- | The most important way to combine behaviors and events.
-- The 'apply' function applies a time-varying function to a stream of events.
-- Semantically,
-- 
-- > apply bf es = [(time, bf time a) | (time, a) <- es]
-- 
-- (Theoretically inclined people might
-- be wondering whether we could achieve the same effect with
-- the 'Applicative' instance. The answer is no, the semantics of
-- 'apply' and '<*>' are subtly different. That's why we need to distinguish
-- between behaviors and events.)
apply :: Behavior (a -> b) -> Event a -> Event b
apply (Behavior f Never) ex    = fmap f ex
apply bf                 Never = Never
apply bf                 ex    =
    filterChanges . snd . mapAccum go (initial bf) $ changes bf `merge` ex
    where
    go _ (Left  f) = (f, Keep)
    go f (Right x) = (f, Change $ f x)

{-----------------------------------------------------------------------------
    Change
------------------------------------------------------------------------------}
{- | Data type to indicate that a value has changed.
Used in conjunction with the 'accumulate' functions.

This is basically the @Maybe@ type with a different name.
Using a different name improves program readability
and makes it easier to automatically select the right 'accumulate'
function by type, see the 'Reactive.Classes' module.
-}
data Change a =
    Keep            -- ^ Signals that the value has not changed.
    | Change a      -- ^ Indicates a change to some value of type @a@.
    deriving (Eq, Show, Read)

instance Functor Change where
    fmap _ Keep       = Keep
    fmap f (Change a) = Change (f a)

-- | The 'isChange' function returns 'True' iff its argument is of the form @Change _@.
isChange :: Change a -> Bool
isChange (Change _) = True
isChange _          = False

-- | The 'isKeep' function returns 'True' iff its argument is of the form @Keep@.
isKeep :: Change a -> Bool
isKeep Keep = True
isKeep _    = False

{-----------------------------------------------------------------------------
    Test examples
    
    The examples return event sources that you can fire.
------------------------------------------------------------------------------}
testCounter :: Prepare (EventSource Int)
testCounter = do
    es <- newEventSource
    let e = fromEventSource es
    reactimate . changes $ print <$> accumulate' (+) 0 e
    return es

-- test the  apply  function
testApply :: Prepare (EventSource Int, EventSource Int)
testApply = do
    es1 <- newEventSource
    let e1 = fromEventSource es1
    
    es2 <- newEventSource
    let e2 = fromEventSource es2

    reactimate . fmap print $ apply (fmap (+) (Behavior 0 e1)) e1
    return (es1, es2)

