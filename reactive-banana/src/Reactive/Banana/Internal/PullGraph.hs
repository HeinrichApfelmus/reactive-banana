{-----------------------------------------------------------------------------
    Reactive-Banana
------------------------------------------------------------------------------}
{-----------------------------------------------------------------------------
    Dynamic event switching
    - serious demand-driven implementation
    - contains neat techniques on the graph manipulation issue,
      without GADTs
    - can hopefully be extended to a push-driven implementation
------------------------------------------------------------------------------}
{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
module Reactive.Banana.Internal.PullGraph (
    -- * Synopsis
    -- | Pull-driven implementation.

    compileToAutomaton, interpret,
    Event, Behavior, Moment,
    inputE, never, mapE, unionWith, filterJust, accumE, applyE,
    stepperB, pureB, applyB, mapB,
    initialB, trimE, trimB, observeE, switchE, switchB
    ) where

import Control.Applicative
import Control.Monad (join, liftM, ap, when, void)

import Data.Maybe (fromJust)
import Data.Functor.Identity

import qualified Data.HashSet as Set
import Data.Unique.Really
import qualified Data.Vault as Vault

import System.IO.Unsafe (unsafePerformIO)

import Reactive.Banana.Internal.InputOutput

import Debug.Trace

type Set = Set.HashSet

{-----------------------------------------------------------------------------
    Event graph and network
------------------------------------------------------------------------------}
-- representation of the current state of the event network
data Graph a
    = Graph
    { grValueE  :: Values       -- currently calculated event values
    , grValueB  :: Values       -- latest behavior values
    , grAccums  :: Vault.Vault  -- accumulation values
    , grIsInit  :: Set Unique   -- is a node initialized?
    , grDemand  :: [SomeNode]   -- nodes that need to be evaluated
    , grInputs  :: [Input]      -- input  nodes
    , grOutput  :: Event a      -- output event
    }

type Values = Vault.Vault       -- currently calculated values
type Input  =                   -- write input value into the graph
    InputValue -> Values -> Values

-- keys associated to a single node of type  a  in the graph
data Keys a
    = Keys
    { keyValueE  :: !(Vault.Key (Maybe a))   -- current event value
    , keyValueB  :: !(Vault.Key a)           -- current behavior value
    , keyAccum   :: !(Vault.Key a)           -- value to be accumulated
    , keyName    :: !Unique                  -- unique identifier
    }

-- create new keys
newKeys :: IO (Keys a)
newKeys = Keys <$> Vault.newKey <*> Vault.newKey <*> Vault.newKey <*> newUnique

-- network monad for modifying the event network and graph
newtype Network a = Network { runNetwork :: forall b.
        Graph b -> (a,            -- calculate things from cached values
            Graph b,              -- change graph right away
            Graph b -> Graph b)   -- record future accumulation values
    }

instance Functor Network where fmap = liftM
instance Applicative Network where pure = return; (<*>) = ap

instance Monad Network where
    return a = Network $ \s -> (a, s, id)
    m >>= g  = Network $ \s1 ->
        let (a, s2, w1) = runNetwork m s1
            (b, s3, w2) = runNetwork (g a) s2
        in  (b, s3, w2 . w1)

-- Perform calculation with side effects and cache the result as event value.
-- If the cached value is already available, we don't perform the side effect.
cachedE :: Keys a -> Network (Maybe a) -> Network (Maybe a)
cachedE self m = Network $ \graph ->
    case Vault.lookup (keyValueE self) (grValueE graph) of
        Just a  -> (a,graph,id)   -- lookup succeeded
        Nothing ->                -- lookup failed
            let (a, graph2, f) = runNetwork m graph
            in  (a, graph2 { grValueE = Vault.insert (keyValueE self) a
                                                     (grValueE graph2) }, f) 

-- Perform calculation with side effects and cache the result as event value.
-- If the cached value is already available, we don't perform the side effect.
cachedB :: Keys a -> Network a -> Network a
cachedB self m = Network $ \graph ->
    case Vault.lookup (keyValueE self) (grValueE graph) of
        Just a  -> (a,graph,id)   -- lookup succeeded
        Nothing ->                -- lookup failed
            let (a, graph2, f) = runNetwork m graph
            in  (a, graph2 { grValueE = Vault.insert (keyValueE self) a
                                                     (grValueE graph2) }, f) 

-- read event value
readValueE :: Keys a -> Network (Maybe a)
readValueE self = Network $ \graph ->
    (join $ Vault.lookup (keyValueE self) (grValueE graph), graph, id)

-- write behavior value
writeValueB :: Keys a -> Maybe a -> Network ()
writeValueB self x = Network $ \graph ->
    ((), graph { grValueB = Vault.insert (keyValueB self) x (grValueB graph) }, id )


-- read the currently accumulated value
readAccum :: Keys a -> Network a
readAccum self = Network $
    \graph -> case Vault.lookup (keyAccum self) (grAccums graph) of
        Just x -> (x, graph, id)

-- schedule a write of the new accumulated value
writeAccum :: Keys a -> a -> Network ()
writeAccum self x = Network $ \graph ->
    ( (), graph
    , \g -> g { grAccums = Vault.insert (keyAccum self) x (grAccums g) }
    )

-- immediately initialize an accumulated value
initAccum :: Keys a -> a -> Network ()
initAccum self x = Network $ \graph ->
    ( (), graph { grAccums = Vault.insert (keyAccum self) x (grAccums graph) }
    , id )


-- test whether we have initialized the node
readIsInit :: Keys a -> Network Bool
readIsInit self = Network $
    \graph -> (Set.member (keyName self) (grIsInit graph), graph, id)

-- set the initialization field
writeIsInit :: Keys a -> Network ()
writeIsInit self = Network $ \graph ->
        ( ()
        , graph { grIsInit = Set.insert (keyName self) (grIsInit graph) }
        , id )

-- make sure that the node is being evaluated
-- because it may accumulate state that we don't want to miss
-- TODO: Undo this when the Haskell RTS garbage collects this node!
addDemand :: SomeNode -> Network ()
addDemand somenode = Network $ \graph ->
    ((), graph, \g -> g { grDemand = somenode : grDemand g })

-- add a node that can receive input
addInput :: Keys a -> InputChannel a -> Network ()
addInput self channel = Network $ \graph ->
    ((), graph, \g -> g { grInputs = f : grInputs g } )
    where
    f :: InputValue -> Values -> Values
    f value
        | getChannel value == getChannel channel =
            Vault.insert (keyValue self) (fromValue channel value)
        | otherwise = id

{-----------------------------------------------------------------------------
    Running the event graph
------------------------------------------------------------------------------}
step :: [InputValue] -> Graph b -> IO (Maybe b, Graph b)
step inputvalues graph1 = return (mb, graph4)
    where
    graph2 = graph1 { grValueE = addInputs (grInputs graph1) inputvalues }
    action = do
        mapM_ (\(E e) -> void $ calculateE e) (grDemand graph2)
        calculateE (grOutput graph2)
    (mb, graph3, f) = runNetwork action graph2
    graph4          = (f graph3) { grValueE = error "grValueE: internal error" }

concatenate = foldr (.) id
addInputs fs xs = (concatenate [f x | x <- xs, f <- fs]) Vault.empty

compileToAutomaton :: Moment (Event b) -> Automaton b
compileToAutomaton me = fromStateful step graph3
    where
    -- initialize the graph
    graph3       = (f graph2) { grOutput = e }
    (e,graph2,f) = runNetwork (join . trimE =<< me) graph1
    graph1       = empty
    empty        =
        Graph { grValueE = undefined
              , grValueB = Vault.empty
              , grAccums = Vault.empty
              , grIsInit = Set.empty
              , grDemand = []
              , grInputs = []
              , grOutput = undefined }

interpret :: (Event a -> Moment (Event b)) -> [Maybe a] -> IO [Maybe b]
interpret f xs = do
    i <- newInputChannel
    let automaton = compileToAutomaton (f $ inputE i)
    unfoldAutomaton automaton i xs

{-----------------------------------------------------------------------------
    Building Events and Behaviors with observable sharing
------------------------------------------------------------------------------}
-- Event or Behavior with observable sharing
data Event a = Event
    { calculateE  :: Network (Maybe a)
    , initializeE :: Network ()
    }

data Behavior a = Behavior
    { latestB     :: Network a
    , changes     :: Event ()
    , initializeB :: Network ()
    }

-- overloaded getters and setters
class Initialize a where
    initialize :: a -> Network ()
instance Initialize (Behavior a) where initialize = initializeB
instance Initialize (Event    a) where initialize = initializeE

-- existentially quantified version of an event or behavior
data SomeNode = forall a. E (Event a) | forall a. B (Behavior a)



-- inital description of the functionality of an event or behavior
data EventSeed a = EventSeed
    { calculate'        :: Keys a -> Network (Maybe a)  -- calculation function
    , initialArgumentsE :: [SomeNode]                   -- inital dependencies
    }

data BehaviorSeed a = BehaviorSeed
    { latest'           :: Network a    -- calculation of new value
    , changes'          :: Event a      -- event indicating changes
    , initialArgumentsB :: [SomeNode]   -- initial dependencies
    }

-- create an event with observable sharing
makeEventAccumSelf :: Maybe a -> EventSeed a -> (Event a, Keys a)
makeEventAccumSelf accum seed = unsafePerformIO $ do
    self <- newKeys
    return $ (Event
        { calculateE  = cached self (calculate' seed self)
        , initializeE = makeInitializer self (initialArgumentsE seed) $
                            initializerAccum self accum
        }
        , self)
makeEventAccum x = fst . makeEventAccumSelf x
makeEvent = makeEventAccum Nothing

-- create a behavior with observable sharing
makeBehaviorAccum :: Maybe a -> BehaviorSeed a -> Behavior a
makeBehaviorAccum accum seed = unsafePerformIO $ do
    self <- newKeys
    return $ Node
        { latestB    = latest' seed
            -- cache the latest value whenever the  changes  event fires
        , changes    = changes' seed
        , initialize = makeInitializer self (initialArgumentsB seed) $
                        initializerAccum self accum
        }
makeBehavior = makeBehaviorAccum Nothing



{-----------------------------------------------------------------------------
    Initialize a node, add it to the event graph
------------------------------------------------------------------------------}
initializeSomeNode :: SomeNode -> Network ()
initializeSomeNode (E e) = initialize e
initializeSomeNode (B b) = initialize b

-- create initialization function that
-- is called only once and recurses into a set of dependencies
makeInitializer :: Keys a -> [SomeNode] -> Network () -> Network ()
makeInitializer self dependencies action = do
    b <- readIsInit self
    when (not b) $ do
        action
        -- mark as initialized
        writeIsInit self
        -- recurse into dependencies
        mapM_ initializeSomeNode dependencies

-- initialize an accumulator
initializeAccum :: Keys a -> Maybe a -> Network ()
initializeAccum self accum =
    case accum of
        Just acc -> initAccum self acc
        Nothing  -> return ()


{-----------------------------------------------------------------------------
    Input nodes
------------------------------------------------------------------------------}
inputE :: InputChannel a -> Event a
inputE channel = unsafePerformIO $ do
    self <- newKeys
    return $ Event
        { calculateE  = readValueE self
        , initializeE = makeInitializer self [] $ addInput self channel
        }

{-----------------------------------------------------------------------------
    Basic combinators
------------------------------------------------------------------------------}
-- TODO: remove nodes that never generate an event from the graph
-- This will yield significant benefits when we collect garbage
-- in a dynamically evolving event graph.
--
-- TODO: Add finalization for events / behaviors,
-- i.e. remove them from the demand list when they are no longer needed

never :: Event a
never = Event
    { calculateE  = return Nothing
    , initializeE = return ()
    }

mapE :: (a -> b) -> Event a -> Event b
mapE f e = makeEvent $ EventSeed
    { calculate'        = \_ -> fmap f <$> calculateE e
    , initialArgumentsE = [E e]
    }

applyE :: Behavior (a -> b) -> Event a -> Event b
applyE b e = makeEvent $ EventSeed
    { calculate'        = \_ -> calculateE e >>= \mx -> case mx of
            Just x  -> Just . ($ x) <$> calculateB b
            Nothing -> return Nothing
    , initialArgumentsE = [B b, E e]
    }

filterJust :: Event (Maybe a) -> Event a
filterJust e = makeEvent $ EventSeed
    { calculate'        = \_ -> return . join =<< calculateE e
    , initialArgumentsE = [E e]
    }

unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith f e1 e2 = makeEvent $ EventSeed
    { calculate'        = \_ -> eval <$> calculateE e1 <*> calculateE e2
    , initialArgumentsE = [E e1, E e2]
    }
    where
    eval (Just x) (Just y) = Just (f x y)
    eval (Just x) Nothing  = Just x
    eval Nothing  (Just y) = Just y
    eval Nothing  Nothing  = Nothing

-- accumulation
accumE :: a -> Event (a -> a) -> Event a
accumE acc = fst . accumESelf acc

accumESelf :: a -> Event (a -> a) -> (Event a, Keys a)
accumESelf acc e = makeEventAccumSelf (Just acc) $ EventSeed
    { calculate'        = \self ->
            maybe (return Nothing) (accum self) =<< calculateE e
    , initialArgumentsE = [E e]
    }
    where
    accum self f = do
        acc' <- f <$> readAccum self
        writeAccum self $! acc'
        return (Just acc')


stepperB :: a -> Event a -> Behavior a
stepperB acc = accumB acc . mapE const

accumB :: a -> Event (a -> a) -> Behavior a
accumB acc e1 = unsafePerformIO $ do
    self <- newKeys
    let (e2,eSelf) = accumESelf acc e1  -- helper event for accumulation
    return $ Node
        { calculate  =                  -- read old accumulator from event
                Identity <$> readAccum eSelf 
        , initialize = cacheInit self $ do
                initialize e2           -- initialize event
                addDemand (E e2)        -- make sure to demand the event
        }


pureB :: a -> Behavior a
pureB x = Behavior
    { latestB     = return x
    , changes     = never
    , initializeB = return ()
    }

applyB :: Behavior (a -> b) -> Behavior a ->  Behavior b
applyB bf bx = makeBehavior $ BehaviorSeed
    { latest'           = latest bf `ap` latest bx
    , changes'          = changes bf `mergeE` changes bx
    , initialArgumentsB = [B bf, B bx]
    }

mapB :: (a -> b) -> Behavior a -> Behavior b
mapB f = applyB (pureB f)


{-----------------------------------------------------------------------------
    Dynamic event switching
------------------------------------------------------------------------------}
-- monad that ensure common start times
type Moment a = Network a

initialB :: Behavior a -> Moment a
initialB b = do
    -- note: initialization must add accumulated value immediately,
    -- or this will not work
    initialize b
    latest b

-- trim an event
-- the trimmed event will not have an occurrence at the time it is observed
trimE :: Event a -> Moment (Moment (Event a))
trimE e = do
    initialize e
    addDemand (E e)
    return $ return e

-- trim a behavior
trimB :: Behavior a -> Moment (Moment (Behavior a))
trimB b = do
    initialize b
    return $ return b

-- evaluate moment action inside an event
observeE :: Event (Moment a) -> Event a
observeE e = makeEvent $ EventSeed
    { calculate'        = \self -> do
            mx <- calculateE e
            case mx of
                Nothing -> return Nothing
                Just x  -> Just <$> x
    , initialArgumentsE = [E e]
    }

-- internal combinator for event switching
internalSwitchE :: Behavior (Event a) -> Event a
internalSwitchE b = makeEvent $ EventSeed
    { calculate'        = \_ ->
            -- sample current event and calculate its occurrence
            calculateE =<< calculateB b
    , initialArgumentsE = [B b]
    }

-- dynamic event switching
switchE :: Event (Moment (Event a)) -> Event a
switchE ee = internalSwitchE bAccum
    where
    -- remember the current event
    bAccum = stepperB never (observeE $ mapE init ee)
    init m = do x <- m; initialize x; return x

-- dynamic behavior switching
switchB :: forall a. Behavior a -> Event (Moment (Behavior a)) -> Behavior a
switchB b eb = makeBehavior $ BehaviorSeed
    { latest'  = latest =<< latest bAccum
    , changes' = changesE
    , initialArgumentsB = [B bAccum, E changesE]
    }
    where
    -- remember the current behavior
    bAccum :: Behavior (Behavior a)
    bAccum = stepperB b (observeE $ mapE init eb)
    init m = do x <- m; initialize x; return x
    
    changesE = internalSwitchE (changes <$> bAccum)

