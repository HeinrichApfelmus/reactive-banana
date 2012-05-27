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

    compileToAutomaton,
    Event, Behavior, Moment,
    inputE, never, mapE, unionWith, filterJust, accumE, applyE,
    stepperB, pureB, applyB, mapB,
    switchE, observeE, trimE,
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
    { grValues  :: Values       -- currently calculated values
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
    { keyValue  :: !(Vault.Key (Maybe a))   -- currently calculated value
    , keyAccum  :: !(Vault.Key a)           -- value to be accumulated
    , keyName   :: !Unique                  -- unique identifier
    }

-- create new keys
newKeys :: IO (Keys a)
newKeys = Keys <$> Vault.newKey <*> Vault.newKey <*> newUnique

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

-- Perform a calculation with side effects and cache the result.
-- If the cached value is already available, we don't perform the side effect.
cached :: Keys a -> Network (Maybe a) -> Network (Maybe a)
cached self m = Network $ \graph ->
    case Vault.lookup (keyValue self) (grValues graph) of
        Just a  -> (a,graph,id)   -- lookup succeeded
        Nothing ->                -- lookup failed
            let (a, graph2, f) = runNetwork m graph
            in  (a, graph2 { grValues = Vault.insert (keyValue self) a
                                                     (grValues graph2) }, f) 

-- read cached value
readValue :: Keys a -> Network (Maybe a)
readValue self = Network $ \graph ->
    (join $ Vault.lookup (keyValue self) (grValues graph), graph, id)

-- write cached value
writeValue :: Keys a -> Maybe a -> Network ()
writeValue self x = Network $ \graph ->
    ((), graph { grValues = Vault.insert (keyValue self) x (grValues graph) }, id )

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


addInput :: Keys a -> InputChannel a -> Network ()
addInput self channel = Network $ \graph ->
    ((), graph { grInputs = f : grInputs graph } , id)
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
    graph2 = graph1 { grValues = addInputs (grInputs graph1) inputvalues }
    action = do
        mapM_ (\(E e) -> void $ calculateE e) (grDemand graph2)
        calculateE (grOutput graph2)
    (mb, graph3, f) = runNetwork action graph2
    graph4          = (f graph3) { grValues = error "grValues: internal error" }

concatenate = foldr (.) id
addInputs fs xs = (concatenate [f x | x <- xs, f <- fs]) Vault.empty

compileToAutomaton :: Event b -> Automaton b
compileToAutomaton e = fromStateful step graph3
    where
    -- initialize the graph
    graph3       = f graph2
    (_,graph2,f) = runNetwork (join $ trimE e) graph1
    graph1       = empty { grOutput = e } 
    empty        =
        Graph { grValues = undefined
              , grAccums = Vault.empty
              , grIsInit = Set.empty
              , grDemand = []
              , grInputs = []
              , grOutput = undefined }

{-----------------------------------------------------------------------------
    Building events and behaviors with
    observable sharing
------------------------------------------------------------------------------}
-- Event or Behavior with observable sharing
data Node f a = Node
    { calculate  :: Network (f a)
    , initialize :: Network ()
    }

type Event    = Node Maybe
type Behavior = Node Identity

-- existentially quantified version of an event or behavior
data SomeNode = forall a. E (Event a) | forall a. B (Behavior a)

-- inital description of the functionality of an event or behavior
data NodeSeed f a = NodeSeed
    { calculate'        :: Keys a -> Network (f a)  -- calculation function
    , initialArguments' :: [SomeNode]               -- inital dependencies
    }

type EventSeed    = NodeSeed Maybe
type BehaviorSeed = NodeSeed Identity

-- create an event with observable sharing
makeEventAccum :: Maybe a -> EventSeed a -> Event a
makeEventAccum accum seed = unsafePerformIO $ do
    self <- newKeys
    return $ Node
        { calculate  = cached self (calculate' seed self)
        , initialize = makeInitializeAccum self accum seed
        }
makeEvent = makeEventAccum Nothing

-- create a behavior with observable sharing
makeBehaviorAccum :: Maybe a -> BehaviorSeed a -> Behavior a
makeBehaviorAccum accum seed = unsafePerformIO $ do
    self <- newKeys
    return $ Node
        { calculate  = calculate' seed self
        , initialize = makeInitializeAccum self accum seed
        }
makeBehavior = makeBehaviorAccum Nothing

-- functions with specialized types
calculateE :: Event a -> Network (Maybe a)
calculateE = calculate

calculateB :: Behavior a -> Network a
calculateB b = runIdentity <$> calculate b

{-----------------------------------------------------------------------------
    Initialize a node, add it to the event graph
------------------------------------------------------------------------------}
initializeSomeNode :: SomeNode -> Network ()
initializeSomeNode (E e) = initialize e
initializeSomeNode (B b) = initialize b

makeInitializeAccum :: Keys a -> Maybe a -> NodeSeed f a -> Network ()
makeInitializeAccum self accum seed = do
    b <- readIsInit self
    when (not b) $ do
        -- initialize accumulator if applicable
        case accum of
            Just acc -> writeAccum self acc
            Nothing  -> return ()
        -- mark as initialized
        writeIsInit self
        -- recurse into dependencies
        mapM_ initializeSomeNode (initialArguments' seed)

cacheInit self action = do
    b <- readIsInit self
    when (not b) $ do
        action
        writeIsInit self

{-----------------------------------------------------------------------------
    Input nodes
------------------------------------------------------------------------------}
inputE :: InputChannel a -> Event a
inputE channel = unsafePerformIO $ do
    self <- newKeys
    return $ Node
        { calculate  = readValue self
        , initialize = cacheInit self $ addInput self channel
        }

{-----------------------------------------------------------------------------
    Basic combinators
------------------------------------------------------------------------------}
-- TODO: remove nodes that never generate an event from the graph
-- This will yield significant benefits when we collect garbage
-- in a dynamically evolving event graph.
never :: Event a
never = Node { calculate = return Nothing, initialize = return () }

mapE :: (a -> b) -> Event a -> Event b
mapE f e = makeEvent $ NodeSeed
    { calculate'        = \_ -> fmap f <$> calculateE e
    , initialArguments' = [E e]
    }

applyE :: Behavior (a -> b) -> Event a -> Event b
applyE b e = makeEvent $ NodeSeed
    { calculate'        = \_ -> calculateE e >>= \mx -> case mx of
            Just x  -> Just . ($ x) <$> calculateB b
            Nothing -> return Nothing
    , initialArguments' = [B b, E e]
    }

filterJust :: Event (Maybe a) -> Event a
filterJust e = makeEvent $ NodeSeed
    { calculate'        = \_ -> return . join =<< calculateE e
    , initialArguments' = [E e]
    }

unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith f e1 e2 = makeEvent $ NodeSeed
    { calculate'        = \_ -> eval <$> calculateE e1 <*> calculateE e2
    , initialArguments' = [E e1, E e2]
    }
    where
    eval (Just x) (Just y) = Just (f x y)
    eval (Just x) Nothing  = Just x
    eval Nothing  (Just y) = Just y
    eval Nothing  Nothing  = Nothing


accumE :: a -> Event (a -> a) -> Event a
accumE acc e = makeEventAccum (Just acc) $ NodeSeed
    { calculate'        = \self ->
            maybe (return Nothing) (accum self) =<< calculateE e
    , initialArguments' = [E e]
    }
    where
    accum self f = do
        acc' <- f <$> readAccum self
        writeAccum self $! acc'
        return (Just acc')


stepperB :: a -> Event a -> Behavior a
stepperB acc e = makeBehaviorAccum (Just acc) $ NodeSeed
    { calculate'        = \self -> do
            -- TODO: Calculating the next value has to be delayed,
            -- otherwise we run into an infinite loop!
            -- write accumulator if applicable
            maybe (return ()) (writeAccum self $!) =<< calculateE e
            -- return previous instance
            Identity <$> readAccum self
    , initialArguments' = [E e]
    }

pureB :: a -> Behavior a
pureB x = makeBehavior $ NodeSeed
    { calculate'        = \_ -> Identity <$> return x
    , initialArguments' = []
    }

applyB :: Behavior (a -> b) -> Behavior a ->  Behavior b
applyB bf bx = makeBehavior $ NodeSeed
    { calculate'        = \_ -> Identity <$> (calculateB bf `ap` calculateB bx)
    , initialArguments' = [B bf, B bx]
    }

mapB f = applyB (pureB f)

{-----------------------------------------------------------------------------
    Dynamic event switching
------------------------------------------------------------------------------}
-- monad that ensure common start times
type Moment a = Network a

-- dynamic event switching
switchE :: Event (Moment (Event a)) -> Event a
switchE ee = makeEvent $ NodeSeed
    { calculate'        = \self ->
            -- sample current event and calculate its occurrence
            calculateE =<< calculateB bAccum
    , initialArguments' = [E ee]
    }
    where
    -- switch into new event
    bAccum = stepperB never (observeE ee)


observeE :: Event (Moment a) -> Event a
observeE e = makeEvent $ NodeSeed
    { calculate'        = \self -> do
            mx <- calculateE e
            case mx of
                Nothing -> return Nothing
                Just x  -> Just <$> x
    , initialArguments' = [E e]
    }


-- trim an event to be used with dynamic event switching
trimE :: Event a -> Moment (Moment (Event a))
trimE e = do
    initialize e
    addDemand (E e)
    return $ return e

