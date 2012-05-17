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
    ) where

import Control.Applicative
import Control.Monad (join, liftM, ap, when)

import Data.Maybe (fromJust)
import Data.Functor.Identity

import qualified Data.HashSet as Set
import Data.Unique.Really
import qualified Data.Vault as Vault

import System.IO.Unsafe (unsafePerformIO)

import Reactive.Banana.Internal.InputOutput

type Set = Set.HashSet

{-----------------------------------------------------------------------------
    Event graph and network
------------------------------------------------------------------------------}
-- representation of the current state of the event network
data Graph a
    = Graph
    { grValues  :: Vault.Vault  -- currently calculated values
    , grAccums  :: Vault.Vault  -- accumulation values
    , grIsInit  :: Set Unique   -- is a node initialized?
    , grDemand  :: [SomeNode]   -- nodes that need to be evaluated
    , grOutput  :: Event a      -- output event
    }

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
            in  (a, graph2 { grAccums = Vault.insert (keyValue self) a
                                                     (grValues graph2) }, f) 

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

{-----------------------------------------------------------------------------
    Running the event graph
------------------------------------------------------------------------------}
step :: [InputValue] -> Graph b -> IO (Maybe b, Graph b)
step inputs graphOld = return (mb, graphNew2)
    where
    calculateGraph = mapM_ (\(E e) -> calculateE e >> return ())
                           (grDemand graphOld)
                   >> calculateE (grOutput graphOld)
    (mb, graphNew1, f) = runNetwork calculateGraph graphOld
    graphNew2 = (f graphNew1) { grValues = Vault.empty }

compileToAutomaton :: Event b -> Automaton b
compileToAutomaton e = fromStateful step $
    Graph { grValues = Vault.empty
          , grAccums = Vault.empty
          , grIsInit = Set.empty
          , grDemand = [E e]
          , grOutput = e }

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
    , hasAccum'         :: Maybe a                  -- carries accumulator
    , initialArguments' :: [SomeNode]               -- inital dependencies
    }

type EventSeed    = NodeSeed Maybe
type BehaviorSeed = NodeSeed Identity

-- create an event with observable sharing
makeEvent :: EventSeed a -> Event a
makeEvent seed = unsafePerformIO $ do
    self <- newKeys
    return $ Node
        { calculate  = cached self (calculate' seed self)
        , initialize = makeInitialize self seed }

-- create a behavior with observable sharing
makeBehavior :: BehaviorSeed a -> Behavior a
makeBehavior seed = unsafePerformIO $ do
    self <- newKeys
    return $ Node
        { calculate  = calculate' seed self
        , initialize = makeInitialize self seed }

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

makeInitialize :: Keys a -> NodeSeed f a -> Network ()
makeInitialize self seed = do
    b <- readIsInit self
    when (not b) $ do
        -- initialize accumulator if applicable
        case hasAccum' seed of
            Just acc -> writeAccum self acc
            Nothing  -> return ()
        -- mark as initialized
        writeIsInit self
        -- recurse into dependencies
        mapM_ initializeSomeNode (initialArguments' seed)

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
    , hasAccum'         = Nothing
    , initialArguments' = [E e]
    }

filterJust :: Event (Maybe a) -> Event a
filterJust e = makeEvent $ NodeSeed
    { calculate'        = \_ -> return . join =<< calculateE e
    , hasAccum'         = Nothing
    , initialArguments' = [E e]
    }

unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith f e1 e2 = makeEvent $ NodeSeed
    { calculate'        = \_ -> eval <$> calculateE e1 <*> calculateE e2
    , hasAccum'         = Nothing
    , initialArguments' = [E e1, E e2]
    }
    where
    eval (Just x) (Just y) = Just (f x y)
    eval (Just x) Nothing  = Just x
    eval Nothing  (Just y) = Just y
    eval Nothing  Nothing  = Nothing


accumE :: a -> Event (a -> a) -> Event a
accumE acc e = makeEvent $ NodeSeed
    { calculate'        = \self ->
            maybe (return Nothing) (accum self) =<< calculateE e
    , hasAccum'         = Just acc
    , initialArguments' = [E e]
    }
    where
    accum self f = do
        acc' <- f <$> readAccum self
        writeAccum self $! acc'
        return (Just acc')

stepperB :: a -> Event a -> Behavior a
stepperB acc e = makeBehavior $ NodeSeed
    { calculate'        = \self -> do
            -- write accumulator if applicable
            maybe (return ()) (writeAccum self $!) =<< calculateE e
            -- return previous instance
            Identity <$> readAccum self
    , hasAccum'         = Just acc
    , initialArguments' = [E e]
    }


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
    , hasAccum'         = Nothing
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
    , hasAccum'         = Nothing
    , initialArguments' = [E e]
    }


-- trim an event to be used with dynamic event switching
trimE :: Event a -> Moment (Moment (Event a))
trimE e = do
    initialize e
    addDemand (E e)
    return $ return e

