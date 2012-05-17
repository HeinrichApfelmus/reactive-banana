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
import Control.Monad (join, liftM, ap)

import Data.Maybe (fromJust)
import qualified Data.Vault as Vault

import System.IO.Unsafe (unsafePerformIO)

import Data.Unique.Really

import Reactive.Banana.Internal.InputOutput

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

-- pointer to a single node of type  a  in the graph
data Node a
    = Node
    { nodeValue  :: !(Vault.Key (Maybe a))   -- currently calculated value
    , nodeAccum  :: !(Vault.Key a)           -- value to be accumulated
    , nodeName   :: !Unique                  -- unique identifier
    }

-- create new keys
newNode :: IO (Node a)
newNode = Node <$> Vault.newKey <*> Vault.newKey

-- network monad for modifying the event network and graph
newtype Network a = Network { runNetwork :: forall b.
        Graph b -> (a,            -- calculate things from cached values
            Graph b,              -- change graph right away
            Graph b -> Graph b)   -- record future accumulation values
    }

instance Functor Network where fmap = liftM
instance Applicative Network where (<*>) = ap

instance Monad Network where
    return a = Network $ \s -> (a, s, id)
    m >>= g  = Network $ \s1 ->
        let (a, s2, w1) = runNetwork m s1
            (b, s3, w2) = runNetwork (g a) s2
        in  (b, s3, w2 . w1)

-- Perform a calculation with side effects and cache the result.
-- If the cached value is already available, we don't perform the side effect.
cached :: Node a -> Network (Maybe a) -> Network (Maybe a)
cached self m = Network $ \graph ->
    case Vault.lookup (nodeValue self) (grValues graph) of
        Just a  -> (a,graph,id)   -- lookup succeeded
        Nothing ->                -- lookup failed
            let (a, graph2, f) = runNetwork m graph
            in  (a, graph2 { grAccums = Vault.insert (nodeValue self) a
                                                     (grValues graph2) }, f) 

-- read the currently accumulated value
readAccum :: Node a -> Network a
readAccum self = Network $
    \graph -> case Vault.lookup (nodeAccum self) (grAccums graph) of
        Just x -> (x, graph, id)

-- schedule a write of the new accumulated value
writeAccum :: Node a -> a -> Network ()
writeAccum self x = Network $
    \graph -> ((), graph,
        \g -> g { grAccums = Vault.insert (nodeAccum self) x (grAccums g) })

-- test whether we have initialized the accumulator
isAccumInitialized :: Node a -> Network Bool
isAccumInitialized self = Network $
    \graph -> (isJust (Vault.lookup (nodeAccum self) (grAccums graph), graph, id)


-- make sure that the node is being evaluated
-- because it may accumulate state that we don't want to miss
-- TODO: Undo this when the Haskell RTS garbage collects this node!
demand :: SomeNode -> Network ()
demand somenode = Network $ \graph ->
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
          , grDemand = [E e]
          , grOutput = e }

{-----------------------------------------------------------------------------
    Building events with
    observable sharing
------------------------------------------------------------------------------}
-- Basic description of the functionality of an event
data EventSeed a = EventSeed
    { calculateE'        :: Node a -> Network (Maybe a)
    , hasAccumE'         :: Maybe a
    , initialArgumentsE' :: [SomeNode]
    }

-- Event with observable sharing
data Event a = Event
    { calculateE :: Network (Maybe a)
    , initAccumE :: Network ()
    }
makeEvent :: EventSeed a -> Event a
makeEvent es = unsafePerformIO $ do
    self <- newNode
    return $ Event
        { calculateE = cached self (calculateE' es self)
        , initAccumE =  }

-- Basic description of the functionality of a behavior
data BehaviorSeed a = BehaviorSeed
    { calculateB'        :: Node a -> Network a
    , hasAccumB'         :: Maybe a
    , initialArgumentsB' :: [SomeNode]
    }

-- Behavior with observable sharing
data Behavior a = Behavior
    { calculateB :: Network a   -- retrieve current value
    , initAccumB :: Network ()
    }

makeBehavior :: BehaviorSeed a -> Behavior a
makeBehavior bs = unsafePerformIO $ do
    self <- newNode
    -- let calculateB = cached self (calculateB' bs self)
    let calculateB = calculateB' bs self
    return $ Behavior { calculateB = calculateB }


-- existentially quantified version of an event or behavior
data SomeNode = forall a. E (Event a) | forall a. B (Behavior a)


{-----------------------------------------------------------------------------
    Initialize a node, add it to the event graph
------------------------------------------------------------------------------}

initAccum :: SomeNode -> Network ()
initAccum (E e) = initAccumE e
initAccum (B b) = initAccumB b

traverseInitAccum :: Node a -> Maybe a -> [SomeNode] -> Network ()
traverseInitAccum self macc dependencies = do
    b <- isAccumInitialized self
    when (not b && isJust macc)
        writeAccum self $! fromJust macc
        mapM_ initAccum dependencies

{-----------------------------------------------------------------------------
    Basic combinators
------------------------------------------------------------------------------}
-- TODO: remove nodes that never generate an event from the graph
-- This will yield significant benefits when we collect garbage
-- in a dynamically evolving event graph.
never :: Event a
never = Event { calculateE = return Nothing }

mapE :: (a -> b) -> Event a -> Event b
mapE f e = makeEvent $ EventSeed
    { calculateE' = \_ -> fmap f <$> calculateE e
    , hasAccumE'  = Nothing
    , initialArgumentsE' = [E e]
    }

filterJust :: Event (Maybe a) -> Event a
filterJust e = makeEvent $ EventSeed
    { calculateE' = \_ -> return . join =<< calculateE e
    , hasAccumE'  = Nothing
    , initialArgumentsE' = [E e]
    }

unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith f e1 e2 = makeEvent $ EventSeed {
    { calculateE' = \_ -> eval <$> calculateE e1 <*> calculateE e2
    , hasAccumE'  = Nothing
    , initialArgumentsE' = [E e1, E e2]
    }
    where
    eval (Just x) (Just y) = Just (f x y)
    eval (Just x) Nothing  = Just x
    eval Nothing  (Just y) = Just y
    eval Nothing  Nothing  = Nothing


accumE :: a -> Event (a -> a) -> Event a
accumE acc e = makeEvent $ EventSeed
    { calculateE' = \self ->
            maybe (return Nothing) (accum self) =<< calculateE e
    , hasAccumE'  = Just acc
    , initialArgumentsE' = [E e]
    }
    where
    accum self f = do
        acc' <- f <$> readAccum self
        writeAccum self $! acc'
        return (Just acc')

stepperB :: a -> Event a -> Behavior a
stepperB acc e = makeBehavior $ BehaviorSeed {
    { calculateB' = \self -> do
            -- write accumulator if applicable
            maybe (return ()) (writeAccum self $!) =<< calculateE e
            -- return previous instance
            readAccum self
    , hasAccumB'  = Just acc
    , initialArgumentsB' = [E e]
    }


{-----------------------------------------------------------------------------
    Dynamic event switching
------------------------------------------------------------------------------}
-- monad that ensure common start times
type Moment a = Network a

-- dynamic event switching
switchE :: Event (Moment (Event a)) -> Event a
switchE ee = makeEvent $ EventSeed
    { calculateE' = \self ->
            -- sample current event and calculate its occurrence
            calculateE =<< calculateB bAccum
    , hasAccumE'  = Nothing
    , initialArgumentsE' = [E ee]
    }
    where
    -- switch into new event
    bAccum = stepperB never (observeE ee)


observeE :: Event (Moment a) -> Event a
observeE e = makeEvent $ EventSeed
    { calculateE' = \self -> do
            me <- calculateE e
            case me of
                Nothing -> return Nothing
                Just x  -> Just <$> x
    , hasAccumE'  = Nothing
    , initialArgumentsE' = [E e]
    }


-- trim an event to be used with dynamic event switching
trimE :: Event a -> Moment (Moment (Event a))
trimE e = do
    demand (E e)
    return $ return e

