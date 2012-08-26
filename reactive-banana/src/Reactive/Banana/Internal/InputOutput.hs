{-----------------------------------------------------------------------------
    Reactive Banana
------------------------------------------------------------------------------}
module Reactive.Banana.Internal.InputOutput (
    -- * Synopsis
    -- | Manage the input and output of event graphs.
    
    -- * Input
    -- | Utilities for managing heterogenous input values.
    Channel, InputChannel, InputValue,
    
    newInputChannel, getChannel,
    fromValue, toValue,
    
    -- * Output
    -- | Stepwise execution of an event graph.
    Automaton(..), fromStateful, unfoldAutomaton,

    ) where

import Control.Applicative
import Control.Exception (evaluate)

import Data.Unique.Really
import qualified Data.Vault  as Vault

{-----------------------------------------------------------------------------
    Storing heterogenous input values
------------------------------------------------------------------------------}
type Channel  = Unique          -- identifies an input
type Key      = Vault.Key       -- key to retrieve a value
type Value    = Vault.Vault     -- value storage

data InputChannel a  = InputChannel { getChannelC :: Channel, getKey :: Key a }
data InputValue      = InputValue   { getChannelV :: Channel, getValue :: Value }

newInputChannel :: IO (InputChannel a)
newInputChannel = InputChannel <$> newUnique <*> Vault.newKey

fromValue :: InputChannel a -> InputValue -> Maybe a
fromValue i v = Vault.lookup (getKey i) (getValue v)

toValue :: InputChannel a -> a -> InputValue
toValue i a = InputValue (getChannelC i) $ Vault.insert (getKey i) a Vault.empty

-- convenience class for overloading
class HasChannel a where
    getChannel :: a -> Channel
instance HasChannel (InputChannel a) where getChannel = getChannelC
instance HasChannel (InputValue) where getChannel = getChannelV


{-----------------------------------------------------------------------------
    Stepwise execution
------------------------------------------------------------------------------}
-- Automaton that takes input values and produces a result
data Automaton a = Step { runStep :: [InputValue] -> IO (Maybe a, Automaton a) }

fromStateful :: ([InputValue] -> s -> IO (Maybe a,s)) -> s -> Automaton a
fromStateful f s = Step $ \i -> do
    (a,s') <- f i s
    return (a, fromStateful f s')

-- | Apply an automaton to a list of input values
unfoldAutomaton :: Automaton b -> InputChannel a -> [Maybe a] -> IO [Maybe b]
unfoldAutomaton _    _ []       = return []
unfoldAutomaton auto i (mx:mxs) = do
    (b, auto) <- runStep auto $ maybe [] (\x -> [toValue i x]) mx
    bs        <- unfoldAutomaton auto i mxs
    return (b:bs)
    
