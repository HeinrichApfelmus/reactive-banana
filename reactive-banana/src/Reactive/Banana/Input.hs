{-----------------------------------------------------------------------------
    Reactive Banana
------------------------------------------------------------------------------}
module Reactive.Banana.Input (
    -- * Synopsis
    -- | Internal module.
    -- Utilities for managing heterogenous input values.
    
    Channel, InputChannel, InputValue,
    
    newInputChannel, getChannel,
    fromValue, toValue
    
    ) where

import Control.Applicative

import qualified Data.Unique as Unique
import qualified Data.Vault as Vault

{-----------------------------------------------------------------------------
    Storing heterogenous input values
------------------------------------------------------------------------------}
type Channel  = Unique.Unique   -- identifies an input
type Key      = Vault.Key       -- key to retrieve a value
type Value    = Vault.Vault     -- value storage

data InputChannel a  = InputChannel { getChannelC :: Channel, getKey :: Key a }
data InputValue      = InputValue   { getChannelV :: Channel, getValue :: Value }

newInputChannel :: IO (InputChannel a)
newInputChannel = InputChannel <$> Unique.newUnique <*> Vault.newKey

fromValue :: InputChannel a -> InputValue -> Maybe a
fromValue i v = Vault.lookup (getKey i) (getValue v)

toValue :: InputChannel a -> a -> InputValue
toValue i a = InputValue (getChannelC i) $ Vault.insert (getKey i) a Vault.empty

-- convenience class for overloading
class HasChannel a where
    getChannel :: a -> Channel
instance HasChannel (InputChannel a) where getChannel = getChannelC
instance HasChannel (InputValue) where getChannel = getChannelV

