{-----------------------------------------------------------------------------
    Reactive Banana

    Helper Module: A typed, inhomogeneous storage.
    Uses  IORefs  to read and write.
------------------------------------------------------------------------------}
module Reactive.Banana.Vault (
    Vault, Key,
    empty, newKey, lookup, insert, delete,
    ) where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Data.Unique
import System.IO.Unsafe (unsafePerformIO)

-- | An inhomogeneous, type safe storage.
type Vault = Map Unique Item
-- Values are stored in closures that write to a temporary IORef
-- This way, we can "circumvent" the type system.
type Item  = IO ()

-- Key for the vault
data Key a   = Key Unique (Item' a)
-- Keeps track of the temporary IORef for reading and writing
type Item' a = IORef (Maybe a)

-- | The empty vault.
empty :: Vault
empty = Map.empty

-- | Create a new key for use with a vault.
newKey   :: IO (Key a)
newKey = do
    k   <- newUnique
    ref <- newIORef Nothing
    return $ Key k ref

-- | Lookup the value of a key in the vault.
lookup :: Key a -> Vault -> Maybe a
lookup (Key k ref) vault = case Map.lookup k vault of
    Nothing   -> Nothing
    Just item -> unsafePerformIO $ do
        item                    -- write into IORef
        mx <- readIORef ref     -- read the value
        writeIORef ref Nothing  -- clear IORef
        return mx

-- | Insert a value for a given key. Overwrites any previous value.
insert :: Key a -> a -> Vault -> Vault
insert (Key k ref) x vault = Map.insert k (writeIORef ref $ Just x) vault

-- | Delete a key from the vault.
delete :: Key a -> Vault -> Vault
delete (Key k ref) vault = Map.delete k vault

