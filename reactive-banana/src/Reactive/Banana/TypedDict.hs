{-----------------------------------------------------------------------------
    Reactive Banana

    Helper Module: A typed, inhomogeneous dictionary.
    Uses  IORefs  to read and write.
------------------------------------------------------------------------------}
module Reactive.Banana.TypedDict (
    Dict, Key,
    empty, newKey, lookup, insert, delete,
    ) where

import Data.Unique
import Data.Map (Map)
import qualified Data.Map as Map

-- | An inhomogeneous, type safe dictionary.
type Dict  = Data.Map Unique Item
-- Values are stored in closures that write to a temporary IORef
-- This way, we can "circumvent" the type system.
data Item  = IO ()

-- Key for the dictionary
data Key a   = Key Unique (Item' a)
-- Keeps track of the temporary IORef for reading and writing
type Item' a = IORef (Maybe a)

-- | The empty dictionary.
empty :: Dict
empty = Map.empty

-- | Create a new key for use with a dictionary.
newKey   :: IO (Key a)
newKey dict = do
    k   <- newUnique
    ref <- newIORef Nothing
    return $ Key k ref

-- | Lookup the value of a key in the dictionary.
lookup :: Key a -> Dict -> IO (Maybe a)
lookup (Key k ref) dict = case Map.lookup k dict of
    Nothing   -> return Nothing
    Just item -> do
        item                    -- write into IORef
        mx <- readIORef ref     -- read the value
        writeIORef ref Nothing  -- clear IORef
        return mx

-- | Insert a value for a given key. Overwrites any previous value.
insert :: Key a -> a -> Dict -> IO Dict
insert (Key k ref) x dict = return $
    Map.insert k (writeIORef ref $ Just x) dict

-- | Delete a key from the dictionary.
delete :: Key a -> Dict -> IO Dict
delete (Key k ref) dict = return $ Map.delete k dict

