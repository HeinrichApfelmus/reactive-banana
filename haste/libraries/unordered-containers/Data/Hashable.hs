-- dummy implementation for the haste compiler
module Data.Hashable where

import Data.Bits
import Data.Unique.Really

class Hashable a where
    hash :: a -> Int
    hash = hashWithSalt defaultSalt
    
    hashWithSalt :: Int -> a -> Int
    hashWithSalt salt x = salt `combine` hash x

defaultSalt :: Int
defaultSalt = 2166136261

instance Hashable Unique where
    hashWithSalt s u = s `combine` hashUnique u

instance Hashable Char where hash = fromEnum

-- | Combine two given hash values.  'combine' has zero as a left
-- identity.
combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2
