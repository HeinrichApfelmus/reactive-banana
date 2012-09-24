-- now idea why, but apparently, haste wants a Data.Unique from somewhere
module Data.Unique (
   -- * Unique objects
   Unique,              -- instance (Eq, Ord)
   newUnique,           -- :: IO Unique
   hashUnique           -- :: Unique -> Int
 ) where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

newtype Unique = Unique Integer deriving (Eq,Ord)

uniqSource :: IORef Integer
uniqSource = unsafePerformIO (newIORef 0)
{-# NOINLINE uniqSource #-}

-- | Creates a new object of type 'Unique'.  The value returned will
-- not compare equal to any other value of type 'Unique' returned by
-- previous calls to 'newUnique'.  There is no limit on the number of
-- times 'newUnique' may be called.
newUnique :: IO Unique
newUnique = do
  val <- readIORef uniqSource
  let next = val+1
  writeIORef uniqSource $! next
  return (Unique next)

-- SDM (18/3/2010): changed from MVar to STM.  This fixes
--  1. there was no async exception protection
--  2. there was a space leak (now new value is strict)
--  3. using atomicModifyIORef would be slightly quicker, but can
--     suffer from adverse scheduling issues (see #3838)
--  4. also, the STM version is faster.

-- | Hashes a 'Unique' into an 'Int'.  Two 'Unique's may hash to the
-- same value, although in practice this is unlikely.  The 'Int'
-- returned makes a good hash key.
hashUnique :: Unique -> Int
hashUnique (Unique u) = fromInteger (u `mod` (toInteger (maxBound :: Int) + 1))
