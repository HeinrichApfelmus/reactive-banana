{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.Dated (
    -- | A cache with timestamps.
    
    -- * Time
    Time, ancient, beginning, next,
    -- * Cache
    Vault, Key, empty, newKey, findWithDefault,
    -- * Strictness
    Box(..),
    -- * Computations
    Dated, runDated, update', cache,
    
    ) where

import           Control.Applicative               hiding (empty)
import           Control.Monad.Trans.RWS
import           Data.Functor
import           Data.Monoid
import qualified Data.Vault.Strict       as Strict
import           Prelude                           hiding (lookup)

{-----------------------------------------------------------------------------
    Time monoid
------------------------------------------------------------------------------}
newtype Time = T Integer deriving (Eq, Ord, Show, Read)

ancient :: Time
ancient = T 0

beginning :: Time
beginning = T 1

next :: Time -> Time
next (T n) = T (n+1)

instance Monoid Time where
    mappend (T x) (T y) = T (max x y)
    mempty              = ancient

{-----------------------------------------------------------------------------
    Strictness
------------------------------------------------------------------------------}
-- | A strict box of potentially lazy value.
data Box a = Box { unBox :: a }

instance Functor Box where
    fmap f (Box x) = Box (f x)

instance Applicative Box where
    pure x = Box x
    (Box f) <*> (Box x) = Box (f x)

{-----------------------------------------------------------------------------
    Cache data type
------------------------------------------------------------------------------}
newKey :: IO (Key a)
newKey = Strict.newKey

empty :: Vault
empty = Strict.empty

type Vault = Strict.Vault
type Key a = Strict.Key (Timed a)

{-----------------------------------------------------------------------------
    Cached computations
------------------------------------------------------------------------------}
type Dated   = RWS () Time Vault
data Timed a = Timed !(Box a) !Time

runDated :: Dated a -> Vault -> (a, Vault)
runDated m s1 = let (a,s2,_) = runRWS m () s1 in (a,s2)

findWithDefault :: a -> Key a -> Dated (Box a)
findWithDefault a key = do
    ma <- Strict.lookup key <$> get
    case ma of
        Nothing          -> return (Box a)
        Just (Timed a t) -> tell t >> return a

-- | Update a value inside the cache.
-- The value will be evaluated to WHNF when the cache is evaluated to WHNF.
update' :: Key a -> a -> Time -> Vault -> Vault
update' key a t = Strict.insert key (Timed (a `seq` Box a) t)

cache :: Key a -> Dated (Box a) -> Dated (Box a)
-- cache key m = m
-- Observation: If  a  is a function type, then forcing
-- it will not necessarily remove all the function application things.
cache key m = do
    (aNew, timeNew) <- listen m
    let refresh = do
            modify $ Strict.insert key (Timed aNew timeNew)
            return aNew
    
    ma <- Strict.lookup key <$> get
    case ma of
        Just (Timed aOld timeOld)
            | timeOld >= timeNew -> do          -- cache is more recent 
                                    tell timeOld
                                    return aOld
            | otherwise          -> refresh     -- cache is too old
        Nothing                  -> refresh