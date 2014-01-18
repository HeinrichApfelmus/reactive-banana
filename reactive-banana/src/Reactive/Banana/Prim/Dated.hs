{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.Dated (
    -- | A cache with timestamps.
    
    Time, ancient, beginning, next,
    Vault, Key, empty, newKey, lookup,
    Dated, runDated, update, cache,
    
    ) where

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
    Cache data type
------------------------------------------------------------------------------}
newKey :: IO (Key a)
newKey = Strict.newKey

empty :: Vault
empty = Strict.empty

type Vault = Strict.Vault
type Key a = Strict.Key (a, Time)

{-----------------------------------------------------------------------------
    Cached computations
------------------------------------------------------------------------------}
type Dated = RWS () Time Vault

runDated :: Dated a -> Vault -> (a, Vault)
runDated m s1 = let (a,s2,_) = runRWS m () s1 in (a,s2)

lookup :: Key a -> Dated (Maybe a)
lookup key = do
    m <- Strict.lookup key <$> get
    case m of
        Nothing    -> return Nothing
        Just (a,t) -> tell t >> return (Just a)

update :: Key a -> Time -> a -> Vault -> Vault
update key t a = Strict.insert key (a,t)

cache :: Key a -> Dated a -> Dated a
cache key m = do
    (aNew, timeNew) <- listen m
    let refresh = do
            modify $ Strict.insert key (aNew, timeNew)
            return aNew
    
    c <- get
    case Strict.lookup key c of
        Just (aOld,timeOld)
            | timeOld >= timeNew -> do          -- cache is more recent 
                                    tell timeOld
                                    return aOld
            | otherwise          -> refresh     -- cache is too old
        Nothing                  -> refresh
