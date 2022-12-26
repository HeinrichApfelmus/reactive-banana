{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UnboxedTuples #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.Low.Ref
    ( -- * Mutable references with 'Unique'
      Ref
    , getUnique
    , new
    , equal
    , read
    , put
    , modify'

      -- * Garbage collection and weak pointers to 'Ref'
    , addFinalizer
    , getWeakRef

    , WeakRef
    , mkWeak
    , deRefWeak
    , deRefWeaks
    , finalize
    ) where

import Prelude hiding ( read )

import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( void )
import Control.Monad.IO.Class
    ( MonadIO (liftIO) )
import Data.Hashable
    ( Hashable (..) )
import Data.IORef
    ( IORef, newIORef, readIORef, writeIORef )
import Data.Maybe
    ( catMaybes )
import Data.Unique.Really
    ( Unique, newUnique )

import qualified System.Mem.Weak as Weak
import qualified GHC.Base as GHC
import qualified GHC.IORef as GHC
import qualified GHC.STRef as GHC
import qualified GHC.Weak as GHC

{-----------------------------------------------------------------------------
    Ref
------------------------------------------------------------------------------}
-- | A mutable reference which has a 'Unique' associated with it.
data Ref a = Ref
    !Unique         -- Unique associated to the 'Ref'
    !(IORef a)      -- 'IORef' that stores the value of type 'a'
    !(WeakRef a)    -- For convenience, a weak pointer to itself

instance NFData (Ref a) where rnf (Ref _ _ _) = ()

instance Eq (Ref a) where (==) = equal

instance Hashable (Ref a) where hashWithSalt s (Ref u _ _) = hashWithSalt s u

getUnique :: Ref a -> Unique
getUnique (Ref u _ _) = u

getWeakRef :: Ref a -> WeakRef a
getWeakRef (Ref _ _ w) = w

equal :: Ref a -> Ref b -> Bool
equal (Ref ua _ _) (Ref ub _ _) = ua == ub

new :: MonadIO m => a -> m (Ref a)
new a = liftIO $ mdo
    ra     <- newIORef a
    result <- Ref <$> newUnique <*> pure ra <*> pure wa
    wa     <- mkWeakIORef ra result Nothing
    pure result

read :: MonadIO m => Ref a -> m a
read ~(Ref _ r _) = liftIO $ readIORef r

put :: MonadIO m => Ref a -> a -> m ()
put ~(Ref _ r _) = liftIO . writeIORef r

-- | Strictly modify a 'Ref'.
modify' :: MonadIO m => Ref a -> (a -> a) -> m ()
modify' ~(Ref _ r _) f = liftIO $
    readIORef r >>= \x -> writeIORef r $! f x

{-----------------------------------------------------------------------------
    Weak pointers
------------------------------------------------------------------------------}
-- | Add a finalizer to a 'Ref'.
--
-- See 'System.Mem.Weak.addFinalizer'.
addFinalizer :: Ref v -> IO () -> IO ()
addFinalizer (Ref _ r _) = void . mkWeakIORef r () . Just

-- | Weak pointer to a 'Ref'.
type WeakRef v = Weak.Weak (Ref v)

-- | Create a weak pointer that associates a key with a value.
--
-- See 'System.Mem.Weak.mkWeak'.
mkWeak
    :: Ref k -- ^ key
    -> v -- ^ value
    -> Maybe (IO ()) -- ^ finalizer
    -> IO (Weak.Weak v)
mkWeak (Ref _ r _) = mkWeakIORef r

-- | Finalize a 'WeakRef'.
--
-- See 'System.Mem.Weak.finalize'.
finalize :: WeakRef v -> IO ()
finalize = Weak.finalize

-- | Dereference a 'WeakRef'.
--
-- See 'System.Mem.Weak.deRefWeak'.
deRefWeak :: Weak.Weak v -> IO (Maybe v)
deRefWeak = Weak.deRefWeak

-- | Dereference a list of weak pointers while discarding dead ones.
deRefWeaks :: [Weak.Weak v] -> IO [v]
deRefWeaks ws = catMaybes <$> mapM Weak.deRefWeak ws

{-----------------------------------------------------------------------------
    Helpers
------------------------------------------------------------------------------}
-- | Create a weak pointer to an 'IORef'.
--
-- Unpacking the constructors (e.g. 'GHC.IORef' etc.) is necessary
-- because the constructors may be unpacked while the 'IORef' is used
-- — so, the value contained therein is alive, but the constructors are not.
mkWeakIORef
    :: IORef k -- ^ key
    -> v       -- ^ value
    -> Maybe (IO ()) -- ^ finalizer
    -> IO (Weak.Weak v)
mkWeakIORef (GHC.IORef (GHC.STRef r#)) v (Just (GHC.IO finalizer)) =
    GHC.IO $ \s -> case GHC.mkWeak# r# v finalizer s of
        (# s1, w #) -> (# s1, GHC.Weak w #)
mkWeakIORef (GHC.IORef (GHC.STRef r#)) v Nothing =
    GHC.IO $ \s -> case GHC.mkWeakNoFinalizer# r# v s of
        (# s1, w #) -> (# s1, GHC.Weak w #)
