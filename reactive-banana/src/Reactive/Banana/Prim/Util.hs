{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Reactive.Banana.Prim.Util where

import           Control.Monad          (foldM)
import           Data.IORef
import           Data.Maybe             (catMaybes)
import qualified GHC.Base        as GHC
import qualified GHC.IORef       as GHC
import qualified GHC.STRef       as GHC
import qualified GHC.Weak        as GHC
import           System.Mem.Weak


mkWeakIORefValueFinalizer :: IORef a -> value -> IO () -> IO (Weak value)
mkWeakIORefValueFinalizer r@(GHC.IORef (GHC.STRef r#)) v f = GHC.IO $ \s ->
  case GHC.mkWeak# r# v f s of (# s1, w #) -> (# s1, GHC.Weak w #)

mkWeakIORefValue :: IORef a -> value -> IO (Weak value)
mkWeakIORefValue a b = mkWeakIORefValueFinalizer a b (return ())

nop :: Monad m => m ()
nop = return ()

-- | Dereference a list of weak pointers while discarding dead ones.
deRefWeaks :: [Weak v] -> IO [v]
deRefWeaks = fmap catMaybes . mapM deRefWeak
