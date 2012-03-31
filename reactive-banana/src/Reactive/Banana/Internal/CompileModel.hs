{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}

module Reactive.Banana.Internal.CompileModel (
    -- * Synopsis
    -- Compile model implementation to automaton.
    
    InputToEvent(..), Compile, compileWithGlobalInput,
    ) where

import Control.Applicative
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.Trans.Reader
import Data.IORef
import Data.Maybe
import System.IO.Unsafe

import Reactive.Banana.Internal.InputOutput
import Reactive.Banana.Model

{-----------------------------------------------------------------------------
    Compile model to an automaton
------------------------------------------------------------------------------}
data InputToEvent = InputToEvent (forall a. InputChannel a -> Event a)
type Compile a b  = (InputToEvent -> IO (Event a,b)) -> IO (Automaton a,b)

compileWithGlobalInput :: Compile a b
compileWithGlobalInput f = do
    -- reference that holds input values
    (ref    :: IORef [InputValue]) <- newIORef undefined
    -- An infinite list of all future input values. Very unsafe!
    (inputs :: Event [InputValue]) <- unsafeSequence (Just <$> readIORef ref)
    
    let
        inputToEvent = InputToEvent $
            \i -> filterJust $ mapE (fromInputValues i) inputs
    
        filterJust = map fromJust . filter isJust
        
        fromInputValues :: InputChannel a -> [InputValue] -> Maybe a
        fromInputValues i xs = listToMaybe [y | x <- xs, let Just y = fromValue i x]
    
        -- step of the automaton
        step values outputs = do
            writeIORef ref values           -- write new input value
            (o:outputs) <- evaluate outputs -- make sure that output is in WHNF
            return (o, outputs)             -- return result
    
    (outputs, b) <- f inputToEvent
    return $ (fromStateful step outputs, b)


unsafeSequence :: IO a -> IO [a]
unsafeSequence m = unsafeInterleaveIO $ do
    x  <- m
    xs <- unsafeSequence m
    return (x:xs)

