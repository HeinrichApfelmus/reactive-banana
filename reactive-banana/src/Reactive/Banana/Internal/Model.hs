{-----------------------------------------------------------------------------
    Reactive-Banana
------------------------------------------------------------------------------}
{-# LANGUAGE GADTs #-}
module Reactive.Banana.Internal.Model (
    -- * Synopsis
    -- | Model implementation of the abstract syntax tree.

    interpretModel
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.State
import qualified Data.Vault as Vault

import qualified Reactive.Banana.Internal.AST as AST
import Reactive.Banana.Internal.InputOutput

{-----------------------------------------------------------------------------
    Combinators
------------------------------------------------------------------------------}
-- due to observable sharing, the types have to be important from AST
type Event a    = AST.EventModel a     -- = [Maybe a]
type Behavior a = AST.BehaviorModel a  -- = StepperB a (Event a)

never :: Event a
never = repeat Nothing

filterE :: (a -> Bool) -> Event a -> Event a
filterE p = map (>>= \x -> if p x then Just x else Nothing)

unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith f = zipWith g
    where
    g (Just x) (Just y) = Just $ f x y
    g (Just x) Nothing  = Just x
    g Nothing  (Just y) = Just y
    g Nothing  Nothing  = Nothing

applyE :: Behavior (a -> b) -> Event a -> Event b
applyE _                   []     = []
applyE (AST.StepperB f fe) (x:xs) = fmap f x : applyE (step f fe) xs
    where
    step a (Nothing:b) = stepper a b
    step _ (Just a :b) = stepper a b

accumE :: a -> Event (a -> a) -> Event a
accumE x []           = []
accumE x (Nothing:fs) = Nothing : accumE x fs
accumE x (Just f :fs) = let y = f x in y `seq` (Just y:accumE y fs) 

stepper :: a -> [Maybe a] -> Behavior a
stepper = AST.StepperB

{-----------------------------------------------------------------------------
    Interpretation, pays attention to observable sharing
------------------------------------------------------------------------------}
-- state monad for evaluation
type Eval = State Vault.Vault

-- | Interpret an event graph with the model implementation.
-- Mainly useful for testing library internals.
interpretModel
    :: (AST.Event AST.Expr a -> AST.Event AST.Expr b)
    -> Event a -> IO (Event b)
interpretModel f input = do
    i0 <- newInputChannel
    
    let
        evalE :: AST.EventD AST.Expr a -> Eval (Event a)
        evalE (AST.Never)             = return $ never
        evalE (AST.UnionWith f e1 e2) = unionWith f <$> goE e1 <*> goE e2
        evalE (AST.FilterE p e)       = filterE p   <$> goE e
        evalE (AST.ApplyE b e )       = applyE      <$> goB b  <*> goE e
        evalE (AST.AccumE x e )       = accumE x    <$> goE e
        evalE (AST.InputPure i)       =
            return $ maybe err id $ fromValue i (toValue i0 input)
            where err = error "Reactive.Banana.PushIO.interpretModel: internal error: Input"
        evalE _                       =
            error "Reactive.Banana.PushIO.interpretModel: internal error: E"

        evalB :: AST.BehaviorD AST.Expr a -> Eval (Behavior a)
        evalB (AST.Stepper x e) = stepper x <$> goE e
        evalB _                 =
            error "Reactive.Banana.PushIO.interpretModel: internal error: B"

        goE :: AST.Event AST.Expr a -> Eval (Event a)
        goE (AST.Pair node e) = do
            values <- get
            case Vault.lookup (AST.keyModelE node) values of
                Nothing -> mfix $ \v -> do
                    modify $ Vault.insert (AST.keyModelE node) v
                    evalE e
                Just v  -> return v

        goB :: AST.Behavior AST.Expr a -> Eval (Behavior a)
        goB (AST.Pair node b) = do
            values <- get
            case Vault.lookup (AST.keyModelB node) values of
                Nothing -> mfix $ \v -> do
                    modify $ Vault.insert (AST.keyModelB node) v
                    evalB b
                Just v  -> return v
    
    return $
        zipWith const
            (evalState (goE $ f $ AST.inputPure i0) Vault.empty)
            input

