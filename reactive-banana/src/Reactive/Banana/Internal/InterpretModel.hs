{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Internal.InterpretModel (
    -- * Synopsis
    -- | Interpret abstract syntax with model implementation.
    
    interpretModel
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.State
import qualified Data.Vault as Vault

import qualified Reactive.Banana.Internal.AST as AST
import Reactive.Banana.Internal.InputOutput
import Reactive.Banana.Model as Model hiding (interpretModel)

{-----------------------------------------------------------------------------
    Interpret AST with model,
    pay attention to observable sharing
------------------------------------------------------------------------------}
-- state monad for evaluation
type Eval = State Vault.Vault

-- | Interpret an event graph with the model implementation.
-- Mainly useful for testing library internals.
interpretModel
    :: (AST.Event AST.Expr a -> AST.Event AST.Expr b)
    -> Model.Event a -> IO (Model.Event b)
interpretModel f input = do
    i0 <- newInputChannel
    
    let
        evalE :: AST.EventD AST.Expr a -> Eval (Model.Event a)
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

        evalB :: AST.BehaviorD AST.Expr a -> Eval (Model.Behavior a)
        evalB (AST.Stepper x e) = stepperB x <$> goE e
        evalB _                 =
            error "Reactive.Banana.PushIO.interpretModel: internal error: B"

        goE :: AST.Event AST.Expr a -> Eval (Model.Event a)
        goE (AST.Pair node e) = do
            values <- get
            case Vault.lookup (AST.keyModelE node) values of
                Nothing -> mfix $ \v -> do
                    modify $ Vault.insert (AST.keyModelE node) v
                    evalE e
                Just v  -> return v

        goB :: AST.Behavior AST.Expr a -> Eval (Model.Behavior a)
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
