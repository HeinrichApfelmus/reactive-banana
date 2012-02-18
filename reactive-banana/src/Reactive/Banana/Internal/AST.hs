{-----------------------------------------------------------------------------
    Reactive-Banana
------------------------------------------------------------------------------}
{-# LANGUAGE GADTs, TypeFamilies, TupleSections, EmptyDataDecls #-}

module Reactive.Banana.Internal.AST where
-- | Abstract syntax tree and assorted data types.

import Control.Applicative
import qualified Data.Vault as Vault
import qualified Data.Unique as Unique
import System.IO.Unsafe

import Reactive.Banana.Internal.Input

{-----------------------------------------------------------------------------
    Abstract syntax tree
------------------------------------------------------------------------------}
-- Type families allow us to support multiple tags in the AST
type family   Event t a
type family   Behavior t a

-- | Constructors for events.
data EventD t :: * -> * where
    Never     :: EventD t a
    UnionWith :: (a -> a -> a) -> Event t a -> Event t a -> EventD t a
    FilterE   :: (a -> Bool) -> Event t a -> EventD t a
    ApplyE    :: Behavior t (a -> b) -> Event t a -> EventD t b
    AccumE    :: a -> Event t (a -> a) -> EventD t a
    
    InputE    :: InputChannel a -> EventD t a -- represent external inputs

-- | Constructors for behaviors.
data BehaviorD t :: * -> * where
    Stepper :: a -> Event t a -> BehaviorD t a

    InputB :: InputChannel a -> BehaviorD t a -- represent external inputs 

{-----------------------------------------------------------------------------
    Observable sharing
    
    Each constructor is paired with a @Node@ value.
    The @Node@ serves as a unique identifier and stores various keys
    into various vaults.
------------------------------------------------------------------------------}
-- | Type index indicating expressions with observable sharing
data Expr
type instance Event    Expr a = (Node a, EventD Expr a)
type instance Behavior Expr a = (Node a, BehaviorD Expr a)

-- smart constructor that handles observable sharing
shareE :: EventD Expr a -> Event Expr a
shareE e = pair
    where
    {-# NOINLINE pair #-}
    -- mention argument to prevent let-floating
    pair = unsafePerformIO (fmap (,e) newNode)

shareB :: BehaviorD Expr a -> Behavior Expr a
shareB b = pair
    where
    {-# NOINLINE pair #-}
    pair = unsafePerformIO (fmap (,b) newNode)

{-----------------------------------------------------------------------------
    Smart constructors and class instances
------------------------------------------------------------------------------}
unE = id; unB = id

never             = shareE $ Never
unionWith f e1 e2 = shareE $ UnionWith f (unE e1) (unE e2)
filterE p e       = shareE $ FilterE p (unE e)
applyE b e        = shareE $ ApplyE (unB b) (unE e)
accumE acc e      = shareE $ AccumE acc (unE e)
inputE i          = shareE $ InputE i

stepperB acc e    = shareB $ Stepper acc (unE e)
inputB i          = shareB $ InputB i

{-----------------------------------------------------------------------------
    Reactive.Banana.Internal.PushGraph
    
    We need to define the 'Node' type here.
------------------------------------------------------------------------------}
-- | A 'Node' represents a unique identifier for an expression.
-- It actually contains keys for various 'Vault'.
data Node a
    = Node
    { keyValue   :: Vault.Key a
    , keyFormula :: Vault.Key (FormulaD Nodes a)
    , keyOrder   :: Unique.Unique }

newNode :: IO (Node a)
newNode = Node <$> Vault.newKey <*> Vault.newKey <*> Unique.newUnique


data Nodes
type instance Event    Nodes a = Node a
type instance Behavior Nodes a = Node a

-- | Formula that represents events and behaviors as one entity
data FormulaD t a where
    E :: EventD t a    -> FormulaD t a
    B :: BehaviorD t a -> FormulaD t a

caseFormula :: (EventD t a -> c) -> (BehaviorD t a -> c) -> FormulaD t a -> c
caseFormula e b (E x) = e x
caseFormula e b (B x) = b x

type family Formula t a
type instance Formula Expr  a = (Node a, FormulaD Expr a)
type instance Formula Nodes a = Node a

