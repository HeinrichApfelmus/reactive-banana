{-----------------------------------------------------------------------------
    Reactive-Banana
------------------------------------------------------------------------------}
{-# LANGUAGE GADTs, TypeFamilies, EmptyDataDecls #-}

module Reactive.Banana.Internal.AST (
    -- * Synopsis
    -- Abstract syntax tree and assorted data types.
    
    -- * Abstract syntax tree
    EventD(..), BehaviorD(..),
    -- * Observable sharing
    Node(..), Expr, Event, Behavior,
    -- * Smart constructors
    never, unionWith, filterE, applyE, accumE, inputE, mapE,
    pureB, applyB, accumB,
    
    -- * Types associated to Node
    
    ) where

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
    Pure   :: a -> BehaviorD t a
    ApplyB :: Behavior t (a -> b) -> Behavior t a -> BehaviorD t b
    AccumB :: a -> Event t (a -> a) -> BehaviorD t a

    InputB :: InputChannel a -> BehaviorD t a -- represent external inputs 
    

{-----------------------------------------------------------------------------
    Observable sharing
    
    Each constructor is paired with a @Node@ value.
    The @Node@ serves as a unique identifier and stores various keys
    into various vaults.
------------------------------------------------------------------------------}
-- | Unique identifier for an expression.
-- Contains keys for various 'Vault'.
data Node a
    = Node
    { keyValue   :: Vault.Key a
    , keyFormula :: Vault.Key (FormulaD Graph a)
    , keyOrder   :: Unique.Unique }

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

newNode :: IO (Node a)
newNode = Node <$> Vault.newKey <*> Vault.newKey <*> Unique.newUnique

{-----------------------------------------------------------------------------
    Smart constructors and class instances
------------------------------------------------------------------------------}
unE = id; unB = id

never           = shareE $ Never
unionWith e1 e2 = shareE $ UnionWith (unE e1) (unE e2)
filterE p e     = shareE $ FilterE p (unE e)
applyE b e      = shareE $ ApplyE (unB b) (unE e)
accumE acc e    = shareE $ AccumE acc (unE e)
inputE i        = shareE $ InputE i

pureB x         = shareB $ Pure x
applyB b1 b2    = shareB $ applyB (unB b1) (unB b2)
accumB acc e    = shareB $ AccumB acc (unE e)
inputB i        = shareB $ InputB i

{-----------------------------------------------------------------------------
    Reactive.Banana.Internal.PushDriven
    
    Various data types mentioned in @Node@ for use
    with the push-driven implementation
------------------------------------------------------------------------------}






