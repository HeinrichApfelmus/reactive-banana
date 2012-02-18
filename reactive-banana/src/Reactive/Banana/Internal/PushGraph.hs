{-----------------------------------------------------------------------------
    Reactive-Banana
    
    Push-driven, incremental computations
------------------------------------------------------------------------------}
{-# LANGUAGE GADTs, TypeFamilies, RankNTypes, TypeOperators,
             TypeSynonymInstances #-}

module Reactive.Banana.Internal.PushGraph (
    -- * Synopsis
    -- | Push-driven implementation

    compileToAutomaton
    ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Category
import Prelude hiding ((.),id)

import Data.Label
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Monoid (Dual, Endo)
import qualified Data.Set as Set
import qualified Data.Vault as Vault
import Data.Unique

import Reactive.Banana.Internal.AST
import Reactive.Banana.Internal.Automaton
import Reactive.Banana.Internal.TotalOrder as TotalOrder


{-----------------------------------------------------------------------------
    Existential quantification
------------------------------------------------------------------------------}
-- | Formula, existentially quantified over the result type
data SomeFormula t where
    Exists :: Formula t a -> SomeFormula t
type SomeNode = SomeFormula Nodes

-- instances to store  SomeNode  in efficient maps
instance Eq SomeNode where
    (Exists x) == (Exists y) = keyOrder x == keyOrder y
instance Ord SomeNode where
    compare (Exists x) (Exists y) = compare (keyOrder x) (keyOrder y)

{-----------------------------------------------------------------------------
    Representation of the dependency graph
    and associated lenses
------------------------------------------------------------------------------}
-- Dependency graph
data Graph
    = Graph
    { grFormulas  :: Formulas                -- calculation formulas
    , grChildren  :: Map SomeNode [SomeNode] -- reverse dependencies
    , grEvalOrder :: EvalOrder               -- evaluation order
    , grValues    :: Vault.Vault             -- calculated values
    }
type Formulas  = Vault.Vault          -- mapping from nodes to formulas
type EvalOrder = TotalOrder SomeNode  -- evaluation order

-- make lens from a Vault.Key
vaultLens :: Vault.Key a -> (Vault.Vault :-> Maybe a)
vaultLens key = lens (Vault.lookup key) (adjust)
    where
    adjust Nothing  = Vault.delete key
    adjust (Just x) = Vault.insert key x 

-- formula used to calculate the value at a node
formula :: Node a -> (Graph :-> Maybe (FormulaD Nodes a))
formula node = vaultLens (keyFormula node) . formulaLens
    where formulaLens = lens grFormulas (\x g -> g { grFormulas = x})

-- value calculated for the node
value :: Node a -> (Graph :-> Maybe a)
value node = vaultLens (keyValue node) . valuesLens
    where valuesLens = lens grValues (\x g -> g { grValues = x})

-- all nodes that directly depend on this one via the formula
children :: Node a -> Graph -> [SomeNode]
children node g = Map.findWithDefault [] (Exists node) (grChildren g)

{-----------------------------------------------------------------------------
    Operations specific to the DSL
------------------------------------------------------------------------------}
-- | Extract the dependencies of a node from its formula.
-- (boilerplate)
dependencies :: FormulaD t a -> [SomeFormula t]
dependencies = caseFormula goE goB
    where
    goE :: EventD t a -> [SomeFormula t]
    goE (Never)             = []
    goE (UnionWith f e1 e2) = [Exists $ E e1, Exists $ E e2]
    goE (FilterE _ e)       = [Exists $ E e]
    goE (ApplyE  b e)       = [Exists $ B b, Exists $ E e]
    goE (AccumE  _ e)       = [Exists $ E e]
    goE _                   = []

    goB :: BehaviorD t a -> [SomeFormula t]
    goB (Stepper x e)       = [Exists $ E e]
    goB _                   = []

-- | Nodes whose *current* values are needed to calculate
-- the current value of the given node.
-- (boilerplate)
dependenciesEval :: FormulaD t a -> [SomeFormula t]
dependenciesEval (B (Stepper x e)) = []
dependenciesEval formula           = dependencies formula 

-- | Replace expressions by nodes.
-- (boilerplate)
toFormulaNodes :: FormulaD Expr a -> FormulaD Nodes a
toFormulaNodes = caseFormula (E . goE) (B . goB)
    where
    goE :: EventD Expr a -> EventD Nodes a
    goE (Never)             = Never
    goE (UnionWith f e1 e2) = UnionWith f (fst e1) (fst e2)
    goE (FilterE p e)       = FilterE p (fst e)
    goE (ApplyE  b e)       = ApplyE (fst b) (fst e)
    goE (AccumE  x e)       = AccumE x (fst e)
    goE (InputE x)          = InputE x

    goB :: BehaviorD Expr a -> BehaviorD Nodes a
    goB (Stepper x e)       = Stepper x (fst e)
    goB (InputB x)          = InputB x



-- | Evaluate the current value of a given event expression.
calculateE
    :: (forall e. Node e -> Maybe e)
    -> (forall b. Node b -> b)
    -> Maybe a -> EventD Nodes a -> Maybe a
calculateE valueE valueB valuePrev = goE
    where
    goE :: EventD Nodes a -> Maybe a
    goE (Never)             = Nothing
    goE (UnionWith f e1 e2) = case (valueE e1, valueE e2) of
        (Just e1, Just e2) -> Just $ f e1 e2
        (Just e1, Nothing) -> Just e1
        (Nothing, Just e2) -> Just e2
        (Nothing, Nothing) -> Nothing
    goE (FilterE p e)       = valueE e >>=
        \e -> if p e then Just e else Nothing
    goE (ApplyE  b e)       = (valueB b  $) <$> valueE e
    goE (AccumE  x e)       = ($ valuePrev) <$> valueE e
    goE (InputE x)          = valuePrev

calculateB
    :: (forall e. Node e -> Maybe e)
    -> (forall b. Node b -> b)
    -> a -> BehaviorD Nodes a -> a
calculateB valueE _ valueSelf = goB
    where
    goB :: BehaviorD Nodes a -> a
    goB (Stepper x e)     = maybe valueSelf id (valueE e)
    goB (InputB x)        = valueSelf


{-----------------------------------------------------------------------------
    Building the dependency graph
------------------------------------------------------------------------------}
-- build full graph from an expression
buildGraph :: SomeFormula Expr -> Graph
buildGraph expr
        = Graph
        { grFormulas  = grFormulas
        , grChildren  = buildChildren  root grFormulas
        , grEvalOrder = buildEvalOrder root grFormulas
        , grValues    = Vault.empty
        }
    where
    grFormulas = buildFormulas expr
    root       = case expr of Exists e -> Exists $ fst e

-- | Build a graph of formulas from an expression
buildFormulas :: SomeFormula Expr -> Formulas
buildFormulas expr =
    unfoldGraphDFSWith leftComposition f expr $ Vault.empty
    where
    f (Exists (node, formula)) =
        ( \formulas -> Vault.insert (keyFormula node) formula' formulas
        , dependencies formula )
        where
        formula' = toFormulaNodes formula

-- | Build reverse dependencies, starting from one node.
buildChildren :: SomeNode -> Formulas -> Map.Map SomeNode [SomeNode]
buildChildren root formulas =
    unfoldGraphDFSWith leftComposition f root $ Map.empty
    where
    f (Exists node) = (addChild deps, deps)
        where
        addChild    = concatenate . map (Map.update $ Just . (child:))
        child       = Exists node :: SomeNode
        formula'    = fromJust $ Vault.lookup (keyFormula node) formulas
        deps        = dependencies formula'

concatenate :: [a -> a] -> (a -> a)
concatenate = foldr (.) id

-- start at some node and update the evaluation order of
-- the node and all of its dependencies
updateEvalOrder :: SomeNode -> Formulas -> EvalOrder -> EvalOrder
updateEvalOrder = error "TODO"

-- Build evaluation order from scratch
-- = topological sort
buildEvalOrder :: SomeNode -> Formulas -> EvalOrder
buildEvalOrder root formulas = 
    TotalOrder.fromAscList $ unfoldGraphDFSWith leftComposition f root []
    where
    f (Exists node) = ((Exists node:), dependenciesEval formula')
        where
        formula' = fromJust $ Vault.lookup (keyFormula node) formulas

{-----------------------------------------------------------------------------
    Generic Graph Traversals
------------------------------------------------------------------------------}
-- Dictionary for defining monoids on the fly
data MonoidDict t = MonoidDict t (t -> t -> t)

-- Unfold a graph,
-- i.e. unfold a given state  s  into a concatenation of monoid values
-- while ignoring duplicate states.
-- Depth-first order.
unfoldGraphDFSWith :: Ord s => MonoidDict t -> (s -> (t,[s])) -> s -> t
unfoldGraphDFSWith (MonoidDict empty append ) f = go Set.empty
    where
    go :: Set.Set s -> [s] -> t
    go seen []      = empty
    go seen (x:xs)
        | x `Set.member` seen = go seen xs
        | otherwise           = t `append` go (Set.insert x seen) (ys++xs)
        where
        (t,ys) = f x


-- monoid of endomorphisms, leftmost function is applied *last*
leftComposition :: MonoidDict (a -> a)
leftComposition = MonoidDict id (flip (.)) 

-- Unfold a graph.
-- Specialize monoid to function composition from left to right
-- unfoldGraphDualEndo :: Ord s => (s -> (a -> a,[s])) -> s -> (a -> a)
-- unfoldGraphDualEndo f = unDual . unEndo . unfoldGraphDFS f'
--    where f' = first (Dual . Endo) . f

{-----------------------------------------------------------------------------
    Reduction and Evaluation
------------------------------------------------------------------------------}
-- type Queue = [SomeNode]

-- | Perform evaluation steps until all values have percolated through the graph.
evaluate :: Queue q => q SomeNode -> Graph -> Graph
evaluate q g = snd $ until (isEmpty . fst) evaluationStep (q,g)

-- | Perform a single evaluation step.
evaluationStep :: Queue q => (q SomeNode, Graph) -> (q SomeNode, Graph)
evaluationStep (q,g) = case minView q of
        Just (Exists node, q) -> go node q
        Nothing               -> error "reductionStep: internal error"
    where
    go node q = undefined {- (q',g')
        where
        g' = set (value node) $
            calculate (\n -> get (value n) g) (get (formula node) g)
        q' = insertList (children node g) q -}

{-----------------------------------------------------------------------------
    Convert into an automaton
------------------------------------------------------------------------------}
compileToAutomaton :: Event Expr b -> Automaton b
compileToAutomaton = undefined

-- TODO
-- TODO - extra parameter for the root node of the graph data type
toAutomaton :: Graph -> Automaton b
toAutomaton = undefined




