{-----------------------------------------------------------------------------
    Reactive-Banana
    
    Push-driven, incremental computations
------------------------------------------------------------------------------}
{-# LANGUAGE GADTs, TypeFamilies, EmptyDataDecls #-}

module Reactive.Banana.Internal.PushDriven (
    -- * Synopsis
    -- | Push-driven implementation

    compileToAutomaton
    ) where

import Control.Applicative
import Control.Arrow (first)

import Data.Label
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid (Dual, Endo)
import qualified Data.Set as Set
import qualified Data.Vault as Vault
import Data.Unique

import Reactive.Banana.Internal.AST
import Reactive.Banana.Internal.Automaton
import Reactive.Banana.Internal.TotalOrder as TotalOrder

{-----------------------------------------------------------------------------
    Abstract syntax tree
------------------------------------------------------------------------------}
data Expr
data Graph

type family   Formula t a
type instance Formula Expr  a = (Node a, FormulaD Expr a)
type instance Formula Graph a = Node a

-- abstract syntax tree
data FormulaD t :: * -> * where
    Pure  :: a -> FormulaD t a
    Apply :: Formula t (a -> b) -> Formula t a -> FormulaD t b

{-----------------------------------------------------------------------------
    Abstract syntax tree with sharing
------------------------------------------------------------------------------}
-- main type
newtype Incremenal a = Incremental { unIncremental :: Formula Expr a }

instance Applicative Incremental where
    pure x  = incremental (Pure x)
    (Incremental f) <*> (Incremental x) = incremental (f `Apply` x)

-- sharing
incremental :: FormulaD Expr a -> Incremental a
incremental x = Incremental pair
    where
    {-# NOINLINE pair #-}
    -- mention argument to prevent let floating    
    pair = unsafePerformIO (fmap (,x) newNode)


{-----------------------------------------------------------------------------
    Existential quantification
------------------------------------------------------------------------------}
data SomeFormula t where
    Exists :: Formula t a -> SomeFormula t
type SomeNode = SomeFormula Graph

-- instances to store  SomeNode  in efficient maps
instance Eq SomeNode where
    (Exists x) == (Exists y) = keyOrder x == keyOrder y
instance Ord SomeNode where
    compare (Exists x) (Exists y) = compare (keyOrder x) (keyOrder y)

{-----------------------------------------------------------------------------
    Representation of the dependency graph
------------------------------------------------------------------------------}
-- unique identifier for a Node
data Node a
    = Node
    { keyValue   :: Vault.Key a
    , keyFormula :: Vault.Key (FormulaD Graph a)
    , keyOrder   :: Unique }

-- create a new node identifier
newNode :: IO (Node a)
newNode = Node <$> Vault.newKey <*> Vault.newKey <*> newUnique

-- Dependency graph
data Graph
    = Graph
    { grFormulas  :: FormulaGraph            -- calculation formulas
    , grChildren  :: Map SomeNode [SomeNode] -- reverse dependencies
    , grEvalOrder :: EvalOrder               -- evaluation order
    , grValues    :: Vault.Vault             -- calculated values
    }
type FormulaGraph = Vault.Vault          -- mapping from nodes to formulas
type EvalOrder    = TotalOrder SomeNode  -- evaluation order

-- make lens from a Vault.Key
vaultLens :: Vault.Key a -> (Vault.Vault :-> Maybe a)
vaultLens key = lens (Vault.lookup key) (adjust)
    where
    adjust Nothing  = Vault.delete key
    adjust (Just x) = Vault.insert key x 

-- formula used to calculate the value at a node
formula :: Node a -> (Graph :-> (Maybe FormulaD Graph a))
formula = vaultLens . keyFormulas

-- value calculated for the node
value :: Node a -> (Graph :-> Maybe a)
value = vaultLens . keyValues

-- all nodes that directly depend on this one via the formula
children :: Node a -> Graph -> [SomeNode]
children node g = Map.findWithDefault [] (Exists node) (grChildren g)

{-----------------------------------------------------------------------------
    Building the dependency graph
------------------------------------------------------------------------------}
-- extract the dependencies of a node from its formula
dependencies :: FormulaD t a -> [SomeFormula t]
dependencies (Pure _)    = []
dependencies (Apply a b) = [Exists a, Exists b]

-- nodes whose *current* values are needed to calculate
-- the current value of the given node
dependenciesEval :: FormulaD t a -> [SomeFormula t]
dependenciesEval (Pure _)    = []
dependenciesEval (Apply a b) = [Exists a, Exists b]
-- dependenciesEval (ApplyB b e) = [Exists e] -- the value of b is not required!

-- replace expressions by nodes
toGraphFormula :: FormulaD Expr a -> FormulaD Graph a
toGraphFormula (Pure x)    = Pure x
toGraphFormula (Apply a b) = Apply (fst a) (fst b)


-- build full graph from an expression
buildGraph :: Incremental a -> Graph
buildGraph (Incremental expr)
        = Graph
        { grFormulas  = grFormulas
        , grChildren  = grChildren
        , grEvalOrder = makeEvalOrder expr grFormulas
        , grValues    = Vault.empty
        }
    where
    grFormulas = buildFormulaGraph expr
    root       = unIncremental $ fst expr
    grChildren = buildChildren root grFormulaGraph

-- Build a formula graph from an expression
buildFormulaGraph :: Formula Expr a -> FormulaGraph
buildFormulaGraph expr =
    unfoldGraphDFSWith leftComposition f (Exists expr) $ Vault.empty
    where        
    f (Exists (node, formula)) =
        ( \graph -> Vault.insert (keyFormula node) formula' graph
        , dependencies formula )
        where
        formula' = toGraphFormula formula

-- Build reverse dependencies, starting from one node
buildChildren :: SomeNode -> FormulaGraph -> Map.Map SomeNode [SomeNode]
buildChildren root graph =
    unfoldGraphDFSWith leftComposition f root graph Map.empty
    where    
    f (Exists node) = (addChild deps, deps)
        where
        addChild = foldr id (.) . map $ Map.update (Just . (child:))
        child    = Exists node :: SomeNode
        formula' = fromJust $ Vault.lookup node graph
        deps     = dependencies formula'


-- start at some node and update the evaluation order of
-- the node and all of its dependencies
updateEvalOrder :: SomeNode -> FormulaGraph -> EvalOrder -> EvalOrder
updateEvalOrder = err "TODO"

-- Build evaluation order from scratch
-- = topological sort
buildEvalOrder :: SomeNode -> FormulaGraph -> EvalOrder
buildEvalOrder root graph = 
    TotalOrder.fromAscList $ unfoldGraphDFSWith leftComposition f root graph []
    where
    f (Exists node) = ((Exists node:), dependenciesEval formula')
        where
        formula' = fromJust $ Vault.lookup node graph

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
    go seen (x:xs)  =
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
type Queue = [SomeNode]

-- | Perform evaluation steps until all values have percolated through the graph.
evaluate :: Queue q => q SomeNode -> Graph -> Graph
evaluate q g = snd $ until (isEmpty . fst) evaluationStep (q,g)

-- | Perform a single evaluation step.
evaluationStep :: Queue q => (q SomeNode, Graph) -> (q SomeNode, Graph)
evaluationStep (q,g) = case minView q0 of
        Just (Exists node, q) -> go q
        Nothing               -> err "reductionStep: internal error"
    where
    go q = (q',g')
        where
        g' = set (value node) (calculate (get (formula node) g) g
        q' = insertList (children node g) q

    calculate :: Formula a -> a
    calculate (Pure x)      = x
    calculate (Apply n1 n2) = get (value n1) g $ get (value n2) g


{-----------------------------------------------------------------------------
    Convert into an automaton
------------------------------------------------------------------------------}
compileToAutomaton :: AST.Event Expr b -> Automaton b
compileToAutomaton = undefined

-- TODO
-- TODO - extra parameter for the root node of the graph data type
toAutomaton :: Graph -> Automaton b
toAutomaton = undefined




