{-----------------------------------------------------------------------------
    Reactive-Banana
    
    Push-driven, incremental computations
------------------------------------------------------------------------------}
{-# LANGUAGE GADTs, TypeFamilies, RankNTypes, TypeOperators,
             TypeSynonymInstances, FlexibleInstances,
             ScopedTypeVariables #-}

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
    Representation of the dependency graph
    and associated lenses
------------------------------------------------------------------------------}
-- Dependency graph
data Graph
    = Graph
    { grFormulas  :: Formulas                -- formulas for calculation
    , grChildren  :: Map SomeNode [SomeNode] -- reverse dependencies
    , grEvalOrder :: EvalOrder               -- evaluation order
    }
type Formulas  = Vault.Vault          -- mapping from nodes to formulas
type EvalOrder = TotalOrder SomeNode  -- evaluation order
type Values    = Vault.Vault          -- current event values

-- | Turn a 'Vault.Key' into a lens for the vault
vaultLens :: Vault.Key a -> (Vault.Vault :-> Maybe a)
vaultLens key = lens (Vault.lookup key) (adjust)
    where
    adjust Nothing  = Vault.delete key
    adjust (Just x) = Vault.insert key x 

-- | Formula used to calculate the value at a node.
formula :: Node a -> (Graph :-> Maybe (FormulaD Nodes a))
formula node = vaultLens (keyFormula node) . formulaLens
    where formulaLens = lens grFormulas (\x g -> g { grFormulas = x})

-- | All nodes that directly depend on this one via the formula.
children :: Node a -> (Graph :-> [SomeNode])
children node = lens (Map.findWithDefault [] (Exists node) . grChildren)
    (error "TODO: can't set children yet")

-- | Current value for a node.
value :: Node a -> (Values :-> Maybe a)
value node = vaultLens (keyValue node)

{-----------------------------------------------------------------------------
    Operations specific to the DSL
------------------------------------------------------------------------------}
-- | Extract the dependencies of a node from its formula.
-- (boilerplate)
dependencies :: ToFormula t => FormulaD t a -> [SomeFormula t]
dependencies = caseFormula goE goB
    where
    goE :: ToFormula t => EventD t a -> [SomeFormula t]
    goE (Never)             = []
    goE (UnionWith f e1 e2) = [ee e1,ee e2]
    goE (FilterE _ e1)      = [ee e1]
    goE (ApplyE  b1 e1)     = [bb b1, ee e1]
    goE (AccumE  _ e1)      = [ee e1]
    goE _                   = []

    goB :: ToFormula t => BehaviorD t a -> [SomeFormula t]
    goB (Stepper x e1)      = [ee e1]
    goB _                   = []

-- | Nodes whose *current* values are needed to calculate
-- the current value of the given node.
-- (boilerplate)
dependenciesEval :: ToFormula t => FormulaD t a -> [SomeFormula t]
dependenciesEval (E (ApplyE b e)) = [ee e]
dependenciesEval formula          = dependencies formula 

-- | Replace expressions by nodes.
-- (boilerplate)
toFormulaNodes :: FormulaD Expr a -> FormulaD Nodes a
toFormulaNodes = caseFormula (E . goE) (B . goB)
    where
    node :: Pair Node f a -> Node a
    node = fstPair
    
    goE :: forall a. EventD Expr a -> EventD Nodes a
    goE (Never)             = Never
    goE (UnionWith f e1 e2) = UnionWith f (node e1) (node e2)
    goE (FilterE p e)       = FilterE p (node e)
    goE (ApplyE  b e)       = ApplyE (node b) (node e)
    goE (AccumE  x e)       = AccumE x (node e)
    goE (InputE x)          = InputE x

    goB :: BehaviorD Expr a -> BehaviorD Nodes a
    goB (Stepper x e)       = Stepper x (node e)
    goB (InputB x)          = InputB x


-- Evaluation

-- | Evaluate the current value of a given event expression.
calculateE
    :: forall a.
       (forall e. Node e -> Maybe e)  -- retrieve current event values
    -> (forall b. Node b -> b)        -- retrieve old behavior values
    -> Node a                         -- node ID
    -> EventD Nodes a                 -- formula to evaluate
    -> ( Maybe a                      -- current event value
       , Graph -> Graph)              -- (maybe) change formulas in the graph 
calculateE valueE valueB node =
    maybe (Nothing,id) (\(x,f) -> (Just x, f)) . goE
    where
    goE :: EventD Nodes a -> Maybe (a, Graph -> Graph)
    goE (Never)             = nothing
    goE (UnionWith f e1 e2) = case (valueE e1, valueE e2) of
        (Just e1, Just e2) -> just $ f e1 e2
        (Just e1, Nothing) -> just e1
        (Nothing, Just e2) -> just e2
        (Nothing, Nothing) -> nothing
    goE (FilterE p e)       = valueE e >>=
        \e -> if p e then just e else nothing
    goE (ApplyE  b e)       = (just . (valueB b $)) =<< valueE e
    goE (AccumE  x e)       = case valueE e of
        Nothing -> just x
        Just f  -> let y = f x in
            Just (y, set (formula node) . Just $ E (AccumE y e))
    goE (InputE x)          = error "TODO" -- just x

just x  = Just (x, id)
nothing = Nothing

-- | Evalute the new value of a given behavior expression
calculateB
    :: forall a.
       (forall e. Node e -> Maybe e) -- retrieve current event values
    -> Node a                        -- node ID
    -> BehaviorD Nodes a             -- formula to evaluate
    -> Graph -> Graph                -- (maybe) change formulas in the graph
calculateB valueE node = maybe id id . goB
    where
    goB :: BehaviorD Nodes a -> Maybe (Graph -> Graph) 
    goB (Stepper x e)     =
        (\y -> set (formula node) $ Just $ B (Stepper y e)) <$> valueE e
    goB (InputB x)        = error "TODO"


{-----------------------------------------------------------------------------
    Building the dependency graph
------------------------------------------------------------------------------}
-- | Build full graph from an expression.
buildGraph :: SomeFormula Expr -> Graph
buildGraph expr
        = Graph
        { grFormulas  = grFormulas
        , grChildren  = buildChildren  root grFormulas
        , grEvalOrder = buildEvalOrder root grFormulas
        }
    where
    grFormulas = buildFormulas expr
    root       = case expr of Exists e -> Exists $ fstPair e

-- | Build a graph of formulas from an expression
buildFormulas :: SomeFormula Expr -> Formulas
buildFormulas expr =
    unfoldGraphDFSWith leftComposition f expr $ Vault.empty
    where
    f (Exists (Pair node formula)) =
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

-- | Start at some node and update the evaluation order of
-- the node and all of its dependencies.
updateEvalOrder :: SomeNode -> Formulas -> EvalOrder -> EvalOrder
updateEvalOrder = error "TODO"

-- | Build evaluation order from scratch
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
-- | Dictionary for defining monoids on the fly.
data MonoidDict t = MonoidDict t (t -> t -> t)

-- | Unfold a graph,
-- i.e. unfold a given state  s  into a concatenation of monoid values
-- while ignoring duplicate states.
-- Depth-first order.
unfoldGraphDFSWith
    :: forall s t. Ord s => MonoidDict t -> (s -> (t,[s])) -> s -> t
unfoldGraphDFSWith (MonoidDict empty append) f s = go Set.empty [s]
    where
    go :: Set.Set s -> [s] -> t
    go seen []      = empty
    go seen (x:xs)
        | x `Set.member` seen = go seen xs
        | otherwise           = t `append` go (Set.insert x seen) (ys++xs)
        where
        (t,ys) = f x


-- | Monoid of endomorphisms, leftmost function is applied *last*.
leftComposition :: MonoidDict (a -> a)
leftComposition = MonoidDict id (flip (.))

{-----------------------------------------------------------------------------
    Reduction and Evaluation
------------------------------------------------------------------------------}
-- type Queue = [SomeNode]

-- | Perform evaluation steps until all values have percolated through the graph.
evaluate :: Queue q => q SomeNode -> Graph -> Graph
evaluate startQueue startGraph = endGraph
    where
    startValues = Vault.empty
    
    (_,_,endGraph) =
        until (isEmpty . queue) step (startQueue,startValues,startGraph)
    
    queue (q,_,_) = q
    step  (q,v,g) = (q',v',f g)
        where (q',v',f) = evaluationStep startGraph q v

-- | Perform a single evaluation step.
evaluationStep
    :: forall q. Queue q
    => Graph                        -- initial graph shape
    -> q SomeNode                   -- queue of nodes to process
    -> Values                       -- current event values
    -> (q SomeNode, Values, Graph -> Graph)
evaluationStep graph queue values = case minView queue of
        Just (Exists node, queue) -> go node queue
        Nothing                   -> error "evaluationStep: queue empty"
    where
    go :: forall a. Node a -> q SomeNode -> (q SomeNode, Values, Graph -> Graph)
    go node queue =
        let -- lookup functions
            valueE :: forall e. Node e -> Maybe e
            valueE node = get (value node) values
            valueB :: forall b. Node b -> b
            valueB node = case get (formula node) graph of
                Just (B (Stepper b _)) -> b
                _               -> error "evaluationStep: behavior not found"

        in -- evaluation
            case fromJust $ get (formula node) graph of
            B formulaB ->   -- evalute behavior
                (queue, values, calculateB valueE node formulaB)
            E formulaE ->   -- evaluate event
                let -- calculate current value
                    (maybeval, f) = calculateE valueE valueB node formulaE
                    -- set value if applicable
                    valuesF = case maybeval of
                        Just x  -> set (value node) (Just x)
                        Nothing -> id
                    -- evaluate children only if node doesn't return Nothing
                    queueF  = case maybeval of
                        Just _  -> insertList $ get (children node) graph
                        Nothing -> id
                in (queueF queue, valuesF values, f)

{-----------------------------------------------------------------------------
    Convert into an automaton
------------------------------------------------------------------------------}
compileToAutomaton :: Event Expr b -> Automaton b
compileToAutomaton = undefined

-- TODO
-- TODO - extra parameter for the root node of the graph data type
toAutomaton :: Graph -> Automaton b
toAutomaton = undefined




