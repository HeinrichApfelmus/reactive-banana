{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
-- * Synopsis
-- | Merge model and implementation into a single type. Not pretty.

module Reactive.Banana.Test.Plumbing where

import Control.Applicative
import Control.Monad (liftM)

import qualified Reactive.Banana.Model as X
import qualified Reactive.Banana.Internal.PullGraph as Y
import qualified Reactive.Banana.Internal.InputOutput as Y

{-----------------------------------------------------------------------------
    Types as pairs
------------------------------------------------------------------------------}

data Event    a = E (X.Event    a) (Y.Event    a)
data Behavior a = B (X.Behavior a) (Y.Behavior a)
data Moment   a = M (X.Moment   a) (Y.Moment   a)

-- pair extractions
fstM (M x _) = x
sndM (M _ y) = y

fstE (E x _) = x
sndE (E _ y) = y

-- partial embedding functions
mx x = M x undefined
my y = M undefined y

ex x = E x undefined
ey y = E undefined y

-- interpretation
interpretModel :: (Event a -> Moment (Event b)) -> [Maybe a] -> [Maybe b]
interpretModel f = fstE . ($ 0) . fstM . f . ex

interpretPullGraph :: (Event a -> Event b) -> [Maybe a] -> IO [Maybe b]
interpretPullGraph f xs = do
    i <- Y.newInputChannel
    let automaton = Y.compileToAutomaton (sndE . f . ey $ Y.inputE i)
    Y.unfoldAutomaton automaton i xs

{-----------------------------------------------------------------------------
    Primitive combinators
------------------------------------------------------------------------------}
never                           = E X.never Y.never
filterJust (E x y)              = E (X.filterJust x) (Y.filterJust y)
unionWith f (E x1 y1) (E x2 y2) = E (X.unionWith f x1 x2) (Y.unionWith f y1 y2)
mapE f (E x y)                  = E (X.mapE f x) (Y.mapE f y)
applyE ~(B x1 y1) (E x2 y2)     = E (X.applyE x1 x2) (Y.applyE y1 y2)
accumE a (E x y)                = E (X.accumE a x) (Y.accumE a y)

instance Functor Event where fmap = mapE

stepperB a (E x y)              = B (X.stepperB a x) (Y.stepperB a y)
pureB a                         = B (X.pureB a) (Y.pureB a)
applyB (B x1 y1) (B x2 y2)      = B (X.applyB x1 x2) (Y.applyB y1 y2)
mapB f (B x y)                  = B (X.mapB f x) (Y.mapB f y)

instance Functor     Behavior where fmap = mapB
instance Applicative Behavior where pure = pureB; (<*>) = applyB

instance Functor Moment where fmap = liftM
instance Monad Moment where
    return a = M (return a) (return a)
    (M x y) >>= g = M (x >>= fstM . g) (y >>= sndM . g)

trimE :: Event a -> Moment (Moment (Event a))
trimE (E x y) = M
    (fmap (fmap ex . mx) $ X.trimE x)
    (fmap (fmap ey . my) $ Y.trimE y)

observeE = undefined
switchE  = undefined

{-----------------------------------------------------------------------------
    Derived combinators
------------------------------------------------------------------------------}
accumB acc = stepperB acc . accumE acc
whenE b = filterJust . applyE ((\b e -> if b then Just e else Nothing) <$> b)

