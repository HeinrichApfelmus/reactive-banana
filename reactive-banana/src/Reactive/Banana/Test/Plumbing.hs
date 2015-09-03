{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
-- * Synopsis
-- | Merge model and implementation into a single type. Not pretty.

module Reactive.Banana.Test.Plumbing where

import Control.Applicative
import Control.Monad (liftM, ap)
import Control.Monad.Fix

import qualified Reactive.Banana.Model as X
import qualified Reactive.Banana.Internal.Combinators as Y

{-----------------------------------------------------------------------------
    Types as pairs
------------------------------------------------------------------------------}

data Event    a = E (X.Event    a) (Y.Event    a)
data Behavior a = B (X.Behavior a) (Y.Behavior a)
data Moment   a = M (X.Moment   a) (Y.Moment   a)

-- pair extractions
fstE (E x _) = x; sndE (E _ y) = y
fstB (B x _) = x; sndB (B _ y) = y
fstM (M x _) = x; sndM (M _ y) = y

-- partial embedding functions
ex x = E x undefined; ey y = E undefined y
bx x = B x undefined; by y = B undefined y
mx x = M x undefined; my y = M undefined y

-- interpretation
interpretModel :: (Event a -> Moment (Event b)) -> [Maybe a] -> [Maybe b]
interpretModel f = X.interpret (fmap fstE . fstM . f . ex)

interpretGraph :: (Event a -> Moment (Event b)) -> [Maybe a] -> IO [Maybe b]
interpretGraph f = Y.interpret (fmap sndE . sndM . f . ey)

{-----------------------------------------------------------------------------
    Primitive combinators
------------------------------------------------------------------------------}
never                           = E X.never Y.never
filterJust (E x y)              = E (X.filterJust x) (Y.filterJust y)
unionWith f (E x1 y1) (E x2 y2) = E (X.unionWith f x1 x2) (Y.unionWith f y1 y2)
mapE f (E x y)                  = E (X.mapE f x) (Y.mapE f y)
applyE ~(B x1 y1) (E x2 y2)     = E (X.applyE x1 x2) (Y.applyE y1 y2)

instance Functor Event where fmap = mapE

pureB a                         = B (X.pureB a) (Y.pureB a)
applyB (B x1 y1) (B x2 y2)      = B (X.applyB x1 x2) (Y.applyB y1 y2)
mapB f (B x y)                  = B (X.mapB f x) (Y.mapB f y)

instance Functor     Behavior where fmap = mapB
instance Applicative Behavior where pure = pureB; (<*>) = applyB

instance Functor Moment where fmap = liftM
instance Applicative Moment where
    pure  = return
    (<*>) = ap
instance Monad Moment where
    return a = M (return a) (return a)
    (M x y) >>= g = M (x >>= fstM . g) (y >>= sndM . g)
instance MonadFix Moment where
    mfix f = M (mfix fx) (mfix fy)
        where
        fx a = let M x _ = f a in x
        fy a = let M _ y = f a in y


accumE   a (E x y) = M
    (fmap ex $ X.accumE a x)
    (fmap ey $ Y.accumE a y)
stepperB a (E x y) = M
    (fmap bx $ X.stepperB a x)
    (fmap by $ Y.stepperB a y)
stepper            = stepperB

valueB ~(B x y) = M (X.valueB x) (Y.valueB y)

observeE :: Event (Moment a) -> Event a
observeE (E x y) = E (X.observeE $ X.mapE fstM x) (Y.observeE $ Y.mapE sndM y)

switchE :: Event (Event a) -> Event a
switchE (E x y) = E
    (X.switchE $ X.mapE (fstE) x)
    (Y.switchE $ Y.mapE (sndE) y)

switchB :: Behavior a -> Event (Behavior a) -> Behavior a
switchB (B x y) (E xe ye) = B
    (X.switchB x $ X.mapE (fstB) xe)
    (Y.switchB y $ Y.mapE (sndB) ye)

{-----------------------------------------------------------------------------
    Derived combinators
------------------------------------------------------------------------------}
accumB acc e1 = do
    e2 <- accumE acc e1
    stepperB acc e2
whenE b = filterJust . applyE ((\b e -> if b then Just e else Nothing) <$> b)

infixl 4 <@>, <@
b <@ e  = applyE (const <$> b) e
b <@> e = applyE b e
