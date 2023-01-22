{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
-- * Synopsis
-- | Merge model and implementation into a single type. Not pretty.
module Reactive.Banana.Test.High.Plumbing where

import Control.Applicative
import Control.Monad (liftM, ap)
import Control.Monad.Fix

import qualified Reactive.Banana.Model as X
import qualified Reactive.Banana as Y

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
never                               = E X.never Y.never
filterJust (E x y)                  = E (X.filterJust x) (Y.filterJust y)
mergeWith f g h (E x1 y1) (E x2 y2) = E (X.mergeWith f g h x1 x2) (Y.mergeWith f g h y1 y2)
mapE f (E x y)                      = E (fmap f x) (fmap f y)
applyE ~(B x1 y1) (E x2 y2)         = E (X.apply x1 x2) (y1 Y.<@> y2)

instance Functor Event where fmap = mapE

pureB a                         = B (pure a) (pure a)
applyB (B x1 y1) (B x2 y2)      = B (x1 <*> x2) (y1 <*> y2)
mapB f (B x y)                  = B (fmap f x) (fmap f y)

instance Functor     Behavior where fmap = mapB
instance Applicative Behavior where pure = pureB; (<*>) = applyB

instance Functor Moment where fmap = liftM
instance Applicative Moment where
    pure a = M (pure a) (pure a)
    (<*>) = ap
instance Monad Moment where
    ~(M x y) >>= g = M (x >>= fstM . g) (y >>= sndM . g)
instance MonadFix Moment where
    mfix f = M (mfix fx) (mfix fy)
        where
        fx a = let M x _ = f a in x
        fy a = let M _ y = f a in y


accumE   a ~(E x y) = M
    (ex <$> X.accumE a x)
    (ey <$> Y.accumE a y)
stepperB a ~(E x y) = M
    (bx <$> X.stepper a x)
    (by <$> Y.stepper a y)
stepper            = stepperB

valueB ~(B x y) = M (X.valueB x) (Y.valueB y)

observeE :: Event (Moment a) -> Event a
observeE (E x y) = E (X.observeE $ fmap fstM x) (Y.observeE $ fmap sndM y)

switchE :: Event a -> Event (Event a) -> Moment (Event a)
switchE (E x0 y0) (E x y) = M
    (fmap ex $ X.switchE x0 $ fstE <$> x)
    (fmap ey $ Y.switchE y0 $ sndE <$> y)

switchB :: Behavior a -> Event (Behavior a) -> Moment (Behavior a)
switchB (B x y) (E xe ye) = M
    (fmap bx $ X.switchB x $ fmap fstB xe)
    (fmap by $ Y.switchB y $ fmap sndB ye)

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
