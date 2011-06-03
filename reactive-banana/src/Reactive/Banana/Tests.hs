{-----------------------------------------------------------------------------
    Reactive Banana
    
    Test cases and examples
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types, NoMonomorphismRestriction #-}
module Reactive.Banana.Tests where

import Prelude hiding (filter)
import Control.Monad (when)

import Reactive.Banana.Model as Model
import Reactive.Banana.Implementation as Impl

import Test.QuickCheck

{-----------------------------------------------------------------------------
    Testing
------------------------------------------------------------------------------}
matchesModel :: (Typeable a, Show b, Eq b) =>
    (forall f. FRP f => Event f a -> Event f b) -> [a] -> IO Bool
matchesModel f = \xs -> do
        let bs1 = Model.interpret f xs
        bs2 <- Impl.interpret f xs
        when (bs1 /= bs2) $ print bs1 >> print bs2
        return $ bs1 == bs2

{-
testSuite = do
    -- TODO: algebraic laws
    -- larger examples
    quickCheck $ matchesModel decrease
    -}

{-----------------------------------------------------------------------------
    Examples
------------------------------------------------------------------------------}
test f = Impl.interpret f [1..8::Int]

add1      = fmap (+1)
filtering = filter (>= 3) . fmap (subtract 1)
counter e = apply (pure const <*> bcounter) e
    where bcounter = accumB 0 $ fmap (\_ -> (+1)) e
double e  = union e e
sharing e = union e1 e1
    where e1 = filter (< 3) e

type Dummy = Int

-- counter that can be decreased as long as it's >= 0
decrease :: FRP f => Event f Dummy -> Event f Int
decrease edec = apply (const <$> bcounter) ecandecrease
    where
    bcounter     = accumB 4 $ (subtract 1) <$ ecandecrease
    ecandecrease = whenE ((>0) <$> bcounter) edec

-- test accumE vs accumE
accumBvsE :: FRP f => Event f Dummy -> Event f Int
accumBvsE input = e1 `union` e2
    where
    e  = input `union` input
    e1 = accumE 0 ((+1) <$ e)
    e2 = let b = accumB 0 ((+1) <$ e) in apply (const <$> b) e
    
