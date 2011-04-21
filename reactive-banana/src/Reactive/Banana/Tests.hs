{-----------------------------------------------------------------------------
    Reactive Banana
    
    Test cases and examples
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types, NoMonomorphismRestriction #-}
module Reactive.Banana.Tests where

import Prelude hiding (filter)

import Reactive.Banana.Model as Model
import Reactive.Banana.Implementation as Impl

import Test.QuickCheck

{-----------------------------------------------------------------------------
    Testing
------------------------------------------------------------------------------}
matchesModel :: (Typeable a, Eq b) =>
    (forall f. FRP f => Event f a -> Event f b) -> [a] -> IO Bool
matchesModel f = \xs -> do
        let bs1 = Model.run f xs
        bs2 <- Impl.run f xs
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
test f = Impl.run f [1..5]

add1      = fmap (+1)
filtering = filter (>= 3) . fmap (subtract 1)
counter e = apply (pure const <*> bcounter) e
    where bcounter = accumB 0 $ fmap (\_ -> (+1)) e

-- counter that can be decreased as long as it's >= 0
decrease :: FRP f => Event f a -> Event f Int
decrease edec = apply (const <$> bcounter) ecandecrease
    where
    bcounter     = accumB 10 $ (subtract 1) <$ ecandecrease
    ecandecrease = whenE ((>0) <$> bcounter) edec

