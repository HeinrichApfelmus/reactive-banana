{-----------------------------------------------------------------------------
    Reactive Banana
    
    Test cases and examples
------------------------------------------------------------------------------}
{-# LANGUAGE Rank2Types #-}
module Reactive.Banana.Tests where

import Reactive.Banana.Model as Model
import Reactive.Banana.Implementation as Impl hiding (Event, Behavior)

import Test.QuickCheck

{-----------------------------------------------------------------------------
    Testing
------------------------------------------------------------------------------}
matchesModel :: (Typeable a, Eq b) =>
    (forall f. FRP f => Event f a -> Event f b) -> [a] -> Bool
matchesModel f = \xs -> Model.run f xs == Impl.run f xs

testSuite = do
    -- TODO: algebraic laws
    -- larger examples
    quickCheck $ matchesModel decrease
    

{-----------------------------------------------------------------------------
    Examples
------------------------------------------------------------------------------}
-- counter that can be decreased as long as it's >= 0
decrease :: FRP f => Event f () -> Event f Int
decrease edec = apply ((\c _ -> c) <$> bcounter) ecandecrease
    where
    bcounter     = accumB 10 $ (subtract 1) <$ ecandecrease
    ecandecrease = whenE ((>0) <$> bcounter) edec

