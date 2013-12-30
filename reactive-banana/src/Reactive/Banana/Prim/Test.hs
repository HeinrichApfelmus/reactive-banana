{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo #-}
module Reactive.Banana.Prim.Test where

import Reactive.Banana.Prim

{-----------------------------------------------------------------------------
    Some tests of the pulse/latch primitives
------------------------------------------------------------------------------}
test :: Pulse Int -> BuildIO (Pulse Int)
test p1 = liftBuild $ do
    p2     <- mapP (+) p1
    (l1,_) <- accumL 0 p2
    let l2 =  mapL const l1
    p3     <- applyP l2 p1
    return p3

test_recursion1 :: Pulse () -> BuildIO (Pulse Int)
test_recursion1 p1 = liftBuild $ mdo
    p2      <- applyP l2 p1
    p3      <- mapP (const (+1)) p2
    ~(l1,_) <- accumL (0::Int) p3
    let l2  =  mapL const l1
    return p2
