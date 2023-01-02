{-# LANGUAGE RecursiveDo #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.Mid.Test where

import Reactive.Banana.Prim.Mid

main :: IO ()
main = test_space1

{-----------------------------------------------------------------------------
    Functionality tests
------------------------------------------------------------------------------}
test_accumL1 :: Pulse Int -> BuildIO (Pulse Int)
test_accumL1 p1 = liftBuild $ do
    p2     <- mapP (+) p1
    (l1,_) <- accumL 0 p2
    let l2 =  mapL const l1
    applyP l2 p1

test_recursion1 :: Pulse () -> BuildIO (Pulse Int)
test_recursion1 p1 = liftBuild $ mdo
    p2      <- applyP l2 p1
    p3      <- mapP (const (+1)) p2
    ~(l1,_) <- accumL (0::Int) p3
    let l2  =  mapL const l1
    return p2

-- test garbage collection

{-----------------------------------------------------------------------------
    Space leak tests
------------------------------------------------------------------------------}
test_space1 :: IO ()
test_space1 = runSpaceProfile test_accumL1 [1::Int .. 2 * 10 ^ (4 :: Int)]

test_space2 :: IO ()
test_space2 = runSpaceProfile test_recursion1 $ () <$ [1::Int .. 2 * 10 ^ (4 :: Int)]
