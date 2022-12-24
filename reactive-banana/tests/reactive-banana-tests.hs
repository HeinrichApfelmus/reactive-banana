{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Main where

import Test.Tasty
    ( defaultMain, testGroup )

import qualified Reactive.Banana.Test.Low.Graph as Low.Graph
import qualified Reactive.Banana.Test.Low.GraphGC as Low.GraphGC
import qualified Reactive.Banana.Test.High.Combinators as High.Combinators

main = defaultMain $ testGroup "reactive-banana"
    [ High.Combinators.tests
    , Low.Graph.tests
    , Low.GraphGC.tests
    ]
