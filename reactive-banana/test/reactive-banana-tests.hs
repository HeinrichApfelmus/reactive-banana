{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Main where

import Test.Tasty
    ( defaultMain, testGroup )

import qualified Reactive.Banana.Test.Low.Graph
import qualified Reactive.Banana.Test.Low.GraphGC
import qualified Reactive.Banana.Test.Mid.Space
import qualified Reactive.Banana.Test.High.Combinators
import qualified Reactive.Banana.Test.High.Space

main = defaultMain $ testGroup "reactive-banana"
    [ testGroup "Low-level"
        [ Reactive.Banana.Test.Low.Graph.tests
        , Reactive.Banana.Test.Low.GraphGC.tests
        ]
    , testGroup "Mid-level"
        [ Reactive.Banana.Test.Mid.Space.tests
        ]
    , testGroup "High-level"
        [ Reactive.Banana.Test.High.Combinators.tests
        , Reactive.Banana.Test.High.Space.tests
        ]
    ]
