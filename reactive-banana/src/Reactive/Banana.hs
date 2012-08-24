{-----------------------------------------------------------------------------
    Reactive Banana

    A small library for functional reactive programming.
------------------------------------------------------------------------------}

module Reactive.Banana (
    module Reactive.Banana.Combinators,
    module Reactive.Banana.Switch,
    compile,
    ) where

import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Banana.Internal.Types2
import Reactive.Banana.Switch