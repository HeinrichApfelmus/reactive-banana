{-----------------------------------------------------------------------------
    Reactive Banana

    A small library for functional reactive programming.
------------------------------------------------------------------------------}

module Reactive.Banana (
    module Reactive.Banana.Incremental,
    module Reactive.Banana.Model,
    module Reactive.Banana.Implementation,

    Event, Behavior, Discrete,
    ) where

import Reactive.Banana.Incremental hiding (DiscreteD)
import qualified Reactive.Banana.Incremental as Polymorph
import Reactive.Banana.Model hiding (interpret, Event, Behavior)
import qualified Reactive.Banana.Model as Polymorph
import Reactive.Banana.Implementation

type Event    = Polymorph.Event PushIO
type Behavior = Polymorph.Behavior PushIO
type Discrete = Polymorph.DiscreteD PushIO
