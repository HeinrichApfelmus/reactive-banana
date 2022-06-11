{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}

module Reactive.Banana (
    -- * Synopsis
    -- | Reactive-banana is a library for functional reactive programming (FRP).
    -- To use it, import this module:
    --
    -- > import Reactive.Banana

    -- * Overview
    -- $intro

    -- * Exports
    module Reactive.Banana.Combinators,
    compile,
    ) where

import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

{-$intro

The module "Reactive.Banana.Combinators" collects the key types
and concepts of FRP. You will spend most of your time with this module.

The module "Reactive.Banana.Model" is /not/ used in practice.
It contains an easy-to-understand model re-implementation of the FRP API.
This is useful for learning FRP and for internal testing purposes.

The module "Reactive.Banana.Frameworks" allows you to connect
the FRP types and combinators to the outside world (IO).
If you are /using/ an existing binding like reactive-banana-wx,
then you probably won't need this module very often.
On the other hand, if you are /writing/ a binding to an external
library, then you will definitely need this.

The module hierarchy at "Reactive.Banana.Prim"
implements the efficient low-level FRP engine that powers the rest of the library.
This is only useful if you want to implement your own FRP library.

-}
