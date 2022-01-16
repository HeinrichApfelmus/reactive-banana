{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim.Mid (
    -- * Synopsis
    -- | This is an internal module, useful if you want to
    -- implemented your own FRP library.
    -- If you just want to use FRP in your project,
    -- have a look at "Reactive.Banana" instead.

    -- * Evaluation
    Step, Network, emptyNetwork,

    -- * Build FRP networks
    Build, liftIOLater, BuildIO, liftBuild, buildLater, buildLaterReadNow, compile,
    module Control.Monad.IO.Class,

    -- * Caching
    module Reactive.Banana.Prim.High.Cached,

    -- * Testing
    interpret, mapAccumM, mapAccumM_, runSpaceProfile,

    -- * IO
    newInput, addHandler, readLatch,

    -- * Pulse
    Pulse,
    neverP, alwaysP, mapP, Future, tagFuture, unsafeMapIOP, filterJustP, mergeWithP,

    -- * Latch
    Latch,
    pureL, mapL, applyL, accumL, applyP,

    -- * Dynamic event switching
    switchL, executeP, switchP

    -- * Notes
    -- $recursion
  ) where


import Control.Monad.IO.Class
import Reactive.Banana.Prim.Low.Compile
import Reactive.Banana.Prim.Low.IO
import Reactive.Banana.Prim.Low.Plumbing
    ( neverP, alwaysP, liftBuild, buildLater, buildLaterReadNow, liftIOLater )
import Reactive.Banana.Prim.Low.Types
import Reactive.Banana.Prim.Mid.Combinators
import Reactive.Banana.Prim.High.Cached

{-----------------------------------------------------------------------------
    Notes
------------------------------------------------------------------------------}
-- Note [Recursion]
{- $recursion

The 'Build' monad is an instance of 'MonadFix' and supports value recursion.
However, it is built on top of the 'IO' monad, so the recursion is
somewhat limited.

The main rule for value recursion in the 'IO' monad is that the action
to be performed must be known in advance. For instance, the following snippet
will not work, because 'putStrLn' cannot complete its action without
inspecting @x@, which is not defined until later.

>   mdo
>       putStrLn x
>       let x = "Hello recursion"

On the other hand, whenever the sequence of 'IO' actions can be known
before inspecting any later arguments, the recursion works.
For instance the snippet

>   mdo
>       p1 <- mapP p2
>       p2 <- neverP
>       return p1

works because 'mapP' does not inspect its argument. In other words,
a call @p1 <- mapP undefined@ would perform the same sequence of 'IO' actions.
(Internally, it essentially calls 'newIORef'.)

With this issue in mind, almost all operations that build 'Latch'
and 'Pulse' values have been carefully implemented to not inspect
their arguments.
In conjunction with the 'Cached' mechanism for observable sharing,
this allows us to build combinators that can be used recursively.
One notable exception is the 'readLatch' function, which must
inspect its argument in order to be able to read its value.

-}

-- Note [LatchStrictness]
{-

Any value that is stored in the graph over a longer
period of time must be stored in WHNF.

This implies that the values in a latch must be forced to WHNF
when storing them. That doesn't have to be immediately
since we are tying a knot, but it definitely has to be done
before  evaluateGraph  is done.

It also implies that reading a value from a latch must
be forced to WHNF before storing it again, so that we don't
carry around the old collection of latch values.
This is particularly relevant for `applyL`.

Conversely, since latches are the only way to store values over time,
this is enough to guarantee that there are no space leaks in this regard.

-}
