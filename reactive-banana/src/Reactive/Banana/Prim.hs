{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim (
    -- * Synopsis
    -- | Primitive type and combinators for building FRP networks.
    -- More practical FRP combinators can be built on top of these.
    
    -- * Evaluation
    Step, Network, emptyNetwork,
    
    -- * Build network
    Build, liftIOLater, BuildIO, liftBuild, compile, interpret,
    module Control.Monad.IO.Class,
    
    -- * IO
    newInput, addHandler, addHandlerLatch, readLatch,
    
    -- * Pulse
    Pulse,
    neverP, mapP, unsafeMapIOP, filterJustP, unionWithP,
    
    -- * Latch
    Latch,
    pureL, mapL, applyL, accumL, applyP,
    
    -- * Dynamic event switching
    switchL, executeP, switchP
  ) where


import Control.Monad.IO.Class
import Reactive.Banana.Prim.Combinators
import Reactive.Banana.Prim.Compile
import Reactive.Banana.Prim.IO
import Reactive.Banana.Prim.Plumbing (neverP, liftBuild, liftIOLater)
import Reactive.Banana.Prim.Types
