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
    Build, BuildIO, liftBuild, compile, interpret,
    module Control.Monad.IO.Class,
    
    -- * IO
    newPulse, addHandler, readLatch,
    
    -- * Pulse
    Pulse,
    neverP, mapP, unsafeMapIOP, filterJustP, unionWithP,
    
    -- * Latch
    Latch,
    pureL, mapL, applyL, accumL, applyP,
  ) where


import Control.Monad.IO.Class
import Reactive.Banana.Prim.Combinators
import Reactive.Banana.Prim.Compile
import Reactive.Banana.Prim.IO
import Reactive.Banana.Prim.Plumbing (neverP, liftBuild)
import Reactive.Banana.Prim.Types

newPulse :: Build (Pulse a, a -> Step)
newPulse = newInputPulse

