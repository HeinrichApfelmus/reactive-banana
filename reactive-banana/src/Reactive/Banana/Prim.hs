{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Reactive.Banana.Prim (
    -- * Synopsis
    -- | This is an internal module, useful if you want to
    -- implemented your own FRP library.
    -- If you just want to use FRP in your project,
    -- have a look at "Reactive.Banana" instead.
    
    -- * Evaluation
    Step, Network, emptyNetwork,
    
    -- * Build FRP networks
    Build, liftIOLater, BuildIO, BuildT, liftBuild, compile,
    module Control.Monad.IO.Class,
    
    -- * Testing
    interpret, mapAccumM, mapAccumM_, runSpaceProfile,
    
    -- * IO
    newInput, addHandler, readLatch,
    
    -- * Pulse
    Pulse,
    neverP, alwaysP, mapP, Future, tagFuture, unsafeMapIOP, filterJustP, unionWithP,
    
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
import Reactive.Banana.Prim.Plumbing (neverP, alwaysP, liftBuild, liftIOLater)
import Reactive.Banana.Prim.Types
