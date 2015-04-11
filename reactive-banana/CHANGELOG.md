Changelog for the `reactive-banana` package
-------------------------------------------

**version 0.8.1.1**

* Links to the Haskell wiki now point to the `http://wiki.haskell.org` subdomain.

**version 0.8.1.0**

* Module `Reactive.Banana.Switch` now adheres to the "Functor Applicative Monad Proposal" proposal][amp-proposal].

  [amp-proposal]: https://wiki.haskell.org/Functor-Applicative-Monad_Proposal

**version 0.8.0.4**

* Just a reupload. The previous archive was broken.

**version 0.8.0.3**

* Export the `Future` type.
* Restrict `containers` dependency to lower bound 0.5.

**version 0.8.0.2**

* Fix compilation issue with hiding `empty` from the module `Reactive.Banana.Prim.Order`.

**version 0.8.0.1**

* New examples `Counter.hs` and `Octave.hs`.
* Bump `transfomers` dependency.

**version 0.8.0.0**

* A new module `Reactive.Banana.Prim` exports primitive combinators that you can use to implement your own FRP library with a different API.
* The push-driven implementation in `Reactive.Banana.Prim` now has the performance characteristics of an actual push-driven implementation. Some work has gone into optimizing constant factors as well. However there is still no garbage collection for dynamically created events and behaviors.
* The `accumE` and `accumB` combinators evaluate their state to WHNF to avoid a space leak. (Fixes issue #52). On the other hand, `Behavior` values are evaluated on demanded, i.e. only when required by the apply combinator `<@>` or similar.
* Recursion between events and behaviors should now work as advertised. (Fixed issue #56).
* The deprecated `liftIONow` function has been removed.
* The type of the `changes` function now indicates that the new Behavior value is only available in the context of `reactimate`. A variant `reactimate'` makes this explicit.
* The module `Control.Event.Handler` now exports the `AddHandler` type, which is now a `newtype`. The module `Reactive.Banana.Frameworks.AddHandler` has been removed.

**version 0.7.1.0**

* Deprecate the `liftIONow` function in favor of `liftIO`.

**version 0.7.0.0**

* *Dynamic event switching*. Combinators are now available in the module `Reactive.Banana.Switch`.
* Rename `NetworkDescription` to `Moment`, add class constraint `Frameworks t`.
* No longer compiles with the JavaScript backend of the Utrecht Haskell compiler.
* Change the `changes` combinator to be less useful.

**version 0.6.0.0**

* Can now be compiled with the JavaScript backend of the Utrecht Haskell compiler.
* The push-driven implementations needs the `UseExtensions` flag to work. This flag is enabled by default.
* Minor module reorganization.

**version 0.5.0.0** -- [announcement](http://apfelmus.nfshost.com/blog/2012/03/25-frp-banana-0-5.html)

This update includes numerous changes, in particular a complete overhaul of the internal implementation. Here a partial list.

* Add `collect`, `spill` and `unionWith` combinators to deal with simultaneous events.
* Remove general `Monoid` instance for `Event` to simplify reasoning about simultaneous events.
* Add `initial` and `changes` combinators that allow you to observe updates to `Behavior`. Remove the `Reactive.Banana.Incremental` module.
* Rename most modules,
* Change type singaturs: The main types `Event`, `Behavior` and `NetworkDescription` now carry an additional phantom type.

**version 0.4.3.1**

* Model implementation of `accumE` now has the intended semantics.

**version 0.4.3.0**

* Change semantics: `IO` actions from inside `reactimate` may now interleave as dictated by your event-based framework (issue #15).
* Fix bug: compiling a network twice no longer fails due to lingering global state (issue #16).
* Change type: remove `Typeable` constraint from `interpret` and `interpretAsHandler`.
* Misc: Remove the `BlackBoard` application from the repository.

**version 0.4.2.0**

* Change type: remove `Typeable` constraint from `fromAddHandler`.
* Misc: the `Vault` data type gets its own package.
* Misc: `reactive-banana-wx` now compiles properly with cabal.
* Add some more examples to the `reactive-banana-wx` package.

**version 0.4.1.0**

* Add `<@>` operator for more convenience when using `apply`.
* Add support for value recursion to the `NetworkDescription` monad.
* Add many examples to `reactive-banana-wx`.

**version 0.4.0.0** -- [announcement](http://apfelmus.nfshost.com/blog/2011/07/07-frp-banana-0-4.html)

* Add function `fromPoll` to obtain behaviors from mutable data.
* Change name: `run` is now called `actuate`.
* Add derived data type `Discrete`.
* Add function `interpretAsHandler`.

**version 0.3.0.0** -- [announcement](http://apfelmus.nfshost.com/blog/2011/06/22-frp-banana-0-3.html)

* change: event networks are now first-class values, you can `pause` or `run` them.
* change type: `AddHandler` now expects a way to unregister event handlers.
* add example `RunPause.hs`

**version 0.2.0.0** -- [announcement](http://apfelmus.nfshost.com/blog/2011/06/22-frp-banana-0-2.html)

* change: now implements proper semantics as pioneered by Conal Elliott
* model implementation for semantics
* push-driven implementation for efficiency
* add example `SlotMachine.hs`

**version 0.1.0.0**

* initial release
