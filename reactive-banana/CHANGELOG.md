Changelog for the `reactive-banana** package
-------------------------------------------

**unreleased**

* Add `merge` and `mergeWith` combinators. [#163][], [#220][]
* Make internal SCC pragmas compatible with the GHC 9.0 parser. [#208][]

  [#163]: https://github.com/HeinrichApfelmus/reactive-banana/pull/163
  [#208]: https://github.com/HeinrichApfelmus/reactive-banana/pull/208
  [#220]: https://github.com/HeinrichApfelmus/reactive-banana/pull/219

**version 1.2.1.0**

* Add `Num`, `Floating`, `Fractional`, and `IsString` instances for `Behavior`. [#34][]
* Support `containers-0.6`. [#191][]

  [#34]: https://github.com/HeinrichApfelmus/reactive-banana/pull/34
  [#191]: https://github.com/HeinrichApfelmus/reactive-banana/pull/191

**version 1.2.0.0**

* Make `MonadFix` superclass of `MonadMoment`. [#128][]
* Add `Semigroup` and `Monoid` instances for `Event`. [#104][]
* Semigroup compatibility with GHC 8.4.1 [#168][]
* Increased upper-bound on `pqueue`.

  [#128]: https://github.com/HeinrichApfelmus/reactive-banana/pull/128
  [#104]: https://github.com/HeinrichApfelmus/reactive-banana/issues/104
  [#168]: https://github.com/HeinrichApfelmus/reactive-banana/pull/168

**version 1.1.0.1**

* Adapt library to work with GHC-8.0.1.

**version 1.1.0.0**

* Fix bug: Types of `switchB` and `switchE` need to be in the `Moment` monad.
* Clean up and simplify model implementation in the `Reactive.Banana.Model` module.
* Update type signatures of the `interpret*` functions to make it easier to try FRP functions in the REPL.
* Remove `showNetwork` function.

**version 1.0.0.1**

* Improve documentation.
    * Add prose section on recursion.
    * Improve explanation for the `changes` function.
* Bump `transfomers` dependency.
* Remove defunct `UseExtensions` flag from cabal file.

**version 1.0.0.0**

The API has been redesigned significantly in this version!

* Remove phantom type parameter `t` from `Event`, `Behavior` and `Moment` types.
    * Change accumulation functions (`accumB`, `accumE`, `stepper`) to have a monadic result type.
    * Merge module `Reactive.Banana.Switch` into module `Reactive.Banana.Combinators`.
    * Simplify types of the switching functions (`switchE`, `switchB`, `observeB`, `execute`).
    * Remove functions `trimE` and `trimB`.
    * Remove types `AnyMoment` and `Identity`.
* Remove `Frameworks` class constraint, use `MomentIO` type instead.
    * Add class `MonadMoment` for both polymorphism over the `Moment` and `MomentIO` types.
* Change type `Event` to only allow a single event per moment in time.
    * Remove function `union`. Use `unionWith` instead.
    * Change function `unions` to only merge events of type `Event (a -> a)`.
* Remove module `Reactive.Banana.Experimental.Calm`.
* Change the model implementation in the module `Reactive.Banana.Model` to the new API as well.

Other changes:

* Add `mapEventIO` utility function to build an Event that contains the result of an IO computation.
* Add `newBehavior` utility function to build a Behavior that can be updated with a `Handler`.
* Add illustrations to the API documentation.

**version 0.9.0.0**

* Implement garbage collection for dynamically switched events.
* Fix issue [#79][] where recursive declarations would sometimes result in dropped events.
* Limit value recursion in the `Moment` monad slightly.
* Change `initial` and `valueB` to behave subtly different when it comes to value recursion in the `Moment` monad.
* Add `Functor`, `Applicative` and `Monad` instances for the `FrameworksMoment` type.
* Depend on the [pqueue][] package instead of the [psqueues][] package again, as the former has been updated to work with the current version of GHC.

  [#79]: https://github.com/HeinrichApfelmus/reactive-banana/issues/79

**version 0.8.1.2**

* Depend on the [psqueues][] package instead of the [pqueue][] package for the priority queue.

  [psqueues]: https://hackage.haskell.org/package/psqueues
  [pqueue]: http://hackage.haskell.org/package/pqueue

**version 0.8.1.1**

* Links to the Haskell wiki now point to the `http://wiki.haskell.org` subdomain.

**version 0.8.1.0**

* Module `Reactive.Banana.Switch` now adheres to the "Functor Applicative Monad Proposal" proposal][amp-proposal].

  [amp-proposal]: https://wiki.haskell.org/Functor-Applicative-Monad_Proposal

**version 0.8.0.4**

* Just a re-upload. The previous archive was broken.

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
* Change type signatures: The main types `Event`, `Behavior` and `NetworkDescription` now carry an additional phantom type.

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
