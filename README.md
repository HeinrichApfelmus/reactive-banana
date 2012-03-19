## What is reactive-banana?

<div style="float:left;"><img src="https://github.com/HeinrichApfelmus/reactive-banana/raw/develop/banana.png" /></div>

Reactive-banana is a library for [Functional Reactive Programming (FRP)][frp], written in Haskell.

Support the project with a small donation: [![Flattr this](http://api.flattr.com/button/flattr-badge-large.png)](http://flattr.com/thing/384682/reactive-banana)

See the **[project homepage][homepage]** for **documentation**, **examples** and so on.

  [homepage]: http://haskell.org/haskellwiki/Reactive-banana
  [frp]: http://haskell.org/haskellwiki/Functional_Reactive_Programming

I'm writing documentation on a call-by-need basis. So, feel free to write me an email if you want something explained that you don't understand, and I'll explain it to you!

## How is the source code structured?

The project contains two directories:

* `reactive-banana` - the library itself
* `reactive-banana-wx` - bindings to the [wxHaskell][] GUI library, includes many examples

At the moment, the reactive-banana library actually contains *two* FRP implementations:

1. [Reactive.Banana.Model][model] - A model implementation for testing and understanding the semantics. You are encouraged to look at the source code, it is intended to be easy to understand.
2. [Reactive.Banana.PushIO][pushio] - The efficient push-driven implementation used for production code. Contains hard to understand trade secrets. ;-)

  [model]: https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/src/Reactive/Banana/Model.hs
  [pushio]: https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/src/Reactive/Banana/PushIO.hs
  [wxhaskell]: http://haskell.org/haskellwiki/WxHaskell

## Compilation

Prerequisites: the wxHaskell package

    cabal install wx

Note that you need to have a development version of the wxWidgets libraries installed before building wx. If you run into `ExitFailure 1` exceptions, please follow the [wxHaskell Quick Start](http://www.haskell.org/haskellwiki/WxHaskell/Building) instructions and try again.

To build the reactive-banana libraries, type

    cd reactive-banana && cabal configure && cabal build && cd ..
    cd reactive-banana-wx && cabal configure && cabal build && cd ..


## Contribute

Send me your examples, bindings, problems, suggestions, etc!

With contributions from

* Abu Alam
* Kevin Cantu
* Gregory Crosswhite
* Elliott Hird
* John Lato
* Gideon Sireling
* Henning Thielemann
* Daniel Werner
