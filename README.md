[![Build Status](https://travis-ci.org/HeinrichApfelmus/reactive-banana.png)](https://travis-ci.org/HeinrichApfelmus/reactive-banana) 

## What is reactive-banana?

<div style="float:left;"><img src="https://github.com/HeinrichApfelmus/reactive-banana/raw/develop/banana.png" /></div>

Reactive-banana is a library for [Functional Reactive Programming (FRP)][frp], written in Haskell.

Support the project with a small donation: [![Flattr this](http://api.flattr.com/button/flattr-badge-large.png)](http://flattr.com/thing/384682/reactive-banana)

See the **[project homepage][homepage]** for **documentation**, **examples** and so on.

  [homepage]: http://haskell.org/haskellwiki/Reactive-banana
  [frp]: http://haskell.org/haskellwiki/Functional_Reactive_Programming

## Compilation from the repository

To build and install the core library from the source repository, simply type

    cd reactive-banana && cabal install && cd ..

However, to try out the GUI examples, you have to install one of the of the additional packages.

### GUI examples using wxHaskell

Prerequisites: the wxHaskell package

    cabal install wx

Note that you need to have a development version of the wxWidgets libraries installed before building wx. If you run into `ExitFailure 1` exceptions, please follow the [wxHaskell Quick Start](http://www.haskell.org/haskellwiki/WxHaskell/Building) instructions and try again.

To build the wx examples, type

    cd reactive-banana-wx
    cabal configure -fbuildExamples && cabal build
    cd ..

## How is the source code structured?

The project contains several directories:

* `reactive-banana` — the core library
* `reactive-banana-wx` — bindings to the [wxHaskell][] GUI library, includes many examples

  [wxhaskell]: http://haskell.org/haskellwiki/WxHaskell
  [threepenny-gui]: http://www.haskell.org/haskellwiki/Threepenny-gui

The reactive-banana library actually contains *two* FRP implementations:

1. [Reactive.Banana.Model][model] - A model implementation for testing and understanding the semantics. You are encouraged to look at the source code.
2. [Reactive.Banana.Prim][push] - The efficient push-driven implementation used for production code. Contains hard to understand trade secrets. ;-)

  [model]: https://github.com/HeinrichApfelmus/reactive-banana/blob/develop/reactive-banana/src/Reactive/Banana/Model.hs
  [push]: https://github.com/HeinrichApfelmus/reactive-banana/blob/develop/reactive-banana/src/Reactive/Banana/Prim.hs

## Contribute

Send me your examples, bindings, problems, suggestions, etc!

With contributions from

* Abu Alam
* Alexander Berntsen
* Kevin Cantu
* Gregory Crosswhite
* Elliott Hird
* John Lato
* Gideon Sireling
* Henning Thielemann
* Daniel Werner
