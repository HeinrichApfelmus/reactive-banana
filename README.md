[![Hackage](https://img.shields.io/hackage/v/reactive-banana.svg)](https://hackage.haskell.org/package/reactive-banana)

# Reactive-banana

### What's this?

<div style="float:left;"><img src="https://github.com/HeinrichApfelmus/reactive-banana/raw/master/banana.png" /></div>

Reactive-banana is a library for [Functional Reactive Programming (FRP)][frp], written in Haskell.

See the **[project homepage][homepage]** for **documentation**, **examples** and so on.

  [homepage]: http://wiki.haskell.org/Reactive-banana
  [frp]: http://wiki.haskell.org/Functional_Reactive_Programming

# Installation

### Compilation from the repository

To build and install the core library from the source repository, simply type

    cd reactive-banana && cabal install && cd ..

However, to try out the GUI examples, you have to install one of the of the additional packages.

### GUI examples using wxHaskell

Prerequisites: the wxHaskell package

    cabal install wx

Note that you need to have a development version of the wxWidgets libraries installed before building wx. If you run into `ExitFailure 1` exceptions, please follow the [wxHaskell Quick Start](http://wiki.haskell.org/WxHaskell/Building) instructions and try again.

To build the wx examples, type

    cd reactive-banana-wx
    cabal configure -fbuildExamples && cabal build
    cd ..


# Technical overview

### How is the source code structured?

The project contains several directories:

* `reactive-banana` — the core library
* `reactive-banana-wx` — bindings to the [wxHaskell][] GUI library, includes many examples

  [wxhaskell]: http://wiki.haskell.org/WxHaskell
  [threepenny-gui]: http://wiki.haskell.org/Threepenny-gui

The reactive-banana library actually contains *two* FRP implementations:

1. [Reactive.Banana.Model][model] - A model implementation for testing and understanding the semantics. You are encouraged to look at the source code.
2. [Reactive.Banana.Prim][push] - The efficient push-driven implementation used for production code. Contains hard to understand trade secrets. ;-)

  [model]: https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/src/Reactive/Banana/Model.hs
  [push]: https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/src/Reactive/Banana/Prim

### License

The source code is distributed under a BSD3 license. See the `LICENSE` files in the corresponding subdirectories.

The reactive-banana mascot [[png]][mascot] is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a> with attribution to Heinrich Apfelmus and the reactive-banana library.

  [mascot]: https://github.com/HeinrichApfelmus/reactive-banana/raw/master/banana.png


# Contributors

Many thanks to everyone who contributed, provided feedback or simply wrote an application using Reactive-Banana! In particular, many thanks to:

Alexander Berntsen, Oliver Charles, Samuel Gélineau, Vladimir Lopatin, Atze van der Ploeg, Mitchell Rosen, [*and many others*](CONTRIBUTORS).

Special thanks to *Oliver Charles* and *Mitchell Rosen* for co-maintaining this project.

