[![Build Status](https://travis-ci.org/HeinrichApfelmus/reactive-banana.png)](https://travis-ci.org/HeinrichApfelmus/reactive-banana) 

## What is reactive-banana?

<div style="float:left;"><img src="https://github.com/HeinrichApfelmus/reactive-banana/raw/master/banana.png" /></div>

Reactive-banana is a library for [Functional Reactive Programming (FRP)][frp], written in Haskell.

See the **[project homepage][homepage]** for **documentation**, **examples** and so on.

  [homepage]: http://wiki.haskell.org/Reactive-banana
  [frp]: http://wiki.haskell.org/Functional_Reactive_Programming

## Compilation from the repository

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

## How is the source code structured?

The project contains several directories:

* `reactive-banana` — the core library
* `reactive-banana-wx` — bindings to the [wxHaskell][] GUI library, includes many examples

  [wxhaskell]: http://wiki.haskell.org/WxHaskell
  [threepenny-gui]: http://wiki.haskell.org/Threepenny-gui

The reactive-banana library actually contains *two* FRP implementations:

1. [Reactive.Banana.Model][model] - A model implementation for testing and understanding the semantics. You are encouraged to look at the source code.
2. [Reactive.Banana.Prim][push] - The efficient push-driven implementation used for production code. Contains hard to understand trade secrets. ;-)

  [model]: https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/src/Reactive/Banana/Model.hs
  [push]: https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/src/Reactive/Banana/Prim.hs

## License

The source code is distributed under a BSD3 license. See the `LICENSE` files in the corresponding subdirectories.

The reactive-banana mascot [[png]][mascot] is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a> with attribution to Heinrich Apfelmus and the reactive-banana library.

  [mascot]: https://github.com/HeinrichApfelmus/reactive-banana/raw/master/banana.png


## Contribute

Send me your examples, bindings, problems, suggestions, etc!

Many thanks to

Alexander Berntsen, Oliver Charles, Samuel Gélineau, madjestic, *and also* Abu Alam, Vincent Berthoux, Kevin Cantu, Gregory Crosswhite, Peter Hillerström, Joseph Heinemeyer, Elliott Hird, Matt Kraai, John Lato, Vladimir Lopatin, Peter Minten, Gideon Sireling, Henning Thielemann, Daniel Werner

for their contributions!
