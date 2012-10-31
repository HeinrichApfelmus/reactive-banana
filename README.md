## What is reactive-banana?

<div style="float:left;"><img src="https://github.com/HeinrichApfelmus/reactive-banana/raw/develop/banana.png" /></div>

Reactive-banana is a practical library for [Functional Reactive Programming (FRP)][frp], written in Haskell.

Support the project with a small donation: [![Flattr this](http://api.flattr.com/button/flattr-badge-large.png)](http://flattr.com/thing/384682/reactive-banana)

See the **[project homepage][homepage]** for **documentation**, **examples** and so on.

  [homepage]: http://haskell.org/haskellwiki/Reactive-banana
  [frp]: http://haskell.org/haskellwiki/Functional_Reactive_Programming

I'm writing documentation on a call-by-need basis. So, feel free to write me an email if you want something explained that you don't understand, and I'll explain it to you!

## Compilation

To build and install the core library, simply type

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

### GUI examples using Ji

Prerequisites: Chris Done's [ji][] library.

    git clone git://github.com/chrisdone/ji.git
    cabal install

To build the ji examples, type

    cd reactive-banana-ji
    cabal configure -fbuildExamples && cabal build
    cd ..

## How is the source code structured?

The project contains several directories:

* `reactive-banana` - the core library
* `reactive-banana-wx` - bindings to the [wxHaskell][] GUI library, includes many examples
* `reactive-banana-ji` - experimental bindings to Chris Done's [ji][] library for the web browser

  [wxhaskell]: http://haskell.org/haskellwiki/WxHaskell
  [ji]: https://github.com/chrisdone/ji

The reactive-banana library actually contains *two* FRP implementations:

1. [Reactive.Banana.Model][model] - A model implementation for testing and understanding the semantics. You are encouraged to look at the source code.
2. [Reactive.Banana.Internal][push] - The efficient push-driven implementation used for production code. Contains hard to understand trade secrets. ;-)

  [model]: https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/src/Reactive/Banana/Model.hs
  [push]: https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/src/Reactive/Banana/Internal/

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
