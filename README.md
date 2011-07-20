## What is reactive-banana?

Reactive-banana is a library for [Functional Reactive Programming (FRP)][frp], written in Haskell.

See the **[project homepage][homepage]** for **documentation**, **examples** and so on.

  [homepage]: http://haskell.org/haskellwiki/Reactive-banana
  [frp]: http://haskell.org/haskellwiki/Functional_Reactive_Programming

I'm writing documentation on a call-by-need basis. So, feel free to write me an email if you want something explained that you don't understand, and I'll explain it to you!

## How is the source code structured?

The project contains three directories:

* `reactive-banana` - the library itself
* `reactive-banana-wx` - bindings to [wxHaskell][], includes many examples
* `BlackBoard` - Legacy, my personal drawing application for slideshows and videos. This will be removed from the repo soon.

At the moment, the reactive-banana library actually contains *two* FRP implementations:

1. [Reactive.Banana.Model][model] - A model implementation for testing and understanding the semantics. You are encouraged to look at the source code, it is intended to be easy to understand.
2. [Reactive.Banana.PushIO][pushio] - The efficient push-driven implementation used for production code. Contains hard to understand trade secrets. ;-)

  [model]: https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/src/Reactive/Banana/Model.hs
  [pushio]: https://github.com/HeinrichApfelmus/reactive-banana/blob/master/reactive-banana/src/Reactive/Banana/PushIO.hs

## Compilation

Prerequisites: the wxHaskell package

    cabal install wx

To build the reactive-banana libraries, type

   cd reactive-banana && cabal configure && cabal build && cd ..
   cd reactive-banana-wx && cabal configure && cabal build && cd ..

To build the Blackboard application (legacy), type

    cd reactive-banana && cabal configure && cd ..
    cd reactive-banana-wx && cabal configure && cd ..
    make BlackBoard

But take note that this probably **only works for MacOS X**! You'll have to change the `Makefile` a bit to make it work on other platforms.

## Contribute

Send me your examples, bindings, problems, suggestions, etc!

