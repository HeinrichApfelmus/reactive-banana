## What is Blackboard?

Blackboard is my (Heinrich Apfelmus) personal drawing application for making slideshows and videos.

## What is of interest to Haskell programmers here?

Blackboard is implemented in Haskell. In particular, I'm making use of *functional reactive programming* (FRP). I've implemented a small library for FRP, called `reactive-banana`, which is now available on hackage.

For more on **reactive-banana**, see the **[project homepage][0]**.

Currently, the source code for the reactive-banana and reactive-banana-wx libraries is part of the source code for BlackBoard.

  [0]: http://haskell.org/haskellwiki/Reactive-banana


## How do I understand the source code?

You need to have a grasp of basic Haskell to understand anything, of course, but even then it might be difficult to decipher what is going on. The [reactive-banana][2] library has extensive Haddock documentation. Some in-depth documentation can be found in the [`doc` directory][doc]

  [doc]: https://github.com/HeinrichApfelmus/Haskell-BlackBoard/tree/master/reactive-banana/doc

I'm writing documentation on a call-by-need basis. So, feel free to write me an email if you want something explained that you don't understand, and I'll explain it to you!

## Compilation

Prerequisites: the wxHaskell package

    cabal install wx

To build the project, type

    cd reactive-banana && cabal configure && cd ..
    cd reactive-banana-wx && cabal configure && cd ..
    make BlackBoard

But take note that this probably **only works for MacOS X**! You'll have to change the `Makefile` a bit to make it work on other platforms.

If you do make it compile on another platform, please send me your changes! Also, it would be awesome if you could make a proper cabal file that also builds a MacOS X application!

## Contribute

Send me your examples, bindings, problems, suggestions, etc!



