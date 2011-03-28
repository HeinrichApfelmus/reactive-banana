Haskell Blackboard
------------------

## What is Blackboard?

Blackboard is my (Heinrich Apfelmus) personal drawing application for making slideshows and videos.

## What is of interest to Haskell programmers here?

Blackboard is implemented in Haskell. In particular, I'm making use of *functional reactive programming* (FRP). I've written a small domain specific language for FRP, which is now available on hackage.

* [reactive-banana][] - the FRP library itself
* [reactive-banana-wx][] - example binding to wxHaskell

## How do I understand the source code?

You need to have a grasp of basic Haskell to understand anything, of course, but even then it might be difficult to decipher what is going on. The `reactive-banana` has extensive Haddock documentation. Some in-depth documentation can be found in the [`doc` directory][doc]

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



