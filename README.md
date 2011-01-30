Haskell Blackboard
------------------

## What is Blackboard?

Blackboard is my (Heinrich Apfelmus) personal drawing application for making slideshows and videos.

## What is of interest to Haskell programmers here?

Blackboard is implemented in Haskell. In particular, I'm making use of *functional reactive programming* (FRP). I've written a small domain specific language for FRP, which I hope to polish and publish as a proper Haskell library in the future. In the meantime, have a look at the raw and unfinished code here.

## How do I understand the source code?

You need to have a grasp of basic Haskell to understand anything, of course, but even then it might be difficult to decipher what is going on. You can find some preliminary documentation in the `doc/` directory.

I'm writing documentation on a call-by-need basis. So, feel free to write me an email if you want something explained that you don't understand, and I'll explain it to you!

## Compilation

Prerequisites: the wxHaskell package

    cabal install wx

To build the project, type

    make BlackBoard

But take note that this probably **only works for MacOS X**! You'll have to change the `Makefile` a bit to make it work on other platforms.

If you do make it compile on another platform, please send me your changes! Also, it would be awesome if you could make a proper cabal file that also builds a MacOS X application!

## Contribute

I'm currently not making much use of the GUI elements from `wxHaskell`. If you could make a small example with GUi widgets, or convince me that I should make one, that would be great.



