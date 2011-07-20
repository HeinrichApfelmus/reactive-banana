Behaviors
=========
Make it possible to obtain Behaviors from polling. (Oops, that should have been included right from the start.)

    fromPoll :: IO a -> NetworkDescription (Behavior a)


Convenience
===========
Convenience function for writing custom GUI widgets in FRP style.

    interpretAsHandler :: (Event a -> Event b) -> (AddHandler a -> AddHandler b)


Discrete Values
===============
Create a hybrid data type

    data Discrete a = (Behavior a, Event a)

which denotes a behavior but also keeps track of changes for sampling/efficiency.


Misc
====
* Consider a new symbol for the `apply` and `<*>` functions.
  Possibilities:  <#>, <@>
  Consider overloading this symbol.
  Also c

Examples
========

* wxAsteroids
* Read a value from an edit box
* Guitar strummer
