
Change
======
* Think up a good name for the `Change` data type. It should be mnemonic for both events and behaviors. Currently, it's only mnemonic for behaviors

=> Turns out that `Change` is not really necessary for events.


Never
=====
* Optimize `Behavior` for the case of pure functions, so that we can make use the applicative instance when we want to `fmap` something.

=> Done


Overloading
===========
* Behavior version of `accumulate`, `filter`. Should they be overloaded as well? Or should users always interject a `pure` to use them?

=> Only accumulate is overloaded, though that may not even be necessary due to the 

   accumulate ($)

pattern which shifts the IO and Change to the events.