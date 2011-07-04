Present
-------
* Add test cases and examples.

Behaviors
=========
Make it possible to obtain Behaviors from polling. (Oops, that should have been included right from the start.)

    fromPoll :: IO a -> NetworkDescription (Behavior a)
    unsafeFromPoll :: IO a -> NetworkDescription (Behavior a)

Think of the I/O action as `readIORef ref`.

* The I/O action might return different results while one event is processed. This can be solved by caching the value.
* A more subtle problem is that the behavior must be polled *before* the network executes any I/O action. That's because one of the I/O actions might change the value returned, which would violate the `(< time)` semantics of behaviors. The `unsafeFromPoll` function violates this in the name of speed. I should include a few tools for making them safe again, to be used in bindings to GUI libraries.

Convenience
===========
Convenience function for writing custom GUI widgets in FRP style.

    interpretAsHandler :: (Event a -> Event b) -> (AddHandler a -> AddHandler b)

Reactive Values
===============
Create a hybrid data type

    data Reactive a = (Behavior a, Event a)

which denotes a behavior but also keeps track of changes for sampling/efficiency.

Investigate use of the  Change  data type for Reactive . Will only be relevant in a push-based implementation. Consider overloading the  accumR  function to different types.


Intermediate
------------
Animations
==========
* Add timer events/widgets.

    type TimeInteval = Integer -- milliseconds
    timer :: Reactive TimeInteval -> NetworkDescription (Event ())

They receive a rate as input and generate events in regular intervals.

* Delaying events
** Very useful for things like an arpeggiator (repeat notes) and general MIDI programming
* Continuous time is darn handy for animations
* Dynamic event switching as well


Push-based implementation
=========================
* Make sure space behavior is like demand-driven implementation (for better or worse).
* Include sharing in Behaviors

Optimize pure behaviors
=======================
Make sure that never changing behaviors are as efficient as a simple fmap. In particular,

   (pure f <$>)   = fmap f
   apply (pure f) = fmap f




Future
------
Dynamic Event Switching
=======================
http://apfelmus.nfshost.com/blog/2011/05/15-frp-dynamic-event-switching.html

Timing
======
Think about simultaneous event some more. In particular, investigate "splitting the moment". This might be relevant for modularity, but makes the semantics more complicated.

The event `before e` is guaranteed to execute before any event that occurs simultaneoulsy with the event `e`.

Rules for simultaneous occurance

    e ~ e
    fmap f e ~ fmap g e
    e ~ e' => before e ~ before e'

    Transitivity!  e1 ~ e2 && e2 ~ e3  ==> e1 ~ e3

Possibility 1:

    (e1',e2') = order e1 e2

Possibility 2:

    e1' = before e1
    e2' = after e2

Possibility 3:
    (e1,e2) = orderedDuplicate e
    -- This probably violates transitivity of  ~ ?
    -- e1 ~ e and e2 ~ e  but  e1 /~ e2
    -- Needs another interpretation for  ~

Pure implementation
===================
The push-driven implementation can be made pure by putting the `Vault` data type into the `ST` monad instead of the `IO` monad. This might be useful for MIDI, i.e. using one and the same code for both real-time MIDI generation and writing it to files.




