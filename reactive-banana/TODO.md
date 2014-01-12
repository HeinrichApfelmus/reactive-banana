FIXME
-----

* Latch evaluation needs to be cached. How do we invalidate the cache when a latch value has changed?

* Reinstate `changes` function. Update and test `reactive-banana-wx`.

* Clean up `AddHandler` code. In particular, remove duplicate export. Move to `Control.AddHandler`?

* Update `threepenny-gui`.

* Registered handlers need to be executed in declaration order, not in evaluation order.


Implementation Notes
--------------------

Observation: Recursion for latches
    We can build a pulse that depends on a latch that we haven't built yet,
    i.e.  applyP  is lazy in its first argument.
    However, building a latch requires the pulse to have been built already.

Observation:  switchL and latch evaluation
    There are two types of latches:
        1. latches that are updated by a pulse
        2. <$> and <*>
    The evaluation of the latter cases always happens lazily during pulse
    evaluation (the results are cached).
    There are no dependencies between latches that are updated by pulses.    

Present
-------

## Incremental computation

Investigate use of the `Change` data type for `Behavior`.
Consider overloading the `accumB` function to different types.

This goes hand-in-hand with thoughts about incremental computations.

Investigate whether there is a general framework behind discrete values, i.e. where the events are efficient diffs.

## Dynamic Event Switching
http://apfelmus.nfshost.com/blog/2011/05/15-frp-dynamic-event-switching.html

Found a way to keep the previous interface constant.

## Timing and timers

    type Duration = -- time difference in UNIX epoch

    -- event that happens once (not very useful without dynamic event switching)
    once :: a -> Duration -> Event era a

    -- delay a sequence of events, but cut them off when a new one comes
    -- very useful when used recursively
    schedule :: Event (a,Duration) -> Event a

    -- this primitive is all you need to enable continuous time behaviors
    time :: Behavior Time

Special support for time because

* Making sure that scheduling gives *logical* times when observed with  time  that are consistent with real-time.
** Use concurrency -> seems necessary
** Make sure that external events don't happen before scheduled events,
    that would be an observable inconsistency.
    Solution:
        - Execute any scheduled events whenever the  time  behavior is frozen.
        - External events never happen simultaneously with anything else
        - Freeze the  time  behavior to its logical value whenever a scheduled
          event happens.
* Debugging/Testing without having to wait the actual times.


## Efficiency
* Optimize pure behaviors?

    (pure f <$>)   -> fmap f
    apply (pure f) -> fmap f

* Make sure space behavior is like demand-driven implementation (for better or worse).
Actually, we need to speficy the model more carefully. The operations that shuffle events around are strict while the events themselves are lazy.

Future
------
Pure implementation
===================
The push-driven implementation can be made pure by putting the `Vault` data type into the `ST` monad instead of the `IO` monad. This might be useful for MIDI, i.e. using one and the same code for both real-time MIDI generation and writing it to files.
