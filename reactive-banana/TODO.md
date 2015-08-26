FIXME
-----

* New name for  execute   and  observe ?

* Implement optimzation that latches check their timestamp against
  the current time first before chasing dependencies.

* Garbage collection:

  Does it make a difference whether we use  weak  pointers for the
  _parentP field or not? What about dependencies via  readPulseP ?
  the goal is tobe able to garbage collect whole chains at once,
  otherwise it would take too much time.


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

Optimization Possibilities
--------------------------

* Don't push events when the pipeline further down cannot observe them

    I.e. when they are not connected to an output,
    a switch or a latch. However, this is probably tricky to detect.

* Remove events that are children of `neverP`.

    Note that `switchP` can change the child-parent relationship,
    so this optimization is only safe when the `neverP` will
    never be replaced by a different parent.

* Garbage collect events that are no longer alive

    Note that we might still be able to switch in events,
    so this really has to be integrated with host language garbage collection.


Design Questions
----------------

## Incremental computation

Investigate use of the `Change` data type for `Behavior`.
Consider overloading the `accumB` function to different types.

This goes hand-in-hand with thoughts about incremental computations.

Investigate whether there is a general framework behind discrete values, i.e. where the events are efficient diffs.

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

    * Use concurrency -> seems necessary
    * Make sure that external events don't happen before scheduled events,
        that would be an observable inconsistency.
        Solution:
    
        * Execute any scheduled events whenever the  time  behavior is frozen.
        * External events never happen simultaneously with anything else
        * Freeze the  time  behavior to its logical value whenever a scheduled
      event happens.

* Debugging/Testing without having to wait the actual times.

## Pure implementation

The push-driven implementation can be made pure by putting the `Vault` data type into the `ST` monad instead of the `IO` monad. This might be useful for MIDI, i.e. using one and the same code for both real-time MIDI generation and writing it to files.
