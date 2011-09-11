Present
-------

Small fry
=========
* Add test cases and examples.
* Consider a better name / overloading the `filterApply` function while we're at it.
* `Discrete`: Investigate use of the `Change` data type for `Discrete`. Will only be relevant in a push-based implementation. Consider overloading the `accumD` function to different types.


Interface changes
=================

### Managing simultaneous events

    -- Collect simultaneous events into a list
    collect :: Event a -> Event [a]
    -- collect . spread = id
    spread :: Event [a] -> Event a


    -- only keep the last event from simultaneous ones
    calm :: Event a -> Event a
    calm = fmap last . collect

### Timing and timers

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
** Use concurrency
** Make sure that external events don't happen before scheduled events,
    that would be an observable inconsistency.
    Solution:
        - Execute any scheduled events whenever the  time  behavior is frozen.
        - External events never happen simultaneously with anything else
        - Freeze the  time  behavior to its logical value whenever a scheduled
          event happens.
* Debugging/Testing without having to wait the actual times.


### Dynamic Event Switching
http://apfelmus.nfshost.com/blog/2011/05/15-frp-dynamic-event-switching.html

Found a way to keep the previous interface constant.


Efficiency
==========
* Sharing for Behaviors

* Optimize finalizers and initializers.
  Currently, *every* temporariliy cached value will be deleted,
  and *every* accumulated behavior will be updated.
  Ideally, you would only this with references that were actually *used*.
* Actually, we can keep a separate vault for the temporary values
  and throw it away wholesale.

* Polling behaviors from the outside should only be read if we actually need them.
  This is a bit tricky, we may not execute any IO action before doing that,
  because that might change the external behavior.
* Ponder the difference between internal behaviors and external behaviors
    internal: freeze after we have changed the accum value
        Here we have full control, reactimate can't do anything bad.
    external: freeze before we do anything with it
  We could avoid a test if we do it right.
  (Then again, we shouldn't use the Vault if we want to avoid tests...)

* Optimize pure behaviors

    (pure f <$>)   -> fmap f
    apply (pure f) -> fmap f

* Make sure space behavior is like demand-driven implementation (for better or worse).


Future
------

Incremental Computation
=======================
Investigate whether there is a general framework behind discrete values, i.e. where the events are efficient diffs.

Simultaneity
============
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




