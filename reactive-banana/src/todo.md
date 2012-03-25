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

"Splitting the moment"
====================
Think about simultaneous event some more. In particular, investigate "splitting the moment". This might be relevant for modularity, but makes the semantics more complicated. We need infinitesimal delays to allow revents to be defined recursively.

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

