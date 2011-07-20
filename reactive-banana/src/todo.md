Present
-------
* Add test cases and examples.

* Consider a better name / overloading the `filterApply` function while we're at it.

* `Discrete`: Investigate use of the `Change` data type for `Discrete`. Will only be relevant in a push-based implementation. Consider overloading the `accumD` function to different types.


Examples
========
http://comments.gmane.org/gmane.comp.lang.haskell.cafe/90908
http://www.reddit.com/r/haskell/comments/ijsnv/call_for_gui_examples_any_small_gui_applications/
http://apfelmus.nfshost.com/blog/2011/07/08-call-for-gui-examples.html


### Popular requests
* Real-time data display (stock market, system data / CPU usage, darcs repos)

### Misc
* GUI for a command line program
    transformation between files in a directory
* Chess GUI
* "Open" systems, i.e. involving external data, like network, file system etc.
* Color picker, font chooser, file chooser

### Difficult examples
* CRUD table: create, update, delete records in table (for example Person records), with validating user input, auto-calculated fields (current age by birth date), totals by column, filtering. I think same GUI tasks get solved many GUI developers in imperative style. I hope solution of such day-tasks in FRP will be elegant. Aka databases.
* small vector drawing application (or GUI designer)
    events from drawn shapes

* arpeggiator for MIDI events (delay existing events to repeat them in the future)
* dynamic widgets and windows

### Animations
* Table of Contents - http://sjoerdvisscher.handcraft.com/treenav
* Final Fantasy style game
    You wouldn't need a whole game :) I'm thinking of something like a 50x50 grid map with a 20x20 view portal on it. A character moving around under user control, causing the portal to scroll, and maybe 2 or 3 non user controlled characters moving randomly. The interactions between animation, multiple characters, user input and map scrolling would be very interesting using FRP.


Consequences
============
-- Collect simultaneous events into a list
collect :: Event a -> Event [a]

-- only keep the last event from simultaneous ones
calm :: Event a -> Event a
calm = fmap last . collect


-- The following can be simulated with timers
-- But we probably want some pure representations, too.

-- delay all event occurences by an amount of time
-- make sure it doesn't care about execution time
delay :: Time -> Event a -> Event a
-- or even
delay :: Behavior Time -> Event a -> Event a

-- continuous behaviors
time :: Behavior Time



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
Dynamic Event Switching
=======================
http://apfelmus.nfshost.com/blog/2011/05/15-frp-dynamic-event-switching.html

Incremental Computation
=======================
Investigate whether there is a general framework behind discrete values, i.e. where the events are efficient diffs.


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




