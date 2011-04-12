Present
-------
* Add test cases and examples.

Push-base implementation
========================
* Observable sharing
* Compiling execution paths. Make sure space behavior is like demand-driven implementation (for better or worse).

Change
======
Investigate use of the  Change  data type for Behaviors. Will only be relevant in a push-based implementation. Consider overloading the  accumB  function to different types.

Optimize pure behaviors
=======================
Make sure that never changing behaviors are as efficient as a simple fmap. In particular,

   (pure f <$>)   = fmap f
   apply (pure f) = fmap f



Future
------
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