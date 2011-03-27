Timing
======

Events sometimes happen simultaneously, in particular when different events that depend on a common ancestor are `union`ed. The programming model is forced to execute them sequentially, hence the only consequence we can do is that their *order* is *undefined*. However, sometimes it is necessary to control event order. The `before` and `after` functions allow you to determine the order of events that occur at the same time.

The event `before e` is guaranteed to execute before any event that occurs simultaneoulsy with the event `e`.

Rules for simultaneous occurance

    e ~ e
    fmap f e ~ fmap g e
    e ~ e' => before e ~ before e'


Possibility 1:

    (e1',e2') = order e1 e2


Possibility 2:

    e1' = before e1
    e2' = after e2