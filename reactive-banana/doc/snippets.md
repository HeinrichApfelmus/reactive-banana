Note on GUIs and FRP
====================

First, note that the `Blackboard` application is not making much use of `wxHaskell`, I'm even drawing my own controls. That is probably kind of disappointing, since other FRP approaches Yampa can do the same thing. However, my FRP approach is very flexible and it is absolutely no problem to hook it up to `wxHaskell` or `Gtk2hs`, I'm just not making use of that in my application.


Short overview of the FRP library
=================================

The key data types are `Event a` and `Behavior a`. The former represents a series of events that carry a value of type  a , the latter represents a value of type  a  that varies in time. You can *think* of them as

    type Event a    = [(Time,a)]  -- stream of events
    type Behavior a = Time -> a   -- time-varying value

This is a bit too optimistic because  Behavior a  is a *piecewise* function, i.e. we can convert `Event a` and `Behavior a` via
    
    changes :: Behavior a -> Event a
    initial :: Behavior a -> a
    
    Behavior :: a -> Event a -> Behavior a  -- constructor

Despite this similarity, they are quite distinct. The core functionality is given by functions which you can *think* of being implemented as follows:

   -- accumulate a series of events into a behavior
   accumulate :: (a -> b -> b) -> b -> Event a -> Behavior b
   accumulate f b0 es = Behavior b0 (go f b0 es)
        where
        go f b ((time, a):es) = let b' = f a b in (time, b') : go f b' es

   -- applicative instance
   instance Applicative Behavior where
        pure x  = \time -> x
        f <*> x = \time -> f time $ x time 

   -- apply a time-varying function to a series of events
   apply :: Behavior (a -> b) -> Event a -> Event b
   apply f = map (\(time,a) -> (time, f time a))

Note that `apply` and `(<*>)` have similar types, but different semantics! In the case of `apply`, changes in the first argument do not trigger events in the result.


A programming pattern for merging events
========================================

Often, a behavior depends on different events which have to be merged. For instance, ... . One could make a new data type that is a disjoint union of the different event types, like

    data EventBox = Character c | MouseMove ...

and write a function that handles these.

Of course, this has the drawback that you always have to invent a dummy data type just to perform this disjoint union. A better idea is to use ordinary function names

    handleCharacter :: Char -> Box -> Box
    handleMouseMove :: Point -> Box -> Box

and map them on the event streams

    handlers =
        fmap handleCharacter ekeyboard  `union` fmap handleMouseMove emouse

Then, you can simply accumulate the behavior as

    behavior = accumulate (flip ($)) 0 handlers


In a sense, this is the Church-encoding of the dummy type.


**NOTE:** I think it's best to change the type of accumulate, so we can simply write

    accumulate ($) 0 handlers

This way, it's easier to change from an explicit

    accumulate handleCharacter echars

to a more general

    let handlers = fmap handleCharacter echars
    accumulate ($) 0 handlers

without having to flip arguments and thelike.



