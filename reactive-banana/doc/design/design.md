# Design and Implementation of the reactive-banana FRP library

by Heinrich Apfelmus

licensed CC-BY-SA 4.0

DRAFT, INCOMPLETE

This document is intended to describe the design and implementation of the reactive-banana library for functional reactive programming (FRP).

NOTE: This draft text already uses the plural `Events` as opposed to the singular `Event` for the data type.

## Introduction

Functional reactive programming (FRP) is a programming paradigm for declarative programming with data that changes over time. It was introduced in the seminal paper [Elliot and Hudak (1997)][fran] with two concepts, *Behavior* and *Events*. A *Behavior* denotes a value that varies in time, i.e. *Behavior* is a data type that represents the entire history and future of a value. *Events* denote a sequence of discrete moments in time which carry additional data.

For example, consider a graphical user interface (GUI) with a button and a numerical display whose value tracks the number of times that the button has been pressed. This value changes over time and can be represented as a *Behavior*. The clicks on the button are represented as *Events*; specifically, we have a variable of type `Events ()` that represents all future (!) clicks. The purpose of our program is to specify how these button `Events ()` are translated into a counter `Behavior Int` in a declarative manner. Of course, it is not possible to know all future clicks and write them into computer memory in advance, but it is useful to *pretend* to know them in advance, and to write a declarative program with that. As long as this program does not peek into the future, i.e. as long as it does not violate causality, it can be executed. The purpose of the reactive-banana FRP library is to execute such programs.

  [fran]: http://conal.net/papers/icfp97/

[…]

## Semantics

* Behavior uses **continuous time**. There is no notion of "update" for a Behavior. (For reasons of efficiency, the implementation does keep track of updates, though.) The advantage of this design is that it is not possible to write programs that depend on how often a Behavior updates, especially for updates that do not actually change the value.

* **Recursion** is allowed (and necessary). Specifically, all mututal recursions between Behavior and Event are well-defined.

[…]

## Implementation

### Time leaks and Updates

The easiest way to implement Events would be as a list of values tagged with their time of occurrence, e.g.

    type Events a = [(Time, a)]

By using a list lazy, future events which are not yet known can be added to the list when they occur.

However, any representation of Events as a pure data structure is problematic: As long as a variable `x :: Events A` is in scope, purity of the data structure implies that all events that have ever occurred will be kept alive as long as the variable `x` is alive. This is known as a **time leak**: Past events are stored and have to be traversed (repeatedly even) in order to reach the present moment. In order to be efficient, we have to discard old events which are no longer relevant, and this implies that the `Events` data structure needs to support impure updates. Note that lazy lists already use impure updates through their laziness, though these are transparent at the language level. Also, the problem of time leaks becomes apparent mostly in the presence of dynamic event switching, when the first-class nature of `Events` is used extensively.

### Push- versus pull

[…]

The necessity of implementing `Events` as a data structure that supports updates is challenging. We split the problem into several parts:

1. *Low-level*. We implement a mutable graph which represents dependencies between Events and allows us to implement push-based propagation of information.

2. *Mid-level*. We define low-level data structure `Pulse` and `Latch` that roughly correspond to Events and Behavior, but which are essentially just references to nodes in the mutable graph above. New nodes can be created with monadic functions such as

        union :: Pluse a -> Pulse a -> Build (Pulse a)

    where `Build` is a monad that describes changes to the mutable graph.

3. *High-level*. We implement *Event* and *Behavior* in terms of `Pulse` and `Latch`. Here, we use **observable sharing** to map Haskell variables to mutable references, and to turn impure functions into pure functions where possible.

### Low-level: Mutable graph

* The difficulty of `union` in a push-based setting: Dependency order, priorities.
* `Vault` for storage.
* Garbage collection

[…]

### Mid-level: Pulse and Latch

* `Build` monad.

[…]

### High-level: Events and Behavior

* Caching / observable sharing.

[…]
