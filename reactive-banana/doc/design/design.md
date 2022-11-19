# Design and Implementation of the reactive-banana FRP library

by Heinrich Apfelmus,
November 2022

licensed CC-BY-SA 4.0

DRAFT, INCOMPLETE

This document is intended to describe the design and implementation of the reactive-banana library for functional reactive programming (FRP).

NOTE: This draft text already uses the plural `Events` as opposed to the singular `Event` for the data type.

# Introduction

Functional reactive programming (FRP) is a programming paradigm for declarative programming with data that changes over time. It was introduced in the seminal paper [Elliot and Hudak (1997)][fran] with two concepts, *Behavior* and *Events*. A *Behavior* denotes a value that varies in time, i.e. *Behavior* is a data type that represents the entire history and future of a value. *Events* denote a sequence of discrete moments in time which carry additional data.

For example, consider a graphical user interface (GUI) with a button and a numerical display whose value tracks the number of times that the button has been pressed. This value changes over time and can be represented as a *Behavior*. The clicks on the button are represented as *Events*; specifically, we have a variable of type `Events ()` that represents all future (!) clicks. The purpose of our program is to specify how these button `Events ()` are translated into a counter `Behavior Int` in a declarative manner. Of course, it is not possible to know all future clicks and write them into computer memory in advance, but it is useful to *pretend* to know them in advance, and to write a declarative program with that. As long as this program does not peek into the future, i.e. as long as it does not violate causality, it can be executed. The purpose of the reactive-banana FRP library is to execute such programs.

  [fran]: http://conal.net/papers/icfp97/

[…]

# Semantics

* Behavior uses **continuous time**. There is no notion of "update" for a Behavior. (For reasons of efficiency, the implementation does keep track of updates, though.) The advantage of this design is that it is not possible to write programs that depend on how often a Behavior updates, especially for updates that do not actually change the value.

* **Recursion** is allowed (and necessary). Specifically, all mututal recursions between Behavior and Event are well-defined.

[…]

# Implementation

## Time leaks and Updates

The easiest way to implement Events would be as a list of values tagged with their time of occurrence, e.g.

```hs
type Events a = [(Time, a)]
```

By using a list lazy, future events which are not yet known can be added to the list when they occur.

However, any representation of Events as a pure data structure is problematic: As long as a variable `x :: Events A` is in scope, purity of the data structure implies that all events that have ever occurred will be kept alive as long as the variable `x` is alive. This is known as a **time leak**: Past events are stored and have to be traversed (repeatedly even) in order to reach the present moment. In order to be efficient, we have to discard old events which are no longer relevant, and this implies that the `Events` data structure needs to support impure updates. Note that lazy lists already use impure updates through their laziness, though these are transparent at the language level. Also, the problem of time leaks becomes apparent mostly in the presence of dynamic event switching, when the first-class nature of `Events` is used extensively.

## Push- versus pull

[…]

The necessity of implementing `Events` as a data structure that supports updates is challenging. We split the problem into several parts:

1. *Low-level*. We implement a mutable graph which represents dependencies between Events and allows us to implement push-based propagation of information.

2. *Mid-level*. We define low-level data structure `Pulse` and `Latch` that roughly correspond to Events and Behavior, but which are essentially just references to nodes in the mutable graph above. New nodes can be created with monadic functions such as

    ```hs
    union :: Pluse a -> Pulse a -> Build (Pulse a)
    ```

    where `Build` is a monad that describes changes to the mutable graph.

3. *High-level*. We implement *Event* and *Behavior* in terms of `Pulse` and `Latch`. Here, we use **observable sharing** to map Haskell variables to mutable references, and to turn impure functions into pure functions where possible.

## Low-level: Mutable graph

* The difficulty of `union` in a push-based setting: Dependency order, priorities.
* `Vault` for storage.
* Garbage collection

[…]

## Mid-level: Pulse and Latch

* `Build` monad.

[…]

## High-level: Events and Behavior

[…]

### Events and Behaviors

The final step in the implementation of the `Events` and `Behavior` types is their definition in terms of the `Pulse` and `Latch` types. The implementation is very direct, roughly `Events ~ Pulse` and `Behavior ~ Latch`, except that

* we use *observable sharing* to make `Events` and `Behavior` behave more like pure values, and
* for each `Behavior`, we keep track of a its updates, so that `Behavior a ~ (Latch a, Pulse ())`, in order to more efficiently connect it to the outside world.

### Observable sharing

This section explains how observable sharing is used in the implementation of `Events` and `Behavior`.

*Observable sharing* is a technique that aids the implementation of *domain specific languages* (DSL) that are *embedded* in a purely functional host language. It makes the variable binding and sharing mechanism of the host language (e.g. Haskell's `let`) partially observable in the embedded language in order to avoid duplicating large subexpressions ([Claessen and Sands 1999][clsa], [Gill 2009][gill], [Kiselyov 2011][oleg],…). We use this technique for the same purpose in our implementation of FRP.

To explain observable sharing, let us first consider a toy example and only then consider an FRP example. The toy example is a small language for evaluating arithmetic expressions `Exp`:

```hs
data Exp
    = Val Int     -- plain `Int`
    | Add Exp Exp -- addition
    | Mul Exp Exp -- multiplication
```

An interpreter for this language has type signature `eval :: Exp -> Int`, we skip the straightforward implementation here. To embed this language more deeply into the host language, we would also define lower-case (smart) constructors like `val = Val` and even a `Num Exp` instance with `(+) = Add` and `(*) = Mul`.

The host language acts a sort of "macro language" for the embedded language. For example, consider the value

```hs
y :: Exp
y = let x = val 1 + val 2 in (x * x)
```

This expression is equal to

```hs
y = Mul (Add (Val 1) (Val 2)) (Add (Val 1) (Val 2))
```

In other words, the variable `x` behaves like a "abbreviation" or "macro" that expands to a full expression in the embedded language. Unfortunately, the resulting expression has become inefficient — the addition has been *duplicated*! What to do?

In order to have more *sharing* and to avoid this duplication, we have to recognize two things:

* The host language allows sharing expressions by binding them to variables, but the embedded language does not allow that yet. If we want sharing in the DSL, we have to add an explicit *language construct* for that. The simplest solution is to add variables and binders to the language, that is to expand the type with two constructors

    ```hs
    data Name = String
    data Exp
        = …
        | Var Name
        | Let Name Exp Exp -- let name = e1 in e2
    ```

    Then, the example value would be written

    ```hs
    y' = Let "x" (val 1 + val 2) (Var "x" * Var "x")
    ```

* It would be swell if we could reuse the host language `let` syntax for the embedded `Let` as well. But there is a conflict of interest here: Even if we have a way of expressing sharing in our embedded language (`Let`), we may still want to preserve our ability to define macros like `x` that are *not* shared, but instead duplicate code (`let`) — it is not obvious that host language expression `y` should always be translated to its shared variant `y'`. However, in most cases, this is indeed desirable, certainly in the case of our FRP implementation.

The main difficulty we face when attempting to reuse the `let x = …` construct of the host language is that this construct is *referentially transparent*, i.e. the semantics of the host language do not allow us to learn that a variable named `x` was defined. This is where *observable sharing* comes in. Its main idea is as clever as it is devious (another word for "not referentially transparent"): By making the evaluation of the "macro" `x` impure (using `unsafePerformIO`), we can detect (using a mutable reference) repeated uses of the "macro" and expand it to a variable instead of the full expression. In this way, we do not learn the name of the variable `x`, but we do retain the fact that it always refers to the same expression in the embedded language.

Let us leave aside observable sharing for a moment and focus on impurity: It turns out that *monads* lend additional insight into the distinction between a "macro" `x` and a "variable" `x`. Specifically, there are many similarities between the `let` construct and a monadic bind. To wit, the pure expression

```hs
let x = exp1 in
let y = exp2 in
exp3
```

very much resembles the monadic expression

```hs
do
    x <- exp1
    y <- exp2
    pure exp3
```

We can make this explicit by introducing a monad, say `M`, which is (contains) a state monad that stores a collection of associations `Name ↦ Exp` of variable names and expressions assigned to them. Such a monad would support the operations

```hs
fresh  :: M Name
bind   :: Name -> Exp -> M ()
lookup :: Name -> M (Maybe Exp)
```

for creating fresh names, binding names to expressions and looking up expressions.

With such a monad, the type `M Name` can actually replace the type `Exp` for the purpose of constructing expressions of the embedded language in the host language. Specifically, the type `M Name` denotes a "macro", i.e. an action that adds a new expression to the store and returns the name that it was bound to, whereas the type `Name` denotes a "variable", i.e. a reference to an already constructed expression. All combinators would be adapated to work with `M Name` instead of `Exp`. For example, the combinator `val` that constructs a `Val` expression and bind it to a fresh name,

```hs
val :: Int -> M Name
val i = do
    name <- fresh
    bind name $ Val i
    pure name
```

whereas the addition combinator `(+)` would execute the argument "macros" and construct a named `Add` expression:

```hs
(+) :: M Name -> M Name -> M Name
(+) a b = do
    x <- a
    y <- b
    name <- fresh
    bind name $ Add (Var x) (Var y)
    pure name
```

In other words, the idea of `M Name` replacing `Exp` is that no matter how small, each (sub-)expression is automatically bound to a fresh name.

Assuming that all combinators such as `val`, `(+)` and `(*)` have been adapted to the `M Name` style, we can now write the shared and unshared examples from above as

```hs
y :: M Name
y = do
    let x = val 1 + val 2
    x * x

y' :: M Name
y' = do
    x' <- val 1 + val 2
    let x = var x'
    x * x
```

where `var = pure . Var`. The code is almost identical, except that the "macro expansion" case `y` uses the host language `let` construct, whereas the "shared variable" case `y'` uses the monadic assignment `<-`, which essentially serves as a (variant of the) embedded language `Let`.

Now, the *aim* of *observable sharing* is to trick the host language so that `y` will generate the same expressions as `y'`. In other words, the binding of a varibale in the host language, here using `let x = …`, should have the same effect as the binding of a variable in the embedded language, here using `x' <- …`.

The main idea for achieving this is to change the behavior of `(+)` in `let x = val 1 + val 2` so that it generates a fresh name *only* if a fresh name has not been generated and bound before. We have to implement this as a side effect in the pure host language, hence we require `unsafePeformIO` and we need the generation of fresh names to be in `IO`, that is `fresh :: IO Name`. Then, the new definition of `(+)` is

```hs
-- with observable sharing
(+) :: M Name -> M Name -> M Name
(+) = \a b -> unsafePerformIO $ do
    name <- fresh
    pure $ do
        mexp <- lookup name
        case mexp of
            Nothing -> do
                x <- a
                y <- b
                bind name $ Add (Var x) (Var y)
            Just _  ->
                pure $ Var name
```

This works as follows: When evaluating the value `x` to WHNF for the first time, its expression `val 1 + val 2` will be evaluated to a call to `unsafePerformIO`, which will in turn be executed and generate a fresh `name`, which will then be used to `bind` an expression. The key point is that when `x` is evaluated *again*, its WHNF already contains the generated name `name`, and executing the monadic action will now detect that this name has been bound already. In this way, using the value `x` in the host language twice, such as in the host expression `x * x`, will use the corresponding `name` in the embedded name only once, and the bound expression will be shared.

The above describes the main idea for implementing observable sharing. In reactive-banana, the monad `M` and the type `Name` are modeled by different types:

* The type `Name` is replaced by `IORef`, i.e. we reuse a built-in facility of Haskell for generating "names" and binding them to values.

* The monad `M` is replaced by `IO`, or more generally by an instance of `MonadIO`, as only these kinds of monads support manipulation of `IORef`.

* Instead of the "macro" type `M Name`, we use a type `Cached m a` where `a` is the type of the expression that the "macro" generates (a generalization of `Exp`). The additional parameter `a` allows us to have an embedded language which is typed, whereas the toy language above is essentially untyped with a single expression type `Exp`. We note that using `IORef` for storing values works particularly well when the values have different types.

In fact, in reactive-banana, observable sharing is approached from the viewpoint of *caching*: The action of generating a fresh name and binding a value to it is viewed as something that may be executed only once and where the next execution only consists of retrieving the previous result. The module `Reactive.Banana.Prim.High.Cached` defines the type `Cached m a` which describes a general action of type `m a` that may be executed only once in this way. The function `cache :: m a -> Cached m a` takes an arbitrary action and wraps it in such a caching mechanism. In other words, `cache` implements the combination of `unsafePerformIO` and `lookup` as present above.

In turn, the types `Behavior` and `Events` are defined as `Cached` actions in the `Moment` monad. Specifically,

```hs
type Behavior a = Cached Moment (Latch a, Pulse ())
type Events a   = Cached Moment (Pulse a)
```

The `Moment` monad is used to build new `Pulse` and `Latch`, and caching implies that the `Pulse` for a name bound in the host language, say `let x = … :: Event A`, is built only once.

  [clsa]: http://www.cse.chalmers.se/~dave/papers/observable-sharing.pdf 
  [gill]: http://www.ittc.ku.edu/~andygill/papers/reifyGraph.pdf
  [oleg]: https://arxiv.org/abs/1109.0784