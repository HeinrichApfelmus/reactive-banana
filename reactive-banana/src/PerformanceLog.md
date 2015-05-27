Performance Log
===============

The following procedure seems to be effective for improving performance:

* Run profiling tools to see where changes are needed.
* Change the code and record the new running time in a log to see how it improves.
* Rinse and repeat. Stop when the changes become too intrusive or are no longer worth the effort.

This is the log for sessions dedicated to the improvement of `reactive-banana`.

Jan 23, 2014
------------
Starting point

    > benchmark 100 3000

* Monomorphic queue
    ~ 2.09 secs

* Specialize `traverseDependencies`
    ~ 2.03 secs

* Remove writer from `EvalP` monad
    ~ 1.44 secs

* Choose better queue implementation from `pqeue` package
    ~ 1.10 secs

* Correct queue implementation so that it doesn't have duplicate elements anymore.
    ~ 1.26 secs
    ~ 20% of the running time seems to be spent in queue `insert` operation, so subsequent improvements cannot improve upon that.

* Turn `EvalP` into a reader monad to avoid lifting `Build` in many cases.
    ~ 1.03 secs


Jan 27, 2014
------------
Starting point

    > benchmark 100 3000

with all Behaviors kept in scope, so that they cannot be garbage collected. Profile shows 60000 entries of `evaluateNodeL` and 441000 entries for `go`.

* Starting point
    ~ 5.22 secs. Space profile shows many allocations spent on `runBuild`.

* Implementation of `Build` using `ReaderWriterIOT`
    ~ 1.67 secs

* Implementation of `EvalP` using `ReaderWriterIOT`. The `Lazy.Vault` state is threaded by hand. 
    ~ 0.79 secs

* A new type `Ref` represents mutable references that, unlike `IORef`, can be hashed and tested for equality.
    ~ 0.92 secs


Jan 29, 2014
------------
Starting point

    > benchmark 100 3000

* Starting point
    ~ 0.90 secs. Time profile shows that most time is spent in
    closures like `>>=.\` and `>>-.\.\`.

* Export only `step` from `Reactive.Banana.Prim.Evaluation`.
  Unroll the `EvalP` monad transformer.

    ~ 0.80 secs. Time profile still shows that most time is spent in monad plumbing.

    GHC Core indicates that `go` is not fully saturated and
    recursively creates a closure.
    Also, seemingly trivial closures are being allocated and deallocated,
    along the lines of
    
        let x = case .. of { .. -> (\_ _ -> y) } in y a b 

* Lazy pattern match on the `Ref` in `put`, `get` and `modify'`.

    ~ 0.78 secs

    The GHC Core has improved with regards to `put` and `get`,
    but the inner loops still allocates closures.

* Implement a Reader/Writer/State monad `RWSIO` with a single reader-like argument.

    ~ 0.72 secs

    The compiled `step_go` code still allocates a closure for the pattern match on
    `P`, `O` and `L`, and still connects it with a closure for `insterNodes`.

* Two functions `wrapEvalP` and `unwrapEvalP` can access the `RWSIOT` parameter directly. This allows us to reorder argument and reduce sharing.

    This finally gets rid of the closure for the pattern match on the `evaluteNode` function.
    
* Implement `insertNodes` as a recursive loop.

    ~ 0.54 secs

    The program still spends a lot of time in `bindR.\` and `bindR.\.\`,
    but at least it's not as bad as before anymore.
