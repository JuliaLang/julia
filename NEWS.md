Julia v0.2.0 Release Notes
==========================

New language features
---------------------

  * Keyword & optional function arguments (#485, #1817).

  * Immutable types (#13).

  * Triple-quoted string literals (#70).

  * New variable bindings on each for loop and comprehension iteration (#1571).
    For example, before this change:

        julia> map(f->f(), { ()->i for i=1:3 })
        3-element Any Array:
         3
         3
         3

    and after:

        julia> map(f->f(), { ()->i for i=1:3 })
        3-element Any Array:
         1
         2
         3

  * Explicit relative importing (#2375)


New library functions
---------------------

  * Sampling profiler (#2597).

  * Multimedia I/O API (display, writemime, etcetera) (#3932).

  * Sort API changes (#3665 and others).

  * New half-precision IEEE floating-point type, `Float16` (#3467).

  * `mapslices` (#2204)

  * `varm`, `stdm` (#2265)

  * `Collections.PriorityQueue` type and `Collections.heap` functions (#2920).

  * `quadgk` 1d-integration routine (#3140).

  * `erfinv` and `erfcinv` functions (#2987).

  * names for C-compatible types: `Cchar`, `Clong`, etc (#2370).

  * `unsafe_pointer_to_objref` (#2468) and `pointer_from_objref` (#2515)

  * Other new functions: `readandwrite`, `versioninfo`, `methodswith`, `logdet`

  * `readbytes` and `readbytes!` functions (#3878).

  * `flush_cstdio` function (#3949).


Library improvements
--------------------

  * MPFR-based `BigFloat` (#2814), and many new `BigFloat` operations.

  * Linear-algebra factorization routines (`lu`, `chol`, etc.) now
    return `Factorization` objects (and `lud`, `chold`, etc. are
    deprecated) (#2212).

  * More linear algebra fixes and eigensolver hooks for
    `SymTridiagonal`, `Tridiagonal` and `Bidiagonal` matrix types
    (#2606, #2608, #2609, #2611, #2678, #2713, #2720, #2725)

  * Change to `isinteger`, `isreal`, etc.: now different than Matlab/Octave (#3071).

  * Transitive comparison of floats with rationals (#3102).

  * Fast primality testing (da670c4).

  * `sum` and `cumsum` now use pairwise summation for better accuracy (#4039).

  * `isreadable`/`iswritable` functions added for more IO types (#3872).

  * `combinations`, `permutations`, and `partitions` now return
    iterators instead of a task. and `integer_partitions` has been
    renamed to `partitions` (#3989 and #4055).

  * Documentation for writing packages (#2714, 2769, #2791) and linear algebra
    (#2807)

  * Support optional RTLD flags in `dlopen` (#2380)


Deprecated or removed
---------------------

  * `!` was added to the name of many mutating functions, e.g., `push` was
    renamed `push!` (#907).

  * `ref` renamed to `getindex`, and `assign` to `setindex!` (#1484).

  * `writeable` renamed to `writable` (#3874).

  * Many others ...


Bugfixes
--------

Too numerous to mention.
