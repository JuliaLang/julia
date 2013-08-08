Julia v0.2.0 Release Notes
==========================

New features
------------

  * Immutable types (#13).

  * Triple-quoted string literals (#70).

  * Keyword & optional function arguments (#485, #1817).

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

  * mapslices (#2204)

  * Linear algebra updates (#2212)

  * varm, stdm (#2265)

  * names for C-compatible types: Cchar, Clong, etc (#2370).

  * Explicit relative importing (#2375)

  * Support optional RTLD flags in dlopen (#2380)

  * `unsafe_pointer_to_objref` (#2468) and `pointer_from_objref` (#2515)

  * Other new functions: readandwrite, versioninfo, methodswith, logdet

  * Sampling profiler (#2597).

  * More linear algebra fixes and eigensolver hooks for SymTridiagonal,
    Tridiagonal and Bidiagonal matrix types (#2606, #2608, #2609, #2611, #2678,
    #2713, #2720, #2725)

  * Documentation for writing packages (#2714, 2769, #2791) and linear algebra
    (#2807)

  * MPFR-based BigFloats (#2814).

  * priority queues (#2920)

  * erfinv and erfcinv functions (#2987).

  * change to isinteger, isreal, etc.: now different than Matlab/Octave (#3071).

  * Transitive comparison of floats with rationals (#3102).

  * `quadgk` 1d-integration routine (#3140).

  * New half-precision IEEE floating-point type, Float16 (#3467).

  * Sort API changes (#3665 and others).

  * readbytes and readbytes! functions (#3878).

  * Multimedia I/O API (display, writemime, etcetera) (#3932).

  * `flush_cstdio` function (#3949).


Improvements
------------

  * Fast primality testing (da670c4).

  * isreadable/iswritable functions added for more IO types (#3872).


Deprecated or removed
---------------------

  * `!` was added to the name of many mutating functions, e.g., `push` was
    renamed `push!` (#907).

  * `ref` renamed to `getindex`, and `assign` to `setindex!` (#1484).

  * `writeable` renamed to `writable` (#3874).

Bugfixes
--------

Too numerous to mention.
