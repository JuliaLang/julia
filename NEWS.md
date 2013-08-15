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

  * `rmprocs` for removing processors from a parallel computing session. The
    system can also tolerate to some extent processors that die unexpectedly
    (#3050).

  * `interrupt` for interrupting worker processes

  * `Condition` type with `wait` and `notify` functions for synchronizing `Task`s

  * Functions for examining stages of the compiler's output:
    `code_lowered`, `code_typed`, `code_llvm`, and `code_native`

Library improvements
--------------------

  * MPFR-based `BigFloat` (#2814), and many new `BigFloat` operations.

  * `delete!(d::Dict, key)` has now been split into separate `pop!`
    and `delete!` variants: the former returns the deleted value and
    behaves like the old `delete!`, and the latter returns `d` and
    does not throw an exception if `key` was not found (#3439).

  * Linear-algebra factorization routines (`lu`, `chol`, etc.) now
    return `Factorization` objects (and `lud`, `chold`, etc. are
    deprecated) (#2212).

  * More linear algebra fixes and eigensolver hooks for
    `SymTridiagonal`, `Tridiagonal` and `Bidiagonal` matrix types
    (#2606, #2608, #2609, #2611, #2678, #2713, #2720, #2725)

  * Change `integer_valued`, `real_valued`, and so on to `isinteger`,
    `isreal`, and so on, and semantics of the later are now value-based
    rather than type-based, unlike Matlab/Octave (#3071).  `isbool` and
    `iscomplex` are eliminated in favor of general `iseltype` function.

  * Transitive comparison of floats with rationals (#3102).

  * Fast primality testing (da670c4).

  * `sum` and `cumsum` now use pairwise summation for better accuracy (#4039).

  * Dot operators (`.+`, `.*` etc.) now broadcast singleton dimensions of
    array arguments. This behavior can be applied to any function using
    `broadcast(f, ...)`.

  * `isreadable`/`iswritable` functions added for more IO types (#3872).

  * `combinations`, `permutations`, and `partitions` now return
    iterators instead of a task. and `integer_partitions` has been
    renamed to `partitions` (#3989 and #4055).

  * Documentation for writing packages (#2714, 2769, #2791) and linear algebra
    (#2807)

  * Support optional RTLD flags in `dlopen` (#2380)

  * Support for starting processes via custom cluster managers

  * Options in `pmap` for retrying or ignoring failed tasks

Deprecated or removed
---------------------

  * `ComplexPair` was renamed to `Complex` and made `immutable`, and
    `Complex128` and so on are now aliases to the new `Complex` type.

  * `!` was added to the name of many mutating functions, e.g., `push` was
    renamed `push!` (#907).

  * `ref` renamed to `getindex`, and `assign` to `setindex!` (#1484).

  * `writeable` renamed to `writable` (#3874).

  * `logb` and `ilogb` renamed to `exponent` (#2516).

  * `quote_string` renamed to `repr`.

  * `safe_char`, `check_ascii`, and `check_utf8` replaced by
    `is_valid_char`, `is_valid_ascii`, and `is_valid_utf8`,
    respectively.

  * `each_line, `each_match`, `begins_with`, `ends_with`,
    `parse_float`, `parse_int`, and `seek_end` replaced by: `eachline`,
    `eachmatch`, and so on (`_` was removed) (#1539).

  * `parse_bin(s)` replaced by `parseint(s,2)`; `parse_oct(s)`
    replaced by `parseint(s,8)`; `parse_hex(s)` replaced by
    `parseint(s,16)`.

  * `findn_nzs` replaced by `findnz` (#1539).

  * `DivideByZeroError` replaced by `DivideError`.

  * `addprocs_ssh`, `addprocs_ssh_tunnel`, and `addprocs_local`
    replaced by `addprocs` (with keyword options).

  * `remote_call`, `remote_call_fetch`, and `remote_call_wait` replaced
    by `remotecall`, `remotecall_fetch`, and `remotecall_wait`.

  * `has` replaced by `contains` for sets and by `haskey` for dictionaries.

  * `diagmm` and `diagmm!` replaced by `scale` and `scale!` (#2916).

  * `unsafe_ref` and `unsafe_assign` replaced by `unsafe_load` and
    `unsafe_store!`.

  * `add_each!` and `del_each!` replaced by `union!` and `setdiff!`

  * `isdenormal` renamed to `issubnormal` (#3105).

  * `expr` replaced by direct call to `Expr` constructor.

  * `|`, `&`, `$`, `-`, and `~` for sets replaced by `union`, `intersect`
    `symdiff`, `setdiff`, and `complement` (#3272).

  * `square` function removed.

  * `pascal` function removed.

  * `add` and `add!` for `Set` replaced by `push!`.

  * Deprecated `ls` function in favor of `readdir` or `; ls` in the REPL.

  * `start_timer` now expects arguments in units of seconds, not milliseconds.

  * Shell redirection operators `|`, `>`, and `<` were eliminated in favor
    of a new operator `|>` (#3523).

  * `amap` is deprecated in favor of new `mapslices` functionality.

Bugfixes
--------

Too numerous to mention.
