Julia v0.2.0 Release Notes
==========================

The 0.2 release brings improvements to many areas of Julia. Among the
most visible changes are support for 64-bit Windows, keyword arguments
to functions, immutable types, a redesigned and polished package
manager, a multimedia interface supporting usage of Julia in IPython,
a built-in profiler, and major improvements to Julia's linear algebra,
I/O, and parallel capabilities. These are accompanied by many other
changes adding new features, enhancing the library's consistency,
improving performance, increasing test coverage, easing installation,
and expanding the documentation. While not part of Julia proper, the
package ecosystem has also grown and matured considerably since the
0.1 release. See below for more information about the long list of
changes that improve Julia's usability and performance.

New language features
---------------------

  * Keyword & optional function arguments ([#485], [#1817]).

  * Immutable types ([#13]).

  * Triple-quoted string literals ([#70]).

  * New infix operator `in` (e.g. `x in S`), and corresponding function
    `in(x,S)`, replacing `contains(S,x)` function ([#2703]).

  * New variable bindings on each for loop and comprehension iteration ([#1571]).
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

  * Explicit relative importing ([#2375]).

  * Methods can be added to functions in other modules using dot syntax,
    as in `Foo.bar(x) = 0`.

  * A semicolon is now allowed after an `import` or `using` statement ([#4130]).

  * In an interactive session (REPL), you can use `;cmd` to run `cmd` via an interactive
    shell. For example:

        julia> ;ls
        CONTRIBUTING.md  Makefile           VERSION      deps/      julia@  ui/
        DISTRIBUTING.md  NEWS.md            Windows.inc  doc/       src/    usr/
        LICENSE.md       README.md          base/        etc/       test/
        Make.inc         README.windows.md  contrib/     examples/  tmp/

New library functions
---------------------

  * Sampling profiler ([#2597]).

  * Functions for examining stages of the compiler's output:
    `code_lowered`, `code_typed`, `code_llvm`, and `code_native`.

  * Multimedia I/O API (display, writemime, etcetera) ([#3932]).

  * MPFR-based `BigFloat` ([#2814]), and many new `BigFloat` operations.

  * New half-precision IEEE floating-point type, `Float16` ([#3467]).

  * Support for setting floating-point rounding modes ([#3149]).

  * `methodswith` shows all methods with an argument of specific type.

  * `mapslices` provides a general way to perform operations on slices of arrays ([#2204]).

  * `repeat` function for constructing Arrays with repeated elements ([#3605]).

  * `Collections.PriorityQueue` type and `Collections.heap` functions ([#2920]).

  * `quadgk` 1d-integration routine ([#3140]).

  * `erfinv` and `erfcinv` functions ([#2987]).

  * `varm`, `stdm` ([#2265]).

  * `digamma`, `invdigamma`, `trigamma` and `polygamma` for calculating derivatives of `gamma` function ([#3233]). 

  * `logdet` ([#3070]).

  * Names for C-compatible types: `Cchar`, `Clong`, etc. ([#2370]).
  
  * `cglobal` to access global variables ([#1815]).

  * `unsafe_pointer_to_objref` ([#2468]) and `pointer_from_objref` ([#2515]).

  * `readandwrite` for external processes.

  * I/O functions `readbytes` and `readbytes!` ([#3878]).

  * `flush_cstdio` function ([#3949]).

  * ClusterManager makes it possible to support different types of compute clusters
    ([#3649], [#4014]).

  * `rmprocs` for removing processors from a parallel computing session.
    The system can also tolerate to some extent processors that die unexpectedly
    ([#3050]).

  * `interrupt` for interrupting worker processes ([#3819]).
  
  * `timedwait` does a polled wait for an event till a specified timeout.
  
  * `Condition` type with `wait` and `notify` functions for `Task` synchronization.

  * `versioninfo` provides detailed version information, especially useful when
    reporting and diagnosing bugs.

  * `detach` for running child processes in a separate process group.

  * `setenv` for passing environment variables to child processes.

  * `ifelse` eagerly-evaluated conditional function, especially useful for
    vectorized conditionals.

Library improvements
--------------------

  * `isequal` now returns `false` for numbers of different types.
    This makes it much easier to define hashing for new numeric types.
    Uses of `Dict` with numeric keys might need to change
    to account for this increased strictness.

  * A redesigned and rewritten `Pkg` system is much more robust in case of problems.
    The basic interface to adding and removing package requirements remains the
    same, but great deal of additional functionality for developing packages in-place
    was added. See the new [packages chapter] in the manual for further details.

  * Sorting API updates ([#3665]) – see [sorting functions].

  * The `delete!(d::Dict, key)` function has been split into separate `pop!`
    and `delete!` functions ([#3439]).
    `pop!(d,key)` removes `key` from `d` and returns the value that was associated with it;
    it throws an exception if `d` does not contain `key`.
    `delete!(d,key)` removes `key` from `d` and succeeds regardless of whether `d`
    contained `key` or not, returning `d` itself in either case.

  * Linear-algebra factorization routines (`lu`, `chol`, etc.) now return
    `Factorization` objects (and `lud`, `chold`, etc. are deprecated; [#2212]).

  * A number of improvements to sparse matrix capabilities and sparse linear algebra.

  * More linear algebra fixes and eigensolver hooks
    for `SymTridiagonal`, `Tridiagonal` and `Bidiagonal` matrix types
    ([#2606], [#2608], [#2609], [#2611], [#2678], [#2713], [#2720], [#2725]).

  * Change `integer_valued`, `real_valued`, and so on to `isinteger`, `isreal`,
    and so on, and semantics of the later are now value-based rather than type-based,
    unlike MATLAB/Octave ([#3071]). `isbool` and `iscomplex` are eliminated in favor
    of a general `iseltype` function.

  * Transitive comparison of floats with rationals ([#3102]).

  * Fast prime generation with `primes` and fast primality testing with `isprime`.

  * `sum` and `cumsum` now use [pairwise summation] for better accuracy ([#4039]).

  * Dot operators (`.+`, `.*` etc.) now broadcast singleton dimensions of array arguments.
    This behavior can be applied to any function using `broadcast(f, ...)`.

  * `combinations`, `permutations`, and `partitions` now return iterators instead of a task,
    and `integer_partitions` has been renamed to `partitions` ([#3989], [#4055]).

  * `isreadable`/`iswritable` methods added for more IO types ([#3872]).

  * Much faster and improved `readdlm` and `writedlm` ([#3350], [#3468], [#3483]).

  * Faster `matchall` ([#3719]), and various string and regex improvements.

  * Documentation of advanced linear algebra features ([#2807]).

  * Support optional RTLD flags in `dlopen` ([#2380]).

  * `pmap` now works with any iterable collection.

  * Options in `pmap` for retrying or ignoring failed tasks.

  * New `sinpi(x)` and `cospi(x)` functions to compute sine and cosine of `pi*x`
    more accurately ([#4112]).

  * New implementations of elementary complex functions
    `sqrt`, `log`, `asin`, `acos`, `atan`, `tanh`, `asinh`, `acosh`, `atanh`
    with correct branch cuts ([#2891]).

  * Improved behavior of `SubArray` ([#4412], [#4284], [#4044], [#3697], [#3790],
    [#3148], [#2844], [#2644] and various other fixes).

  * New convenience functions in graphics API.

  * Improved backtraces on Windows and OS X.

Deprecated or removed
---------------------

  * Methods of `min` and `max` that do reductions were renamed to
    `minimum` and `maximum`. `min(x)` is now `minimum(x)`, and
    `min(x,(),dim)` is now `minimum(x,dim)`. ([#4235])

  * `ComplexPair` was renamed to `Complex` and made `immutable`,
    and `Complex128` and so on are now aliases to the new `Complex` type.

  * `!` was added to the name of many mutating functions,
    e.g., `push` was renamed `push!` ([#907]).

  * `ref` renamed to `getindex`, and `assign` to `setindex!` ([#1484]).

  * `writeable` renamed to `writable` ([#3874]).

  * `logb` and `ilogb` renamed to `exponent` ([#2516]).

  * `quote_string` became a method of `repr`.

  * `safe_char`, `check_ascii`, and `check_utf8` replaced by
    `is_valid_char`, `is_valid_ascii`, and `is_valid_utf8`, respectively.

  * `each_line`, `each_match`, `begins_with`, `ends_with`, `parse_float`,
    `parse_int`, and `seek_end` replaced by: `eachline`, `eachmatch`, and so on
    (`_` was removed) ([#1539]).

  * `parse_bin(s)` replaced by `parseint(s,2)`;
    `parse_oct(s)` replaced by `parseint(s,8)`;
    `parse_hex(s)` replaced by `parseint(s,16)`.

  * `findn_nzs` replaced by `findnz` ([#1539]).

  * `DivideByZeroError` replaced by `DivideError`.

  * `addprocs_ssh`, `addprocs_ssh_tunnel`, and `addprocs_local`
    replaced by `addprocs` (with keyword options).

  * `remote_call`, `remote_call_fetch`, and `remote_call_wait`
    replaced by `remotecall`, `remotecall_fetch`, and `remotecall_wait`.

  * `has` replaced by `in` for sets and by `haskey` for dictionaries.

  * `diagmm` and `diagmm!` replaced by `scale` and `scale!` ([#2916]).

  * `unsafe_ref` and `unsafe_assign` replaced by `unsafe_load` and `unsafe_store!`.

  * `add_each!` and `del_each!` replaced by `union!` and `setdiff!`.

  * `isdenormal` renamed to `issubnormal` ([#3105]).

  * `expr` replaced by direct call to `Expr` constructor.

  * `|`, `&`, `$`, `-`, and `~` for sets replaced by
    `union`, `intersect`, `symdiff`, `setdiff`, and `complement` ([#3272]).

  * `square` function removed.

  * `pascal` function removed.

  * `add` and `add!` for `Set` replaced by `push!`.

  * `ls` function deprecated in favor of `readdir` or `;ls` in the REPL.

  * `start_timer` now expects arguments in units of seconds, not milliseconds.

  * Shell redirection operators `|`, `>`, and `<` eliminated in favor of a new
    operator `|>` ([#3523]).

  * `amap` is deprecated in favor of new `mapslices` functionality.

  * The `Reverse` iterator was removed since it did not work in many cases.

  * The `gcd` function now returns a non-negative value regardless of
    the argument signs, and various other sign problems with `invmod`,
    `lcm`, `gcdx`, and `powermod` were fixed ([#4811]).

Miscellaneous changes
--------------------------------

  * `julia-release-*` executables renamed to `julia-*`,
    and `libjulia-release` renamed to `libjulia` ([#4177]).

Bugfixes and performance updates
--------------------------------

Too numerous to mention.

[#13]: https://github.com/JuliaLang/julia/issues/13
[#70]: https://github.com/JuliaLang/julia/issues/70
[#485]: https://github.com/JuliaLang/julia/issues/485
[#907]: https://github.com/JuliaLang/julia/issues/907
[#1484]: https://github.com/JuliaLang/julia/issues/1484
[#1539]: https://github.com/JuliaLang/julia/issues/1539
[#1539]: https://github.com/JuliaLang/julia/issues/1539
[#1571]: https://github.com/JuliaLang/julia/issues/1571
[#1815]: https://github.com/JuliaLang/julia/issues/1815
[#1817]: https://github.com/JuliaLang/julia/issues/1817
[#2204]: https://github.com/JuliaLang/julia/issues/2204
[#2212]: https://github.com/JuliaLang/julia/issues/2212
[#2265]: https://github.com/JuliaLang/julia/issues/2265
[#2370]: https://github.com/JuliaLang/julia/issues/2370
[#2375]: https://github.com/JuliaLang/julia/issues/2375
[#2380]: https://github.com/JuliaLang/julia/issues/2380
[#2468]: https://github.com/JuliaLang/julia/issues/2468
[#2515]: https://github.com/JuliaLang/julia/issues/2515
[#2516]: https://github.com/JuliaLang/julia/issues/2516
[#2597]: https://github.com/JuliaLang/julia/issues/2597
[#2606]: https://github.com/JuliaLang/julia/issues/2606
[#2608]: https://github.com/JuliaLang/julia/issues/2608
[#2609]: https://github.com/JuliaLang/julia/issues/2609
[#2611]: https://github.com/JuliaLang/julia/issues/2611
[#2644]: https://github.com/JuliaLang/julia/issues/2644
[#2678]: https://github.com/JuliaLang/julia/issues/2678
[#2703]: https://github.com/JuliaLang/julia/issues/2703
[#2713]: https://github.com/JuliaLang/julia/issues/2713
[#2714]: https://github.com/JuliaLang/julia/issues/2714
[#2720]: https://github.com/JuliaLang/julia/issues/2720
[#2725]: https://github.com/JuliaLang/julia/issues/2725
[#2769]: https://github.com/JuliaLang/julia/issues/2769
[#2791]: https://github.com/JuliaLang/julia/issues/2791
[#2807]: https://github.com/JuliaLang/julia/issues/2807
[#2814]: https://github.com/JuliaLang/julia/issues/2814
[#2844]: https://github.com/JuliaLang/julia/issues/2844
[#2891]: https://github.com/JuliaLang/julia/issues/2891
[#2916]: https://github.com/JuliaLang/julia/issues/2916
[#2920]: https://github.com/JuliaLang/julia/issues/2920
[#2987]: https://github.com/JuliaLang/julia/issues/2987
[#3050]: https://github.com/JuliaLang/julia/issues/3050
[#3070]: https://github.com/JuliaLang/julia/issues/3070
[#3071]: https://github.com/JuliaLang/julia/issues/3071
[#3102]: https://github.com/JuliaLang/julia/issues/3102
[#3105]: https://github.com/JuliaLang/julia/issues/3105
[#3140]: https://github.com/JuliaLang/julia/issues/3140
[#3148]: https://github.com/JuliaLang/julia/issues/3148
[#3149]: https://github.com/JuliaLang/julia/issues/3149
[#3272]: https://github.com/JuliaLang/julia/issues/3272
[#3350]: https://github.com/JuliaLang/julia/issues/3350
[#3439]: https://github.com/JuliaLang/julia/issues/3439
[#3467]: https://github.com/JuliaLang/julia/issues/3467
[#3468]: https://github.com/JuliaLang/julia/issues/3468
[#3483]: https://github.com/JuliaLang/julia/issues/3483
[#3523]: https://github.com/JuliaLang/julia/issues/3523
[#3649]: https://github.com/JuliaLang/julia/issues/3649
[#3665]: https://github.com/JuliaLang/julia/issues/3665
[#3697]: https://github.com/JuliaLang/julia/issues/3697
[#3719]: https://github.com/JuliaLang/julia/issues/3719
[#3790]: https://github.com/JuliaLang/julia/issues/3790
[#3819]: https://github.com/JuliaLang/julia/issues/3819
[#3872]: https://github.com/JuliaLang/julia/issues/3872
[#3874]: https://github.com/JuliaLang/julia/issues/3874
[#3878]: https://github.com/JuliaLang/julia/issues/3878
[#3932]: https://github.com/JuliaLang/julia/issues/3932
[#3949]: https://github.com/JuliaLang/julia/issues/3949
[#3989]: https://github.com/JuliaLang/julia/issues/3989
[#4014]: https://github.com/JuliaLang/julia/issues/4014
[#4039]: https://github.com/JuliaLang/julia/issues/4039
[#4044]: https://github.com/JuliaLang/julia/issues/4044
[#4055]: https://github.com/JuliaLang/julia/issues/4055
[#4112]: https://github.com/JuliaLang/julia/issues/4112
[#4130]: https://github.com/JuliaLang/julia/issues/4130
[#4177]: https://github.com/JuliaLang/julia/issues/4177
[#4235]: https://github.com/JuliaLang/julia/issues/4235
[#4284]: https://github.com/JuliaLang/julia/issues/4284
[#4412]: https://github.com/JuliaLang/julia/issues/4412

[packages chapter]: http://docs.julialang.org/en/latest/manual/packages/
[sorting functions]: http://docs.julialang.org/en/latest/stdlib/sort/
[pairwise summation]: https://en.wikipedia.org/wiki/Pairwise_summation
