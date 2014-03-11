Julia v0.3.0 Release Notes
==========================

New language features
---------------------

  * Greatly enhanced performance for passing and returning `Tuple`s ([#4042]).

  * `Tuple`s (of `Integer`s, `Symbol`s, or `Bool`s) can now be used as type
    parameters ([#5164]).

  * Default "inner" constructors now accept any arguments. Constructors that
    look like `MyType(a, b) = new(a, b)` can and should be removed ([#4026]).

  * Expanded array type hierarchy, including ``StoredArray`` for all
    container-like arrays, and ``DenseArray`` for in-memory arrays with
    standard strided storage ([#987], [#2345]).

  * When reloading code, types whose definitions have not changed can be
    ignored in some cases.

  * Binary `~` now parses as a vararg macro call to `@~`.
    For example `x~y~z` => `@~ x y z` ([#4882]).

  * Structure fields can now be accessed by index ([#4806]).

  * Unicode identifiers are normalized (NFC) so that different encodings
    of equivalent strings are treated as the same identifier ([#5462]).

  * If a module contains a function `__init__()`, it will be called when
    the module is first loaded, and on process startup if a pre-compiled
    version of the module is present ([#1268]).

Library improvements
--------------------

  * `convert(Ptr{T1}, x::Array{T2})` is now deprecated unless `T1 == T2`
    or `T1 == None` ([#6073]).  (You can still explicitly `convert`
    one pointer type into another if needed.)

  * Well-behaved floating-point ranges ([#2333], [#5636]).
    Introduced the `FloatRange` type for floating-point ranges with a step,
    which will give intuitive/correct results for classically problematic
    ranges like `0.1:0.1:0.3`, `0.0:0.7:2.1` or `1.0:1/49:27.0`.

  * `mod2pi` function ([#4799], [#4862]).

  * New functions `minmax` and `extrema` ([#5275]).

  * `consume(p)` extended to `consume(p, args...)`, allowing it
    to optionally pass `args...` back to the producer ([#4775]).

  * `.juliarc.jl` is now loaded for both script and REPL execution ([#5076]).

  * The `Sys` module now includes convenient functions for working with
    dynamic library handles; `Sys.dllist` will list out all paths currently
    loaded via `dlopen`, and `Sys.dlpath` will lookup a path from a handle

  * `readdlm` treats multiple whitespace characters as a single delimiter
    by default (when no delimiter is specified). This is useful for reading
    fixed-width or messy whitespace-delimited data ([#5403]).

  * The Airy, Bessel, Hankel, and related functions (`airy*`,
    `bessel*`, `hankel*`) now detect errors returned by the underlying 
    AMOS library, throwing an `AmosException` in that case ([#4967]).

  * `methodswith` now returns an array of `Method`s ([#5464]) rather
    than just printing its results.

  * `errno([code])` function to get or set the C library's `errno`.

  * `GitHub` module for interacting with the GitHub API

  * Package improvements

    * Packages are now installed into `.julia/v0.3` by default (or
      whatever the current Julia version is), so that different
      versions of Julia can co-exist with incompatible packages.
      Existing `.julia` installations are unaffected unless `Pkg.init()`
      is run to re-create the package directories ([#3344], [#5737]).

    * `Pkg.submit(pkg[,commit])` function to automatically submit
      a GitHub pull request to the package author.

  * Collections improvements

    * `Array` assignment (e.g. `x[:] = y`) ignores singleton dimensions
      and allows the last dimension of one side to match all trailing dimensions
      of the other ([#4048], [#4383]).

    * `Dict(kv)` constructor for any iterator on `(key,value)` pairs.

    * Multi-key `Dict`s: `D[x,y...]` is now a synonym for `D[(x,y...)]`
      for associations `D` ([#4870]).

    * `push!` and `unshift!` can push multiple arguments ([#4782])

    * `writedlm` and `writecsv` now accept any iterable collection of
      iterable rows, in addition to `AbstractArray` arguments, and the
      ``writedlm`` delimiter can be any printable object (e.g. a
      ``String``) instead of just a ``Char``.

    * `isempty` now works for any iterable collection ([#5827]).

    * `unique` now accepts an optional `dim` argument for finding
      unique rows or columns of a matrix or regions of a
      multidimensional array ([#5811]).

  * `Number` improvements

    * The `ImaginaryUnit` type no longer exists. Instead, `im` is of type
      `Complex{Bool}`. Making this work required changing the semantics of
      boolean multiplication to approximately, `true * x = x` and
      `false * x = zero(x)`, which can itself be considered useful ([#5468]).

    * `big` is now vectorized ([#4766])

    * `nextpow` and `prevpow` now return the `a^n` values instead of the
      exponent `n` ([#4819])

    * Overflow detection in `parseint` ([#4874]).

    * `rand` now supports arbitrary `Ranges` arguments ([#5059]).

  * `String` improvements

    * Triple-quoted regex strings, `r"""..."""` ([#4934]).

    * New string type, `UTF16String` ([#4930]).

    * `CharString` is renamed to `UTF32String` ([#4943]).

    * `normalize_string` function to perform Unicode normalization,
      case-folding, and other transformations ([#5576]).

    * `pointer(s, i=1)` for `ByteString`, `UTF16String`, `UTF32String`,
      and `SubString`s thereof ([#5703]).

    * `bytestring` is automatically called on `String` arguments for
      conversion to `Ptr{Uint8}` in `ccall` ([#5677]).

  * `LinAlg` (linear algebra) improvements

      * Balancing options for eigenvector calculations for general matrices ([#5428]).

      * Mutating linear algebra functions no longer promote ([#5526]).

      * `condskeel` for Skeel condition numbers ([#5726]).

      * `norm(::Matrix)` no longer calculates a vector norm when the first 
        dimension is one ([#5545]); it always uses the operator (induced)
        matrix norm.

      * New `vecnorm(itr, p=2)` function that computes the norm of
        any iterable collection of numbers as if it were a vector of
        the same length.  This generalizes and replaces `normfro` ([#6057]),
        and `norm` is now type-stable ([#6056]).

      * `+` and `-` now require the sizes of the arrays to be the
        same: the operations no longer do broadcasting. New
        `UniformScaling` matrix type and identity `I` constant (#5810).

    * Sparse linear algebra

      * Faster sparse `kron` ([#4958]).

      * `sparse(A) \ B` now supports a matrix `B` of right-hand sides ([#5196]).

      * `eigs(A, sigma)` now uses shift-and-invert for nonzero shifts `sigma` and inverse iteration for `which="SM"`. If `sigma==nothing` (the new default), computes ordinary (forward) iterations. ([#5776])

    * Dense linear algebra for special matrix types

      * Interconversions between the special matrix types `Diagonal`, `Bidiagonal`,
        `SymTridiagonal`, `Triangular`, and `Triangular`, and `Matrix` are now allowed
        for matrices which are representable in both source and destination types. ([5e3f074b])

      * Allow for addition and subtraction over mixed matrix types, automatically promoting
        the result to the denser matrix type ([a448e080])

      * new algorithms for linear solvers and eigensystems of `Bidiagonal`
        matrices of generic element types ([#5277])

      * new algorithms for linear solvers, eigensystems and singular systems of `Diagonal`
        matrices of generic element types ([#5263])

      * new algorithms for linear solvers and eigensystems of `Triangular`
        matrices of generic element types ([#5255])

      * specialized `inv` and `det` methods for `Tridiagonal` and `SymTridiagonal`
        based on recurrence relations between principal minors ([#5358])

      * specialized `transpose`, `ctranspose`, `istril`, `istriu` methods for
        `Triangular` ([#5255]) and `Bidiagonal` ([#5277])

      * new LAPACK wrappers
        - condition number estimate `cond(A::Triangular)` ([#5255])

    * Dense linear algebra for generic matrix element types

      * LU factorization ([#5381] and [#5430])

      * QR factorization ([#5526])

  * New function `deleteat!` deletes a specified index or indices and
    returns the updated collection

  * `prevfloat` and `nextfloat` now saturate at -Inf and Inf, respectively, and
    have otherwise been fixed to follow the IEEE-754 standard functions `nextDown`
    and `nextUp` ([#5025]).

  * The `setenv` function for external processes now accepts a `dir` keyword
    argument for specifying the directory to start the child process in ([#4888]).

  * Constructors for collections (`Set`, `Dict`, etc.) now generally accept a
    single iterable argument giving the elements of the collection ([#4996], [#4871])

  * Ranges and arrays with the same elements are now unequal. This allows hashing
    and comparing ranges to be faster. ([#5778])

Deprecated or removed
---------------------

  * `Sys.shlib_ext` has been renamed to `Sys.dlext`

  * `dense` is deprecated in favor of `full` ([#4759])

  * The `Stat` type is renamed `StatStruct` ([#4670])

  * `set_rounding`, `get_rounding` and `with_rounding` now take an additional
    argument specifying the floating point type to which they apply. The old
    behaviour and `[get/set/with]_bigfloat_rounding` functions are deprecated ([#5007])

  * `cholpfact` and `qrpfact` are deprecated in favor of keyword arguments in 
    `cholfact(...,pivot=true)` and `qrfact(...,pivot=true)` ([#5330]) 

  * `symmetrize!` is deprecated in favor of `Base.LinAlg.copytri!` ([#5427])

  * `myindexes` has been renamed to `localindexes` ([#5475])

  * `factorize!` is deprecated in favor of `factorize`. ([#5526])
  
  * `nnz` is removed. Use `countnz` or `nfilled` instead ([#5538])

  * `setfield` is renamed `setfield!` ([#5748])

  * `put` and `take` are renamed `put!` and `take!` ([#5511])

  * `put!` now returns its first argument, the remote reference ([#5819])

[#4042]: https://github.com/JuliaLang/julia/issues/4042
[#5164]: https://github.com/JuliaLang/julia/issues/5164
[#4026]: https://github.com/JuliaLang/julia/issues/4026
[#4799]: https://github.com/JuliaLang/julia/issues/4799
[#4862]: https://github.com/JuliaLang/julia/issues/4862
[#4048]: https://github.com/JuliaLang/julia/issues/4048
[#4383]: https://github.com/JuliaLang/julia/issues/4383
[#4775]: https://github.com/JuliaLang/julia/issues/4775
[#4870]: https://github.com/JuliaLang/julia/issues/4870
[#4874]: https://github.com/JuliaLang/julia/issues/4874
[#4766]: https://github.com/JuliaLang/julia/issues/4766
[#4782]: https://github.com/JuliaLang/julia/issues/4782
[#4759]: https://github.com/JuliaLang/julia/issues/4759
[#4819]: https://github.com/JuliaLang/julia/issues/4819
[#4670]: https://github.com/JuliaLang/julia/issues/4670
[#5007]: https://github.com/JuliaLang/julia/issues/5007
[#5076]: https://github.com/JuliaLang/julia/issues/5076
[#5255]: https://github.com/JuliaLang/julia/issues/5255
[#5263]: https://github.com/JuliaLang/julia/issues/5263
[#4934]: https://github.com/JuliaLang/julia/issues/4934
[#4930]: https://github.com/JuliaLang/julia/issues/4930
[#4943]: https://github.com/JuliaLang/julia/issues/4943
[#4958]: https://github.com/JuliaLang/julia/issues/4958
[#5059]: https://github.com/JuliaLang/julia/issues/5059
[#5196]: https://github.com/JuliaLang/julia/issues/5196
[#5275]: https://github.com/JuliaLang/julia/issues/5275
[#5277]: https://github.com/JuliaLang/julia/issues/5277
[#987]: https://github.com/JuliaLang/julia/issues/987
[#2345]: https://github.com/JuliaLang/julia/issues/2345
[#5330]: https://github.com/JuliaLang/julia/issues/5330
[#4882]: https://github.com/JuliaLang/julia/issues/4882
[#4806]: https://github.com/JuliaLang/julia/issues/4806
[#5358]: https://github.com/JuliaLang/julia/pull/5358
[#5381]: https://github.com/JuliaLang/julia/pull/5381
[#5430]: https://github.com/JuliaLang/julia/pull/5430
[a448e080]: https://github.com/JuliaLang/julia/commit/a448e080dc736c7fb326426dfcb2528be36973d3
[5e3f074b]: https://github.com/JuliaLang/julia/commit/5e3f074b9173044a0a4219f9b285879ff7cec041
[#4967]: https://github.com/JuliaLang/julia/pull/4967
[#5428]: https://github.com/JuliaLang/julia/pull/5428
[#5468]: https://github.com/JuliaLang/julia/pull/5468
[#5025]: https://github.com/JuliaLang/julia/pull/5025
[#4888]: https://github.com/JuliaLang/julia/pull/4888
[#5475]: https://github.com/JuliaLang/julia/pull/5475
[#5526]: https://github.com/JuliaLang/julia/pull/5526
[#5538]: https://github.com/JuliaLang/julia/pull/5538
[#5726]: https://github.com/JuliaLang/julia/pull/5726
[#5811]: https://github.com/JuliaLang/julia/pull/5811
[#5462]: https://github.com/JuliaLang/julia/pull/5462
[#5403]: https://github.com/JuliaLang/julia/pull/5403
[#5464]: https://github.com/JuliaLang/julia/pull/5464
[#5827]: https://github.com/JuliaLang/julia/pull/5827
[#5576]: https://github.com/JuliaLang/julia/pull/5576
[#5703]: https://github.com/JuliaLang/julia/pull/5703
[#5427]: https://github.com/JuliaLang/julia/pull/5427
[#5748]: https://github.com/JuliaLang/julia/issues/5748
[#5511]: https://github.com/JuliaLang/julia/issues/5511
[#5776]: https://github.com/JuliaLang/julia/issues/5776
[#5819]: https://github.com/JuliaLang/julia/issues/5819
[#4871]: https://github.com/JuliaLang/julia/issues/4871
[#4996]: https://github.com/JuliaLang/julia/issues/4996
[#2333]: https://github.com/JuliaLang/julia/issues/2333
[#5636]: https://github.com/JuliaLang/julia/issues/5636
[#1268]: https://github.com/JuliaLang/julia/issues/1268
[#5677]: https://github.com/JuliaLang/julia/issues/5677
[#5545]: https://github.com/JuliaLang/julia/issues/5545
[#6057]: https://github.com/JuliaLang/julia/issues/6057
[#6056]: https://github.com/JuliaLang/julia/issues/6056
[#3344]: https://github.com/JuliaLang/julia/issues/3344
[#5737]: https://github.com/JuliaLang/julia/issues/5737
[#6073]: https://github.com/JuliaLang/julia/issues/6073
[#5778]: https://github.com/JuliaLang/julia/issues/5778

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

  * `import module: name1, name2, ...` ([#5214]).

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
---------------------

  * `julia-release-*` executables renamed to `julia-*`,
    and `libjulia-release` renamed to `libjulia` ([#4177]).

  * Packages will now be installed in `.julia/vX.Y`, where
    X.Y is the current Julia version.

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
[#5214]: https://github.com/JuliaLang/julia/issues/5214
[#3605]: https://github.com/JuliaLang/julia/pull/3605
[#3233]: https://github.com/JuliaLang/julia/pull/3233
[#4811]: https://github.com/JuliaLang/julia/pull/4811

[packages chapter]: http://docs.julialang.org/en/latest/manual/packages/
[sorting functions]: http://docs.julialang.org/en/latest/stdlib/sort/
[pairwise summation]: https://en.wikipedia.org/wiki/Pairwise_summation
