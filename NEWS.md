Julia v0.5.0 Release Notes
==========================

New language features
---------------------

  * Generator expressions: `f(i) for i in 1:n` ([#4470]). This returns an iterator
    that computes the specified values on demand. This is useful for computing, e.g.
    `sum(f(i) for i in 1:n)` without creating an intermediate array of values.

  * Generators and comprehensions support filtering using `if` ([#550]) and nested
    iteration using multiple `for` keywords ([#4867]).

  * Fused broadcasting syntax: ``f.(args...)`` is equivalent to ``broadcast(f, args...)`` ([#15032]),
    and nested `f.(g.(args...))` calls are fused into a single `broadcast` loop ([#17300]).
    Similarly, the syntax `x .= ...` is equivalent to a `broadcast!(identity, x, ...)`
    call and fuses with nested "dot" calls; also, `x .+= y` and similar is now
    equivalent to `x .= x .+ y`, rather than `x = x .+ y` ([#17510]).

  * Macro expander functions are now generic, so macros can have multiple definitions
    (e.g. for different numbers of arguments, or optional arguments) ([#8846], [#9627]).
    However note that the argument types refer to the syntax tree representation, and not
    to the types of run time values.

  * Varargs functions like `foo{T}(x::T...)` may now restrict the number
    of such arguments using `foo{T,N}(x::Vararg{T,N})` ([#11242]).

  * `x ∈ X` is now a synonym for `x in X` in `for` loops and comprehensions,
    as it already was in comparisons ([#13824]).

  * The `PROGRAM_FILE` global is now available for determining the name of the running script ([#14114]).

  * The syntax `x.:sym` (e.g. `Base.:+`) is now supported, and `x.(:sym)` is deprecated ([#15032]).

  * Function return type syntax `function f()::T` has been added ([#1090]). Values returned
    from a function with such a declaration will be converted to the specified type `T`.

  * Experimental support for arrays with indexing starting at values
    different from 1. The array types are expected to be defined in
    packages, but now Julia provides an API for writing generic
    algorithms for arbitrary indexing schemes ([#16260]).

  * Many more operators now support `.` prefixes (e.g. `.≤`) ([#17393]).  However,
    users are discouraged from overloading these, since they are mainly parsed
    in order to implement backwards compatibility with planned automatic
    broadcasting of dot operators in Julia 0.6 ([#16285]).  Explicitly qualified
    operator names like `Base.≤` should now use `Base.:≤` (prefixed by `@compat`
    if you need 0.4 compatibility via the `Compat` package).

Language changes
----------------

  * Each function and closure now has its own type. The captured variables of a closure
    are fields of its type. `Function` is now an abstract type, and is the default supertype
    of functions and closures. All functions, including anonymous functions,
    are generic and support all features (e.g. keyword arguments).
    Instead of adding methods to `call`, methods are added by type using the syntax
    `(::ftype)(...) = ...`. `call` is deprecated ([#13412]).

  * `using` and `import` are now case-sensitive even on case-insensitive filesystems
    (common on Mac and Windows) ([#13542]).

  * Relational algebra symbols are now allowed as infix operators ([#8036]):
    `⨝`, `⟕`, `⟖`, `⟗` for joins and `▷` for anti-join.

  * A warning is always given when a method is overwritten (previously, this was done
    only when the new and old definitions were in separate modules) ([#14759]).

  * The `if` keyword cannot be followed immediately by a line break ([#15763]).

  * Juxtaposition of numeric literals ending in `.` (e.g. `1.x`) is no longer
    allowed ([#15731]).

  * The built-in `NTuple` type has been removed; `NTuple{N,T}` is now
    implemented internally as `Tuple{Vararg{T,N}}` ([#11242]).

  * Use of the syntax `x::T` to declare the type of a local variable is deprecated.
    In the future this will always mean type assertion, and declarations should use
    `local x::T` instead ([#16071]).
    When `x` is global, `x::T = ...` and `global x::T` used to mean type assertion,
    but this syntax is now reserved for type declaration ([#964]).

Command-line option changes
---------------------------

Compiler/Runtime improvements
-----------------------------

  * Machine SIMD types can be represented in Julia as a homogeneous tuple of `VecElement` ([#15244]).

New architectures
-----------------

  This release greatly improves support for ARM, and introduces support for Power.

  * [ARM](https://github.com/JuliaLang/julia/issues?utf8=%E2%9C%93&q=label%3Aarm):
    [#14194], [#14519], [#16645], [#16621]

  * [Power](https://github.com/JuliaLang/julia/issues?utf8=%E2%9C%93&q=label%3Apower):
    [#16455], [#16404]

Breaking changes
----------------

This section lists changes that do not have deprecation warnings.

  * The assignment operations `.+=`, `.*=` and so on now generate calls
    to `broadcast!` on the left-hand side (or call to `view(a, ...)` on the left-hand side
    if the latter is an indexing expression, e.g. `a[...]`). This means that they will fail
    if the left-hand side is immutable (or does not support `view`), and will otherwise
    change the left-hand side in-place ([#17510], [#17546]).

  * Method ambiguities no longer generate warnings when files are loaded,
    nor do they dispatch to an arbitrarily-chosen method; instead, a call that
    cannot be resolved to a single method results in a `MethodError` at run time,
    rather than the previous definition-time warning. ([#6190])

  * Array comprehensions preserve the dimensions of the input ranges. For example,
    `[2x for x in A]` will have the same dimensions as `A` ([#16622]).

  * The result type of an array comprehension depends only on the types of elements
    computed, instead of using type inference ([#7258]). If the result is empty, then
    type inference is still used to determine the element type.

  * `reshape` is now defined to always share data with the original array.
    If a reshaped copy is needed, use `copy(reshape(a))` or `copy!` to a new array of
    the desired shape ([#4211]).

  * `mapslices` now re-uses temporary storage. Recipient functions that expect
    input slices to be persistent should copy data to other storage ([#17266]).
    All usages of `mapslices` should be carefully audited since this change can cause
    silent, incorrect behavior, rather than failing noisily.

  * Local variables and arguments are represented in lowered code as numbered `Slot`
    objects instead of as symbols ([#15609]).

  * The information that used to be in the `ast` field of the `LambdaStaticData` type
    is now divided among the fields `code`, `slotnames`, `slottypes`, `slotflags`,
    `gensymtypes`, `rettype`, `nargs`, and `isva` in the `LambdaInfo` type ([#15609]).

  * `A <: B` is parsed as `Expr(:(<:), :A, :B)` in all cases ([#9503]).
    This also applies to the `>:` operator.

  * Simple 2-argument comparisons like `A < B` are parsed as calls instead of using the
    `:comparison` expression type ([#15524]). The `:comparison` expression type is still
    produced in ASTs when comparisons are chained (e.g. `A < B ≤ C`).

Library improvements
--------------------

  * Strings ([#16107]):

    * The `UTF8String` and `ASCIIString` types have been merged into a single
      `String` type ([#16058]).  Use `isascii(s)` to check whether
      a string contains only ASCII characters. The `ascii(s)` function now
      converts `s` to `String`, raising an `ArgumentError` exception if `s` is
      not pure ASCII.

    * The `UTF16String` and `UTF32String` types and corresponding `utf16` and
      `utf32` converter functions have been removed from the standard library.
      If you need these types, they have been moved to the
      [LegacyStrings](https://github.com/JuliaArchive/LegacyStrings.jl)
      package. In the future, more robust Unicode string support will be provided
      by the `StringEncodings` package. If you only need these types to call wide
      string APIs (UTF-16 on Windows, UTF-32 on UNIX), consider using the new
      `transcode` function (see below) or the `Cwstring` type as a `ccall` argument
      type, which also ensures correct NUL termination of string data.

    * The basic string construction routines are now `string(args...)`,
      `String(s)`, `unsafe_string(ptr)` (formerly `bytestring(ptr)`), and
      `unsafe_wrap(String, ptr)` (formerly `pointer_to_string`) ([#16731]).

    * A `transcode(T, src)` function is now exported for converting data
      between UTF-xx Unicode encodings ([#17323]).

    * Comparisons between `Char`s and `Integer`s are now deprecated ([#16024]):
      `'x' == 120` now produces a warning but still evaluates to `true`. In the
      future it may evaluate to `false` or the comparison may be an error. To
      compare characters with integers you should either convert the integer to
      a character value or convert the character to the corresponding code point
      first: e.g. `'x' == Char(120)` or `Int('x') == 120`. The former is usually
      preferable.

    * Support for Unicode 9 ([#17402]).

  * Packages:

    * The package system (`Pkg`) is now based on the `libgit2` library, rather
      than running the `git` program, increasing performance (especially on
      Windows) ([#11196]).

    * Package-development functions like `Pkg.tag` and `Pkg.publish`
      have been moved to an external [PkgDev] package ([#13387]).

    * Updating only a subset of the packages is now supported,
      e.g. `Pkg.update("Example")` ([#17132])

  * The `Base.Test` module now has a `@testset` feature to bundle
    tests together and delay throwing an error until the end ([#13062]).

    * The new features are mirrored in the
      [BaseTestNext](https://github.com/IainNZ/BaseTestNext.jl)
      package for users who would like to use the new functionality on Julia v0.4.

    * The [BaseTestDeprecated](https://github.com/IainNZ/BaseTestDeprecated.jl)
      package provides the old-style `handler` functionality, for compatibility
      with code that needs to support both Julia v0.4 and v0.5.

  * Most of the combinatorics functions have been moved from `Base`
    to the [Combinatorics.jl package](https://github.com/JuliaLang/Combinatorics.jl) ([#13897]).

  * `pmap` keyword arguments `err_retry=true` and `err_stop=false` are deprecated.
    Action to be taken on errors can be specified via the `on_error` keyword argument.
    Retry is specified via `retry_n`, `retry_on` and `retry_max_delay` ([#15409], [#15975], [#16663]).

  * The functions `remotecall`, `remotecall_fetch`, and `remotecall_wait` now have the
    function argument as the first argument to allow for do-block syntax ([#13338]).

  * `cov` and `cor` don't use keyword arguments anymore and are therefore now type stable ([#13465]).

  * Arrays and linear algebra:

    * All dimensions indexed by scalars are now dropped, whereas previously only
      trailing scalar dimensions would be omitted from the result ([#13612]). This
      is a very major behavioral change, but should cause obvious failures. To retain
      a dimension sliced with a scalar `i` slice with `i:i` instead.

    * Dimensions indexed by multidimensional arrays add dimensions. More generally, the
      dimensionality of the result is the sum of the dimensionalities of the indices ([#15431]).

    * New `normalize` and `normalize!` convenience functions for normalizing
      vectors ([#13681]).

    * QR matrix factorization:

      * New method for generic QR with column pivoting ([#13480]).

      * New method for polar decompositions of `AbstractVector`s ([#13681]).

    * A new `SparseVector` type allows for one-dimensional sparse arrays.
      Slicing and reshaping sparse matrices now return vectors when
      appropriate. The `sparsevec` function returns a one-dimensional sparse
      vector instead of a one-column sparse matrix. ([#13440])

    * Rank one update and downdate functions, `lowrankupdate`, `lowrankupdate!`, `lowrankdowndate`,
      and `lowrankdowndate!`, for dense Cholesky factorizations ([#14243], [#14424])

    * All `sparse` methods now retain provided numerical zeros as structural nonzeros; to
      drop numerical zeros, use `dropzeros!` ([#14798], [#15242]).

    * `setindex!` methods for sparse matrices and vectors no longer purge allocated entries
      on zero assignment. To drop stored entries from sparse matrices and vectors, use
      `Base.SparseArrays.dropstored!` ([#17404]).

  * New `foreach` function for calling a function on every element of a collection when
    the results are not needed ([#13774]). As compared to `map(f, v)`, which allocates and
    returns a result array, `foreach(f, v)` calls `f` on each element of `v`, returning nothing.

  * `Cmd(cmd; ...)` now accepts new Windows-specific options `windows_verbatim`
    (to alter Windows command-line generation) and `windows_hide` (to
    suppress creation of new console windows) ([#13780]).

  * Statistics:

    * Improve performance of `quantile` ([#14413]).

    * `extrema` can now operate over a region ([#15550]).

  * The new `Base.StackTraces` module makes stack traces easier to use programmatically ([#14469]).

  * There is now a default no-op `flush(io)` function for all `IO` types ([#16403]).

  * Concatenating dense and sparse matrices now returns a sparse matrix ([#15172]).

  * The `libjulia` library is now properly versioned and installed to the public `<prefix>/lib`
    directory, instead of the private `<prefix>/lib/julia` directory ([#16362]).

  * System reflection is now more consistently exposed from Sys and not Base.
    `OS_NAME` has been replaced by `Sys.KERNEL` and always reports the name of the
    kernel (as reported by `uname`). The `@windows_only` and `@osx` family of macros
    have been replaced with functions such as `is_windows()` and `is_apple()`.
    There is now also a `@static` macro that will evaluate the condition of an
    if-statement at compile time, for when a static branch is required ([#16219]).

  * Prime number related functions have been moved from `Base` to the
    [Primes.jl package](https://github.com/JuliaMath/Primes.jl) ([#16481]).

  * `Date` and `DateTime` values can now be rounded to a specified resolution (e.g., 1 month or
    15 minutes) with `floor`, `ceil`, and `round` ([#17037]).

  * File handling:

    * The `open` function now respects `umask` on UNIX when creating files ([#16466], [#16502]).

    * A new function `walkdir()` returns an iterator that walks the directory tree of a directory. ([#1765])

       ```
       for (root, dirs, files) in walkdir(expanduser("~/.julia/v0.5/Plots/src"))
           println("$(length(files)) \t files in $root")
       end
       19    files in /Users/me/.julia/v0.5/Plots/src
       15    files in /Users/me/.julia/v0.5/Plots/src/backends
       4     files in /Users/me/.julia/v0.5/Plots/src/deprecated
      ```

    * A new function `chown()` changes the ownership of files. ([#15007])

Deprecated or removed
---------------------

  * The following function names have been simplified and unified ([#13232]):

    * `get_bigfloat_precision`  -> `precision(BigFloat)`
    * `set_bigfloat_precision`  -> `setprecision`
    * `with_bigfloat_precision` -> `setprecision`

    * `get_rounding`            -> `rounding`
    * `set_rounding`            -> `setrounding`
    * `with_rounding`           -> `setrounding`

  * The method `A_ldiv_B!(SparseMatrixCSC, StrideVecOrMat)` has been deprecated
    in favor of versions that require the matrix to be in factored form
    ([#13496]).

  * Deprecate `chol(A,Val{:U/:L})` in favor of `chol(A)` ([#13680]).

  * `issym` is deprecated in favor of `issymmetric` to match similar functions
    (`ishermitian`, ...) ([#15192])

  * `scale` is deprecated in favor of either `α*A`, `Diagonal(x)*A`, or `A*Diagonal(x)`. ([#15258])

  * `xdump` is removed, and `dump` now simply shows the full representation of a value.
    `dump` should not be overloaded, since it is for examining concrete structure ([#4163]).

  * `sub` and `slice` have been deprecated in favor of `view` ([#16972])

  * The no-op `transpose` fallback has been deprecated. Consider introducing suitable
    `transpose` methods or calling `permutedims(x, [2,1])` ([#13171], [#17075], [#17374]).

[PkgDev]: https://github.com/JuliaLang/PkgDev.jl
<!--- generated by NEWS-update.jl: -->
[#550]: https://github.com/JuliaLang/julia/issues/550
[#964]: https://github.com/JuliaLang/julia/issues/964
[#1090]: https://github.com/JuliaLang/julia/issues/1090
[#1765]: https://github.com/JuliaLang/julia/issues/1765
[#4163]: https://github.com/JuliaLang/julia/issues/4163
[#4211]: https://github.com/JuliaLang/julia/issues/4211
[#4470]: https://github.com/JuliaLang/julia/issues/4470
[#4867]: https://github.com/JuliaLang/julia/issues/4867
[#6190]: https://github.com/JuliaLang/julia/issues/6190
[#7258]: https://github.com/JuliaLang/julia/issues/7258
[#8036]: https://github.com/JuliaLang/julia/issues/8036
[#8846]: https://github.com/JuliaLang/julia/issues/8846
[#9503]: https://github.com/JuliaLang/julia/issues/9503
[#9627]: https://github.com/JuliaLang/julia/issues/9627
[#11196]: https://github.com/JuliaLang/julia/issues/11196
[#11242]: https://github.com/JuliaLang/julia/issues/11242
[#13062]: https://github.com/JuliaLang/julia/issues/13062
[#13171]: https://github.com/JuliaLang/julia/issues/13171
[#13232]: https://github.com/JuliaLang/julia/issues/13232
[#13338]: https://github.com/JuliaLang/julia/issues/13338
[#13387]: https://github.com/JuliaLang/julia/issues/13387
[#13412]: https://github.com/JuliaLang/julia/issues/13412
[#13440]: https://github.com/JuliaLang/julia/issues/13440
[#13465]: https://github.com/JuliaLang/julia/issues/13465
[#13480]: https://github.com/JuliaLang/julia/issues/13480
[#13496]: https://github.com/JuliaLang/julia/issues/13496
[#13542]: https://github.com/JuliaLang/julia/issues/13542
[#13612]: https://github.com/JuliaLang/julia/issues/13612
[#13680]: https://github.com/JuliaLang/julia/issues/13680
[#13681]: https://github.com/JuliaLang/julia/issues/13681
[#13774]: https://github.com/JuliaLang/julia/issues/13774
[#13780]: https://github.com/JuliaLang/julia/issues/13780
[#13824]: https://github.com/JuliaLang/julia/issues/13824
[#13897]: https://github.com/JuliaLang/julia/issues/13897
[#14114]: https://github.com/JuliaLang/julia/issues/14114
[#14194]: https://github.com/JuliaLang/julia/issues/14194
[#14243]: https://github.com/JuliaLang/julia/issues/14243
[#14413]: https://github.com/JuliaLang/julia/issues/14413
[#14424]: https://github.com/JuliaLang/julia/issues/14424
[#14469]: https://github.com/JuliaLang/julia/issues/14469
[#14519]: https://github.com/JuliaLang/julia/issues/14519
[#14759]: https://github.com/JuliaLang/julia/issues/14759
[#14798]: https://github.com/JuliaLang/julia/issues/14798
[#15007]: https://github.com/JuliaLang/julia/issues/15007
[#15032]: https://github.com/JuliaLang/julia/issues/15032
[#15172]: https://github.com/JuliaLang/julia/issues/15172
[#15192]: https://github.com/JuliaLang/julia/issues/15192
[#15242]: https://github.com/JuliaLang/julia/issues/15242
[#15244]: https://github.com/JuliaLang/julia/issues/15244
[#15258]: https://github.com/JuliaLang/julia/issues/15258
[#15409]: https://github.com/JuliaLang/julia/issues/15409
[#15431]: https://github.com/JuliaLang/julia/issues/15431
[#15524]: https://github.com/JuliaLang/julia/issues/15524
[#15550]: https://github.com/JuliaLang/julia/issues/15550
[#15609]: https://github.com/JuliaLang/julia/issues/15609
[#15731]: https://github.com/JuliaLang/julia/issues/15731
[#15763]: https://github.com/JuliaLang/julia/issues/15763
[#15975]: https://github.com/JuliaLang/julia/issues/15975
[#16024]: https://github.com/JuliaLang/julia/issues/16024
[#16058]: https://github.com/JuliaLang/julia/issues/16058
[#16071]: https://github.com/JuliaLang/julia/issues/16071
[#16107]: https://github.com/JuliaLang/julia/issues/16107
[#16219]: https://github.com/JuliaLang/julia/issues/16219
[#16260]: https://github.com/JuliaLang/julia/issues/16260
[#16285]: https://github.com/JuliaLang/julia/issues/16285
[#16362]: https://github.com/JuliaLang/julia/issues/16362
[#16403]: https://github.com/JuliaLang/julia/issues/16403
[#16404]: https://github.com/JuliaLang/julia/issues/16404
[#16455]: https://github.com/JuliaLang/julia/issues/16455
[#16466]: https://github.com/JuliaLang/julia/issues/16466
[#16481]: https://github.com/JuliaLang/julia/issues/16481
[#16502]: https://github.com/JuliaLang/julia/issues/16502
[#16621]: https://github.com/JuliaLang/julia/issues/16621
[#16622]: https://github.com/JuliaLang/julia/issues/16622
[#16645]: https://github.com/JuliaLang/julia/issues/16645
[#16663]: https://github.com/JuliaLang/julia/issues/16663
[#16731]: https://github.com/JuliaLang/julia/issues/16731
[#16972]: https://github.com/JuliaLang/julia/issues/16972
[#17037]: https://github.com/JuliaLang/julia/issues/17037
[#17075]: https://github.com/JuliaLang/julia/issues/17075
[#17132]: https://github.com/JuliaLang/julia/issues/17132
[#17266]: https://github.com/JuliaLang/julia/issues/17266
[#17300]: https://github.com/JuliaLang/julia/issues/17300
[#17323]: https://github.com/JuliaLang/julia/issues/17323
[#17374]: https://github.com/JuliaLang/julia/issues/17374
[#17393]: https://github.com/JuliaLang/julia/issues/17393
[#17402]: https://github.com/JuliaLang/julia/issues/17402
[#17404]: https://github.com/JuliaLang/julia/issues/17404
[#17510]: https://github.com/JuliaLang/julia/issues/17510
[#17546]: https://github.com/JuliaLang/julia/issues/17546
