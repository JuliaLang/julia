Julia v0.5.0 Release Notes
==========================

New language features
---------------------

  * Generator expressions, e.g. `f(i) for i in 1:n` ([#4470]). This returns an iterator
    that computes the specified values on demand.

  * Broadcasting syntax: ``f.(args...)`` is equivalent to ``broadcast(f, args...)`` ([#15032]).

  * Macro expander functions are now generic, so macros can have multiple definitions
    (e.g. for different numbers of arguments, or optional arguments) ([#8846], [#9627]).
    However note that the argument types refer to the syntax tree representation, and not
    to the types of run time values.

  * Varargs functions like `foo{T}(x::T...)` may now restrict the number
    of such arguments using `foo{T,N}(x::Vararg{T,N})` ([#11242]).

  * `x ∈ X` is now a synonym for `x in X` in `for` loops and comprehensions,
    as it already was in comparisons ([#13824]).

  * `PROGRAM_FILE` global is now available for determining the name of the running script ([#14114]).

  * The syntax `x.:sym` (e.g. `Base.:+`) is now supported, and `x.(:sym)` is deprecated ([#15032]).

Language changes
----------------

  * Each function and closure now has its own type. The captured variables of a closure
    are fields of its type. `Function` is now an abstract type, and is the default supertype
    of functions and closures. All functions, including anonymous functions,
    are generic and support all features (e.g. keyword arguments).
    Instead of adding methods to `call`, methods are added by type using the syntax
    `(::ftype)(...) = ...`. `call` is deprecated ([#13412]).

  * `using` and `import` are now case-sensitive even on case-insensitive filesystems (common on Mac and Windows) ([#13542]).

  * Relational symbols are now allowed as infix operators ([#8036]).

  * A warning is always given when a method is overwritten (previously, this was done only when the new
    and old definitions were in separate modules) ([#14759]).

  * `A <: B` is parsed as `Expr(:(<:), :A, :B)` in all cases ([#9503]). This also applies to the
    `>:` operator.

  * Simple 2-argument comparisons like `A < B` are parsed as calls instead of using the
    `:comparison` expression type.

  * The `if` keyword cannot be followed immediately by a line break ([#15763]).

  * The built-in `NTuple` type has been removed; `NTuple{N,T}` is now
    implemented internally as `Tuple{Vararg{T,N}}` ([#11242]).

Command-line option changes
---------------------------

Compiler/Runtime improvements
-----------------------------

Breaking changes
----------------

  * Method ambiguities no longer generate warnings when files are
    loaded, nor do they dispatch to an arbitrarily-chosen method;
    instead, a call that cannot be resolved to a single method results
    in a `MethodError`. ([#6190])

  * `pmap` keyword arguments `err_retry=true` and `err_stop=false` are deprecated.
    `pmap` no longer retries or returns `Exception` objects in the result collection.
    `pmap(retry(f), c)` or `pmap(@catch(f), c)` can be used instead.
    ([#15409](https://github.com/JuliaLang/julia/pull/15409#discussion_r57494701)).

  * `reshape` is now defined to always share data with the original array.
    If a reshaped copy is needed, use `copy(reshape(a))` or `copy!` to a new array of
    the desired shape ([#4211]).

  * Local variables and arguments are represented in lowered code as numbered `Slot`
    objects instead of as symbols ([#15609]).

  * The information that used to be in the `ast` field of the `LambdaStaticData` type
    is now divided among the fields `code`, `slotnames`, `slottypes`, `slotflags`,
    `gensymtypes`, `rettype`, `nargs`, and `isva` in the `LambdaInfo` type ([#15609]).

Library improvements
--------------------

  * Most of the  combinatorics functions have been moved from `Base`
    to the [Combinatorics.jl package](https://github.com/JuliaLang/Combinatorics.jl) ([#13897]).

  * Packages:

    * The package system (`Pkg`) is now based on the `libgit2` library, rather
      than running the `git` program, increasing performance (especially on
      Windows) ([#11196]).

    * Package-development functions like `Pkg.tag` and `Pkg.publish`
      have been moved to an external [PkgDev] package ([#13387]).

  * The `Base.Test` module now has a `@testset` feature to bundle
    tests together and delay throwing an error until the end ([#13062]).

    * The new features are mirrored in the
      [BaseTestNext](https://github.com/IainNZ/BaseTestNext.jl)
      package for users who would like to use the new functionality on Julia v0.4.

    * The [BaseTestDeprecated](https://github.com/IainNZ/BaseTestDeprecated.jl)
      package provides the old-style `handler` functionality, for compatibility
      with code that needs to support both Julia v0.4 and v0.5.

  * The functions `remotecall`, `remotecall_fetch`, and `remotecall_wait` now have the
    function argument as the first argument to allow for do-block syntax ([#13338]).

  * `cov` and `cor` don't use keyword arguments anymore and are therefore now type stable ([#13465]).

  * Linear algebra:

    * All dimensions indexed by scalars are now dropped, whereas previously only
      trailing scalar dimensions would be omitted from the result ([#13612]).

    * Dimensions indexed by multidimensional arrays add dimensions. More generally, the dimensionality of the result is the sum of the dimensionalities of the indices ([#15431]).

    * New `normalize` and `normalize!` convenience functions for normalizing
      vectors ([#13681]).

    * QR

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

  * New `foreach` function for calling a function on every element of a collection when
    the results are not needed.

  * `Cmd(cmd; ...)` now accepts new Windows-specific options `windows_verbatim`
    (to alter Windows command-line generation) and `windows_hide` (to
    suppress creation of new console windows) ([#13780]).

  * Statistics:

    * Improve performance of `quantile` ([#14413]).

    * `extrema` can now operate over a region ([#15550]).

  * The new `Base.StackTraces` module makes stack traces easier to use programmatically. ([#14469])

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

  * `issym` is deprecated in favor of `issymmetric` to match similar functions (`ishermitian`, ...) ([#15192])

  * `scale` is deprecated in favor of either `α*A`, `Diagonal(x)*A`, or `A*Diagonal(x)`. ([#15258])

  * `xdump` is removed, and `dump` now simply shows the full representation of a value.
    `dump` should not be overloaded, since it is for examining concrete structure ([#4163]).

[PkgDev]: https://github.com/JuliaLang/PkgDev.jl
<!--- generated by NEWS-update.jl: -->
[#4163]: https://github.com/JuliaLang/julia/issues/4163
[#4211]: https://github.com/JuliaLang/julia/issues/4211
[#4470]: https://github.com/JuliaLang/julia/issues/4470
[#6190]: https://github.com/JuliaLang/julia/issues/6190
[#8036]: https://github.com/JuliaLang/julia/issues/8036
[#8846]: https://github.com/JuliaLang/julia/issues/8846
[#9503]: https://github.com/JuliaLang/julia/issues/9503
[#9627]: https://github.com/JuliaLang/julia/issues/9627
[#11196]: https://github.com/JuliaLang/julia/issues/11196
[#11242]: https://github.com/JuliaLang/julia/issues/11242
[#13062]: https://github.com/JuliaLang/julia/issues/13062
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
[#13780]: https://github.com/JuliaLang/julia/issues/13780
[#13824]: https://github.com/JuliaLang/julia/issues/13824
[#13897]: https://github.com/JuliaLang/julia/issues/13897
[#14114]: https://github.com/JuliaLang/julia/issues/14114
[#14243]: https://github.com/JuliaLang/julia/issues/14243
[#14413]: https://github.com/JuliaLang/julia/issues/14413
[#14424]: https://github.com/JuliaLang/julia/issues/14424
[#14469]: https://github.com/JuliaLang/julia/issues/14469
[#14759]: https://github.com/JuliaLang/julia/issues/14759
[#14798]: https://github.com/JuliaLang/julia/issues/14798
[#15032]: https://github.com/JuliaLang/julia/issues/15032
[#15192]: https://github.com/JuliaLang/julia/issues/15192
[#15242]: https://github.com/JuliaLang/julia/issues/15242
[#15258]: https://github.com/JuliaLang/julia/issues/15258
[#15409]: https://github.com/JuliaLang/julia/issues/15409
[#15431]: https://github.com/JuliaLang/julia/issues/15431
[#15550]: https://github.com/JuliaLang/julia/issues/15550
[#15609]: https://github.com/JuliaLang/julia/issues/15609
[#15763]: https://github.com/JuliaLang/julia/issues/15763
