Julia v0.6.0 Release Notes
==========================

New language features
---------------------

Language changes
----------------

  * Multi-line and single-line nonstandard command literals have been added. A
    nonstandard command literal is like a nonstandard string literal, but the
    syntax uses backquotes (``` ` ```) instead of double quotes, and the
    resulting macro called is suffixed with `_cmd`. For instance, the syntax
    ``` q`xyz` ``` is equivalent to `@q_cmd "xyz"`. ([#18644])

  * Nonstandard string and command literals can now be qualified with their
    module. For instance, `Base.r"x"` is now parsed as `Base.@r_str "x"`.
    Previously, this syntax parsed as an implicit multiplication. ([#18690])

  * For every binary operator `⨳`, `a .⨳ b` is now automatically equivalent to
    the `broadcast` call `(⨳).(a, b)`.  Hence, one no longer defines methods
    for `.*` etcetera.  This also means that "dot operations" automatically
    fuse into a single loop, along with other dot calls `f.(x)`. ([#17623])

  * Newly defined methods are no longer callable from the same dynamic runtime
    scope they were defined in ([#17057]).

  * `isa` is now parsed as an infix operator with the same precedence as `in`
    ([#19677]).

Breaking changes
----------------

This section lists changes that do not have deprecation warnings.

  * Operations between `Float16` and `Integers` now return `Float16` instead of `Float32`. ([#17261])

  * Keyword arguments are processed left-to-right: if the same keyword is specified more than
    once, the rightmost occurrence takes precedence ([#17785]).

  * The `lgamma(z)` function now uses a different (more standard) branch cut
    for `real(z) < 0`, which differs from `log(gamma(z))` by multiples of 2π
    in the imaginary part ([#18330]).

  * `broadcast` now handles tuples, and treats any argument that is not a tuple
    or an array as a "scalar" ([#16986]).

  * `broadcast` now produces a `BitArray` instead of `Array{Bool}` for
    functions yielding a boolean result.  If you want `Array{Bool}`, use
    `broadcast!` or `.=` ([#17623]).

  * Operations like `.+` and `.*` on `Range` objects are now generic
    `broadcast` calls (see above) and produce an `Array`.  If you want
    a `Range` result, use `+` and `*`, etcetera ([#17623]).

  * `broadcast` now treats `Ref` (except for `Ptr`) arguments as 0-dimensional
    arrays ([#18965]).

  * `broadcast` now handles missing data (`Nullable`s) allowing operations to
    be lifted over mixtures of `Nullable`s and scalars, as if the `Nullable`
    were like an array with zero or one element. ([#16961], [#19787]).

  * The runtime now enforces when new method definitions can take effect ([#17057]).
    The flip-side of this is that new method definitions should now reliably actually
    take effect, and be called when evaluating new code ([#265]).

  * The array-scalar operations `div`, `mod`, `rem`, `&`, `|`, `xor`, `/`, `\`, `*`, `+`, and `-`
    now follow broadcast promotion rules ([#19692]).

  * `broadcast!(f, A)` now calls `f()` for each element of `A`, rather than doing `fill!(A, f())` ([#19722]).

  * `rmprocs` now throws an exception if requested workers have not been completely
    removed before `waitfor` seconds. With a `waitfor=0`, `rmprocs` returns immediately
    without waiting for worker exits.

  * `quadgk` has been moved from Base into a separate package. ([#19741])

Library improvements
--------------------

  * `max`, `min`, and related functions (`minmax`, `maximum`, `minimum`, `extrema`) now return `NaN` for `NaN` arguments ([#12563]).

  * The `chop` and `chomp` functions now return a `SubString` ([#18339]).

  * Numbered stackframes printed in stacktraces can be opened in an editor by entering the corresponding number in the REPL and pressing `^Q` ([#19680]).

  * The REPL now supports something called *prompt pasting* ([#17599]).
    This activates when pasting text that starts with `julia> ` into the REPL.
    In that case, only expressions starting with `julia> ` are parsed, the rest are removed.
    This makes it possible to paste a chunk of code that has been copied from a REPL session
    without having to scrub away prompts and outputs.
    This can be disabled or enabled at will with `Base.REPL.enable_promptpaste(::Bool)`.

  * The function `print_with_color` can now take a color represented by an integer between 0 and 255 inclusive as its first argument ([#18473]).
    For a number to color mapping please refer to [this chart](https://upload.wikimedia.org/wikipedia/en/1/15/Xterm_256color_chart.svg).
    It is also possible to use numbers as colors in environment variables that customizes colors in the REPL.
    For example, to get orange warning messages, simply set `ENV["JULIA_WARN_COLOR"] = 208`.
    Please note that not all terminals support 256 colors.

  * The function `print_with_color` no longer prints text in bold by default ([#18628]).
    Instead, the function now take a keyword argument `bold::Bool` which determines whether to print in bold or not.
    On some terminals, printing a color in non bold results in slightly darker colors being printed than when printing in bold.
    Therefore, light versions of the colors are now supported.
    For the available colors see the help entry on `print_with_color`.

  * The default color for info messages has been changed from blue to cyan and for warning messages from red to yellow.
    This can be changed back to the original colors by setting the environment variables `JULIA_INFO_COLOR` to `"blue"` and `JULIA_WARN_COLOR` to `"red"`.
    One way of doing this is by adding for example `ENV["JULIA_INFO_COLOR"] = :blue` and `ENV["JULIA_WARN_COLOR"] = :red` to the `.juliarc.jl` file.
    For more information regarding customizing colors in the REPL, see this [manual section]( http://docs.julialang.org/en/latest/manual/interacting-with-julia/#customizing-colors).

  * Iteration utilities that wrap iterators and return other iterators (`enumerate`, `zip`, `rest`,
    `countfrom`, `take`, `drop`, `cycle`, `repeated`, `product`, `flatten`, `partition`) have been
    moved to the module `Base.Iterators` ([#18839]).

  * BitArrays can now be constructed from arbitrary iterables, in particular from generator expressions,
    e.g. `BitArray(isodd(x) for x = 1:100)` ([#19018]).

  * `hcat`, `vcat`, and `hvcat` now work with `UniformScaling` objects, so
    you can now do e.g. `[A I]` and it will concatenate an appropriately sized
    identity matrix ([#19305]).

  * New `accumulate` and `accumulate!` functions, which generalize `cumsum` and `cumprod`. Also known as a [scan](https://en.wikipedia.org/wiki/Prefix_sum) operation ([#18931]).

  * New `titlecase` function, which capitalizes the first character of each word within a string ([#19469]).

  * `any` and `all` now always short-circuit, and `mapreduce` never short-circuits ([#19543]).
    That is, not every member of the input iterable will be visited if a `true` (in the case of `any`) or
    `false` (in the case of `all`) value is found, and `mapreduce` will visit all members of the iterable.

  * Additional methods for `ones` and `zeros` functions to support the same signature as the `similar` function ([#19635]).

  * Methods for `map` and `filter` with `Nullable` arguments have been
    implemented; the semantics are as if the `Nullable` were a container with
    zero or one elements ([#16961]).

Compiler/Runtime improvements
-----------------------------

Deprecated or removed
---------------------

  * `isdefined(a::Array, i::Int)` has been deprecated in favor of `isassigned` ([#18346]).

  * `is` has been deprecated in favor of `===` (which used to be an alias for `is`) ([#17758]).

  * `num` and `den` have been deprecated in favor of `numerator` and `denominator` respectively ([#19233]).

  * infix operator `$` has been deprecated in favor of infix `⊻` or function `xor()` ([#18977]).

  * `Dates.recur` has been deprecated in favor of `filter` ([#19288])

  * `cummin` and `cummax` have been deprecated in favor of `accumulate`.

  * `sumabs` and `sumabs2` have been deprecated in favor of `sum(abs, x)` and `sum(abs2, x)`, respectively.
    `maxabs` and `minabs` have similarly been deprecated in favor of `maximum(abs, x)` and `minimum(abs, x)`.
    Likewise for the in-place counterparts of these functions ([#19598]).

  * `airy`, `airyx` and `airyprime` have been deprecated in favor of more specific
    functions (`airyai`, `airybi`, `airyaiprime`, `airybiprimex`, `airyaix`, `airybix`,
    `airyaiprimex`, `airybiprimex`) ([#18050]).

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

  * The syntax `x.:sym` (e.g. `Base.:+`) is now supported, while using `x.(:sym)`
    or `x.(i)` for field access are deprecated in favor of `getfield` ([#15032]).

  * Function return type syntax `function f()::T` has been added ([#1090]). Values returned
    from a function with such a declaration will be converted to the specified type `T`.

  * Many more operators now support `.` prefixes (e.g. `.≤`) ([#17393]).  However,
    users are discouraged from overloading these, since they are mainly parsed
    in order to implement backwards compatibility with planned automatic
    broadcasting of dot operators in Julia 0.6 ([#16285]).  Explicitly qualified
    operator names like `Base.≤` should now use `Base.:≤` (prefixed by `@compat`
    if you need 0.4 compatibility via the `Compat` package).

  * User-extensible bounds check elimination is now possible with the new
    `@boundscheck` macro ([#14474]). This macro marks bounds checking code blocks,
    which the compiler may remove when encountered inside an `@inbounds` call.

Experimental language features
------------------------------

  * Support for
    [multi-threading](http://docs.julialang.org/en/latest/manual/parallel-computing/#multi-threading-experimental).
    Loops with independent iterations can be easily parallelized with the
    `Threads.@threads` macro.

  * Support for arrays with indexing starting at values different from 1.
    The array types are expected to be defined in packages, but now
    Julia provides an API for writing generic algorithms for arbitrary
    indexing schemes ([#16260]).

Language changes
----------------

  * Each function and closure now has its own type. The captured variables of a closure
    are fields of its type. `Function` is now an abstract type, and is the default
    supertype of functions and closures. All functions, including anonymous functions,
    are generic and support all features (e.g. keyword arguments). Instead of adding
    methods to `call`, methods are added by type using the syntax
    `(::ftype)(...) = ...`. `call` is deprecated ([#13412]). A significant result of
    this language change is that higher order functions can be specialized on their
    function arguments, leading to much faster functional programming, typically as
    fast as if function arguments were manually inlined. See below for details.

  * Square brackets and commas (e.g. `[x, y]`) no longer concatenate arrays, and always
    simply construct a vector of the provided values. If `x` and `y` are arrays,
    `[x, y]` will be an array of arrays ([#3737], [#2488], [#8599]).

  * `using` and `import` are now case-sensitive even on case-insensitive filesystems
    (common on Mac and Windows) ([#13542]).

  * Relational algebra symbols are now allowed as infix operators ([#8036]):
    `⨝`, `⟕`, `⟖`, `⟗` for joins and `▷` for anti-join.

  * A warning is always given when a method is overwritten; previously, this was done
    only when the new and old definitions were in separate modules ([#14759]).

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

  * Dictionary comprehension syntax `[ a=>b for x in y ]` is deprecated.
    Use `Dict(a=>b for x in y)` instead ([#16510]).

  * Parentheses are no longer allowed around iteration specifications, e.g.
    `for (i = 1:n)` ([#17668]).

Breaking changes
----------------

This section lists changes that do not have deprecation warnings.

  * All dimensions indexed by scalars are now dropped, whereas previously only
    trailing scalar dimensions would be omitted from the result ([#13612]). This
    is a very major behavioral change, but should cause obvious failures. To retain
    a dimension sliced with a scalar `i` slice with `i:i` instead.

  * The assignment operations `.+=`, `.*=` and so on now generate calls
    to `broadcast!` on the left-hand side (or call to `view(a, ...)` on the left-hand side
    if the latter is an indexing expression, e.g. `a[...]`). This means that they will fail
    if the left-hand side is immutable (or does not support `view`), and will otherwise
    change the left-hand side in-place ([#17510], [#17546]).

  * Method ambiguities no longer generate warnings when files are loaded,
    nor do they dispatch to an arbitrarily-chosen method; instead, a call that
    cannot be resolved to a single method results in a `MethodError` at run time,
    rather than the previous definition-time warning ([#6190]).

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

  * `map` on a dictionary now expects a function that expects and returns a `Pair`.
    The result is now another dictionary instead of an array ([#16622]).

  * Bit shift operations (i.e. `<<`, `>>`, and `>>>`) now handle
    negative shift counts differently: Negative counts are interpreted
    as shifts in the opposite direction. For example, `4 >> -1 == 4 <<
    +1 == 8`. Previously, negative counts would implicitly overflow to
    large positive counts, always yielding either `0` or `-1`.

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
      [LegacyStrings.jl package](https://github.com/JuliaArchive/LegacyStrings.jl).
      In the future, more robust Unicode string support will be provided by the
      [StringEncodings.jl package](https://github.com/nalimilan/StringEncodings.jl).
      If you only need these types to call wide string APIs (UTF-16 on Windows,
      UTF-32 on UNIX), consider using the new `transcode` function (see below)
      or the `Cwstring` type as a `ccall` argument type, which also ensures
      correct NUL termination of string data.

    * A `transcode(T, src)` function is now exported for converting data
      between UTF-xx Unicode encodings ([#17323]).

    * The basic string construction routines are now `string(args...)`,
      `String(s)`, `unsafe_string(ptr)` (formerly `bytestring(ptr)`), and
      `unsafe_wrap(String, ptr)` (formerly `pointer_to_string`) ([#16731]).

    * Comparisons between `Char`s and `Integer`s are now deprecated ([#16024]):
      `'x' == 120` now produces a warning but still evaluates to `true`. In the
      future it may evaluate to `false` or the comparison may be an error. To
      compare characters with integers you should either convert the integer to
      a character value or convert the character to the corresponding code point
      first: e.g. `'x' == Char(120)` or `Int('x') == 120`. The former is usually
      preferable.

    * Support for Unicode 9 ([#17402]).

  * Arrays and linear algebra:

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
      vector instead of a one-column sparse matrix. The `SparseMatrix` module
      has been renamed to `SparseArrays` ([#13440]).

    * Rank one update and downdate functions, `lowrankupdate`, `lowrankupdate!`, `lowrankdowndate`,
      and `lowrankdowndate!`, have been introduced for dense Cholesky factorizations ([#14243], [#14424]).

    * All `sparse` methods now retain provided numerical zeros as structural nonzeros; to
      drop numerical zeros, use `dropzeros!` ([#14798], [#15242]).

    * `setindex!` methods for sparse matrices and vectors no longer purge allocated entries
      on zero assignment. To drop stored entries from sparse matrices and vectors, use
      `Base.SparseArrays.dropstored!` ([#17404]).

    * Concatenating dense and sparse matrices now returns a sparse matrix ([#15172]).

  * Files and I/O:

    * The `open` function now respects `umask` on UNIX when creating files ([#16466], [#16502]).

    * A new function `walkdir()` returns an iterator that walks the tree of a directory ([#8814], [#13707]).

       ```
       for (root, dirs, files) in walkdir(expanduser("~/.julia/v0.5/Plots/src"))
           println("$(length(files)) \t files in $root")
       end
       19    files in /Users/me/.julia/v0.5/Plots/src
       15    files in /Users/me/.julia/v0.5/Plots/src/backends
       4     files in /Users/me/.julia/v0.5/Plots/src/deprecated
      ```

    * A new function `chown()` changes the ownership of files ([#15007]).

    * Display properties can now be passed among output functions (e.g. `show`)
      using an `IOContext` object ([#13825]).

    * `Cmd(cmd; ...)` now accepts new Windows-specific options `windows_verbatim`
      (to alter Windows command-line generation) and `windows_hide` (to
      suppress creation of new console windows) ([#13780]).

    * There is now a default no-op `flush(io)` function for all `IO` types ([#16403]).

  * Parallel computing:

    * `pmap` keyword arguments `err_retry=true` and `err_stop=false` are deprecated.
      Action to be taken on errors can be specified via the `on_error` keyword argument.
      Retry is specified via `retry_n`, `retry_on` and `retry_max_delay` ([#15409], [#15975], [#16663]).

    * The functions `remotecall`, `remotecall_fetch`, and `remotecall_wait` now have the
      function argument as the first argument to allow for do-block syntax ([#13338]).

  * Statistics:

    * Improve performance of `quantile` ([#14413]).

    * `extrema` can now operate over a region ([#15550]).

    * `cov` and `cor` don't use keyword arguments anymore and are therefore now type stable ([#13465]).

    * Histogram functionality has been deprecated in `Base`. Use the
      [StatsBase.jl package](https://github.com/JuliaStats/StatsBase.jl)
      instead ([#6842], [#16450]).

  * Testing:

    * The `Base.Test` module now has a `@testset` feature to bundle
      tests together and delay throwing an error until the end ([#13062]).

    * The new features are mirrored in the
      [BaseTestNext.jl package](https://github.com/IainNZ/BaseTestNext.jl)
      for users who would like to use the new functionality on Julia v0.4.

    * The [BaseTestDeprecated.jl package](https://github.com/IainNZ/BaseTestDeprecated.jl)
      provides the old-style `handler` functionality, for compatibility
      with code that needs to support both Julia v0.4 and v0.5.

  * Package management:

    * The package system (`Pkg`) is now based on the `libgit2` library, rather
      than running the `git` program, increasing performance (especially on
      Windows) ([#11196]).

    * Package-development functions like `Pkg.tag` and `Pkg.publish`
      have been moved to an external [PkgDev] package ([#13387]).

    * Updating only a subset of the packages is now supported,
      e.g. `Pkg.update("Example")` ([#17132]).

  * Miscellanous:

    * Prime number related functions have been moved from `Base` to the
      [Primes.jl package](https://github.com/JuliaMath/Primes.jl) ([#16481]).

    * Most of the combinatorics functions have been moved from `Base`
      to the [Combinatorics.jl package](https://github.com/JuliaLang/Combinatorics.jl) ([#13897]).

    * New `foreach` function for calling a function on every element of a collection when
      the results are not needed ([#13774]). Compared to `map(f, v)`, which allocates and
      returns a result array, `foreach(f, v)` calls `f` on each element of `v`, returning
      nothing.

    * The new `Base.StackTraces` module makes stack traces easier to use programmatically ([#14469]).

    * The `libjulia` library is now properly versioned and installed to the public `<prefix>/lib`
      directory, instead of the private `<prefix>/lib/julia` directory ([#16362]).

    * System reflection is now more consistently exposed from `Sys` and not `Base`
      (e.g. constants such as `WORD_SIZE` and `CPU_CORES`). `OS_NAME` has been
      replaced by `Sys.KERNEL` and always reports the name of the kernel (as
      reported by `uname`). The `@windows_only` and `@osx` family of macros
      have been replaced with functions such as `is_windows()` and `is_apple()`.
      There is now also a `@static` macro that will evaluate the condition of an
      if-statement at compile time, for when a static branch is required ([#16219]).

    * `Date` and `DateTime` values can now be rounded to a specified resolution (e.g., 1 month or
      15 minutes) with `floor`, `ceil`, and `round` ([#17037]).

[PkgDev]: https://github.com/JuliaLang/PkgDev.jl

Compiler/Runtime improvements
-----------------------------

  * Machine SIMD types can be represented in Julia as a homogeneous tuple of `VecElement` ([#15244]).

  * The performance of higher-order and anonymous functions has been greatly improved.
    For example, `map(x->2x, A)` performs as well as `2.*A`([#13412]).

  * On windows, a DLL of standard library code is now precompiled and used by default,
    improving startup time ([#16953]).

  * LLVM has been upgraded to version 3.7.1, improving the quality of generated
    code and debug info. However compile times may be slightly longer ([#14623]).

New architectures
-----------------

  This release greatly improves support for ARM, and introduces support for Power.

  * [ARM](https://github.com/JuliaLang/julia/issues?utf8=%E2%9C%93&q=label%3Aarm):
    [#14194], [#14519], [#16645], [#16621]

  * [Power](https://github.com/JuliaLang/julia/issues?utf8=%E2%9C%93&q=label%3Apower):
    [#16455], [#16404]

Deprecated or removed
---------------------

  * The following function names have been simplified and unified ([#13232]):

    * `get_bigfloat_precision`  -> `precision(BigFloat)`
    * `set_bigfloat_precision`  -> `setprecision`
    * `with_bigfloat_precision` -> `setprecision`

    * `get_rounding`            -> `rounding`
    * `set_rounding`            -> `setrounding`
    * `with_rounding`           -> `setrounding`

  * The method `A_ldiv_B!(SparseMatrixCSC, StridedVecOrMat)` has been deprecated
    in favor of versions that require the matrix to be in factored form
    ([#13496]).

  * `chol(A,Val{:U/:L})` has been deprecated in favor of `chol(A)` ([#13680]).

  * `rem1(x,y)` is discontinued due to inconsistency for `x==0`. Use `mod1` instead ([#14140]).

  * The `FS` module has been renamed to `Filesystem`. Calling the functions `isreadable`,
   `iswritable`, and `isexecutable` on filesystem paths has been deprecated ([#12819]).

  * `RemoteRef` has been deprecated in favor of `RemoteChannel` ([#14458]).

  * `super` has been renamed to `supertype` ([#14335]).

  * `parseip(str)` has been deprecated in favor of `parse(IPAddr, str)` ([#14676]).

  * `readall` has been renamed to `readstring`, and `readbytes` has been renamed to `read` ([#14608], [#14660]).

  * `fieldoffsets(x)` has been deprecated in favor of calling `fieldoffset(x, i)` on each field ([#14777]).

  * `issym` is deprecated in favor of `issymmetric` to match similar functions
    (`ishermitian`, ...) ([#15192]).

  * `scale` is deprecated in favor of either `α*A`, `Diagonal(x)*A`, or `A*Diagonal(x)` ([#15258]).

  * `istext` has been renamed to `istextmime` ([#12872], [#15708]).

  * "Functor" types are no longer necessary and have been deprecated ([#15804]). To maintain
    performance on older versions of Julia the [Compat.jl package](https://github.com/JuliaLang/Compat.jl/pull/184)
    provides a `@functorize` macro.

  * `bitunpack(B)` and `bitpack(A)` have been deprecated in favor of
    `Array(B)` and `BitArray(A)`, respectively ([#16010]).

  * `xdump` is removed, and `dump` now simply shows the full representation of a value.
    `dump` should not be overloaded, since it is for examining concrete structure ([#4163]).

  * `sprandbool` has been deprecated in favor of `sprand(Bool, ...)` or
    `sprand(rng, Bool, ...)` ([#11688], [#16098]).

  * The lowercase `symbol` function has been deprecated in favor of the `Symbol`
    constructor ([#16154]).

  * `writemime` is deprecated, and output methods specifying a MIME type are now
    methods of `show` ([#14052]).

  * BLAS utility functions `blas_set_num_threads`, `blas_vendor`, and `check_blas`
    have been moved to the BLAS module as `BLAS.set_num_threads`, `BLAS.vendor`,
    and `BLAS.check` ([#10548], [#16600]).

  * `print_escaped` has been renamed to `escape_string`, `print_unescaped` has been
    renamed to `unescape_string`, and `print_joined` has been renamed to `join` ([#16603]).

  * `pointer_to_string` has been renamed to `unsafe_wrap(String, ...)`, and
    `pointer_to_array` has been renamed to `unsafe_wrap(Array, ...)` ([#16731]).

  * `sub` and `slice` have been deprecated in favor of `view` ([#16972]).

  * Sparse matrix functions `etree`, `ereach`, `csc_permute`, and `symperm` have been moved
    to the [SuiteSparse.jl package](https://github.com/JuliaSparse/SuiteSparse.jl) ([#12231], [#17033]).

  * The no-op `transpose` fallback for non-numeric arrays has been deprecated. Consider introducing suitable
    `transpose` methods or calling `permutedims(x, (2, 1))` for matrices and `reshape(x, 1, length(x))` for
    vectors.  ([#13171], [#17075], [#17374]).

  * The following macros have been deprecated ([#16219]):
    * `@windows` is deprecated in favor of `is_windows()`
    * `@unix` is deprecated in favor of `is_unix()`
    * `@osx` is deprecated in favor of `is_apple()`
    * `@linux` is deprecated in favor of `is_linux()`
    * `@windows_only` is deprecated in favor of `if is_windows()`
    * `@unix_only` is deprecated in favor of `if is_unix()`
    * `@osx_only` is deprecated in favor of `if is_apple()`
    * `@linux_only` is deprecated in favor of `if is_linux()`
    * NOTE: Using `@static` could be useful/necessary when used in a function's local scope. See details at the section entitled [Handling Operating System Variation](http://docs.julialang.org/en/latest/manual/handling-operating-system-variation/#man-handling-operating-system-variation) in the manual.

Command-line option changes
---------------------------

  * The `-F` flag to load `~/.juliarc` has been deprecated in favor of
    `--startup-file=yes` ([#9482]).

  * The `-f` and `--no-startup` flags to disable loading of `~/.juliarc` have
    been deprecated in favor of `--startup-file=no` ([#9482]).

  * The `-P` and `--post-boot` flags for evaluating an expression in "interactive mode"
    have been deprecated in favor of `-i -e` ([#16854]).

  * The `--no-history-file` flag to disable loading of `~/.julia_history` has been
    deprecated in favor of `--history-file=no` ([#9482]).

Language tooling improvements
-----------------------------

   * The [Julia debugger](https://github.com/Keno/Gallium.jl) makes its debut
     with this release. Install it with `Pkg.add("Gallium")`, and the
     [documentation](https://github.com/Keno/Gallium.jl#gallium) should
     get you going. The [JuliaCon
     talk](https://www.youtube.com/watch?v=e6-hcOHO0tc&list=PLP8iPy9hna6SQPwZUDtAM59-wPzCPyD_S&index=5)
     on Gallium shows off various features of the debugger.

   * The [Juno IDE](http://junolab.org) has matured significantly, and now
     also includes support for plotting and debugging.

   * [Cxx.jl](https://github.com/Keno/Cxx.jl) provides a convenient FFI for
     calling C++ code from Julia.

<!--- generated by NEWS-update.jl: -->
[#265]: https://github.com/JuliaLang/julia/issues/265
[#550]: https://github.com/JuliaLang/julia/issues/550
[#964]: https://github.com/JuliaLang/julia/issues/964
[#1090]: https://github.com/JuliaLang/julia/issues/1090
[#2488]: https://github.com/JuliaLang/julia/issues/2488
[#3737]: https://github.com/JuliaLang/julia/issues/3737
[#4163]: https://github.com/JuliaLang/julia/issues/4163
[#4211]: https://github.com/JuliaLang/julia/issues/4211
[#4470]: https://github.com/JuliaLang/julia/issues/4470
[#4867]: https://github.com/JuliaLang/julia/issues/4867
[#6190]: https://github.com/JuliaLang/julia/issues/6190
[#6842]: https://github.com/JuliaLang/julia/issues/6842
[#7258]: https://github.com/JuliaLang/julia/issues/7258
[#8036]: https://github.com/JuliaLang/julia/issues/8036
[#8599]: https://github.com/JuliaLang/julia/issues/8599
[#8814]: https://github.com/JuliaLang/julia/issues/8814
[#8846]: https://github.com/JuliaLang/julia/issues/8846
[#9482]: https://github.com/JuliaLang/julia/issues/9482
[#9503]: https://github.com/JuliaLang/julia/issues/9503
[#9627]: https://github.com/JuliaLang/julia/issues/9627
[#10548]: https://github.com/JuliaLang/julia/issues/10548
[#11196]: https://github.com/JuliaLang/julia/issues/11196
[#11242]: https://github.com/JuliaLang/julia/issues/11242
[#11688]: https://github.com/JuliaLang/julia/issues/11688
[#12231]: https://github.com/JuliaLang/julia/issues/12231
[#12563]: https://github.com/JuliaLang/julia/issues/12563
[#12819]: https://github.com/JuliaLang/julia/issues/12819
[#12872]: https://github.com/JuliaLang/julia/issues/12872
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
[#13707]: https://github.com/JuliaLang/julia/issues/13707
[#13774]: https://github.com/JuliaLang/julia/issues/13774
[#13780]: https://github.com/JuliaLang/julia/issues/13780
[#13824]: https://github.com/JuliaLang/julia/issues/13824
[#13825]: https://github.com/JuliaLang/julia/issues/13825
[#13897]: https://github.com/JuliaLang/julia/issues/13897
[#14052]: https://github.com/JuliaLang/julia/issues/14052
[#14114]: https://github.com/JuliaLang/julia/issues/14114
[#14140]: https://github.com/JuliaLang/julia/issues/14140
[#14194]: https://github.com/JuliaLang/julia/issues/14194
[#14243]: https://github.com/JuliaLang/julia/issues/14243
[#14335]: https://github.com/JuliaLang/julia/issues/14335
[#14413]: https://github.com/JuliaLang/julia/issues/14413
[#14424]: https://github.com/JuliaLang/julia/issues/14424
[#14458]: https://github.com/JuliaLang/julia/issues/14458
[#14469]: https://github.com/JuliaLang/julia/issues/14469
[#14474]: https://github.com/JuliaLang/julia/issues/14474
[#14519]: https://github.com/JuliaLang/julia/issues/14519
[#14608]: https://github.com/JuliaLang/julia/issues/14608
[#14623]: https://github.com/JuliaLang/julia/issues/14623
[#14660]: https://github.com/JuliaLang/julia/issues/14660
[#14676]: https://github.com/JuliaLang/julia/issues/14676
[#14759]: https://github.com/JuliaLang/julia/issues/14759
[#14777]: https://github.com/JuliaLang/julia/issues/14777
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
[#15708]: https://github.com/JuliaLang/julia/issues/15708
[#15731]: https://github.com/JuliaLang/julia/issues/15731
[#15763]: https://github.com/JuliaLang/julia/issues/15763
[#15804]: https://github.com/JuliaLang/julia/issues/15804
[#15975]: https://github.com/JuliaLang/julia/issues/15975
[#16010]: https://github.com/JuliaLang/julia/issues/16010
[#16024]: https://github.com/JuliaLang/julia/issues/16024
[#16058]: https://github.com/JuliaLang/julia/issues/16058
[#16071]: https://github.com/JuliaLang/julia/issues/16071
[#16098]: https://github.com/JuliaLang/julia/issues/16098
[#16107]: https://github.com/JuliaLang/julia/issues/16107
[#16154]: https://github.com/JuliaLang/julia/issues/16154
[#16219]: https://github.com/JuliaLang/julia/issues/16219
[#16260]: https://github.com/JuliaLang/julia/issues/16260
[#16285]: https://github.com/JuliaLang/julia/issues/16285
[#16362]: https://github.com/JuliaLang/julia/issues/16362
[#16403]: https://github.com/JuliaLang/julia/issues/16403
[#16404]: https://github.com/JuliaLang/julia/issues/16404
[#16450]: https://github.com/JuliaLang/julia/issues/16450
[#16455]: https://github.com/JuliaLang/julia/issues/16455
[#16466]: https://github.com/JuliaLang/julia/issues/16466
[#16481]: https://github.com/JuliaLang/julia/issues/16481
[#16502]: https://github.com/JuliaLang/julia/issues/16502
[#16510]: https://github.com/JuliaLang/julia/issues/16510
[#16600]: https://github.com/JuliaLang/julia/issues/16600
[#16603]: https://github.com/JuliaLang/julia/issues/16603
[#16621]: https://github.com/JuliaLang/julia/issues/16621
[#16622]: https://github.com/JuliaLang/julia/issues/16622
[#16645]: https://github.com/JuliaLang/julia/issues/16645
[#16663]: https://github.com/JuliaLang/julia/issues/16663
[#16731]: https://github.com/JuliaLang/julia/issues/16731
[#16854]: https://github.com/JuliaLang/julia/issues/16854
[#16953]: https://github.com/JuliaLang/julia/issues/16953
[#16961]: https://github.com/JuliaLang/julia/issues/16961
[#16972]: https://github.com/JuliaLang/julia/issues/16972
[#16986]: https://github.com/JuliaLang/julia/issues/16986
[#17033]: https://github.com/JuliaLang/julia/issues/17033
[#17037]: https://github.com/JuliaLang/julia/issues/17037
[#17057]: https://github.com/JuliaLang/julia/issues/17057
[#17075]: https://github.com/JuliaLang/julia/issues/17075
[#17132]: https://github.com/JuliaLang/julia/issues/17132
[#17261]: https://github.com/JuliaLang/julia/issues/17261
[#17266]: https://github.com/JuliaLang/julia/issues/17266
[#17300]: https://github.com/JuliaLang/julia/issues/17300
[#17323]: https://github.com/JuliaLang/julia/issues/17323
[#17374]: https://github.com/JuliaLang/julia/issues/17374
[#17393]: https://github.com/JuliaLang/julia/issues/17393
[#17402]: https://github.com/JuliaLang/julia/issues/17402
[#17404]: https://github.com/JuliaLang/julia/issues/17404
[#17510]: https://github.com/JuliaLang/julia/issues/17510
[#17546]: https://github.com/JuliaLang/julia/issues/17546
[#17599]: https://github.com/JuliaLang/julia/issues/17599
[#17623]: https://github.com/JuliaLang/julia/issues/17623
[#17668]: https://github.com/JuliaLang/julia/issues/17668
[#17758]: https://github.com/JuliaLang/julia/issues/17758
[#17785]: https://github.com/JuliaLang/julia/issues/17785
[#18050]: https://github.com/JuliaLang/julia/issues/18050
[#18330]: https://github.com/JuliaLang/julia/issues/18330
[#18339]: https://github.com/JuliaLang/julia/issues/18339
[#18346]: https://github.com/JuliaLang/julia/issues/18346
[#18473]: https://github.com/JuliaLang/julia/issues/18473
[#18628]: https://github.com/JuliaLang/julia/issues/18628
[#18644]: https://github.com/JuliaLang/julia/issues/18644
[#18690]: https://github.com/JuliaLang/julia/issues/18690
[#18839]: https://github.com/JuliaLang/julia/issues/18839
[#18931]: https://github.com/JuliaLang/julia/issues/18931
[#18965]: https://github.com/JuliaLang/julia/issues/18965
[#18977]: https://github.com/JuliaLang/julia/issues/18977
[#19018]: https://github.com/JuliaLang/julia/issues/19018
[#19233]: https://github.com/JuliaLang/julia/issues/19233
[#19288]: https://github.com/JuliaLang/julia/issues/19288
[#19305]: https://github.com/JuliaLang/julia/issues/19305
[#19469]: https://github.com/JuliaLang/julia/issues/19469
[#19543]: https://github.com/JuliaLang/julia/issues/19543
[#19598]: https://github.com/JuliaLang/julia/issues/19598
[#19635]: https://github.com/JuliaLang/julia/issues/19635
[#19680]: https://github.com/JuliaLang/julia/issues/19680
[#19787]: https://github.com/JuliaLang/julia/issues/19787
