Julia v0.4.0 Release Notes
==========================

New language features
---------------------

  * Function-call overloading: for arbitrary objects `x` (not of type
    `Function`), `x(...)` is transformed into `call(x, ...)`, and `Base.call`
    can be overloaded as desired.  Constructors are now a special case of
    this mechanism, which allows e.g. constructors for abstract types
    and typealiases.  `T(...)` falls back to `convert(T, x)`, so all
    `convert` methods implicitly define a constructor ([#8712], [#2403]).

  * Unicode version 7 is now supported for identifiers etcetera ([#7917]).

  * Type parameters now permit any arbitrary `isbits` type, not just
    `Int` and `Bool` ([#6081]).

  * Keyword argument names can be computed, using syntax such as `f(; symbol => val)` ([#7704]).

  * (TODO pending final syntax) staged functions ([#7311]).

  * (Also with syntax todo) Documentation system for functions, methods, types
    and macros in packages and user code ([#8791]). Type `?@doc` at the repl
    to see the current syntax and more information.

  * New multidimensional iterators and index types for efficient
    iteration over general AbstractArrays

Language changes
----------------

  * `Uint` et al. are now spelled `UInt` ([#8905]).

  * `String` has been renamed to `AbstractString` ([#8872]).

  * `None` is deprecated; use `Union()` instead ([#8423]).

  * `Nothing` (the type of `nothing`) is renamed to `Void` ([#8423]).

  * `Dict` literal syntax `[a=>b,c=>d]` is replaced with `Dict(a=>b,c=>d)`.
    `{a=>b}` is replaced with `Dict{Any,Any}(a=>b)`.
    `(K=>V)[...]` is replaced with `Dict{K,V}(...)`.
    The new syntax has many advantages: all of its components are first-class,
    it generalizes to other types of containers, it is easier to guess how to
    specify key and value types, and the syntaxes for empty and pre-populated
    dicts are synchronized. As part of this change, `=>` is parsed as a normal
    operator, and `Base` defines it to construct `Pair` objects ([#6739]).

  * `Char` is no longer a subtype of `Integer`. ([#8816])
    Char now supports a more limited set of operations with `Integer` types:

      * comparison / equality
      * `Char` + `Int` = `Char`
      * `Char` - `Char` = `Int`

  * `round` rounds to the nearest integer using the default rounding mode,
    which is ties to even by default ([#8750]).

Compiler improvements
---------------------

  * Functions may be annotated with metadata (`:meta` expressions) to be used by the compiler ([#8297]).

  * `@inline` before a function definition forces the compiler to inline the function ([#8297]).

  * Loads from heap-allocated immutables are hoisted out of loops in more cases ([#8867]).

  * Accessing fields that are always initialized no longer produces undefined checks ([#8827]).

  * `--depwarn={yes|no}` command line flag added to enable / disable syntax and method deprecation warnings ([#9294]).

Library improvements
--------------------

  * `convert` now checks for overflow when truncating integers or converting between
    signed and unsigned ([#5413]).

  * Arithmetic is type-preserving for more types; e.g. `(x::Int8) + (y::Int8)` now
    yields an `Int8` ([#3759]).

  * Reductions (e.g. `reduce`, `sum`) widen small types (integers smaller than `Int`, and `Float16`).

  * New `Dates` module for calendar dates and other time-interval calculations ([#7654]).

  * New implementation of SubArrays with substantial performance and functionality improvements ([#8501]).

  * Added generic Cholesky factorization, and the Cholesky factorization is now parametrized on the matrix type ([#7236]).

  * Symmetric and Hermitian immutables are now parametrized on matrix type ([#7992]).

  * New `sortperm!` function for pre-allocated index arrays ([#8792]).

  * Switch from `O(N)` to `O(logN)` algorithm for `dequeue!(pq, key)`
    with `PriorityQueue`. This provides major speedups for large
    queues ([#8011]).

  * `PriorityQueue` now includes the order type among its parameters,
    `PriorityQueue{KeyType,ValueType,OrderType}`. An empty queue can
    be constructed as `pq = PriorityQueue(KeyType,ValueType)`, if you
    intend to use the default `Forward` order, or
    `pq = PriorityQueue(KeyType, ValueType, OrderType)` otherwise ([#8011]).

  * Efficient `mean` and `median` for ranges ([#8089]).

  * `graphemes(s)` returns an iterator over grapheme substrings of `s` ([#9261]).

  * Character predicates such as `islower()`, `isspace()`, etc. use utf8proc/libmojibake
    to provide uniform cross-platform behavior and up-to-date, locale-independent support
    for Unicode standards ([#5939]).

  * `reverseind` function to convert indices in reversed strings (e.g. from
    reversed regex searches) to indices in the original string ([#9249]).

  * New `Nullable` type for missing data ([#8152]).

  * New `ordschur` and `ordschur!` functions for sorting a schur factorization by the eigenvalues.

  * `deepcopy` recurses through immutable types and makes copies of their mutable fields ([#8560]).

  * `@simd` now rejects invalid control flow (`@goto` / break / continue) in the inner loop body at compile time ([#8624]).

  * Givens type doesn't have a size anymore and is no longer a subtype of AbstractMatrix ([#8660])

  * OpenBLAS 64-bit (ILP64) interface is now compiled with a `64_` suffix ([#8734]) to avoid conflicts with external libraries using a 32-bit BLAS ([#4923]).

  * The `machinefile` now supports a host count ([#7616]).

  * Added optional rounding argument to floating-point constructors ([#8845]).

  * `code_native` now outputs branch labels ([#8897]).

  * Streamlined random number generation APIs [#8246].
    The default `rand` no longer uses global state in the underlying C library,
    dSFMT, making it closer to being thread-safe ([#8399], [#8832]).
    All APIs can now take an `AbstractRNG` argument ([#8854], [#9065]).
    The APIs accepting a range argument are extended to accept an arbitrary
    `AbstractArray` ([#9049]).
    Passing a range of `BigInt` to `rand` or `rand!` is now supported ([#9122]).
    There are speed improvements across the board ([#8808], [#8941], [#8958], [#9083]).

  * Significantly faster `randn` ([#9126], [#9132]).

  * The `randexp` and `randexp!` functions are exported ([#9144])


Deprecated or removed
---------------------

  * `median` and `median!` no longer accept a `checknan` keyword argument ([#8605]).

  * `inf` and `nan` are now deprecated in favor of `T(Inf)` and `NaN`, respectively ([#8776]).

  * `oftype(T::Type, x)` is deprecated in favor of `convert(T,x)` (or `T(x)`).

  * `{...}` syntax is deprecated in favor of `Any[...]` ([#8578]).

  * `itrunc`, `ifloor`, `iceil` and `iround` are deprecated in favour of
  `trunc{T<:Integer}(T,x)`, `floor{T<:Integer}(T,x)`, etc.. `trunc` is now
  always bound-checked;`Base.unsafe_trunc` provides the old unchecked `itrunc`
  behaviour ([#9133]).
 
  * `squeeze` now requires that passed dimension(s) are an `Int` or tuple of `Int`s;
    calling `squeeze` with an arbitrary iterator is deprecated ([#9271]).
    Additionally, passed dimensions must be unique and correspond to extant
    dimensions of the input array.
  

Julia v0.3.0 Release Notes
==========================

New language features
---------------------

  * Greatly enhanced performance for passing and returning `Tuple`s ([#4042]).

  * `Tuple`s (of `Integer`s, `Symbol`s, or `Bool`s) can now be used as type
    parameters ([#5164]).

  * An additional default "inner" constructor accepting any arguments is now
    generated. Constructors that look like `MyType(a, b) = new(a, b)` do not
    need to be added manually ([#4026], [#7071]).

  * Expanded array type hierarchy to include an abstract `DenseArray` for
    in-memory arrays with standard strided storage ([#987], [#2345],
    [#6212]).

  * When reloading code, types whose definitions have not changed can be
    ignored in some cases.

  * Binary `~` now parses as a vararg macro call to `@~`.
    For example `x~y~z` => `@~ x y z` ([#4882]).

  * Structure fields can now be accessed by index ([#4806]).

  * If a module contains a function `__init__()`, it will be called when
    the module is first loaded, and on process startup if a pre-compiled
    version of the module is present ([#1268]).

  * Multi-line comments ([#69], [#6128]): `#= .... =#`

  * `--check-bounds=yes|no` compiler option

  * Unicode identifiers are normalized (NFC) so that different encodings
    of equivalent strings are treated as the same identifier ([#5462]).

  * The set of characters permitted in identifiers has been restricted based
    on Unicode categories. Generally, punctuation, formatting and control
    characters, and operator symbols are not allowed in identifiers.
    Number-like characters cannot begin identifiers ([#5936]).

  * Define a limited number of infix Unicode operators ([#552], [#6582]):

    | Precedence class | Operators (with synonyms, if any)
    | ---------------- | ---------------------------------
    |   ==             |  ≥ (>=) ≤ (<=) ≡ (===) ≠ (!=) ≢ (!==) .≥ (.>=) .≤ (.<=) .!= (.≠) ∈ (`in`) ∉ (`(x,y)->!in(x, y)`) ∋ (`(x,y)->in(y, x)`) ∌ (`(x,y)->!in(y, x)`) ⊆ (`issubset`) ⊈ (`(x,y)->!issubset(x, y)`) ⊊ (`(x,y)->x⊆y && x!=y`) |
    |   +              | ∪ (`union`) |
    |   *              | ÷ (`div`) ⋅ (`dot`) × (`cross`) ∩ (`intersect`) |
    |   unary          | √ ∛ |

    In addition to these, many of the Unicode operator symbols are parsed
    as infix operators and are available for user-defined methods ([#6929]).

  * Improved reporting of syntax errors ([#6179])

  * `break` inside a `for` loop with multiple ranges now exits the entire loop nest ([#5154])

  * Local goto statements using the `@goto` and `@label` macros. ([#101])

REPL improvements
-----------------

  * New native-Julia REPL implementation, eliminating many problems
    stemming from the old GNU Readline-based REPL ([#6270]).

  * Tab-substitution of LaTeX math symbols (e.g. `\alpha` by `α`) ([#6911]).
    This also works in IJulia and in Emacs ([#6920]).

  * `workspace()` function for obtaining a fresh workspace ([#1195]).

Library improvements
--------------------

  * `isequal` now compares all numbers by value, ignoring type ([#6624]).

  * Implement limited shared-memory parallelism with `SharedArray`s ([#5380]).

  * Well-behaved floating-point ranges ([#2333], [#5636]).
    Introduced the `FloatRange` type for floating-point ranges with a step,
    which will give intuitive/correct results for classically problematic
    ranges like `0.1:0.1:0.3`, `0.0:0.7:2.1` or `1.0:1/49:27.0`.

  * `mod2pi` function ([#4799], [#4862]).

  * New functions `minmax` and `extrema` ([#5275]).

  * New macros `@edit`, `@less`, `@code_typed`, `@code_lowered`, `@code_llvm` and `@code_native` that all function like `@which` ([#5832]).

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

  * `GitHub` module for interacting with the GitHub API.

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

    * `push!` and `unshift!` can push multiple arguments ([#4782]).

    * `writedlm` and `writecsv` now accept any iterable collection of
      iterable rows, in addition to `AbstractArray` arguments, and the
      `writedlm` delimiter can be any printable object (e.g. a
      `String`) instead of just a `Char`.

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

    * `expm1` and `log1p` now support complex arguments ([#3141]).

    * Broadcasting `.//` is now included ([#7094]).

    * `prevfloat` and `nextfloat` now saturate at -Inf and Inf,
      respectively, and have otherwise been fixed to follow the IEEE-754
      standard functions `nextDown` and `nextUp` ([#5025]).

    * New function `widen` for widening numeric types and values, and `widemul`
      for multiplying to a larger type ([#6169]).

    * `polygamma`, `digamma`, and `trigamma` now accept complex
      arguments, and `zeta(s, z)` now provides the Hurwitz zeta ([#7125]).

    * Narrow integer types (< 32 bits) are promoted to `Float64` rather
      than to `Float32` by `float(x)` ([#7390]).

  * `String` improvements

    * Triple-quoted regex strings, `r"""..."""` ([#4934]).

    * New string type, `UTF16String` ([#4930]), constructed by
      `utf16(s)` from another string, a `Uint16` array or pointer, or
      a byte array (possibly prefixed by a byte-order marker to
      indicate endian-ness).  Its data is internally `NULL`-terminated
      for passing to C ([#7016]).

    * `CharString` is renamed to `UTF32String` ([#4943]), and its data
      is now internally `NULL`-terminated for passing to C ([#7016]).
      `CharString(c::Char...)` is deprecated in favor of `utf32(c...)`,
      and `utf32(s)` otherwise has functionality similar to `utf16(s)`.

    * New `WString` and `wstring` synonyms for either `UTF16String`
      and `utf16` or `UTF32String` and `utf32`, respectively, depending
      on the width of `Cwchar_t` ([#7016]).

    * `normalize_string` function to perform Unicode normalization,
      case-folding, and other transformations ([#5576]).

    * `pointer(s, i=1)` for `ByteString`, `UTF16String`, `UTF32String`,
      and `SubString`s thereof ([#5703]).

    * `bytestring` is automatically called on `String` arguments for
      conversion to `Ptr{Uint8}` in `ccall` ([#5677]).

  * Linear algebra improvements

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

      * New `UniformScaling` matrix type and identity `I` constant ([#5810]).

      * None of the concrete matrix factorization types are exported from `Base`
        by default anymore.

    * Sparse linear algebra

      * 1-d sparse `getindex` has been implemented ([#7047])

      * Faster sparse `getindex` ([#7131]).

      * Faster sparse `kron` ([#4958]).

      * `sparse(A) \ B` now supports a matrix `B` of right-hand sides ([#5196]).

      * `eigs(A, sigma)` now uses shift-and-invert for nonzero shifts `sigma` and inverse iteration for `which="SM"`. If `sigma==nothing` (the new default), computes ordinary (forward) iterations. ([#5776])

      * `sprand` is faster, and whether any entry is nonzero is now determined independently with the specified probability ([#6726]).

    * Dense linear algebra for special matrix types

      * Interconversions between the special matrix types `Diagonal`, `Bidiagonal`,
        `SymTridiagonal`, `Triangular`, and `Triangular`, and `Matrix` are now allowed
        for matrices which are representable in both source and destination types. ([5e3f074b])

      * Allow for addition and subtraction over mixed matrix types, automatically promoting
        the result to the denser matrix type ([a448e080], [#5927])

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

      * parametrize `Triangular` on matrix type ([#7064])

      * Lyapunov / Sylvester solver ([#7435])

      * `eigvals` for `Symmetric`, `Tridiagonal` and `Hermitian` matrices now
        support additional method signatures: ([#3688], [#6652], [#6678], [#7647])
        - `eigvals(M, el, eu)` finds all eigenvalues in the interval `(el, eu]`
        - `eigvals(M, il:iu)` finds the `il`th through the `iu`th eigenvalues (in ascending order)

    * Dense linear algebra for generic matrix element types

      * LU factorization ([#5381] and [#5430])

      * QR factorization ([#5526])

  * New function `deleteat!` deletes a specified index or indices and
    returns the updated collection

  * The `setenv` function for external processes now accepts a `dir` keyword
    argument for specifying the directory to start the child process in ([#4888]).

  * Constructors for collections (`Set`, `Dict`, etc.) now generally accept a
    single iterable argument giving the elements of the collection ([#4996], [#4871])

  * Ranges and arrays with the same elements are now unequal. This allows hashing
    and comparing ranges to be faster. ([#5778])

  * Broadcasting now works on arbitrary `AbstractArrays` ([#5387])

  * Reduction functions that accept a pre-allocated output array, including
    `sum!`, `prod!`, `maximum!`, `minimum!`, `all!`, `any!` ([#6197], [#5387])

  * Faster performance on `fill!` and `copy!` for array types not supporting
    efficient linear indexing ([#5671], [#5387])

  * Changes to range types ([#5585])

    * `Range` is now the abstract range type, instead of `Ranges`

    * New function `range` for constructing ranges by length

    * `Range` is now `StepRange`, and `Range1` is now `UnitRange`. Their
      constructors accept end points instead of lengths. Both are subtypes of a
      new abstract type `OrdinalRange`.

    * Ranges now support `BigInt` and general ordinal types.

    * Very large ranges (e.g. `0:typemax(Int)`) can now be constructed, but some
      operations (e.g. `length`) will raise an `OverflowError`.

  * Extended API for `cov` and `cor`, which accept keyword arguments `vardim`,
    `corrected`, and `mean` ([#6273])

  * New functions `randsubseq` and `randsubseq!` to create a random subsequence of an array ([#6726])

  * New macro `@evalpoly` for efficient inline evaluation of polynomials ([#7146]).

  * The signal filtering function `filt` now accepts an optional initial filter state vector. A new in-place function `filt!` is also exported. ([#7513])

  * Significantly faster `cumsum` and `cumprod`. ([#7359])

  * Implement `findmin` and `findmax` over specified array dimensions. ([#6716])

  * Support memory-mapping of files with offsets on Windows. ([#7242])

  * Catch writes to protect memory, such as when trying to modify a mmapped file opened in read-only mode. ([#3434])

Environment improvements
------------------------

  * New `--code-coverage` and `--track-allocation` startup features allow one to measure the number of executions or the amount of memory allocated, respectively, at each line of code. ([#5423],[#7464])

  * `Profile.init` now accepts keyword arguments, and returns the current settings when no arguments are supplied. ([#7365])

Build improvements
------------------

  * Dependencies are now verified against stored MD5/SHA512 hashes, to ensure
    that the correct file has been downloaded and was not modified. ([#6773])


Deprecated or removed
---------------------

  * `convert(Ptr{T1}, x::Array{T2})` is now deprecated unless `T1 == T2`
    or `T1 == Void` ([#6073]).  (You can still explicitly `convert`
    one pointer type into another if needed.)

  * `Sys.shlib_ext` has been renamed to `Sys.dlext`

  * `dense` is deprecated in favor of `full` ([#4759])

  * The `Stat` type is renamed `StatStruct` ([#4670])

  * `set_rounding`, `get_rounding` and `with_rounding` now take an additional
    argument specifying the floating point type to which they apply. The old
    behaviour and `[get/set/with]_bigfloat_rounding` functions are deprecated ([#5007])

  * `cholpfact` and `qrpfact` are deprecated in favor of keyword arguments in
    `cholfact(..., pivot=true)` and `qrfact(..., pivot=true)` ([#5330])

  * `symmetrize!` is deprecated in favor of `Base.LinAlg.copytri!` ([#5427])

  * `myindexes` has been renamed to `localindexes` ([#5475])

  * `factorize!` is deprecated in favor of `factorize`. ([#5526])

  * `nnz` counts the number of structural nonzeros in a sparse
    matrix. Use `countnz` for the actual number of nonzeros. ([#6769])

  * `setfield` is renamed `setfield!` ([#5748])

  * `put` and `take` are renamed `put!` and `take!` ([#5511])

  * `put!` now returns its first argument, the remote reference ([#5819])

  * `read` methods that modify a passed array are now called `read!` ([#5970])

  * `infs` and `nans` are deprecated in favor of the more general `fill`.

  * `*` and `div` are no longer supported for `Char`.

  * `Range` is renamed `StepRange` and `Range1` is renamed `UnitRange`.
    `Ranges` is renamed `Range`.

  * `bitmix` is replaced by a 2-argument form of `hash`.

  * `readsfrom` and `writesto` are replaced by `open` ([#6948]).

  * `insert!` now throws a `BoundsError` if
    `index > length(collection)+1` ([#7373]).

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

  * Implementation of reduction functions (including `reduce`, `mapreduce`, `sum`, `prod`,
    `maximum`, `minimum`, `all`, and `any`) are refactored, with improved type stability,
    efficiency, and consistency. ([#6116], [#7035], [#7061], [#7106])

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

[packages chapter]: http://docs.julialang.org/en/latest/manual/packages/
[sorting functions]: http://docs.julialang.org/en/latest/stdlib/sort/
[pairwise summation]: https://en.wikipedia.org/wiki/Pairwise_summation
[a448e080]: https://github.com/JuliaLang/julia/commit/a448e080dc736c7fb326426dfcb2528be36973d3
[5e3f074b]: https://github.com/JuliaLang/julia/commit/5e3f074b9173044a0a4219f9b285879ff7cec041
<!--- generated by NEWS-update.jl: -->
[#13]: https://github.com/JuliaLang/julia/issues/13
[#69]: https://github.com/JuliaLang/julia/issues/69
[#70]: https://github.com/JuliaLang/julia/issues/70
[#101]: https://github.com/JuliaLang/julia/issues/101
[#485]: https://github.com/JuliaLang/julia/issues/485
[#552]: https://github.com/JuliaLang/julia/issues/552
[#907]: https://github.com/JuliaLang/julia/issues/907
[#987]: https://github.com/JuliaLang/julia/issues/987
[#1195]: https://github.com/JuliaLang/julia/issues/1195
[#1268]: https://github.com/JuliaLang/julia/issues/1268
[#1484]: https://github.com/JuliaLang/julia/issues/1484
[#1539]: https://github.com/JuliaLang/julia/issues/1539
[#1571]: https://github.com/JuliaLang/julia/issues/1571
[#1815]: https://github.com/JuliaLang/julia/issues/1815
[#1817]: https://github.com/JuliaLang/julia/issues/1817
[#2204]: https://github.com/JuliaLang/julia/issues/2204
[#2212]: https://github.com/JuliaLang/julia/issues/2212
[#2265]: https://github.com/JuliaLang/julia/issues/2265
[#2333]: https://github.com/JuliaLang/julia/issues/2333
[#2345]: https://github.com/JuliaLang/julia/issues/2345
[#2370]: https://github.com/JuliaLang/julia/issues/2370
[#2375]: https://github.com/JuliaLang/julia/issues/2375
[#2380]: https://github.com/JuliaLang/julia/issues/2380
[#2403]: https://github.com/JuliaLang/julia/issues/2403
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
[#2720]: https://github.com/JuliaLang/julia/issues/2720
[#2725]: https://github.com/JuliaLang/julia/issues/2725
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
[#3141]: https://github.com/JuliaLang/julia/issues/3141
[#3148]: https://github.com/JuliaLang/julia/issues/3148
[#3149]: https://github.com/JuliaLang/julia/issues/3149
[#3233]: https://github.com/JuliaLang/julia/issues/3233
[#3272]: https://github.com/JuliaLang/julia/issues/3272
[#3344]: https://github.com/JuliaLang/julia/issues/3344
[#3350]: https://github.com/JuliaLang/julia/issues/3350
[#3434]: https://github.com/JuliaLang/julia/issues/3434
[#3439]: https://github.com/JuliaLang/julia/issues/3439
[#3467]: https://github.com/JuliaLang/julia/issues/3467
[#3468]: https://github.com/JuliaLang/julia/issues/3468
[#3483]: https://github.com/JuliaLang/julia/issues/3483
[#3523]: https://github.com/JuliaLang/julia/issues/3523
[#3605]: https://github.com/JuliaLang/julia/issues/3605
[#3649]: https://github.com/JuliaLang/julia/issues/3649
[#3665]: https://github.com/JuliaLang/julia/issues/3665
[#3688]: https://github.com/JuliaLang/julia/issues/3688
[#3697]: https://github.com/JuliaLang/julia/issues/3697
[#3719]: https://github.com/JuliaLang/julia/issues/3719
[#3759]: https://github.com/JuliaLang/julia/issues/3759
[#3790]: https://github.com/JuliaLang/julia/issues/3790
[#3819]: https://github.com/JuliaLang/julia/issues/3819
[#3872]: https://github.com/JuliaLang/julia/issues/3872
[#3874]: https://github.com/JuliaLang/julia/issues/3874
[#3878]: https://github.com/JuliaLang/julia/issues/3878
[#3932]: https://github.com/JuliaLang/julia/issues/3932
[#3949]: https://github.com/JuliaLang/julia/issues/3949
[#3989]: https://github.com/JuliaLang/julia/issues/3989
[#4014]: https://github.com/JuliaLang/julia/issues/4014
[#4026]: https://github.com/JuliaLang/julia/issues/4026
[#4039]: https://github.com/JuliaLang/julia/issues/4039
[#4042]: https://github.com/JuliaLang/julia/issues/4042
[#4044]: https://github.com/JuliaLang/julia/issues/4044
[#4048]: https://github.com/JuliaLang/julia/issues/4048
[#4055]: https://github.com/JuliaLang/julia/issues/4055
[#4112]: https://github.com/JuliaLang/julia/issues/4112
[#4130]: https://github.com/JuliaLang/julia/issues/4130
[#4177]: https://github.com/JuliaLang/julia/issues/4177
[#4235]: https://github.com/JuliaLang/julia/issues/4235
[#4284]: https://github.com/JuliaLang/julia/issues/4284
[#4383]: https://github.com/JuliaLang/julia/issues/4383
[#4412]: https://github.com/JuliaLang/julia/issues/4412
[#4670]: https://github.com/JuliaLang/julia/issues/4670
[#4759]: https://github.com/JuliaLang/julia/issues/4759
[#4766]: https://github.com/JuliaLang/julia/issues/4766
[#4775]: https://github.com/JuliaLang/julia/issues/4775
[#4782]: https://github.com/JuliaLang/julia/issues/4782
[#4799]: https://github.com/JuliaLang/julia/issues/4799
[#4806]: https://github.com/JuliaLang/julia/issues/4806
[#4811]: https://github.com/JuliaLang/julia/issues/4811
[#4819]: https://github.com/JuliaLang/julia/issues/4819
[#4862]: https://github.com/JuliaLang/julia/issues/4862
[#4870]: https://github.com/JuliaLang/julia/issues/4870
[#4871]: https://github.com/JuliaLang/julia/issues/4871
[#4874]: https://github.com/JuliaLang/julia/issues/4874
[#4882]: https://github.com/JuliaLang/julia/issues/4882
[#4888]: https://github.com/JuliaLang/julia/issues/4888
[#4923]: https://github.com/JuliaLang/julia/issues/4923
[#4930]: https://github.com/JuliaLang/julia/issues/4930
[#4934]: https://github.com/JuliaLang/julia/issues/4934
[#4943]: https://github.com/JuliaLang/julia/issues/4943
[#4958]: https://github.com/JuliaLang/julia/issues/4958
[#4967]: https://github.com/JuliaLang/julia/issues/4967
[#4996]: https://github.com/JuliaLang/julia/issues/4996
[#5007]: https://github.com/JuliaLang/julia/issues/5007
[#5025]: https://github.com/JuliaLang/julia/issues/5025
[#5059]: https://github.com/JuliaLang/julia/issues/5059
[#5076]: https://github.com/JuliaLang/julia/issues/5076
[#5154]: https://github.com/JuliaLang/julia/issues/5154
[#5164]: https://github.com/JuliaLang/julia/issues/5164
[#5196]: https://github.com/JuliaLang/julia/issues/5196
[#5214]: https://github.com/JuliaLang/julia/issues/5214
[#5255]: https://github.com/JuliaLang/julia/issues/5255
[#5263]: https://github.com/JuliaLang/julia/issues/5263
[#5275]: https://github.com/JuliaLang/julia/issues/5275
[#5277]: https://github.com/JuliaLang/julia/issues/5277
[#5330]: https://github.com/JuliaLang/julia/issues/5330
[#5358]: https://github.com/JuliaLang/julia/issues/5358
[#5380]: https://github.com/JuliaLang/julia/issues/5380
[#5381]: https://github.com/JuliaLang/julia/issues/5381
[#5387]: https://github.com/JuliaLang/julia/issues/5387
[#5403]: https://github.com/JuliaLang/julia/issues/5403
[#5413]: https://github.com/JuliaLang/julia/issues/5413
[#5423]: https://github.com/JuliaLang/julia/issues/5423
[#5427]: https://github.com/JuliaLang/julia/issues/5427
[#5428]: https://github.com/JuliaLang/julia/issues/5428
[#5430]: https://github.com/JuliaLang/julia/issues/5430
[#5462]: https://github.com/JuliaLang/julia/issues/5462
[#5464]: https://github.com/JuliaLang/julia/issues/5464
[#5468]: https://github.com/JuliaLang/julia/issues/5468
[#5475]: https://github.com/JuliaLang/julia/issues/5475
[#5511]: https://github.com/JuliaLang/julia/issues/5511
[#5526]: https://github.com/JuliaLang/julia/issues/5526
[#5545]: https://github.com/JuliaLang/julia/issues/5545
[#5576]: https://github.com/JuliaLang/julia/issues/5576
[#5585]: https://github.com/JuliaLang/julia/issues/5585
[#5636]: https://github.com/JuliaLang/julia/issues/5636
[#5671]: https://github.com/JuliaLang/julia/issues/5671
[#5677]: https://github.com/JuliaLang/julia/issues/5677
[#5703]: https://github.com/JuliaLang/julia/issues/5703
[#5726]: https://github.com/JuliaLang/julia/issues/5726
[#5737]: https://github.com/JuliaLang/julia/issues/5737
[#5748]: https://github.com/JuliaLang/julia/issues/5748
[#5776]: https://github.com/JuliaLang/julia/issues/5776
[#5778]: https://github.com/JuliaLang/julia/issues/5778
[#5810]: https://github.com/JuliaLang/julia/issues/5810
[#5811]: https://github.com/JuliaLang/julia/issues/5811
[#5819]: https://github.com/JuliaLang/julia/issues/5819
[#5827]: https://github.com/JuliaLang/julia/issues/5827
[#5832]: https://github.com/JuliaLang/julia/issues/5832
[#5927]: https://github.com/JuliaLang/julia/issues/5927
[#5936]: https://github.com/JuliaLang/julia/issues/5936
[#5939]: https://github.com/JuliaLang/julia/issues/5939
[#5970]: https://github.com/JuliaLang/julia/issues/5970
[#6056]: https://github.com/JuliaLang/julia/issues/6056
[#6057]: https://github.com/JuliaLang/julia/issues/6057
[#6073]: https://github.com/JuliaLang/julia/issues/6073
[#6081]: https://github.com/JuliaLang/julia/issues/6081
[#6116]: https://github.com/JuliaLang/julia/issues/6116
[#6128]: https://github.com/JuliaLang/julia/issues/6128
[#6169]: https://github.com/JuliaLang/julia/issues/6169
[#6179]: https://github.com/JuliaLang/julia/issues/6179
[#6197]: https://github.com/JuliaLang/julia/issues/6197
[#6212]: https://github.com/JuliaLang/julia/issues/6212
[#6270]: https://github.com/JuliaLang/julia/issues/6270
[#6273]: https://github.com/JuliaLang/julia/issues/6273
[#6582]: https://github.com/JuliaLang/julia/issues/6582
[#6624]: https://github.com/JuliaLang/julia/issues/6624
[#6652]: https://github.com/JuliaLang/julia/issues/6652
[#6678]: https://github.com/JuliaLang/julia/issues/6678
[#6716]: https://github.com/JuliaLang/julia/issues/6716
[#6726]: https://github.com/JuliaLang/julia/issues/6726
[#6739]: https://github.com/JuliaLang/julia/issues/6739
[#6769]: https://github.com/JuliaLang/julia/issues/6769
[#6773]: https://github.com/JuliaLang/julia/issues/6773
[#6911]: https://github.com/JuliaLang/julia/issues/6911
[#6920]: https://github.com/JuliaLang/julia/issues/6920
[#6929]: https://github.com/JuliaLang/julia/issues/6929
[#6948]: https://github.com/JuliaLang/julia/issues/6948
[#7016]: https://github.com/JuliaLang/julia/issues/7016
[#7035]: https://github.com/JuliaLang/julia/issues/7035
[#7047]: https://github.com/JuliaLang/julia/issues/7047
[#7061]: https://github.com/JuliaLang/julia/issues/7061
[#7064]: https://github.com/JuliaLang/julia/issues/7064
[#7071]: https://github.com/JuliaLang/julia/issues/7071
[#7094]: https://github.com/JuliaLang/julia/issues/7094
[#7106]: https://github.com/JuliaLang/julia/issues/7106
[#7125]: https://github.com/JuliaLang/julia/issues/7125
[#7131]: https://github.com/JuliaLang/julia/issues/7131
[#7146]: https://github.com/JuliaLang/julia/issues/7146
[#7236]: https://github.com/JuliaLang/julia/issues/7236
[#7242]: https://github.com/JuliaLang/julia/issues/7242
[#7311]: https://github.com/JuliaLang/julia/issues/7311
[#7359]: https://github.com/JuliaLang/julia/issues/7359
[#7365]: https://github.com/JuliaLang/julia/issues/7365
[#7373]: https://github.com/JuliaLang/julia/issues/7373
[#7390]: https://github.com/JuliaLang/julia/issues/7390
[#7435]: https://github.com/JuliaLang/julia/issues/7435
[#7464]: https://github.com/JuliaLang/julia/issues/7464
[#7513]: https://github.com/JuliaLang/julia/issues/7513
[#7616]: https://github.com/JuliaLang/julia/issues/7616
[#7647]: https://github.com/JuliaLang/julia/issues/7647
[#7654]: https://github.com/JuliaLang/julia/issues/7654
[#7704]: https://github.com/JuliaLang/julia/issues/7704
[#7917]: https://github.com/JuliaLang/julia/issues/7917
[#7992]: https://github.com/JuliaLang/julia/issues/7992
[#8011]: https://github.com/JuliaLang/julia/issues/8011
[#8089]: https://github.com/JuliaLang/julia/issues/8089
[#8152]: https://github.com/JuliaLang/julia/issues/8152
[#8246]: https://github.com/JuliaLang/julia/issues/8246
[#8297]: https://github.com/JuliaLang/julia/issues/8297
[#8399]: https://github.com/JuliaLang/julia/issues/8399
[#8423]: https://github.com/JuliaLang/julia/issues/8423
[#8501]: https://github.com/JuliaLang/julia/issues/8501
[#8560]: https://github.com/JuliaLang/julia/issues/8560
[#8578]: https://github.com/JuliaLang/julia/issues/8578
[#8605]: https://github.com/JuliaLang/julia/issues/8605
[#8624]: https://github.com/JuliaLang/julia/issues/8624
[#8660]: https://github.com/JuliaLang/julia/issues/8660
[#8712]: https://github.com/JuliaLang/julia/issues/8712
[#8734]: https://github.com/JuliaLang/julia/issues/8734
[#8776]: https://github.com/JuliaLang/julia/issues/8776
[#8791]: https://github.com/JuliaLang/julia/issues/8791
[#8792]: https://github.com/JuliaLang/julia/issues/8792
[#8808]: https://github.com/JuliaLang/julia/issues/8808
[#8816]: https://github.com/JuliaLang/julia/issues/8816
[#8827]: https://github.com/JuliaLang/julia/issues/8827
[#8832]: https://github.com/JuliaLang/julia/issues/8832
[#8845]: https://github.com/JuliaLang/julia/issues/8845
[#8854]: https://github.com/JuliaLang/julia/issues/8854
[#8867]: https://github.com/JuliaLang/julia/issues/8867
[#8872]: https://github.com/JuliaLang/julia/issues/8872
[#8897]: https://github.com/JuliaLang/julia/issues/8897
[#8905]: https://github.com/JuliaLang/julia/issues/8905
[#8941]: https://github.com/JuliaLang/julia/issues/8941
[#8958]: https://github.com/JuliaLang/julia/issues/8958
[#9049]: https://github.com/JuliaLang/julia/issues/9049
[#9065]: https://github.com/JuliaLang/julia/issues/9065
[#9083]: https://github.com/JuliaLang/julia/issues/9083
[#9122]: https://github.com/JuliaLang/julia/issues/9122
[#9126]: https://github.com/JuliaLang/julia/issues/9126
[#9132]: https://github.com/JuliaLang/julia/issues/9132
[#9133]: https://github.com/JuliaLang/julia/issues/9133
[#9144]: https://github.com/JuliaLang/julia/issues/9144
[#9249]: https://github.com/JuliaLang/julia/issues/9249
[#9261]: https://github.com/JuliaLang/julia/issues/9261
[#9271]: https://github.com/JuliaLang/julia/issues/9271
[#9294]: https://github.com/JuliaLang/julia/issues/9294
