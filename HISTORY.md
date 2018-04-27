Julia v0.6.0 Release Notes
==========================

New language features
---------------------

  * New type system capabilities ([#8974], [#18457])

    + Type parameter constraints can refer to previous parameters, e.g.
      `type Foo{R<:Real, A<:AbstractArray{R}}`. Can also be used in method definitions.

    + New syntax `Array{T} where T<:Integer`, indicating a union of types over all
      specified values of `T` (represented by a `UnionAll` type). This provides behavior
      similar to parametric methods or `typealias`, but can be used anywhere a type is
      accepted. This syntax can also be used in method definitions, e.g.
      `function inv(M::Matrix{T}) where T<:AbstractFloat`.
      Anonymous functions can have type parameters via the syntax
      `((x::Array{T}) where T<:Real) -> 2x`.

    + Implicit type parameters, e.g. `Vector{<:Real}` is equivalent to
      `Vector{T} where T<:Real`, and similarly for `Vector{>:Int}` ([#20414]).

    + Much more accurate subtype and type intersection algorithms. Method sorting and
      identification of equivalent and ambiguous methods are improved as a result.

Language changes
----------------

  * "Inner constructor" syntax for parametric types is deprecated. For example,
    in this definition:
    ```
    type Foo{T,S<:Real}
        x
        Foo(x) = new(x)
    end
    ```
    the syntax `Foo(x) = new(x)` actually defined a constructor for `Foo{T,S}`,
    i.e. the case where the type parameters are specified. For clarity, this
    definition now must be written as `Foo{T,S}(x) where {T,S<:Real} = new(x)`
    ([#11310], [#20308]).

  * The keywords used to define types have changed ([#19157], [#20418]).

    + `immutable` changes to `struct`

    + `type` changes to `mutable struct`

    + `abstract` changes to `abstract type ... end`

    + `bitstype 32 Char` changes to `primitive type Char 32 end`

    In 0.6, `immutable` and `type` are still allowed as synonyms without a deprecation
    warning.

  * Multi-line and single-line nonstandard command literals have been added. A
    nonstandard command literal is like a nonstandard string literal, but the
    syntax uses backquotes (``` ` ```) instead of double quotes, and the
    resulting macro called is suffixed with `_cmd`. For instance, the syntax
    ``` q`xyz` ``` is equivalent to `@q_cmd "xyz"` ([#18644]).

  * Nonstandard string and command literals can now be qualified with their
    module. For instance, `Base.r"x"` is now parsed as `Base.@r_str "x"`.
    Previously, this syntax parsed as an implicit multiplication ([#18690]).

  * For every binary operator `⨳`, `a .⨳ b` is now automatically equivalent to
    the `broadcast` call `(⨳).(a, b)`.  Hence, one no longer defines methods
    for `.*` etcetera.  This also means that "dot operations" automatically
    fuse into a single loop, along with other dot calls `f.(x)` ([#17623]).
    Similarly for unary operators ([#20249]).

  * Newly defined methods are no longer callable from the same dynamic runtime
    scope they were defined in ([#17057]).

  * `isa` is now parsed as an infix operator with the same precedence as `in`
    ([#19677]).

  * `@.` is now parsed as `@__dot__`, and can be used to add dots to
    every function call, operator, and assignment in an expression ([#20321]).

  * The identifier `_` can be assigned, but accessing its value is deprecated,
    allowing this syntax to be used in the future for discarding values ([#9343],
    [#18251], [#20328]).

  * The `typealias` keyword is deprecated, and should be replaced with
    `Vector{T} = Array{T,1}` or a `const` assignment ([#20500]).

  * Experimental feature: `x^n` for integer literals `n` (e.g. `x^3`
    or `x^-3`) is now lowered to `Base.literal_pow(^, x, Val{n})`, to enable
    compile-time specialization for literal integer exponents ([#20530], [#20889]).

Breaking changes
----------------

This section lists changes that do not have deprecation warnings.

  * `readline`, `readlines` and `eachline` return lines without line endings by default.
    You *must* use `readline(s, chomp=false)`, etc. to get the old behavior where
    returned lines include trailing end-of-line character(s) ([#19944]).

  * `String`s no longer have a `.data` field (as part of a significant performance
    improvement). Use `Vector{UInt8}(str)` to access a string as a byte array.
    However, allocating the `Vector` object has overhead. You can also use
    `codeunit(str, i)` to access the `i`th byte of a `String`.
    Use `sizeof(str)` instead of `length(str.data)`, and `pointer(str)` instead of
    `pointer(str.data)` ([#19449]).

  * Operations between `Float16` and `Integers` now return `Float16` instead of `Float32` ([#17261]).

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

  * Broadcast `A[I...] .= X` with entirely scalar indices `I` is deprecated as
    its behavior will change in the future.  Use `A[I...] = X` instead.

  * Operations like `.+` and `.*` on `Range` objects are now generic
    `broadcast` calls (see [above](#language-changes)) and produce an `Array`.
    If you want a `Range` result, use `+` and `*`, etcetera ([#17623]).

  * `broadcast` now treats `Ref` (except for `Ptr`) arguments as 0-dimensional
    arrays ([#18965]).

  * `broadcast` now handles missing data (`Nullable`s) allowing operations to
    be lifted over mixtures of `Nullable`s and scalars, as if the `Nullable`
    were like an array with zero or one element ([#16961], [#19787]).

  * The runtime now enforces when new method definitions can take effect ([#17057]).
    The flip-side of this is that new method definitions should now reliably actually
    take effect, and be called when evaluating new code ([#265]).

  * The array-scalar methods of `/`, `\`, `*`, `+`, and `-` now follow broadcast promotion
    rules. (Likewise for the now-deprecated array-scalar methods of `div`, `mod`, `rem`,
    `&`, `|`, and `xor`; see "Deprecated or removed" below.) ([#19692]).

  * `broadcast!(f, A)` now calls `f()` for each element of `A`, rather than doing `fill!(A, f())` ([#19722]).

  * `rmprocs` now throws an exception if requested workers have not been completely
    removed before `waitfor` seconds. With a `waitfor=0`, `rmprocs` returns immediately
    without waiting for worker exits.

  * `quadgk` has been moved from Base into a separate package ([#19741]).

  * The `Collections` module has been removed, and all functions defined therein have been
    moved to the `DataStructures` package ([#19800]).

  * The `RepString` type has been moved to the
    [LegacyStrings.jl package](https://github.com/JuliaArchive/LegacyStrings.jl).

  * In macro calls with parentheses, e.g. `@m(a=1)`, assignments are now parsed as
    `=` expressions, instead of as `kw` expressions ([#7669]).

  * When used as an infix operator, `~` is now parsed as a call to an ordinary operator
    with assignment precedence, instead of as a macro call ([#20406]).

  * (µ "micro" and ɛ "latin epsilon") are considered equivalent to
    the corresponding Greek characters in identifiers.  `\varepsilon`
    now tab-completes to U+03B5 (greek small letter epsilon) ([#19464]).

  * `retry` now inputs the keyword arguments `delays` and `check` instead of
    `n` and `max_delay`.  The previous functionality can be achieved setting
    `delays` to `ExponentialBackOff` ([#19331]).

  * `transpose(::AbstractVector)` now always returns a `RowVector` view of the input (which is a
     special 1×n-sized `AbstractMatrix`), not a `Matrix`, etc. In particular, for
     `v::AbstractVector` we now have `(v.').' === v` and `v.' * v` is a scalar ([#19670]).

  * Parametric types with "unspecified" parameters, such as `Array`, are now represented
    as `UnionAll` types instead of `DataType`s ([#18457]).

  * `Union` types have two fields, `a` and `b`, instead of a single `types` field.
    The empty type `Union{}` is represented by a singleton of type `TypeofBottom` ([#18457]).

  * The type `NTuple{N}` now refers to tuples where every element has the same type
    (since it is shorthand for `NTuple{N,T} where T`). To get the old behavior of matching
    any tuple, use `NTuple{N,Any}` ([#18457]).

  * `FloatRange` has been replaced by `StepRangeLen`, and the internal
    representation of `LinSpace` has changed. Aside from changes in
    the internal field names, this leads to several differences in
    behavior ([#18777]):

    + Both `StepRangeLen` and `LinSpace` can represent ranges of
      arbitrary object types---they are no longer limited to
      floating-point numbers.

    + For ranges that produce `Float64`, `Float32`, or `Float16`
      numbers, `StepRangeLen` can be used to produce values with
      little or no roundoff error due to internal arithmetic that is
      typically twice the precision of the output result.

    + To take advantage of this precision, `linspace(start, stop,
      len)` now returns a range of type `StepRangeLen` rather than
      `LinSpace` when `start` and `stop` are
      `FloatNN`. `LinSpace(start, stop, len)` always returns a
      `LinSpace`.

    + `StepRangeLen(a, step, len)` constructs an ordinary-precision range
      using the values and types of `a` and `step` as given, whereas
      `range(a, step, len)` will attempt to match inputs `a::FloatNN`
      and `step::FloatNN` to rationals and construct a `StepRangeLen`
      that internally uses twice-precision arithmetic.  These two
      outcomes exhibit differences in both precision and speed.

  * `A=>B` expressions are now parsed as calls instead of using `=>` as the
    expression head ([#20327]).

  * The `count` function no longer sums non-boolean values ([#20404])

  * The generic `getindex(::AbstractString, ::AbstractVector)` method's signature has been
    tightened to `getindex(::AbstractString, ::AbstractVector{<:Integer})`. Consequently,
    indexing into `AbstractString`s with non-`AbstractVector{<:Integer}` `AbstractVector`s
    now throws a `MethodError` in the absence of an appropriate specialization.
    (Previously such cases failed less explicitly with the exception of
    `AbstractVector{Bool}`, which now throws an `ArgumentError` noting that
    logical indexing into strings is not supported.)  ([#20248])

  * Bessel, Hankel, Airy, error, Dawson, eta, zeta, digamma, inverse digamma,
    trigamma, and polygamma special functions have been moved from Base to
    the
    [SpecialFunctions.jl package](https://github.com/JuliaMath/SpecialFunctions.jl)
    ([#20427]).  Note that `airy`, `airyx` and `airyprime` have been deprecated
    in favor of more specific functions (`airyai`, `airybi`, `airyaiprime`,
    `airybiprimex`, `airyaix`, `airybix`, `airyaiprimex`, `airybiprimex`)
    ([#18050]).

  * When a macro is called in the module in which that macro is defined, global variables
    in the macro are now correctly resolved in the macro definition environment. Breakage
    from this change commonly manifests as undefined variable errors that do not occur
    under 0.5. Fixing such breakage typically requires sprinkling additional `esc`s in
    the offending macro ([#15850]).

  * `write` on an `IOBuffer` now returns a signed integer in order to be
    consistent with other buffers ([#20609]).

  * The `<:Integer` division fallback `/(::Integer, ::Integer)`, which formerly
    inappropriately took precedence over other division methods for some
    mixed-integer-type division calls, has been removed ([#19779]).

  * `@async`, `@spawn`, `@spawnat`, `@fetch` and `@fetchfrom` no longer implicitly
    localize variables. Previously, the expression would be wrapped in an implicit
    `let` block  ([#19594]).

  * `parse` no longer accepts IPv4 addresses including leading zeros, octal, or hexadecimal.
    Convert IPv4 addresses including octal or hexadecimal to decimal, and remove leading
    zeros in decimal addresses ([#19811]).

  * Closures shipped for remote execution via `@spawn` or `remotecall` now automatically
    serialize globals defined under Main. For details, please refer to the paragraph
    on "Global variables" under the "Parallel computing" chapter in the manual ([#19594]).

  * `homedir` now determines the user's home directory via `libuv`'s `uv_os_homedir`,
    rather than from environment variables ([#19636]).

  * Workers now listen on an ephemeral port assigned by the OS. Previously workers would
    listen on the first free port available from 9009 ([#21818]).


Library improvements
--------------------

  * A new `@views` macro was added to convert a whole expression or block of code to
    use views for all slices ([#20164]).

  * `max`, `min`, and related functions (`minmax`, `maximum`, `minimum`, `extrema`)
     now return `NaN` for `NaN` arguments ([#12563]).

  * `oneunit(x)` function to return a dimensionful version of `one(x)`
    (which is clarified to mean a dimensionless quantity if `x` is dimensionful) ([#20268]).

  * The `chop` and `chomp` functions now return a `SubString` ([#18339]).

  * Numbered stackframes printed in stacktraces can now be opened in an editor by
    entering the corresponding number in the REPL and pressing `^Q` ([#19680]).

  * The REPL now supports something called *prompt pasting* ([#17599]).
    This activates when pasting text that starts with `julia> ` into the REPL.
    In that case, only expressions starting with `julia> ` are parsed, the rest are removed.
    This makes it possible to paste a chunk of code that has been copied from a REPL session
    without having to scrub away prompts and outputs.
    This can be disabled or enabled at will with `Base.REPL.enable_promptpaste(::Bool)`.

  * The function `print_with_color` can now take a color
    represented by an integer between 0 and 255 inclusive
    as its first argument ([#18473]). For a number-to-color mapping, please refer to
    [this chart](https://upload.wikimedia.org/wikipedia/commons/1/15/Xterm_256color_chart.svg).
    It is also possible to use numbers as colors in environment variables that customizes colors in the REPL.
    For example, to get orange warning messages, simply set `ENV["JULIA_WARN_COLOR"] = 208`.
    Please note that not all terminals support 256 colors.

  * The function `print_with_color` no longer prints text in bold by default ([#18628]).
    Instead, the function now take a keyword argument `bold::Bool`
    which determines whether to print in bold or not. On some terminals, printing a color in non bold
    results in slightly darker colors being printed than when printing in bold.
    Therefore, light versions of the colors are now supported.
    For the available colors see the help entry on `print_with_color`.

  * The default text style for REPL input and answers has been changed from bold to normal ([#11250]).
    They can be changed back to bold by setting the environment variables
    `JULIA_INPUT_COLOR` and `JULIA_ANSWER_COLOR` to `"bold"`.
    For example, one way of doing this is adding `ENV["JULIA_INPUT_COLOR"] = :bold`
    and `ENV["JULIA_ANSWER_COLOR"] = :bold` to the `.juliarc.jl` file. See the
    [manual section on customizing colors](https://docs.julialang.org/en/latest/manual/interacting-with-julia#Customizing-Colors-1)
    for more information.

  * The default color for info messages has been changed from blue to cyan
    ([#18442]), and for warning messages from red to yellow ([#18453]).  This
    can be changed back to the original colors by setting the environment
    variables `JULIA_INFO_COLOR` to `"blue"` and `JULIA_WARN_COLOR` to `"red"`.

  * Iteration utilities that wrap iterators and return other iterators (`enumerate`, `zip`, `rest`,
    `countfrom`, `take`, `drop`, `cycle`, `repeated`, `product`, `flatten`, `partition`) have been
    moved to the module `Base.Iterators` ([#18839]).

  * BitArrays can now be constructed from arbitrary iterables, in particular from generator expressions,
    e.g. `BitArray(isodd(x) for x = 1:100)` ([#19018]).

  * `hcat`, `vcat`, and `hvcat` now work with `UniformScaling` objects, so
    you can now do e.g. `[A I]` and it will concatenate an appropriately sized
    identity matrix ([#19305]).

  * New `accumulate` and `accumulate!` functions were added, which generalize `cumsum` and `cumprod`.
    Also known as a [scan](https://en.wikipedia.org/wiki/Prefix_sum) operation ([#18931]).

  * `reshape` now allows specifying one dimension with a `Colon()` (`:`) for the new shape, in which case
    that dimension's length will be computed such that its product with all the other dimensions is equal
    to the length of the original array ([#19919]).

  * The new `to_indices` function provides a uniform interface for index conversions,
    taking an array and a tuple of indices as arguments and returning a tuple of
    integers and/or arrays of supported scalar indices. It will throw an `ArgumentError`
    for any unsupported indices, and the returned arrays should be iterated over (and
    not indexed into) to support more efficient logical indexing ([#19730]).

    + Using colons (`:`) to represent a collection of indices is deprecated. They now must be
      explicitly converted to a specialized array of integers with the `to_indices` function.
      As a result, the type of `SubArray`s that represent views over colon indices has changed.

    + Logical indexing is now more efficient. Logical arrays are converted by `to_indices` to
      a lazy, iterable collection of indices that doesn't support indexing. A deprecation
      provides indexing support with O(n) lookup.

    + The performance of indexing with `CartesianIndex`es is also improved in many situations.

  * A new `titlecase` function was added, to capitalize the first character of each word within a string ([#19469]).

  * `any` and `all` now always short-circuit, and `mapreduce` never short-circuits ([#19543]).
    That is, not every member of the input iterable will be visited if a `true` (in the case of `any`) or
    `false` (in the case of `all`) value is found, and `mapreduce` will visit all members of the iterable.

  * Additional methods for `ones` and `zeros` functions were added
    to support the same signature as the `similar` function ([#19635]).

  * `count` now has a `count(itr)` method equivalent to `count(identity, itr)` ([#20403]).

  * Methods for `map` and `filter` with `Nullable` arguments have been implemented;
    the semantics are as if the `Nullable` were a container with zero or one elements ([#16961]).

  * New `@test_warn` and `@test_nowarn` macros were added in the `Base.Test` module to
    test for the presence or absence of warning messages ([#19903]).

  * `logging` can now be used to redirect `info`, `warn`, and `error` messages
    either universally or on a per-module/function basis ([#16213]).

  * New function `Base.invokelatest(f, args...)` to call the latest version
    of a function in circumstances where an older version may be called
    instead (e.g. in a function calling `eval`) ([#19784]).

  * A new `iszero(x)` function was added, to quickly check whether `x` is zero
    (or is all zeros, for an array) ([#19950]).

  * `notify` now returns a count of tasks woken up ([#19841]).

  * A new nonstandard string literal `raw"..."` was added,
    for creating strings with no interpolation or unescaping ([#19900]).

  * A new `Dates.Time` type was added that supports representing the time of day
    with up to nanosecond resolution ([#12274]).

  * Raising one or negative one to a negative integer power formerly threw a `DomainError`.
    One raised to any negative integer power now yields one, negative one raised to any
    negative even integer power now yields one, and negative one raised to any negative
    odd integer power now yields negative one. Similarly, raising `true` to any negative
    integer power now yields `true` rather than throwing a `DomainError` ([#18342]).

  * A new `@macroexpand` macro was added as a convenient alternative to the `macroexpand` function ([#18660]).

  * `invoke` now supports keyword arguments ([#20345]).

  * A new `ConjArray` type was added, as a wrapper type for lazy complex conjugation of arrays.
    Currently, it is used by default for the new `RowVector` type only, and
    enforces that both `transpose(vec)` and `ctranspose(vec)` are views not copies ([#20047]).

  * `rem` now accepts a `RoundingMode` argument via `rem(x, y, r::RoundingMode)`, yielding
    `x - y*round(x/y, r)` without intermediate rounding. In particular, `rem(x, y, RoundNearest)`
    yields a value in the interval `[-abs(y)/2, abs(y)/2]`), which corresponds to the IEE754
    `remainder` function. Similarly, `rem2pi(x, r::RoundingMode)` now exists as well, yielding
    `rem(x, 2pi, r::RoundingMode)` but with greater accuracy ([#10946]).

  * `map[!]` and `broadcast[!]` now have dedicated methods for sparse/structured
    vectors/matrices. Specifically, `map[!]` and `broadcast[!]` over combinations including
    one or more `SparseVector`, `SparseMatrixCSC`, `Diagonal`, `Bidiagonal`, `Tridiagonal`,
    or `SymTridiagonal`, and any number of `broadcast` scalars, `Vector`s, or `Matrix`s,
    now efficiently yield `SparseVector`s or `SparseMatrix`s as appropriate ([#19239],
    [#19371], [#19518], [#19438], [#19690], [#19724], [#19926], [#19934], [#20009]).

  * The operators `!` and `∘` (`\circ<tab>` at the REPL and in most code editors) now
    respectively perform predicate function negation and function composition. For example,
    `map(!iszero, (0, 1))` is now equivalent to `map(x -> !iszero(x), (0, 1))` and
    `map(uppercase ∘ hex, 250:255)` is now equivalent to
    `map(x -> uppercase(hex(x)), 250:255)` ([#17155]).

  * `enumerate` now supports the two-argument form `enumerate(::IndexStyle, iterable)`.
    This form allows specification of the returned indices' style. For example,
    `enumerate(IndexLinear, iterable)` yields linear indices and
    `enumerate(IndexCartesian, iterable)` yields cartesian indices ([#16378]).

Compiler/Runtime improvements
-----------------------------

  * `ccall` is now implemented as a macro,
    removing the need for special code-generator support for `Intrinsics` ([#18754]).

  * `ccall` gained limited support for a `llvmcall` calling-convention.
    This can replace many uses of `llvmcall` with a simpler, shorter declaration ([#18754]).

  * All `Intrinsics` are now `Builtin` functions instead and have proper error checking
    and fall-back static compilation support ([#18754]).

Deprecated or removed
---------------------

  * `ipermutedims(A::AbstractArray, p)` has been deprecated in favor of
    `permutedims(A, invperm(p))` ([#18891]).

  * Linear indexing is now only supported when there is exactly one
    non-cartesian index provided. Allowing a trailing index at dimension `d` to
    linearly access the higher dimensions from array `A` (beyond `size(A, d)`)
    has been deprecated as a stricter constraint during bounds checking.
    Instead, `reshape` the array such that its dimensionality matches the
    number of indices ([#20079]).

  * `Multimedia.@textmime "mime"` has been deprecated. Instead define
    `Multimedia.istextmime(::MIME"mime") = true` ([#18441]).

  * `isdefined(a::Array, i::Int)` has been deprecated in favor of `isassigned` ([#18346]).

  * The three-argument `SubArray` constructor (which accepts `dims::Tuple` as its third
    argument) has been deprecated in favor of the two-argument equivalent (the
    `dims::Tuple` argument being superfluous) ([#19259]).

  * `is` has been deprecated in favor of `===` (which used to be an alias for `is`) ([#17758]).

  * Ambiguous methods for addition and subtraction between `UniformScaling`s and `Number`s,
    for example `(+)(J::UniformScaling, x::Number)`, have been deprecated in favor of
    unambiguous, explicit equivalents, for example `J.λ + x` ([#17607]).

  * `num` and `den` have been deprecated in favor of `numerator` and `denominator` respectively ([#19233],[#19246]).

  * `delete!(ENV::EnvDict, k::AbstractString, def)` has been deprecated in favor of
    `pop!(ENV, k, def)`. Be aware that `pop!` returns `k` or `def`, whereas `delete!`
    returns `ENV` or `def` ([#18012]).

  * infix operator `$` has been deprecated in favor of infix `⊻` or function `xor` ([#18977]).

  * The single-argument form of `write` (`write(x)`, with implicit `STDOUT` output stream),
    has been deprecated in favor of the explicit equivalent `write(STDOUT, x)` ([#17654]).

  * `Dates.recur` has been deprecated in favor of `filter` ([#19288])

  * A number of ambiguous `convert` operations between `Number`s (especially `Real`s)
    and `Date`, `DateTime`, and `Period` types have been deprecated in favor of
    unambiguous `convert` and explicit constructor calls. Additionally, ambiguous colon
    construction of `<:Period` ranges without step specification, for example
    `Dates.Hour(1):Dates.Hour(2)`, has been deprecated in favor of such construction
    including step specification, for example `Dates.Hour(1):Dates.Hour(1):Dates.Hour(2)`
    ([#19920]).

  * `cummin` and `cummax` have been deprecated in favor of `accumulate` ([#18931]).

  * The `Array` constructor syntax `Array(T, dims...)` has been deprecated
    in favor of the forms `Array{T,N}(dims...)` (where `N` is known, or
    particularly `Vector{T}(dims...)` for `N = 1` and `Matrix{T}(dims...)` for `N = 2`),
    and `Array{T}(dims...)` (where `N` is not known). Likewise for `SharedArray`s ([#19989]).

  * `sumabs` and `sumabs2` have been deprecated in favor of `sum(abs, x)` and `sum(abs2, x)`, respectively.
    `maxabs` and `minabs` have similarly been deprecated in favor of `maximum(abs, x)` and `minimum(abs, x)`.
    Likewise for the in-place counterparts of these functions ([#19598]).

  * The array-reducing form of `isinteger` (`isinteger(x::AbstractArray)`) has been
    deprecated in favor of `all(isinteger, x)` ([#19925]).

  * `produce`, `consume` and iteration over a Task object have been deprecated in favor of
    using Channels for inter-task communication  ([#19841]).

  * The `negate` keyword has been deprecated from all functions in the `Dates` adjuster
    API (`adjust`, `tonext`, `toprev`, `Date`, `Time`, and `DateTime`). Instead use
    predicate function negation via the `!` operator
    (see [Library Improvements](#library-improvements)) ([#20213]).

  * `@test_approx_eq x y` has been deprecated in favor of `@test isapprox(x,y)` or `@test x ≈ y` ([#4615]).

  * `Matrix()` and `Matrix{T}()` have been deprecated in favor of the explicit forms
    `Matrix(0, 0)` and `Matrix{T}(0, 0)` ([#20330]).

  * Vectorized functions have been deprecated in favor of dot syntax ([#17302], [#17265],
    [#18558], [#19711], [#19712], [#19791], [#19802], [#19931], [#20543], [#20228]).

  *  All methods of character predicates (`isalnum`, `isalpha`, `iscntrl`, `isdigit`,
     `isnumber`, `isgraph`, `islower`, `isprint`, `ispunct`, `isspace`, `isupper`,
     `isxdigit`) that accept `AbstractStrings` have been deprecated in favor of `all`.
     For example, `isnumber("123")` should now be expressed `all(isnumber, "123")`
     ([#20342]).

  * A few names related to indexing traits have been changed: `LinearIndexing` and
    `linearindexing` have been deprecated in favor of `IndexStyle`. `LinearFast` has
    been deprecated in favor of `IndexLinear`, and `LinearSlow` has been deprecated in
    favor of `IndexCartesian` ([#16378]).

  * The two-argument forms of `map` (`map!(f, A)`) and `asyncmap!` (`asyncmap!(f, A)`)
    have been deprecated in anticipation of future semantic changes ([#19721]).

  * `unsafe_wrap(String, ...)` has been deprecated in favor of `unsafe_string` ([#19449]).

  * `zeros` and `ones` methods accepting an element type as the first argument and an
    array as the second argument, for example `zeros(Float64, [1, 2, 3])`, have been
    deprecated in favor of equivalent methods with the second argument instead the
    size of the array, for example `zeros(Float64, size([1, 2, 3]))` ([#21183]).

  * `Base.promote_eltype_op` has been deprecated ([#19669], [#19814], [#19937]).

  * `isimag` has been deprecated ([#19949]).

  * The tuple-of-types form of `invoke`, `invoke(f, (types...), ...)`, has been deprecated
    in favor of the tuple-type form `invoke(f, Tuple{types...}, ...)` ([#18444]).

  * `Base._promote_array_type` has been deprecated ([#19766]).

  * `broadcast_zpreserving` has been deprecated ([#19533], [#19720]).

  * Methods allowing indexing of tuples by `AbstractArray`s with more than one dimension
    have been deprecated. (Indexing a tuple by such a higher-dimensional `AbstractArray`
    should yield a tuple with more than one dimension, but tuples are one-dimensional.)
    ([#19737]).

  * `@test_approx_eq a b` has been deprecated in favor of `@test a ≈ b` (or,
    equivalently, `@test ≈(a, b)` or `@test isapprox(a, b)`).
    `@test_approx_eq_eps` has been deprecated in favor of new `@test` syntax:
    `@test` now supports the syntax `@test f(args...) key=val ...` for
    `@test f(args..., key=val...)`. This syntax allows, for example, writing
    `@test a ≈ b atol=c` in place of `@test ≈(a, b, atol=c)` (and hence
    `@test_approx_eq_eps a b c`) ([#19901]).

  * `takebuf_array` has been deprecated in favor of `take!`, and `takebuf_string(x)`
    has been deprecated in favor of `String(take!(x))` ([#19088]).

  * `convert` methods from `Diagonal` and `Bidiagonal` to subtypes of
    `AbstractTriangular` have been deprecated ([#17723]).

  * `Base.LinAlg.arithtype` has been deprecated. If you were using `arithtype` within a
    `promote_op` call, instead use `promote_op(Base.LinAlg.matprod, Ts...)`. Otherwise,
    consider defining equivalent functionality locally ([#18218]).

  * Special characters (`#{}()[]<>|&*?~;`) should now be quoted in commands. For example,
    ``` `export FOO=1\;` ``` should replace ``` `export FOO=1;` ``` and
    ``` `cd $dir '&&' $thingie` ``` should replace ``` `cd $dir && $thingie` ``` ([#19786]).

  * Zero-argument `Channel` constructors (`Channel()`, `Channel{T}()`) have been deprecated
    in favor of equivalents accepting an explicit `Channel` size
    (`Channel(2)`, `Channel{T}(2)`) ([#18832]).

  * The zero-argument constructor `MersenneTwister()` has been
    deprecated in favor of the explicit `MersenneTwister(0)` ([#16984]).

  * `Base.promote_type(op::Type, Ts::Type...)` has been removed as part of an overhaul
    of `broadcast`'s promotion mechanism. If you need the functionality of that
    `Base.promote_type` method, consider defining it locally via
    `Core.Compiler.return_type(op, Tuple{Ts...})` ([#18642]).

  * `bitbroadcast` has been deprecated in favor of `broadcast`, which now produces a
    `BitArray` instead of `Array{Bool}` for functions yielding a boolean result ([#19771]).

  * To complete the deprecation of histogram-related functions, `midpoints` has been
    deprecated. Instead use the
    [StatsBase.jl package](https://github.com/JuliaStats/StatsBase.jl)'s
    `midpoints` function ([#20058]).

  * Passing a type argument to `LibGit2.cat` has been deprecated in favor of a simpler,
    two-argument method for `LibGit2.cat` ([#20435]).

  * The `LibGit2.owner` function for finding the repository which owns a given Git object
    has been deprecated in favor of `LibGit2.repository` ([#20135]).

  * The `LibGit2.GitAnyObject` type has been renamed to `LibGit2.GitUnknownObject` to
    clarify its intent ([#19935]).

  * The `LibGit2.GitOid` type has been renamed to `LibGit2.GitHash` for clarity ([#19878]).

  * Finalizing `LibGit2` objects with `finalize` has been deprecated in favor of using `close`
    ([#19660]).

  * Parsing string dates from a `Dates.DateFormat` object has been deprecated as part of a
    larger effort toward faster, more extensible date parsing ([#20952]).

Command-line option changes
---------------------------

  * In `polly` builds (`USE_POLLY := 1`), the new flag `--polly={yes|no}` controls whether
    `@polly` declarations are respected. (With `--polly=no`, `@polly` declarations are
    ignored.) This flag is also available in non-`polly` builds (`USE_POLLY := 0`),
    but has no effect ([#18159]).

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
    [multi-threading](https://docs.julialang.org/en/latest/manual/parallel-computing/#multi-threading-experimental).
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
    * NOTE: Using `@static` could be useful/necessary when used in a function's local scope. See details at the section entitled [Handling Operating System Variation](https://docs.julialang.org/en/latest/manual/handling-operating-system-variation/#man-handling-operating-system-variation) in the manual.

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

Julia v0.4.0 Release Notes
==========================

New language features
---------------------

  * Function call overloading: for arbitrary objects `x` (not of type
    `Function`), `x(...)` is transformed into `call(x, ...)`, and `call`
    can be overloaded as desired.  Constructors are now a special case of
    this mechanism, which allows e.g. constructors for abstract types.
    `T(...)` falls back to `convert(T, x)`, so all `convert` methods implicitly
    define a constructor ([#8712], [#2403]).

  * Unicode version 8 is now supported for identifiers etcetera ([#7917], [#12031]).

  * Type parameters now permit any `isbits` type, not just `Int` and `Bool` ([#6081]).

  * Keyword argument names can be computed, using syntax such as `f(; symbol => val)` ([#7704]).

  * The syntax `@generated function` enables generation of specialized methods based on
    argument types. At compile time, the function is called with its arguments bound to their
    types instead of to their values. The function then returns an expression forming the
    body of the function to be called at run time ([#7311]).

  * [Documentation system](https://docs.julialang.org/en/latest/manual/documentation/)
    for functions, methods, types and macros in packages and user code ([#8791]).

  * The syntax `function foo end` can be used to introduce a generic function without
    yet adding any methods ([#8283]).

  * Incremental precompilation of modules: call `VERSION >= v"0.4.0-dev+6521" && __precompile__()` at the top of a
    module file to automatically precompile it when it is imported ([#12491]), or manually
    run `Base.compilecache(modulename)`. The resulting precompiled `.ji` file is saved in
    `~/.julia/lib/v0.4` ([#8745]).

      * See manual section on `Module initialization and precompilation` (under `Modules`) for
        details and errata.  In particular, to be safely precompilable a module may need an
        `__init__` function to separate code that must be executed at runtime rather than precompile
        time.  Modules that are *not* precompilable should call `__precompile__(false)`.

      * The precompiled `.ji` file includes a list of dependencies (modules and files that
        were imported/included at precompile-time), and the module is automatically recompiled
        upon `import` when any of its dependencies have changed.  Explicit dependencies
        on other files can be declared with `include_dependency(path)` ([#12458]).

      * New option `--output-incremental={yes|no}` added to invoke the equivalent of `Base.compilecache`
        from the command line.

  * The syntax `new{parameters...}(...)` can be used in constructors to specify parameters for
    the type to be constructed ([#8135]).

  * `++` is now parsed as an infix operator, but does not yet have a default definition ([#11030], [#11686]).

  * Support for inter-task communication using `Channels` ([#12264]).
    See https://docs.julialang.org/en/latest/manual/parallel-computing/#channels for details.

  * `RemoteRef`s now point to remote channels. The remote channels can be of length greater than 1.
    Default continues to be of length 1 ([#12385]).
    See https://docs.julialang.org/en/latest/manual/parallel-computing/#remoterefs-and-abstractchannels for details.

  * `@__LINE__` special macro now available to reflect invocation source line number ([#12727]).

Language changes
----------------

  * Tuple types are now written as `Tuple{A, B}` instead of as `(A, B)`.
    Tuples of bits types are inlined into structs and arrays, like other
    immutable types.
    `...` now does splatting inside parentheses, instead of constructing a
    variadic tuple type ([#10380]).
    Variadic tuple types are written as `Tuple{Vararg{T}}`.

  * Using `[x,y]` to concatenate arrays is deprecated, and in the future will
    construct a vector of `x` and `y` instead ([#3737], [#2488], [#8599]).

  * Significant improvements to `ccall` and `cfunction`

    * As a safer alternative to creating pointers (`Ptr`), the managed reference type
      `Ref` has been added. A `Ref` points to the data contained by a value in an
      abstract sense, and in a way that is GC-safe. For example, `Ref(2)` points to
      a storage location that contains the integer `2`, and `Ref(array,3)` points
      to the third element of an array. A `Ref` can be automatically converted to a
      native pointer when passed to a `ccall`.

    * When passing a by-reference argument to `ccall`, you can declare
      the argument type to be `Ref{T}` instead of `Ptr{T}`, and just
      pass `x` instead of `&x`.

    * `ccall` is now lowered to call `unsafe_convert(T, cconvert(T, x))` on each
      argument. `cconvert` falls back to `convert`, but can be used to convert an
      argument to an arbitrarily-different representation more suitable for passing
      to C. `unsafe_convert` then handles conversions to `Ptr`.

    * `ccall` and `cfunction` now support correctly passing and returning structs,
      following the platform ABI (assuming the C types are mirrored accurately in Julia).

    * `cfunction` arguments of struct-like Julia types are now passed by value.
      If `Ref{T}` is used as a `cfunction` argument type, it will look up the
      method applicable to `T`, but pass the argument by reference (as Julia functions
      usually do). However, this should only be used for objects allocated by Julia
      and for `isbits` types.

  * `convert(Ptr,x)` is deprecated for most types, replaced by
    `unsafe_convert`. You can still `convert` between pointer types,
    and between pointers and `Int` or `UInt`.

  * Module `__init__` methods no longer swallow thrown exceptions; they now
    throw an `InitError` wrapping the thrown exception ([#12576]).

  * Unsigned `BigInt` literal syntax has been removed ([#11105]).
    Unsigned literals larger than `UInt128` now throw a syntax error.

  * `error(::Exception)` and `error(::Type{Exception})` have been deprecated
     in favor of using an explicit `throw` ([#9690]).

  * `Uint` etcetera are renamed to `UInt` ([#8905]).

  * `String` is renamed to `AbstractString` ([#8872]).

  * `FloatingPoint` is renamed to `AbstractFloat` ([#12162]).

  * `None` is deprecated; use `Union{}` instead ([#8423]).

  * `Nothing` (the type of `nothing`) is renamed to `Void` ([#8423]).

  * Arrays can be constructed with the syntax `Array{T}(m,n)` ([#3214], [#10075]).

  * `Dict` literal syntax `[a=>b,c=>d]` is replaced by `Dict(a=>b,c=>d)`,
    `{a=>b}` is replaced by `Dict{Any,Any}(a=>b)`, and
    `(K=>V)[...]` is replaced by `Dict{K,V}(...)`.
    The new syntax has many advantages: all of its components are first-class,
    it generalizes to other types of containers, it is easier to guess how to
    specify key and value types, and the syntaxes for empty and pre-populated
    dicts are synchronized. As part of this change, `=>` is parsed as a normal
    operator, and `Base` defines it to construct `Pair` objects ([#6739]).

  * `Char` is no longer a subtype of `Integer` ([#8816]).
    Char now supports a more limited set of operations with `Integer` types:

      * comparison / equality
      * `Char` + `Int` = `Char`
      * `Char` - `Char` = `Int`

  * `round` rounds to the nearest integer using the default rounding mode,
    which is ties-to-even by default ([#8750]).

  * A custom triple-quoted string like `x"""..."""` no longer invokes an `x_mstr`
    macro. Instead, the string is first unindented and then `x_str` is invoked,
    as if the string had been single-quoted ([#10228]).

  * Colons (`:`) within indexing expressions are no longer lowered to the range
    `1:end`. Instead, the `:` identifier is passed directly. Custom array types
    that implement `getindex` or `setindex!` methods must also extend those
    methods to support arguments of type `Colon` ([#10331]).

  * Unions of types should now be written with curly braces instead of parentheses, i.e.
    `Union{Type1, Type2}` instead of `Union(Type1, Type2)` ([#11432]).

  * The keyword `local` is no longer allowed in global scope. Use `let` instead of
    `begin` to create a new scope from the top level ([#7234], [#10472]).

  * Triple-quoted strings no longer treat tabs as 8 spaces. Instead, the
    longest common prefix of spaces and tabs is removed.

  * `global x` in a nested scope is now a syntax error if `x` is local
    to the enclosing scope ([#7264]/[#11985]).

  * The default `importall Base.Operators` is deprecated, and relying on it
    will give a warning ([#8113]).

  * `remotecall_fetch` and `fetch` now rethrow any uncaught remote exception locally as a
    `RemoteException`. Previously they would return the remote exception object.
    The worker pid, remote exception and remote backtrace are available in the
    thrown `RemoteException`.

  * If any of the enclosed async operations in a `@sync` block throw exceptions, they
    are now collected in a `CompositeException` and the `CompositeException` thrown.


Command line option changes
---------------------------

  * The `-i` option now forces the REPL to run after loading the specified script (if any) ([#11347]).

  * New option `--handle-signals={yes|no}` to disable Julia's signal handlers.

  * The `--depwarn={yes|no|error}` option enables/disables syntax and method deprecation warnings,
    or turns them into errors ([#9294]).

  * Some command line options are slated for deprecation / removal
    - `-f, --no-startup` Don't load ~/.juliarc (deprecated, use --startup-file=no)
    - `-F` Load ~/.juliarc (deprecated, use --startup-file=yes)`
    - `-P, --post-boot <expr>`  Evaluate <expr>, but don't disable interactive mode (deprecated, use -i -e instead)
    - `--no-history-file`  Don't load history file (deprecated, use --history-file=no)

Compiler/Runtime improvements
-----------------------------

  * Functions may be annotated with metadata (`:meta` expressions) to be used by the compiler ([#8297]).

  * `@inline` before a function definition forces the compiler to inline the function ([#8297]).

  * Loads from heap-allocated immutables are hoisted out of loops in more cases ([#8867]).

  * Accessing fields that are always initialized no longer produces undefined checks ([#8827]).

  * New generational garbage collector which greatly reduces GC overhead for many commmon workloads ([#5227]).

Library improvements
--------------------

  * Build with USE_GPL_LIBS=0 to exclude all GPL libraries and code ([#10870]).

  * Linear algebra

    * The `LinAlg` module is now exported.

    * `sparse(A)` now takes any `AbstractMatrix` A as an argument ([#10031]).

    * Factorization API is now type-stable; functions dispatch on `Val{false}` or `Val{true}` instead of a boolean value ([#9575]).

    * Added generic Cholesky factorization, and the Cholesky factorization is now parametrized by the matrix type ([#7236]).

    * Sparse `cholfact` and `ldltfact` functions now accept a `perm` keyword
      for user-provided permutations and a `shift` keyword to factorize
      a shifted matrix ([#10844]).

    * New `svds` function for the sparse truncated SVD ([#9425]).

    * `Symmetric` and `Hermitian` immutables are now parametrized by the matrix type ([#7992]).

    * New `ordschur` and `ordschur!` functions for sorting a Schur factorization by the eigenvalues ([#8467],[#9701]).

    * `Givens` type doesn't have a size anymore and is no longer a subtype of `AbstractMatrix` ([#8660]).

    * Large speedup in sparse `\` and splitting of Cholesky and LDLᵀ factorizations into `cholfact` and `ldltfact` ([#10117]).

    * Add sparse least squares to `\` by adding `qrfact` for sparse matrices based on the SPQR library ([#10180]).

    * Split `Triangular` type into `UpperTriangular`, `LowerTriangular`, `UnitUpperTriagular` and `UnitLowerTriangular` ([#9779])

    * OpenBLAS 64-bit (ILP64) interface is now compiled with a `64_` suffix ([#8734]) to avoid conflicts with external libraries using a 32-bit BLAS ([#4923]).

    * New `vecdot` function, analogous to `vecnorm`, for Euclidean inner products over any iterable container ([#11067]).

    * `p = plan_fft(x)` and similar functions now return a `Base.DFT.Plan` object, rather
    than an anonymous function.  Calling it via `p(x)` is deprecated in favor of
    `p * x` or `p \ x` (for the inverse), and it can also be used with `A_mul_B!`
    to employ pre-allocated output arrays ([#12087]).

    * `LU{T,Tridiagonal{T}}` now supports extraction of `L`, `U`, `p`, and `P` factors ([#12137]).

    * Allocations in sparse matrix factorizations are now tracked by Julia's garbage collector ([#12034]).

  * Strings

    * NUL-terminated strings should now be passed to C via the new `Cstring` type, not `Ptr{UInt8}` or `Ptr{Cchar}`,
      in order to check whether the string is free of NUL characters (which would cause silent truncation in C).
      The analogous type `Cwstring` should be used for NUL-terminated `wchar_t*` strings ([#10994]).

    * `graphemes(s)` returns an iterator over grapheme substrings of `s` ([#9261]).

    * Character predicates such as `islower()`, `isspace()`, etc. use
      utf8proc to provide uniform cross-platform behavior and
      up-to-date, locale-independent support for Unicode standards
      ([#5939]).

    * `reverseind` function to convert indices in reversed strings (e.g. from
      reversed regex searches) to indices in the original string ([#9249]).

    * `charwidth(c)` and `strwidth(s)` now return up-to-date cross-platform
      results (via utf8proc) ([#10659]): Julia now likes pizza ([#3721]), but some terminals still don't.

    * `is_valid_char(c)`, (now `isvalid(Char,c)` ([#11241])), now correctly handles Unicode "non-characters", which are valid Unicode codepoints ([#11171]).

    * Backreferences in replacement strings in calls to `replace` with a `Regex` pattern are now supported ([#11849]).
      Use the `s` string prefix to indicate a replacement string contains a backreference. For example, `replace("ab", r"(.)(.)", s"\2\1")` yields "ba".

    * Capture groups in regular expressions can now be named using PCRE syntax, `(?P<group_name>...)`. Capture group matches can be accessed by name by indexing a `Match` object with the name of the group ([#11566]).

    * `countlines()` now counts all lines, not just non-empty ([#11947]).

  * Array and AbstractArray improvements

    * New multidimensional iterators and index types for efficient iteration over `AbstractArray`s. Array iteration should generally be written as `for i in eachindex(A) ... end` rather than `for i = 1:length(A) ... end` ([#8432]).

    * New implementation of SubArrays with substantial performance and functionality improvements ([#8501]).

    * AbstractArray subtypes only need to implement `size` and `getindex`
      for scalar indices to support indexing; all other indexing behaviors
      (including logical indexing, ranges of indices, vectors, colons, etc.) are
      implemented in default fallbacks. Similarly, they only need to implement
      scalar `setindex!` to support all forms of indexed assingment ([#10525]).

    * AbstractArrays that do not extend `similar` now return an `Array` by
      default ([#10525]).

  * Data structures

    * New `sortperm!` function for pre-allocated index arrays ([#8792]).

    * Switch from `O(N)` to `O(log N)` algorithm for `dequeue!(pq, key)`
    with `PriorityQueue`. This provides major speedups for large
    queues ([#8011]).

    * `PriorityQueue` now includes the order type among its
      parameters, `PriorityQueue{KeyType,ValueType,OrderType}`. An
      empty queue can be constructed as `pq =
      PriorityQueue(KeyType,ValueType)`, if you intend to use the
      default `Forward` order, or `pq = PriorityQueue(KeyType,
      ValueType, OrderType)` otherwise ([#8011]).

    * Efficient `mean` and `median` for ranges ([#8089]).

    * `deepcopy` recurses through immutable types and makes copies of their mutable fields ([#8560]).

    * `copy(a::DArray)` will now make a copy of a `DArray` ([#9745]).

  * New types

    * Enums are now supported through the `@enum EnumName EnumValue1
      EnumValue2` syntax. Enum member values also support abitrary
      value assignment by the `@enum EnumName EnumValue1=1
      EnumValue2=10 EnumValue3=20` syntax ([#10168]).

    * New `Dates` module for calendar dates and other time-interval calculations ([#7654]).

    * New `Nullable` type for missing data ([#8152]).

    * A new `Val{T}` type allows one to dispatch on bits-type values ([#9452]).

    * `linspace` now returns a `LinSpace` object which lazily computes linear interpolation of values between the start and stop values. It "lifts" endpoints which are approximately rational in the same manner as the `colon` operator.

  * Arithmetic

    * `convert` now checks for overflow when truncating integers or converting between
    signed and unsigned ([#5413]).

    * Arithmetic is type-preserving for more types; e.g. `(x::Int8) + (y::Int8)` now
    yields an `Int8` ([#3759]).

    * Reductions (e.g. `reduce`, `sum`) widen small types (integers smaller than `Int`, and `Float16`).

    * Added optional rounding argument to floating-point constructors ([#8845]).

    * Equality (`==`) and inequality (`<`/`<=`) comparisons are now correct
      across all numeric types ([#9133], [#9198]).

    * Rational arithmetic throws errors on overflow ([#8672]).

    * Optional `log` and `log1p` functions implemented in pure Julia (experimental) ([#10008]).

    * The `MathConst` type has been renamed `Irrational` ([#11922]).

    * `isapprox` now has simpler and more sensible default tolerances ([#12393]), supports arrays, and has synonyms `≈` ([U+2248](http://www.fileformat.info/info/unicode/char/2248/index.htm), LaTeX `\approx`) and `≉` ([U+2249](http://www.fileformat.info/info/unicode/char/2249/index.htm), LaTeX `\napprox`) for `isapprox` and `!isapprox`, respectively ([#12472]).

  * Numbers

    * `primes` is now faster and has been extended to generate the primes in a user defined closed interval ([#12025]).

    * The function `primesmask` which generates a prime sieve for a user defined closed interval is now exported ([#12025]).

  * Random numbers

    * Streamlined random number generation APIs [#8246].
    The default `rand` no longer uses global state in the underlying C library,
    dSFMT, making it closer to being thread-safe ([#8399], [#8832]).
    All APIs can now take an `AbstractRNG` argument ([#8854], [#9065]). The seed argument to `srand` is now optional ([#8320], [#8854]).
    The APIs accepting a range argument are extended to accept an arbitrary
    `AbstractArray` ([#9049]).
    Passing a range of `BigInt` to `rand` or `rand!` is now supported ([#9122]).
    There are speed improvements across the board ([#8808], [#8941], [#8958], [#9083]).

    * Significantly faster `randn` ([#9126], [#9132]).

    * The `randexp` and `randexp!` functions are exported ([#9144]).

  * File

    * Added function `readlink` which returns the value of a symbolic link "path" ([#10714]).

    * Added function `ismount` which checks if a directory is a mount point ([#11279]).

    * The `cp` function now accepts keyword arguments `remove_destination` and `follow_symlinks` ([#10888]).

    * The `mv` function now accepts keyword argument `remove_destination` ([#11145]).

  * `Pipe()` creates a bidirectional I/O object that can be passed to `spawn` or `pipeline`
    for redirecting process streams ([#12739]).

  * Other improvements

    * You can now tab-complete emoji via their [short names](http://www.emoji-cheat-sheet.com/), using `\:name:<tab>` ([#10709]).

    * `gc_enable` subsumes `gc_disable`, and also returns the previous GC state.

    * `assert`, `@assert` now throws an `AssertionError` exception type ([#9734]).

    * `@simd` now rejects invalid control flow (`@goto` / break / continue) in the inner loop body at compile time ([#8624]).

    * The `machinefile` now supports a host count ([#7616]).

    * `code_native` now outputs branch labels ([#8897]).

    * Added `recvfrom` to get source address of UDP packets ([#9418]).

    * `ClusterManager` performance improvements ([#9309]) and support for changing transports([#9434]).

    * Added `Base.get_process_title` / `Base.set_process_title` ([#9957]).

    * `readavailable` now returns a byte vector instead of a string.

    * New `lock` and `unlock` functions, operating on `ReentrantLock`, to lock a stream during
      concurrent writes from multiple tasks ([#10679]).

    * `code_llvm` now outputs stripped IR without debug info or other attached metadata.
      Use `code_llvm_raw` for the unstripped output ([#10747]).

    * New `withenv(var=>val, ...) do ... end` function to temporarily
      modify environment variables ([#10914]).

    * New function `relpath` returns a relative filepath to path either from the current
      directory or from an optional start directory ([#10893]).

    * `mktemp` and `mktempdir` now take an optional argument to set which
      directory the temporary file or directory is created in.

    * New garbage collector tracked memory allocator functions: `jl_malloc`, `jl_calloc`,
    `jl_realloc`, and `jl_free` with libc API ([[#12034]]).

    * `mktempdir` and `mktemp` now have variants that take a function as its
      first argument for automated clean-up ([[#9017]]).

Deprecated or removed
---------------------

  * several syntax whitespace insensitivities have been deprecated ([#11891]).
    ```julia
     # function call
     f (x)

     # getindex
     x [17]
     rand(2) [1]

     # function definition
     f (x) = x^2
     function foo (x)
	x^2
     end
    ```

  * indexing with `Real`s that are not subtypes of `Integer` (`Rational`, `AbstractFloat`, etc.) has been deprecated ([#10458]).

  * `push!(A)` has been deprecated, use `append!` instead of splatting arguments to `push!` ([#10400]).

  * `names` for composite datatypes has been deprecated and
    renamed to `fieldnames` ([#10332]).

  * `DArray` functionality has been removed from `Base` and is now a
    standalone package under the JuliaParallel umbrella organization ([#10333]).

  * The `Graphics` module has been removed from `Base` and is now a
    standalone package ([#10150], [#9862]).

  * The `Woodbury` special matrix type has been removed from `LinAlg` ([#10024]).

  * `median` and `median!` no longer accept a `checknan` keyword argument ([#8605]).

  * `inf` and `nan` are now deprecated in favor of `T(Inf)` and `T(NaN)`, respectively ([#8776]).

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

  * `randbool` is deprecated. Use `rand(Bool)` to produce a random boolean value, and
    `bitrand` to produce a random BitArray ([#9105], [#9569]).

  * `beginswith` is renamed to `startswith` ([#9578]).

  * `null` is renamed to `nullspace` ([#9714]).

  * The operators `|>`, `.>`, `>>`, and `.>>` as used for process I/O redirection
    are replaced with the `pipeline` function ([#5349], [#12739]).

  * `flipud(A)` and `fliplr(A)` have been deprecated in favor of `flipdim(A, 1)` and
    `flipdim(A, 2)`, respectively ([#10446]).

  * Numeric conversion functions whose names are lower-case versions of type
    names have been removed. To convert a scalar, use the type name, e.g.
    `Int32(x)`. To convert an array to a different element type, use
    `Array{T}(x)`, `map(T,x)`, or `round(T,x)`. To parse a string as an integer
    or floating-point number, use `parse` ([#1470], [#6211]).

  * Low-level functions from the C library and dynamic linker have been moved to
    modules `Libc` and `Libdl`, respectively ([#10328]).

  * The functions `parseint`, `parsefloat`, `float32_isvalid`,
  `float64_isvalid`, and the string-argument `BigInt` and `BigFloat` have
  been replaced by `parse` and `tryparse` with a type argument. The string
  macro `big"xx"` can be used to construct `BigInt` and `BigFloat` literals
  ([#3631], [#5704], [#9487], [#10543], [#10955]).

  * the `--int-literals` compiler option is no longer accepted ([#9597]).

  * Instead of `linrange`, use `linspace` ([#9666]).

  * The functions `is_valid_char`, `is_valid_ascii`, `is_valid_utf8`, `is_valid_utf16`, and
    `is_valid_utf32` have been replaced by generic `isvalid` methods.
    The single argument form `isvalid(value)` can now be used for values of type `Char`, `ASCIIString`,
    `UTF8String`, `UTF16String` and `UTF32String`.
    The two argument form `isvalid(type, value)` can be used with the above types, with values
    of type `Vector{UInt8}`, `Vector{UInt16}`, `Vector{UInt32}`, and `Vector{Char}` ([#11241]).

  * Instead of `utf32(64,123,...)` use `utf32(UInt32[64,123,...])` ([#11379]).

  * `start_timer` and `stop_timer` are replaced by `Timer` and `close`.

  * The following internal julia C functions have been renamed, in order to prevent
    potential naming conflicts with C libraries: ([#11741])

    * `gc_wb*` -> `jl_gc_wb*`

    * `gc_queue_root` -> `jl_gc_queue_root`

    * `allocobj` -> `jl_gc_allocobj`

    * `alloc_[0-3]w` -> `jl_gc_alloc_*w`

    * `diff_gc_total_bytes` -> `jl_gc_diff_total_bytes`

    * `sync_gc_total_bytes` -> `jl_gc_sync_total_bytes`

  * `require(::AbstractString)` and `reload` (see news about addition of `compile`).

  * `cartesianmap` is deprecated in favor of iterating over a `CartesianRange`

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

  * Local goto statements using the `@goto` and `@label` macros ([#101]).

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

      * `eigs(A, sigma)` now uses shift-and-invert for nonzero shifts `sigma` and inverse iteration for `which="SM"`. If `sigma==nothing` (the new default), computes ordinary (forward) iterations ([#5776]).

      * `sprand` is faster, and whether any entry is nonzero is now determined independently with the specified probability ([#6726]).

    * Dense linear algebra for special matrix types

      * Interconversions between the special matrix types `Diagonal`, `Bidiagonal`,
        `SymTridiagonal`, `Triangular`, and `Triangular`, and `Matrix` are now allowed
        for matrices which are representable in both source and destination types ([5e3f074b]).

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
    and comparing ranges to be faster ([#5778]).

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

  * The signal filtering function `filt` now accepts an optional initial filter state vector. A new in-place function `filt!` is also exported ([#7513]).

  * Significantly faster `cumsum` and `cumprod` ([#7359]).

  * Implement `findmin` and `findmax` over specified array dimensions ([#6716]).

  * Support memory-mapping of files with offsets on Windows ([#7242]).

  * Catch writes to protect memory, such as when trying to modify a mmapped file opened in read-only mode ([#3434]).

Environment improvements
------------------------

  * New `--code-coverage` and `--track-allocation` startup features allow one to measure the number of executions or the amount of memory allocated, respectively, at each line of code ([#5423],[#7464]).

  * `Profile.init` now accepts keyword arguments, and returns the current settings when no arguments are supplied ([#7365]).

Build improvements
------------------

  * Dependencies are now verified against stored MD5/SHA512 hashes, to ensure
    that the correct file has been downloaded and was not modified ([#6773]).


Deprecated or removed
---------------------

  * `convert(Ptr{T1}, x::Array{T2})` is now deprecated unless `T1 == T2`
    or `T1 == Void` ([#6073]).  (You can still explicitly `convert`
    one pointer type into another if needed.)

  * `Sys.shlib_ext` has been renamed to `Sys.dlext`

  * `dense` is deprecated in favor of `full` ([#4759]).

  * The `Stat` type is renamed `StatStruct` ([#4670]).

  * `setrounding`, `rounding` and `setrounding` now take an additional
    argument specifying the floating point type to which they apply. The old
    behaviour and `[get/set/with]_bigfloat_rounding` functions are deprecated ([#5007]).

  * `cholpfact` and `qrpfact` are deprecated in favor of keyword arguments in
    `cholfact(..., pivot=true)` and `qrfact(..., pivot=true)` ([#5330]).

  * `symmetrize!` is deprecated in favor of `Base.LinAlg.copytri!` ([#5427]).

  * `myindexes` has been renamed to `localindexes` ([#5475]).

  * `factorize!` is deprecated in favor of `factorize` ([#5526]).

  * `nnz` counts the number of structural nonzeros in a sparse
    matrix. Use `countnz` for the actual number of nonzeros ([#6769]).

  * `setfield` is renamed `setfield!` ([#5748]).

  * `put` and `take` are renamed `put!` and `take!` ([#5511]).

  * `put!` now returns its first argument, the remote reference ([#5819]).

  * `read` methods that modify a passed array are now called `read!` ([#5970])

  * `infs` and `nans` are deprecated in favor of the more general `fill`.

  * `*` and `div` are no longer supported for `Char`.

  * `Range` is renamed `StepRange` and `Range1` is renamed `UnitRange`.
    `Ranges` is renamed `Range`.

  * `bitmix` is replaced by a 2-argument form of `hash`.

  * `readsfrom` and `writesto` are replaced by `open` ([#6948]).

  * `insert!` now throws a `BoundsError` if
    `index > length(collection)+1` ([#7373]).

  * No longer exported from `Base`:
    * `start_reading`, `stop_reading`, `start_watching` ([#10885]).

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
    efficiency, and consistency ([#6116], [#7035], [#7061], [#7106]).

Deprecated or removed
---------------------

  * Methods of `min` and `max` that do reductions were renamed to
    `minimum` and `maximum`. `min(x)` is now `minimum(x)`, and
    `min(x,(),dim)` is now `minimum(x,dim)` ([#4235]).

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

[packages chapter]: https://docs.julialang.org/en/latest/manual/packages/
[sorting functions]: https://docs.julialang.org/en/latest/stdlib/sort/
[pairwise summation]: https://en.wikipedia.org/wiki/Pairwise_summation
[a448e080]: https://github.com/JuliaLang/julia/commit/a448e080dc736c7fb326426dfcb2528be36973d3
[5e3f074b]: https://github.com/JuliaLang/julia/commit/5e3f074b9173044a0a4219f9b285879ff7cec041
<!--- generated by NEWS-update.jl: -->
[#13]: https://github.com/JuliaLang/julia/issues/13
[#69]: https://github.com/JuliaLang/julia/issues/69
[#70]: https://github.com/JuliaLang/julia/issues/70
[#101]: https://github.com/JuliaLang/julia/issues/101
[#265]: https://github.com/JuliaLang/julia/issues/265
[#485]: https://github.com/JuliaLang/julia/issues/485
[#550]: https://github.com/JuliaLang/julia/issues/550
[#552]: https://github.com/JuliaLang/julia/issues/552
[#907]: https://github.com/JuliaLang/julia/issues/907
[#964]: https://github.com/JuliaLang/julia/issues/964
[#987]: https://github.com/JuliaLang/julia/issues/987
[#1090]: https://github.com/JuliaLang/julia/issues/1090
[#1195]: https://github.com/JuliaLang/julia/issues/1195
[#1268]: https://github.com/JuliaLang/julia/issues/1268
[#1470]: https://github.com/JuliaLang/julia/issues/1470
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
[#2488]: https://github.com/JuliaLang/julia/issues/2488
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
[#3214]: https://github.com/JuliaLang/julia/issues/3214
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
[#3631]: https://github.com/JuliaLang/julia/issues/3631
[#3649]: https://github.com/JuliaLang/julia/issues/3649
[#3665]: https://github.com/JuliaLang/julia/issues/3665
[#3688]: https://github.com/JuliaLang/julia/issues/3688
[#3697]: https://github.com/JuliaLang/julia/issues/3697
[#3719]: https://github.com/JuliaLang/julia/issues/3719
[#3721]: https://github.com/JuliaLang/julia/issues/3721
[#3737]: https://github.com/JuliaLang/julia/issues/3737
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
[#4163]: https://github.com/JuliaLang/julia/issues/4163
[#4177]: https://github.com/JuliaLang/julia/issues/4177
[#4211]: https://github.com/JuliaLang/julia/issues/4211
[#4235]: https://github.com/JuliaLang/julia/issues/4235
[#4284]: https://github.com/JuliaLang/julia/issues/4284
[#4383]: https://github.com/JuliaLang/julia/issues/4383
[#4412]: https://github.com/JuliaLang/julia/issues/4412
[#4470]: https://github.com/JuliaLang/julia/issues/4470
[#4615]: https://github.com/JuliaLang/julia/issues/4615
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
[#4867]: https://github.com/JuliaLang/julia/issues/4867
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
[#5227]: https://github.com/JuliaLang/julia/issues/5227
[#5255]: https://github.com/JuliaLang/julia/issues/5255
[#5263]: https://github.com/JuliaLang/julia/issues/5263
[#5275]: https://github.com/JuliaLang/julia/issues/5275
[#5277]: https://github.com/JuliaLang/julia/issues/5277
[#5330]: https://github.com/JuliaLang/julia/issues/5330
[#5349]: https://github.com/JuliaLang/julia/issues/5349
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
[#5704]: https://github.com/JuliaLang/julia/issues/5704
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
[#6190]: https://github.com/JuliaLang/julia/issues/6190
[#6197]: https://github.com/JuliaLang/julia/issues/6197
[#6211]: https://github.com/JuliaLang/julia/issues/6211
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
[#6842]: https://github.com/JuliaLang/julia/issues/6842
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
[#7234]: https://github.com/JuliaLang/julia/issues/7234
[#7236]: https://github.com/JuliaLang/julia/issues/7236
[#7242]: https://github.com/JuliaLang/julia/issues/7242
[#7258]: https://github.com/JuliaLang/julia/issues/7258
[#7264]: https://github.com/JuliaLang/julia/issues/7264
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
[#7669]: https://github.com/JuliaLang/julia/issues/7669
[#7704]: https://github.com/JuliaLang/julia/issues/7704
[#7917]: https://github.com/JuliaLang/julia/issues/7917
[#7992]: https://github.com/JuliaLang/julia/issues/7992
[#8011]: https://github.com/JuliaLang/julia/issues/8011
[#8036]: https://github.com/JuliaLang/julia/issues/8036
[#8089]: https://github.com/JuliaLang/julia/issues/8089
[#8113]: https://github.com/JuliaLang/julia/issues/8113
[#8135]: https://github.com/JuliaLang/julia/issues/8135
[#8152]: https://github.com/JuliaLang/julia/issues/8152
[#8246]: https://github.com/JuliaLang/julia/issues/8246
[#8283]: https://github.com/JuliaLang/julia/issues/8283
[#8297]: https://github.com/JuliaLang/julia/issues/8297
[#8320]: https://github.com/JuliaLang/julia/issues/8320
[#8399]: https://github.com/JuliaLang/julia/issues/8399
[#8423]: https://github.com/JuliaLang/julia/issues/8423
[#8432]: https://github.com/JuliaLang/julia/issues/8432
[#8467]: https://github.com/JuliaLang/julia/issues/8467
[#8501]: https://github.com/JuliaLang/julia/issues/8501
[#8560]: https://github.com/JuliaLang/julia/issues/8560
[#8578]: https://github.com/JuliaLang/julia/issues/8578
[#8599]: https://github.com/JuliaLang/julia/issues/8599
[#8605]: https://github.com/JuliaLang/julia/issues/8605
[#8624]: https://github.com/JuliaLang/julia/issues/8624
[#8660]: https://github.com/JuliaLang/julia/issues/8660
[#8672]: https://github.com/JuliaLang/julia/issues/8672
[#8712]: https://github.com/JuliaLang/julia/issues/8712
[#8734]: https://github.com/JuliaLang/julia/issues/8734
[#8745]: https://github.com/JuliaLang/julia/issues/8745
[#8750]: https://github.com/JuliaLang/julia/issues/8750
[#8776]: https://github.com/JuliaLang/julia/issues/8776
[#8791]: https://github.com/JuliaLang/julia/issues/8791
[#8792]: https://github.com/JuliaLang/julia/issues/8792
[#8808]: https://github.com/JuliaLang/julia/issues/8808
[#8814]: https://github.com/JuliaLang/julia/issues/8814
[#8816]: https://github.com/JuliaLang/julia/issues/8816
[#8827]: https://github.com/JuliaLang/julia/issues/8827
[#8832]: https://github.com/JuliaLang/julia/issues/8832
[#8845]: https://github.com/JuliaLang/julia/issues/8845
[#8846]: https://github.com/JuliaLang/julia/issues/8846
[#8854]: https://github.com/JuliaLang/julia/issues/8854
[#8867]: https://github.com/JuliaLang/julia/issues/8867
[#8872]: https://github.com/JuliaLang/julia/issues/8872
[#8897]: https://github.com/JuliaLang/julia/issues/8897
[#8905]: https://github.com/JuliaLang/julia/issues/8905
[#8941]: https://github.com/JuliaLang/julia/issues/8941
[#8958]: https://github.com/JuliaLang/julia/issues/8958
[#8974]: https://github.com/JuliaLang/julia/issues/8974
[#9017]: https://github.com/JuliaLang/julia/issues/9017
[#9049]: https://github.com/JuliaLang/julia/issues/9049
[#9065]: https://github.com/JuliaLang/julia/issues/9065
[#9083]: https://github.com/JuliaLang/julia/issues/9083
[#9105]: https://github.com/JuliaLang/julia/issues/9105
[#9122]: https://github.com/JuliaLang/julia/issues/9122
[#9126]: https://github.com/JuliaLang/julia/issues/9126
[#9132]: https://github.com/JuliaLang/julia/issues/9132
[#9133]: https://github.com/JuliaLang/julia/issues/9133
[#9144]: https://github.com/JuliaLang/julia/issues/9144
[#9198]: https://github.com/JuliaLang/julia/issues/9198
[#9249]: https://github.com/JuliaLang/julia/issues/9249
[#9261]: https://github.com/JuliaLang/julia/issues/9261
[#9271]: https://github.com/JuliaLang/julia/issues/9271
[#9294]: https://github.com/JuliaLang/julia/issues/9294
[#9309]: https://github.com/JuliaLang/julia/issues/9309
[#9343]: https://github.com/JuliaLang/julia/issues/9343
[#9418]: https://github.com/JuliaLang/julia/issues/9418
[#9425]: https://github.com/JuliaLang/julia/issues/9425
[#9434]: https://github.com/JuliaLang/julia/issues/9434
[#9452]: https://github.com/JuliaLang/julia/issues/9452
[#9482]: https://github.com/JuliaLang/julia/issues/9482
[#9487]: https://github.com/JuliaLang/julia/issues/9487
[#9503]: https://github.com/JuliaLang/julia/issues/9503
[#9569]: https://github.com/JuliaLang/julia/issues/9569
[#9575]: https://github.com/JuliaLang/julia/issues/9575
[#9578]: https://github.com/JuliaLang/julia/issues/9578
[#9597]: https://github.com/JuliaLang/julia/issues/9597
[#9627]: https://github.com/JuliaLang/julia/issues/9627
[#9666]: https://github.com/JuliaLang/julia/issues/9666
[#9690]: https://github.com/JuliaLang/julia/issues/9690
[#9701]: https://github.com/JuliaLang/julia/issues/9701
[#9714]: https://github.com/JuliaLang/julia/issues/9714
[#9734]: https://github.com/JuliaLang/julia/issues/9734
[#9745]: https://github.com/JuliaLang/julia/issues/9745
[#9779]: https://github.com/JuliaLang/julia/issues/9779
[#9862]: https://github.com/JuliaLang/julia/issues/9862
[#9957]: https://github.com/JuliaLang/julia/issues/9957
[#10008]: https://github.com/JuliaLang/julia/issues/10008
[#10024]: https://github.com/JuliaLang/julia/issues/10024
[#10031]: https://github.com/JuliaLang/julia/issues/10031
[#10075]: https://github.com/JuliaLang/julia/issues/10075
[#10117]: https://github.com/JuliaLang/julia/issues/10117
[#10150]: https://github.com/JuliaLang/julia/issues/10150
[#10168]: https://github.com/JuliaLang/julia/issues/10168
[#10180]: https://github.com/JuliaLang/julia/issues/10180
[#10228]: https://github.com/JuliaLang/julia/issues/10228
[#10328]: https://github.com/JuliaLang/julia/issues/10328
[#10331]: https://github.com/JuliaLang/julia/issues/10331
[#10332]: https://github.com/JuliaLang/julia/issues/10332
[#10333]: https://github.com/JuliaLang/julia/issues/10333
[#10380]: https://github.com/JuliaLang/julia/issues/10380
[#10400]: https://github.com/JuliaLang/julia/issues/10400
[#10446]: https://github.com/JuliaLang/julia/issues/10446
[#10458]: https://github.com/JuliaLang/julia/issues/10458
[#10472]: https://github.com/JuliaLang/julia/issues/10472
[#10525]: https://github.com/JuliaLang/julia/issues/10525
[#10543]: https://github.com/JuliaLang/julia/issues/10543
[#10548]: https://github.com/JuliaLang/julia/issues/10548
[#10659]: https://github.com/JuliaLang/julia/issues/10659
[#10679]: https://github.com/JuliaLang/julia/issues/10679
[#10709]: https://github.com/JuliaLang/julia/issues/10709
[#10714]: https://github.com/JuliaLang/julia/issues/10714
[#10747]: https://github.com/JuliaLang/julia/issues/10747
[#10844]: https://github.com/JuliaLang/julia/issues/10844
[#10870]: https://github.com/JuliaLang/julia/issues/10870
[#10885]: https://github.com/JuliaLang/julia/issues/10885
[#10888]: https://github.com/JuliaLang/julia/issues/10888
[#10893]: https://github.com/JuliaLang/julia/issues/10893
[#10914]: https://github.com/JuliaLang/julia/issues/10914
[#10946]: https://github.com/JuliaLang/julia/issues/10946
[#10955]: https://github.com/JuliaLang/julia/issues/10955
[#10994]: https://github.com/JuliaLang/julia/issues/10994
[#11030]: https://github.com/JuliaLang/julia/issues/11030
[#11067]: https://github.com/JuliaLang/julia/issues/11067
[#11105]: https://github.com/JuliaLang/julia/issues/11105
[#11145]: https://github.com/JuliaLang/julia/issues/11145
[#11171]: https://github.com/JuliaLang/julia/issues/11171
[#11196]: https://github.com/JuliaLang/julia/issues/11196
[#11241]: https://github.com/JuliaLang/julia/issues/11241
[#11242]: https://github.com/JuliaLang/julia/issues/11242
[#11250]: https://github.com/JuliaLang/julia/issues/11250
[#11279]: https://github.com/JuliaLang/julia/issues/11279
[#11310]: https://github.com/JuliaLang/julia/issues/11310
[#11347]: https://github.com/JuliaLang/julia/issues/11347
[#11379]: https://github.com/JuliaLang/julia/issues/11379
[#11432]: https://github.com/JuliaLang/julia/issues/11432
[#11566]: https://github.com/JuliaLang/julia/issues/11566
[#11686]: https://github.com/JuliaLang/julia/issues/11686
[#11688]: https://github.com/JuliaLang/julia/issues/11688
[#11741]: https://github.com/JuliaLang/julia/issues/11741
[#11849]: https://github.com/JuliaLang/julia/issues/11849
[#11891]: https://github.com/JuliaLang/julia/issues/11891
[#11922]: https://github.com/JuliaLang/julia/issues/11922
[#11947]: https://github.com/JuliaLang/julia/issues/11947
[#11985]: https://github.com/JuliaLang/julia/issues/11985
[#12025]: https://github.com/JuliaLang/julia/issues/12025
[#12031]: https://github.com/JuliaLang/julia/issues/12031
[#12034]: https://github.com/JuliaLang/julia/issues/12034
[#12087]: https://github.com/JuliaLang/julia/issues/12087
[#12137]: https://github.com/JuliaLang/julia/issues/12137
[#12162]: https://github.com/JuliaLang/julia/issues/12162
[#12231]: https://github.com/JuliaLang/julia/issues/12231
[#12264]: https://github.com/JuliaLang/julia/issues/12264
[#12274]: https://github.com/JuliaLang/julia/issues/12274
[#12385]: https://github.com/JuliaLang/julia/issues/12385
[#12393]: https://github.com/JuliaLang/julia/issues/12393
[#12458]: https://github.com/JuliaLang/julia/issues/12458
[#12472]: https://github.com/JuliaLang/julia/issues/12472
[#12491]: https://github.com/JuliaLang/julia/issues/12491
[#12563]: https://github.com/JuliaLang/julia/issues/12563
[#12576]: https://github.com/JuliaLang/julia/issues/12576
[#12727]: https://github.com/JuliaLang/julia/issues/12727
[#12739]: https://github.com/JuliaLang/julia/issues/12739
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
[#15850]: https://github.com/JuliaLang/julia/issues/15850
[#15975]: https://github.com/JuliaLang/julia/issues/15975
[#16010]: https://github.com/JuliaLang/julia/issues/16010
[#16024]: https://github.com/JuliaLang/julia/issues/16024
[#16058]: https://github.com/JuliaLang/julia/issues/16058
[#16071]: https://github.com/JuliaLang/julia/issues/16071
[#16098]: https://github.com/JuliaLang/julia/issues/16098
[#16107]: https://github.com/JuliaLang/julia/issues/16107
[#16154]: https://github.com/JuliaLang/julia/issues/16154
[#16213]: https://github.com/JuliaLang/julia/issues/16213
[#16219]: https://github.com/JuliaLang/julia/issues/16219
[#16260]: https://github.com/JuliaLang/julia/issues/16260
[#16285]: https://github.com/JuliaLang/julia/issues/16285
[#16362]: https://github.com/JuliaLang/julia/issues/16362
[#16378]: https://github.com/JuliaLang/julia/issues/16378
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
[#16984]: https://github.com/JuliaLang/julia/issues/16984
[#16986]: https://github.com/JuliaLang/julia/issues/16986
[#17033]: https://github.com/JuliaLang/julia/issues/17033
[#17037]: https://github.com/JuliaLang/julia/issues/17037
[#17057]: https://github.com/JuliaLang/julia/issues/17057
[#17075]: https://github.com/JuliaLang/julia/issues/17075
[#17132]: https://github.com/JuliaLang/julia/issues/17132
[#17155]: https://github.com/JuliaLang/julia/issues/17155
[#17261]: https://github.com/JuliaLang/julia/issues/17261
[#17265]: https://github.com/JuliaLang/julia/issues/17265
[#17266]: https://github.com/JuliaLang/julia/issues/17266
[#17300]: https://github.com/JuliaLang/julia/issues/17300
[#17302]: https://github.com/JuliaLang/julia/issues/17302
[#17323]: https://github.com/JuliaLang/julia/issues/17323
[#17374]: https://github.com/JuliaLang/julia/issues/17374
[#17393]: https://github.com/JuliaLang/julia/issues/17393
[#17402]: https://github.com/JuliaLang/julia/issues/17402
[#17404]: https://github.com/JuliaLang/julia/issues/17404
[#17510]: https://github.com/JuliaLang/julia/issues/17510
[#17546]: https://github.com/JuliaLang/julia/issues/17546
[#17599]: https://github.com/JuliaLang/julia/issues/17599
[#17607]: https://github.com/JuliaLang/julia/issues/17607
[#17623]: https://github.com/JuliaLang/julia/issues/17623
[#17654]: https://github.com/JuliaLang/julia/issues/17654
[#17668]: https://github.com/JuliaLang/julia/issues/17668
[#17723]: https://github.com/JuliaLang/julia/issues/17723
[#17758]: https://github.com/JuliaLang/julia/issues/17758
[#17785]: https://github.com/JuliaLang/julia/issues/17785
[#18012]: https://github.com/JuliaLang/julia/issues/18012
[#18050]: https://github.com/JuliaLang/julia/issues/18050
[#18159]: https://github.com/JuliaLang/julia/issues/18159
[#18218]: https://github.com/JuliaLang/julia/issues/18218
[#18251]: https://github.com/JuliaLang/julia/issues/18251
[#18330]: https://github.com/JuliaLang/julia/issues/18330
[#18339]: https://github.com/JuliaLang/julia/issues/18339
[#18342]: https://github.com/JuliaLang/julia/issues/18342
[#18346]: https://github.com/JuliaLang/julia/issues/18346
[#18441]: https://github.com/JuliaLang/julia/issues/18441
[#18442]: https://github.com/JuliaLang/julia/issues/18442
[#18444]: https://github.com/JuliaLang/julia/issues/18444
[#18453]: https://github.com/JuliaLang/julia/issues/18453
[#18457]: https://github.com/JuliaLang/julia/issues/18457
[#18473]: https://github.com/JuliaLang/julia/issues/18473
[#18558]: https://github.com/JuliaLang/julia/issues/18558
[#18628]: https://github.com/JuliaLang/julia/issues/18628
[#18642]: https://github.com/JuliaLang/julia/issues/18642
[#18644]: https://github.com/JuliaLang/julia/issues/18644
[#18660]: https://github.com/JuliaLang/julia/issues/18660
[#18690]: https://github.com/JuliaLang/julia/issues/18690
[#18754]: https://github.com/JuliaLang/julia/issues/18754
[#18777]: https://github.com/JuliaLang/julia/issues/18777
[#18832]: https://github.com/JuliaLang/julia/issues/18832
[#18839]: https://github.com/JuliaLang/julia/issues/18839
[#18891]: https://github.com/JuliaLang/julia/issues/18891
[#18931]: https://github.com/JuliaLang/julia/issues/18931
[#18965]: https://github.com/JuliaLang/julia/issues/18965
[#18977]: https://github.com/JuliaLang/julia/issues/18977
[#19018]: https://github.com/JuliaLang/julia/issues/19018
[#19088]: https://github.com/JuliaLang/julia/issues/19088
[#19157]: https://github.com/JuliaLang/julia/issues/19157
[#19233]: https://github.com/JuliaLang/julia/issues/19233
[#19239]: https://github.com/JuliaLang/julia/issues/19239
[#19246]: https://github.com/JuliaLang/julia/issues/19246
[#19259]: https://github.com/JuliaLang/julia/issues/19259
[#19288]: https://github.com/JuliaLang/julia/issues/19288
[#19305]: https://github.com/JuliaLang/julia/issues/19305
[#19331]: https://github.com/JuliaLang/julia/issues/19331
[#19371]: https://github.com/JuliaLang/julia/issues/19371
[#19438]: https://github.com/JuliaLang/julia/issues/19438
[#19449]: https://github.com/JuliaLang/julia/issues/19449
[#19464]: https://github.com/JuliaLang/julia/issues/19464
[#19469]: https://github.com/JuliaLang/julia/issues/19469
[#19518]: https://github.com/JuliaLang/julia/issues/19518
[#19533]: https://github.com/JuliaLang/julia/issues/19533
[#19543]: https://github.com/JuliaLang/julia/issues/19543
[#19594]: https://github.com/JuliaLang/julia/issues/19594
[#19598]: https://github.com/JuliaLang/julia/issues/19598
[#19635]: https://github.com/JuliaLang/julia/issues/19635
[#19636]: https://github.com/JuliaLang/julia/issues/19636
[#19660]: https://github.com/JuliaLang/julia/issues/19660
[#19669]: https://github.com/JuliaLang/julia/issues/19669
[#19670]: https://github.com/JuliaLang/julia/issues/19670
[#19677]: https://github.com/JuliaLang/julia/issues/19677
[#19680]: https://github.com/JuliaLang/julia/issues/19680
[#19690]: https://github.com/JuliaLang/julia/issues/19690
[#19692]: https://github.com/JuliaLang/julia/issues/19692
[#19711]: https://github.com/JuliaLang/julia/issues/19711
[#19712]: https://github.com/JuliaLang/julia/issues/19712
[#19720]: https://github.com/JuliaLang/julia/issues/19720
[#19721]: https://github.com/JuliaLang/julia/issues/19721
[#19722]: https://github.com/JuliaLang/julia/issues/19722
[#19724]: https://github.com/JuliaLang/julia/issues/19724
[#19730]: https://github.com/JuliaLang/julia/issues/19730
[#19737]: https://github.com/JuliaLang/julia/issues/19737
[#19741]: https://github.com/JuliaLang/julia/issues/19741
[#19766]: https://github.com/JuliaLang/julia/issues/19766
[#19771]: https://github.com/JuliaLang/julia/issues/19771
[#19779]: https://github.com/JuliaLang/julia/issues/19779
[#19784]: https://github.com/JuliaLang/julia/issues/19784
[#19786]: https://github.com/JuliaLang/julia/issues/19786
[#19787]: https://github.com/JuliaLang/julia/issues/19787
[#19791]: https://github.com/JuliaLang/julia/issues/19791
[#19800]: https://github.com/JuliaLang/julia/issues/19800
[#19802]: https://github.com/JuliaLang/julia/issues/19802
[#19811]: https://github.com/JuliaLang/julia/issues/19811
[#19814]: https://github.com/JuliaLang/julia/issues/19814
[#19841]: https://github.com/JuliaLang/julia/issues/19841
[#19878]: https://github.com/JuliaLang/julia/issues/19878
[#19900]: https://github.com/JuliaLang/julia/issues/19900
[#19901]: https://github.com/JuliaLang/julia/issues/19901
[#19903]: https://github.com/JuliaLang/julia/issues/19903
[#19919]: https://github.com/JuliaLang/julia/issues/19919
[#19920]: https://github.com/JuliaLang/julia/issues/19920
[#19925]: https://github.com/JuliaLang/julia/issues/19925
[#19926]: https://github.com/JuliaLang/julia/issues/19926
[#19931]: https://github.com/JuliaLang/julia/issues/19931
[#19934]: https://github.com/JuliaLang/julia/issues/19934
[#19935]: https://github.com/JuliaLang/julia/issues/19935
[#19937]: https://github.com/JuliaLang/julia/issues/19937
[#19944]: https://github.com/JuliaLang/julia/issues/19944
[#19949]: https://github.com/JuliaLang/julia/issues/19949
[#19950]: https://github.com/JuliaLang/julia/issues/19950
[#19989]: https://github.com/JuliaLang/julia/issues/19989
[#20009]: https://github.com/JuliaLang/julia/issues/20009
[#20047]: https://github.com/JuliaLang/julia/issues/20047
[#20058]: https://github.com/JuliaLang/julia/issues/20058
[#20079]: https://github.com/JuliaLang/julia/issues/20079
[#20135]: https://github.com/JuliaLang/julia/issues/20135
[#20164]: https://github.com/JuliaLang/julia/issues/20164
[#20213]: https://github.com/JuliaLang/julia/issues/20213
[#20228]: https://github.com/JuliaLang/julia/issues/20228
[#20248]: https://github.com/JuliaLang/julia/issues/20248
[#20249]: https://github.com/JuliaLang/julia/issues/20249
[#20268]: https://github.com/JuliaLang/julia/issues/20268
[#20308]: https://github.com/JuliaLang/julia/issues/20308
[#20321]: https://github.com/JuliaLang/julia/issues/20321
[#20327]: https://github.com/JuliaLang/julia/issues/20327
[#20328]: https://github.com/JuliaLang/julia/issues/20328
[#20330]: https://github.com/JuliaLang/julia/issues/20330
[#20342]: https://github.com/JuliaLang/julia/issues/20342
[#20345]: https://github.com/JuliaLang/julia/issues/20345
[#20403]: https://github.com/JuliaLang/julia/issues/20403
[#20404]: https://github.com/JuliaLang/julia/issues/20404
[#20406]: https://github.com/JuliaLang/julia/issues/20406
[#20414]: https://github.com/JuliaLang/julia/issues/20414
[#20418]: https://github.com/JuliaLang/julia/issues/20418
[#20427]: https://github.com/JuliaLang/julia/issues/20427
[#20435]: https://github.com/JuliaLang/julia/issues/20435
[#20500]: https://github.com/JuliaLang/julia/issues/20500
[#20530]: https://github.com/JuliaLang/julia/issues/20530
[#20543]: https://github.com/JuliaLang/julia/issues/20543
[#20609]: https://github.com/JuliaLang/julia/issues/20609
[#20889]: https://github.com/JuliaLang/julia/issues/20889
[#20952]: https://github.com/JuliaLang/julia/issues/20952
[#21183]: https://github.com/JuliaLang/julia/issues/21183
[#21818]: https://github.com/JuliaLang/julia/issues/21818
