Julia v0.6.0 Release Notes
==========================

New language features
---------------------

  * New type system capabilities ([#8974], [#18457])
    * Type parameter constraints can refer to previous parameters, e.g.
      `type Foo{R<:Real, A<:AbstractArray{R}}`. Can also be used in method definitions.
    * New syntax `Array{T} where T<:Integer`, indicating a union of types over all
      specified values of `T` (represented by a `UnionAll` type). This provides behavior
      similar to parametric methods or `typealias`, but can be used anywhere a type is
      accepted. This syntax can also be used in method definitions, e.g.
      `function inv(M::Matrix{T}) where T<:AbstractFloat`.
      Anonymous functions can have type parameters via the syntax
      `((x::Array{T}) where T<:Real) -> 2x`.
    * Implicit type parameters, e.g. `Vector{<:Real}` is equivalent to
      `Vector{T} where T<:Real`, and similarly for `Vector{>:Int}` ([#20414]).
    * Much more accurate subtype and type intersection algorithms. Method sorting and
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
    definition now must be written as `Foo{T,S}(x) where {T,S<:Real} = new(x)`. ([#11310])

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
    Similarly for unary operators ([#20249]).

  * Newly defined methods are no longer callable from the same dynamic runtime
    scope they were defined in ([#17057]).

  * `isa` is now parsed as an infix operator with the same precedence as `in`
    ([#19677]).

  * `@.` is now parsed as `@__dot__`, and can be used to add dots to
    every function call, operator, and assignment in an expression ([#20321]).

Breaking changes
----------------

This section lists changes that do not have deprecation warnings.

  * `readline`, `readlines` and `eachline` return lines without line endings by default.
    You *must* use `readline(s, chomp=false)`, etc. to get the old behavior where
    returned lines include trailing end-of-line character(s). ([#19944])

  * `String`s no longer have a `.data` field (as part of a significant performance
    improvement). Use `Vector{UInt8}(str)` to access a string as a byte array.
    However, allocating the `Vector` object has overhead. You can also use
    `codeunit(str, i)` to access the `i`th byte of a `String`.
    Use `sizeof(str)` instead of `length(str.data)`, and `pointer(str)` instead of
    `pointer(str.data)`. ([#19449])

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

  * The `Collections` module has been removed, and all functions defined therein have been
    moved to the `DataStructures` package. ([#19800])

  * The `RepString` type has been moved to the
    [LegacyStrings.jl package](https://github.com/JuliaArchive/LegacyStrings.jl).

  * In macro calls with parentheses, e.g. `@m(a=1)`, assignments are now parsed as
    `=` expressions, instead of as `kw` expressions. ([#7669])

  * When used as an infix operator, `~` is now parsed as a call to an ordinary operator
    with assignment precedence, instead of as a macro call. ([#20406])

  * (µ "micro" and ɛ "latin epsilon") are considered equivalent to
    the corresponding Greek characters in identifiers.  `\varepsilon`
    now tab-completes to U+03B5 (greek small letter epsilon) ([#19464]).

  * `retry` now inputs the keyword arguments `delays` and `check` instead of
    `n` and `max_delay`.  The previous functionality can be achieved setting
    `delays` to `ExponentialBackOff`. ([#19331])

  * `transpose(::AbstractVector)` now always returns a `RowVector` view of the input (which is a
     special 1×n-sized `AbstractMatrix`), not a `Matrix`, etc. In particular, for
     `v::AbstractVector` we now have `(v.').' === v` and `v.' * v` is a scalar. ([#19670])

  * Parametric types with "unspecified" parameters, such as `Array`, are now represented
    as `UnionAll` types instead of `DataType`s ([#18457]).

  * `Union` types have two fields, `a` and `b`, instead of a single `types` field.
    The empty type `Union{}` is represented by a singleton of type `BottomType` ([#18457]).

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

Library improvements
--------------------

  * `@views` macro to convert a whole expression or block of code to
    use views for all slices ([#20164]).

  * `max`, `min`, and related functions (`minmax`, `maximum`, `minimum`,
    `extrema`) now return `NaN` for `NaN` arguments ([#12563]).

  * `oneunit(x)` function to return a dimensionful version of `one(x)` (which
    is clarified to mean a dimensionless quantity if `x` is dimensionful) ([#20268]).

  * The `chop` and `chomp` functions now return a `SubString` ([#18339]).

  * Numbered stackframes printed in stacktraces can be opened in an editor by
    entering the corresponding number in the REPL and pressing `^Q` ([#19680]).

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

  * The default text style for REPL input and answers has been changed from bold to normal ([#11250]).
    They can be changed back to bold by setting the environment variables `JULIA_INPUT_COLOR` and `JULIA_ANSWER_COLOR` to `"bold"`.
    For example, one way of doing this is adding `ENV["JULIA_INPUT_COLOR"] = :bold` and `ENV["JULIA_ANSWER_COLOR"] = :bold` to the `.juliarc.jl` file.
    See the [manual section on customizing colors](http://docs.julialang.org/en/latest/manual/interacting-with-julia#Customizing-Colors-1) for more information.

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

  * New `accumulate` and `accumulate!` functions, which generalize `cumsum` and
  `cumprod`. Also known as a [scan](https://en.wikipedia.org/wiki/Prefix_sum)
  operation ([#18931]).

  * `reshape` now allows specifying one dimension with a `Colon()` (`:`) for the new shape, in which case
    that dimension's length will be computed such that its product with all the other dimensions is equal
    to the length of the original array ([#19919]).

  * New `titlecase` function, which capitalizes the first character of each word within a string ([#19469]).

  * `any` and `all` now always short-circuit, and `mapreduce` never short-circuits ([#19543]).
    That is, not every member of the input iterable will be visited if a `true` (in the case of `any`) or
    `false` (in the case of `all`) value is found, and `mapreduce` will visit all members of the iterable.

  * Additional methods for `ones` and `zeros` functions to support the same signature as the `similar` function ([#19635]).

  * `count` now has a `count(itr)` method equivalent to `count(identity, itr)` ([#20403]).

  * Methods for `map` and `filter` with `Nullable` arguments have been
    implemented; the semantics are as if the `Nullable` were a container with
    zero or one elements ([#16961]).

  * New `@test_warn` and `@test_nowarn` macros in the `Base.Test` module to
    test for the presence or absence of warning messages ([#19903]).

  * `logging` can be used to redirect `info`, `warn`, and `error` messages
    either universally or on a per-module/function basis ([#16213]).

  * New `iszero(x)` function to quickly check whether `x` is zero (or is all zeros, for an array) ([#19950]).

  * `notify` now returns a count of tasks woken up ([#19841]).

  * New nonstandard string literal `raw"..."` for creating strings
    with no interpolation or unescaping ([#19900]).

  * A new `Dates.Time` type was added that supports representing the time of day with up to nanosecond resolution ([#12274]).

  * New `@macroexpand` macro as a convenient alternative to the `macroexpand` function ([#18660]).

Compiler/Runtime improvements
-----------------------------

* `ccall` is now implemented as a macro, removing the need for special code-generator support for Intrinsics.

* `ccall` gained limited support for a `llvmcall` calling-convention. This can replace many uses of `llvmcall` with a simpler, shorter declaration.

* All Intrinsics are now Builtin functions instead and have proper error checking and fall-back static compilation support.

Deprecated or removed
---------------------

  * Linear indexing is now only supported when there is exactly one
    non-cartesian index provided. Allowing a trailing index at dimension `d` to
    linearly access the higher dimensions from array `A` (beyond `size(A, d)`)
    has been deprecated as a stricter constraint during bounds checking.
    Instead, `reshape` the array such that its dimensionality matches the
    number of indices ([#20079]).

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

  * `produce`, `consume` and iteration over a Task object have been deprecated in favor of
    using Channels for inter-task communication  ([#19841]).

<!--- generated by NEWS-update.jl: -->
[#265]: https://github.com/JuliaLang/julia/issues/265
[#7669]: https://github.com/JuliaLang/julia/issues/7669
[#8974]: https://github.com/JuliaLang/julia/issues/8974
[#11250]: https://github.com/JuliaLang/julia/issues/11250
[#12274]: https://github.com/JuliaLang/julia/issues/12274
[#12563]: https://github.com/JuliaLang/julia/issues/12563
[#16213]: https://github.com/JuliaLang/julia/issues/16213
[#16961]: https://github.com/JuliaLang/julia/issues/16961
[#16986]: https://github.com/JuliaLang/julia/issues/16986
[#17057]: https://github.com/JuliaLang/julia/issues/17057
[#17261]: https://github.com/JuliaLang/julia/issues/17261
[#17599]: https://github.com/JuliaLang/julia/issues/17599
[#17623]: https://github.com/JuliaLang/julia/issues/17623
[#17758]: https://github.com/JuliaLang/julia/issues/17758
[#17785]: https://github.com/JuliaLang/julia/issues/17785
[#18050]: https://github.com/JuliaLang/julia/issues/18050
[#18330]: https://github.com/JuliaLang/julia/issues/18330
[#18339]: https://github.com/JuliaLang/julia/issues/18339
[#18346]: https://github.com/JuliaLang/julia/issues/18346
[#18442]: https://github.com/JuliaLang/julia/issues/18442
[#18453]: https://github.com/JuliaLang/julia/issues/18453
[#18457]: https://github.com/JuliaLang/julia/issues/18457
[#18473]: https://github.com/JuliaLang/julia/issues/18473
[#18628]: https://github.com/JuliaLang/julia/issues/18628
[#18644]: https://github.com/JuliaLang/julia/issues/18644
[#18690]: https://github.com/JuliaLang/julia/issues/18690
[#18777]: https://github.com/JuliaLang/julia/issues/18777
[#18839]: https://github.com/JuliaLang/julia/issues/18839
[#18931]: https://github.com/JuliaLang/julia/issues/18931
[#18965]: https://github.com/JuliaLang/julia/issues/18965
[#18977]: https://github.com/JuliaLang/julia/issues/18977
[#19018]: https://github.com/JuliaLang/julia/issues/19018
[#19233]: https://github.com/JuliaLang/julia/issues/19233
[#19288]: https://github.com/JuliaLang/julia/issues/19288
[#19305]: https://github.com/JuliaLang/julia/issues/19305
[#19331]: https://github.com/JuliaLang/julia/issues/19331
[#19449]: https://github.com/JuliaLang/julia/issues/19449
[#19464]: https://github.com/JuliaLang/julia/issues/19464
[#19469]: https://github.com/JuliaLang/julia/issues/19469
[#19543]: https://github.com/JuliaLang/julia/issues/19543
[#19598]: https://github.com/JuliaLang/julia/issues/19598
[#19635]: https://github.com/JuliaLang/julia/issues/19635
[#19670]: https://github.com/JuliaLang/julia/issues/19670
[#19677]: https://github.com/JuliaLang/julia/issues/19677
[#19680]: https://github.com/JuliaLang/julia/issues/19680
[#19692]: https://github.com/JuliaLang/julia/issues/19692
[#19722]: https://github.com/JuliaLang/julia/issues/19722
[#19741]: https://github.com/JuliaLang/julia/issues/19741
[#19787]: https://github.com/JuliaLang/julia/issues/19787
[#19800]: https://github.com/JuliaLang/julia/issues/19800
[#19841]: https://github.com/JuliaLang/julia/issues/19841
[#19900]: https://github.com/JuliaLang/julia/issues/19900
[#19903]: https://github.com/JuliaLang/julia/issues/19903
[#19919]: https://github.com/JuliaLang/julia/issues/19919
[#19944]: https://github.com/JuliaLang/julia/issues/19944
[#19950]: https://github.com/JuliaLang/julia/issues/19950
[#20079]: https://github.com/JuliaLang/julia/issues/20079
[#20164]: https://github.com/JuliaLang/julia/issues/20164
