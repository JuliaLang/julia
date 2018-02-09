# Compat Package for Julia

[![Build Status](https://travis-ci.org/JuliaLang/Compat.jl.svg?branch=master)](https://travis-ci.org/JuliaLang/Compat.jl)
[![Build status](https://ci.appveyor.com/api/projects/status/github/JuliaLang/Compat.jl?branch=master)](https://ci.appveyor.com/project/quinnj/compat-jl/branch/master)

[![Compat](http://pkg.julialang.org/badges/Compat_0.3.svg)](http://pkg.julialang.org/?pkg=Compat&ver=0.3)
[![Compat](http://pkg.julialang.org/badges/Compat_0.4.svg)](http://pkg.julialang.org/?pkg=Compat&ver=0.4)
[![Compat](http://pkg.julialang.org/badges/Compat_0.5.svg)](http://pkg.julialang.org/?pkg=Compat&ver=0.5)
[![Compat](http://pkg.julialang.org/badges/Compat_0.6.svg)](http://pkg.julialang.org/?pkg=Compat&ver=0.6)
[![Compat](http://pkg.julialang.org/badges/Compat_0.7.svg)](http://pkg.julialang.org/?pkg=Compat&ver=0.7)

The **Compat** package is designed to ease interoperability between
older and newer versions of the [Julia
language](http://julialang.org/).  In particular, in cases where it is
impossible to write code that works with both the latest Julia
`master` branch and older Julia versions, or impossible to write code
that doesn't generate a deprecation warning in some Julia version, the
Compat package provides a macro that lets you use the *latest syntax*
in a backwards-compatible way.

This is primarily intended for use by other [Julia
packages](http://docs.julialang.org/en/latest/manual/packages/), where
it is important to maintain cross-version compatibility.

## Usage

To use Compat in your Julia package, add a line `Compat` to the
`REQUIRE` file in your package directory.  Then, in your package,
shortly after the `module` statement include lines like these:

```julia
using Compat
import Compat.String
```

and then as needed add

```julia
@compat ...compat syntax...
```

wherever you want to use syntax that differs in the latest Julia
`master` (the development version of Julia). The `compat syntax` is usually
the syntax on Julia `master`. However, in a few cases where this is not possible,
a slightly different syntax might be used.
Please check the list below for the specific syntax you need.

## Supported syntax

Currently, the `@compat` macro supports the following syntaxes:

* `@compat (a::B{T}){T}(c) = d` — the Julia 0.5-style call overload

* `@compat(get(io, s, false))`, with `s` equal to `:limit`, `:compact` or `:multiline`, to detect the corresponding print settings (performs useful work only on Julia 0.5, defaults to `false` otherwise)

* `@compat Nullable(value, hasvalue)` to handle the switch from the `Nullable` `:isnull` field to `:hasvalue` field ([#18510])

* `@compat x .= y` converts to an in-place assignment to `x` (via `broadcast!`) ([#17510]).
  However, beware that `.=` in Julia 0.4 has the precedence of `==`, not of assignment `=`, so if the right-hand-side `y`
  includes expressions with lower precedence than `==` you should enclose it in parentheses `x .= (y)` to ensure the
  correct order of evaluation.   Also, `x .+= y` converts to `x .= (x .+ y)`, and similarly for the other updating
  assignment operators (`.*=` and so on).

* `@compat Array{<:Real}`, `@compat Array{>:Int}`, and similar uses of `<:T` (resp. `>:T`) to define a set of "covariant" (resp. "contravariant") parameterized types ([#20414]).
  In 0.5, this only works for non-nested usages (e.g. you can't define `Array{<:Array{<:Real}}`).

* `@compat abstract type T end` and `@compat primitive type T 8 end`
  to declare abstract and primitive types. [#20418]
  This only works when `@compat` is applied directly on the declaration.

* `@compat A{T} = B{T}` or `@compat const A{T} = B{T}` to declare type alias with free
  parameters. [#20500]. Use `const A = B{T}` or `const A = B` for type alias without free parameters (i.e. no type parameter on the left hand side).

* `@compat Base.IndexStyle(::Type{<:MyArray}) = IndexLinear()` and `@compat Base.IndexStyle(::Type{<:MyArray}) = IndexCartesian()` to define traits for abstract arrays, replacing the former `Base.linearindexing{T<:MyArray}(::Type{T}) = Base.LinearFast()` and `Base.linearindexing{T<:MyArray}(::Type{T}) = Base.LinearSlow()`, respectively.

* `Compat.collect(A)` returns an `Array`, no matter what indices the array `A` has. [#21257]

* `@compat foo(::CartesianRange{N})` to replace the former
  `foo(::CartesianRange{CartesianIndex{N}})` ([#20974]). Note that
  `CartesianRange` now has two type parameters, so using them as
  fields in other `struct`s requires manual intervention.

## Module Aliases

* In 0.6, some 0.5 iterator functions have been moved to the `Base.Iterators`
  module. Code can be written to work on both 0.5 and 0.6 by `import`ing or
  `using` the `Compat.Iterators` module instead. ([#18839])

* `using Compat.Test`, `using Compat.SharedArrays`, `using Compat.Mmap`, and `using
  Compat.DelimitedFiles` are provided on versions older than 0.7, where these are not yet
  part of the standard library. ([#23931])

* `using Compat.Base64` is provided on versions older than 0.7, where this library is not
  yet a part of the standard library. ([#24361])

* `using Compat.Dates` is provided on versions older than 0.7, where this library is not
  yet a part of the standard library. ([#24459])

* `using Compat.Unicode` is provided on versions older than 0.7, where this library is not
  yet a part of the standard library. ([#25021])

* `using Compat.Printf` is provided on versions older than 0.7, where this library is not
  yet a part of the standard library. ([#25056])

* `using Compat.IterativeEigensolvers` is provided on versions older than 0.7, where this
  library is not yet a part of the standard library. ([#24714])

* `using Compat.SuiteSparse` is provided on versions older than 0.7, where this library is
  not yet part of the standard library ([#24648]).

* `using Compat.SparseArrays` is provided on versions older than 0.7, where this library is
  not yet part of the standard library ([#25249]).

* `using Compat.LinearAlgebra` is provided on versions older than 0.7, where this library is
  not yet part of the standard library ([#25571]).

* `using Compat.Random` is provided on versions older than 0.7, where this library is
  not yet part of the standard library ([#24874]).

* `using Compat.Libdl` is provided on versions older than 0.7, where this library is
  not yet part of the standard library ([#25459]).

* `using Compat.REPL` is provided on versions older than 0.7, where this library is
  not yet part of the standard library ([#25544]).

* `using Compat.Serialization` is provided on versions older than 0.7, where this library is
  not yet part of the standard library ([#25628]).

* `using Compat.Distributed` is provided on versions older than 0.7, where this library is
  not yet part of the standard library ([#24443]).

* `using Compat.Pkg` is provided on versions older than 0.7, where this library is
  not yet part of the standard library ([#25705]).

* `using Compat.InteractiveUtils` is provided on versions older than 0.7, where this library is
  not yet part of the standard library ([#25780]).

* `using Compat.LibGit2` is provided on versions older than 0.7, where this library is
  not yet part of the standard library ([#25706]).

## New functions, macros, and methods

* `@views` takes an expression and converts all slices to views ([#20164]), while
  `@view` ([#16564]) converts a single array reference to a view ([#20164]).

* `@__dot__` takes an expression and converts all assignments, function calls,
  and operators to their broadcasting "dot-call" equivalents ([#20321]).   In Julia 0.6, this
  can be abbreviated `@.`, but that macro name does not parse in earlier Julia versions.
  For this to work in older versions of Julia (prior to 0.5) that don't have dot calls,
  you should instead use `@dotcompat`, which combines the `@__dot__` and `@compat` macros.

* [`normalize`](http://docs.julialang.org/en/latest/stdlib/linalg/?highlight=normalize#Base.normalize) and [`normalize!`](http://docs.julialang.org/en/latest/stdlib/linalg/?highlight=normalize#Base.normalize!), normalizes a vector with respect to the p-norm ([#13681])

* `redirect_stdout`, `redirect_stderr`, and `redirect_stdin` take an optional function as a first argument, `redirect_std*(f, stream)`, so that one may use `do` block syntax (as first available for Julia 0.6)

* `unsafe_get` returns the `:value` field of a `Nullable` object without any null-check and has a generic fallback for non-`Nullable` argument ([#18484])

* `isnull` has a generic fallback for non-`Nullable` argument

* `transcode` converts between UTF-xx string encodings in Julia 0.5 (as a lightweight
   alternative to the LegacyStrings package) ([#17323])

* `∘` (typically used infix as `f ∘ g`) for function composition can be used in 0.5 and earlier ([#17155])

* `>:`, a supertype operator for symmetry with `issubtype` (`A >: B` is equivalent to `B <: A`), can be used in 0.5 and earlier ([#20407]).

* The method of `!` to negate functions (typically used as a unary operator, as in `!isinteger`) can be used in 0.5 and earlier ([#17155]).

* `iszero(x)` efficiently checks whether `x == zero(x)` (including arrays) can be used in 0.5 and earlier ([#19950]).

* `.&` and `.|` are short syntax for `broadcast(&, xs...)` and `broadcast(|, xs...)` (respectively) in Julia 0.6 (only supported on Julia 0.5 and above) ([#17623])

* `Compat.isapprox` with `nans` keyword argument ([#20022])

* `Compat.readline` with `keep` keyword argument ([#25646])

* `take!` method for `Task`s since some functions now return `Channel`s instead of `Task`s ([#19841])

* The `isabstract`, `parameter_upper_bound`, `typename` reflection methods were added in Julia 0.6. This package re-exports these from the `Compat.TypeUtils` submodule. On earlier versions of julia, that module contains the same functions, but operating on the pre-0.6 type system representation.

* `broadcast` is supported on tuples of the same lengths on 0.5. ([#16986])

* `zeros` and `ones` support an interface the same as `similar` ([#19635])

* `convert` can convert between different `Set` types on 0.5 and below. ([#18727])

* `isassigned(::RefValue)` is supported on 0.5 and below. ([#18082])

* `unsafe_trunc(::Type{<:Integer}, ::Integer)` is supported on 0.5. ([#18629])

* `bswap` is supported for `Complex` arguments on 0.5 and below. ([#21346])

* `Compat.invokelatest` is equivalent to `Base.invokelatest` in Julia 0.6,
  but works in Julia 0.5+, and allows you to guarantee that a function call
  invokes the latest version of a function ([#19784]).

* `Compat.invokelatest` supports keywords ([#22646]).

* `Compat.StringVector` is supported on 0.5 and below. On 0.6 and later, it aliases `Base.StringVector`. This function allocates a `Vector{UInt8}` whose data can be made into a `String` in constant time; that is, without copying. On 0.5 and later, use `String(...)` with the vector allocated by `StringVector` as an argument to create a string without copying. Note that if 0.4 support is needed, `Compat.UTF8String(...)` should be used instead. ([#19449])

* `==(::Period, ::Period)` and `isless(::Period, ::Period)` is supported for 0.5 and below. Earlier versions of Julia only supported limited comparison methods between Periods which did not support comparing custom Period subtypes. ([#21378])

* `@__MODULE__` is aliased to `current_module()` for Julia versions 0.6 and below. Versions of `Base.binding_module`, `expand`, `macroexpand`, and `include_string` were added that accept a module as the first argument. ([#22064])

* `Cmd` elements can be accessed as if the `Cmd` were an array of strings for 0.6 and below ([#21197]).

* `Val(x)` constructs `Val{x}()`. ([#22475])

* The `reshape` and `ntuple` APIs are extended to support `Val{x}()` arguments on 0.6 and below.

* `chol` and `chol!` for `UniformScalings` ([#22633]).

* `logdet` for `Number`s ([#22629]).

* `fieldcount` is equivalent to `nfields` for Julia versions 0.6 and below and is used to
  determine the number of fields in a data type ([#22350]).

* There are versions of `InexactError`, `DomainError`, and `OverflowError` that take the same arguments as introduced in Julia 0.7-DEV ([#20005], [#22751], [#22761]).

* `retry` for the more flexible `retry` method introduced in 0.6 which includes support for kwargs ([#19331], [#21419]).

* `Base.rtoldefault` how takes a third parameter `atol`.
  The two argument form is deprecated in favor of the three arguments form with `atol=0`.

* The `corrected` optional argument of `cov` becomes a keyword argument.
  Due to conflict with 0.5 deprecation,
  `cov(::AbstractVector; corrected=)` and `cov(::AbstractVector, ::AbstractVector; corrected=)`
  are only available on 0.6. ([#21709])

* `equalto` constructs an `EqualTo` object that can be used as a predicate ([#23812]).

* `*(::Union{Char,AbstractString},::Union{Char,AbstractString})` concatenation. ([#22512])

* `diagm` and `spdiagm` accept pairs mapping diagonals to vectors ([#24047], [#23757])

* Constructors for `Matrix{T}`, `Array{T}`, and `SparseMatrixCSC{T}` from `UniformScaling` ([#24372], [#24657])

* Constructor for `Matrix` from `UniformScaling` ([#24372], [#24657]).

* `Uninitialized` and `uninitialized` with corresponding `Array` constructors ([#24652]).

* `BitArray` constructors for `uninitialized` ([#24785]).

* `@compat finalizer(func, obj)` with the finalizer to run as the first argument and the object to be finalized
  as the second ([#24605]).

* `IOContext` accepting key-value `Pair`s ([#23271]).

* `pairs` for iterating over key-value `Pair`s ([#22907]).

* `get` do-block syntax supported when using `ENV` ([#23412]).

* `Some{T}` wraps `T` to signify that a result of `T<:Void` is expected ([#23642]).

* `replace` accepts a pair of pattern and replacement, with the number of replacements as
  a keyword argument ([#25165]).

* `CartesianIndices` and `LinearIndices` types represent cartesian and linear indices of
  an array (respectively), and indexing such objects allows translating from one kind of index
  to the other ([#25113]).

* `codeunits(s)` returns an array-like view of the `UInt8` code units of
  a string and `ncodeunits(s)` returns the number of code units ([#25241]).

* `printstyled` prints to a given stream optionally in color and/or bolded ([#25522]).

* `Dates.Period` rounding (e.g., `round(Dates.Hour(36), Dates.Day, RoundNearestTiesUp) == Dates.Day(2)` ([#24182]).


## Renaming

* `Display` is now `AbstractDisplay` ([#24831]).

* `$` is now `xor` or `⊻` ([#18977])

* `num` and `den` are now `numerator` and `denominator` ([#19246])

* `takebuf_array` is now a method of `take!`. `takebuf_string(io)` becomes `String(take!(io))` ([#19088])

* `is_apple`, `is_bsd`, `is_linux`, `is_unix`, and `is_windows` are now `Sys.isapple`, `Sys.isbsd`,
  `Sys.islinux`, `Sys.isunix`, and `Sys.iswindows`, respectively. These are available in the `Compat.Sys`
  submodule. ([#22182])

* `readstring` is replaced by methods of `read`. ([#22864])

    `read(::IO, ::Type{String})`, `read(::AbstractString, ::Type{String})`,
    and `read(::Cmd, ::Type{String})` are defined for 0.6 and below.

* `Range` is now `AbstractRange` ([#23570])

* `select`* functions (`select`, `select!`, `selectperm`, `selectperm!`) are renamed to
  `partialsort`* (`partialsort`, `partialsort!`, `partialsortperm`, `partialsortperm!`) ([#23051])

* `ctranspose` and `ctranspose!` are now `adjoint` and `adjoint!` ([#23235])

* Math constants (`π`, `pi`, `e`, `γ`, `eulergamma`, `catalan`, `φ`, `golden`) are moved to the
  `MathConstants` module (available as `Compat.MathConstants`).
  The name exported from `Base` for `e` is changed to `ℯ`. ([#23427])

* `IntSet` is now `BitSet` ([#24282])

* `Complex32`, `Complex64`, and `Complex128` are now `ComplexF16`, `ComplexF32`, and
  `ComplexF64`, respectively ([#24647]).

* `JULIA_HOME` is now `Sys.BINDIR`, available in the `Compat.Sys` submodule. ([#25102])

* `Associative` is now `AbstractDict` ([#25012]).

* `indices` is now `axes` ([#25057]). This function is not exported from Compat to avoid
  conflicts with AxisArrays and other such packages.

* `Void` is now `Nothing` with an alias `Cvoid` for C interop ([#25162]).

* `unshift!` and `shift!` are now `pushfirst!` and `popfirst!` ([#25100]).

* `Base.IteratorSize` and `Base.IteratorEltype` are available as
  `Compat.IteratorSize` and `Compat.IteratorEltype` ([#25402]).

* `copy!` and `unsafe_copy!` are now `copyto!` and `unsafe_copyto!` ([#24808]).

* `ismatch(r::Regex, str::AbstractString)` is now `contains(str, r)` ([#24673]).

* `ipermute!` is now `invpermute!` ([#25168]).

* `module_parent`, `Base.function_module`, and `Base.datatype_module` are now methods of
  a new function called `parentmodule` ([#25629]).

* `module_name`, `Base.function_name`, and `Base.datatype_name` are now methods of a
  new function called `nameof` ([#25622]).

* `find` is now `findall` ([#25545]).

* `search` is now `findfirst`/`findnext` and `rsearch` is now `findlast`/`findprev`,
  sometimes combined with `equalto` or `occursin` ([#24673]).

* `Compat.findfirst`, `Compat.findnext`, `Compat.findlast` and `Compat.findprev`,
  return `nothing` when no match is found (rather than `0`) as on Julia 0.7 ([#24673]).

* `findin(a, b)` is now `findall(occursin(b), a)` ([#24673]).

* `indmin` and `indmax` are now `argmin` and `argmax`, respectively ([#25654]).

* `isabstract` and `isleaftype` are now `isabstracttype` and `isconcretetype`, respectively
  ([#23666], [#25496]).

* `gc` and `gc_enable` are now `GC.gc` and `GC.enable`, respectively ([#25616]).

* `endof` is now `lastindex` ([#25458]). (Note that `lastindex(A, n)` is not supported.)

* `nb_available` is now `bytesavailable` ([#25634]).

* `method_exists` is now `hasmethod` ([#25615]).

* `object_id` is now `objectid` ([#25615]).

## New macros

* `@__DIR__` has been added ([#18380])

* `@vectorize_1arg` and `@vectorize_2arg` are deprecated on Julia 0.6 in favor
  of the broadcast syntax ([#17302]). `Compat.@dep_vectorize_1arg` and
  `Compat.@dep_vectorize_2arg` are provided so that packages can still provide
  the deprecated definitions without causing a depwarn in the package itself
  before all the users are upgraded.

  Packages are expected to use this until all users of the deprecated
  vectorized function have migrated. These macros will be dropped when the
  support for `0.6` is dropped from `Compat`.

* `@nospecialize` has been added ([#22666]).

* The logging macros `@error`, `@warn`, `@info` and `@debug` can be used as
  `Compat.@error`, `Compat.@warn`, `Compat.@info` and `Compat.@debug` on Julia 0.6 ([#24490]).
  Note that the behavior do not mirror the logging macros in Julia 0.7, instead on Julia 0.6:
  - Messages are printed to `STDERR` (like `info` and `warn` on Julia 0.6) and not to a
    dedicated logging stream.
  - The loglevel can not be controlled, but `Compat.@debug` messages can be turned on/off
    by calling `Compat.enable_debug(true/false)`.
  - Extra metadata sent to the macros are ignored.

  As an alternative, see the MicroLogging.jl package for a logging interface similar to the one in Julia 0.7.

## Other changes

* On versions of Julia that do not contain a Base.Threads module, Compat defines a Threads module containing a no-op `@threads` macro.

* The `Expr(:macrocall)` has an extra initial argument `__source__`, which can be tested for with `Compat.macros_have_sourceloc`.

## New types

* `Compat.AbstractDateTime` is an alias for `Compat.Dates.AbstractDateTime` as of ([#25227]) and `Compat.Dates.TimeType` prior to that.

## Developer tips

One of the most important rules for `Compat.jl` is to avoid breaking user code
whenever possible, especially on a released version.

Although the syntax used in the most recent Julia version
is the preferred compat syntax, there are cases where this shouldn't be used.
Examples include when the new syntax already has a different meaning
on previous versions of Julia, or when functions are removed from `Base`
Julia and the alternative cannot be easily implemented on previous versions.
In such cases, possible solutions are forcing the new feature to be used with
qualified name in `Compat.jl` (e.g. use `Compat.<name>`) or
reimplementing the old features on a later Julia version.

If you're adding additional compatibility code to this package, the [`contrib/commit-name.sh`](https://github.com/JuliaLang/julia/blob/master/contrib/commit-name.sh) script in the base Julia repository is useful for extracting the version number from a git commit SHA. For example, from the git repository of `julia`, run something like this:

```sh
bash $ contrib/commit-name.sh a378b60fe483130d0d30206deb8ba662e93944da
0.5.0-dev+2023
```

This prints a version number corresponding to the specified commit of the form
`X.Y.Z-aaa+NNNN`, and you can then test whether Julia
is at least this version by `VERSION >= v"X.Y.Z-aaa+NNNN"`.

### Tagging the correct minimum version of Compat

One of the most frequent problems package developers encounter is finding the right
version of `Compat` to add to their REQUIRE. This is meant to be a guide on how to
specify the right lower bound.

* Find the appropriate fix needed for your package from the `Compat` README. Every
function or feature added to `Compat` is documented in its README, so you are
guaranteed to find it.

* Navigate to the [blame page of the README](https://github.com/JuliaLang/Compat.jl/blame/master/README.md)
by clicking on the README file on GitHub, and then clicking on the `blame` button
which can be found in the top-right corner.

* Now find your fix, and then find the corresponding commit ID of that fix on the
left-hand side. Click on the commit ID. This navigates to a page which recorded
that particular commit.

* On the top pane, you should find the list of the tagged versions of Compat that
includes this fix. Find the minimum version from there.

* Now specify the correct minimum version for Compat in your REQUIRE file by
`Compat <version>`

[#13681]: https://github.com/JuliaLang/julia/issues/13681
[#16564]: https://github.com/JuliaLang/julia/issues/16564
[#16986]: https://github.com/JuliaLang/julia/issues/16986
[#17155]: https://github.com/JuliaLang/julia/issues/17155
[#17302]: https://github.com/JuliaLang/julia/issues/17302
[#17323]: https://github.com/JuliaLang/julia/issues/17323
[#17510]: https://github.com/JuliaLang/julia/issues/17510
[#17623]: https://github.com/JuliaLang/julia/issues/17623
[#18082]: https://github.com/JuliaLang/julia/issues/18082
[#18380]: https://github.com/JuliaLang/julia/issues/18380
[#18484]: https://github.com/JuliaLang/julia/issues/18484
[#18510]: https://github.com/JuliaLang/julia/issues/18510
[#18629]: https://github.com/JuliaLang/julia/issues/18629
[#18727]: https://github.com/JuliaLang/julia/issues/18727
[#18839]: https://github.com/JuliaLang/julia/issues/18839
[#18977]: https://github.com/JuliaLang/julia/issues/18977
[#19088]: https://github.com/JuliaLang/julia/issues/19088
[#19246]: https://github.com/JuliaLang/julia/issues/19246
[#19331]: https://github.com/JuliaLang/julia/issues/19331
[#19449]: https://github.com/JuliaLang/julia/issues/19449
[#19635]: https://github.com/JuliaLang/julia/issues/19635
[#19784]: https://github.com/JuliaLang/julia/issues/19784
[#19841]: https://github.com/JuliaLang/julia/issues/19841
[#19950]: https://github.com/JuliaLang/julia/issues/19950
[#20005]: https://github.com/JuliaLang/julia/issues/20005
[#20022]: https://github.com/JuliaLang/julia/issues/20022
[#20164]: https://github.com/JuliaLang/julia/issues/20164
[#20321]: https://github.com/JuliaLang/julia/issues/20321
[#20407]: https://github.com/JuliaLang/julia/issues/20407
[#20414]: https://github.com/JuliaLang/julia/issues/20414
[#20418]: https://github.com/JuliaLang/julia/issues/20418
[#20500]: https://github.com/JuliaLang/julia/issues/20500
[#20974]: https://github.com/JuliaLang/julia/issues/20974
[#21197]: https://github.com/JuliaLang/julia/issues/21197
[#21257]: https://github.com/JuliaLang/julia/issues/21257
[#21346]: https://github.com/JuliaLang/julia/issues/21346
[#21378]: https://github.com/JuliaLang/julia/issues/21378
[#21419]: https://github.com/JuliaLang/julia/issues/21419
[#21709]: https://github.com/JuliaLang/julia/issues/21709
[#22064]: https://github.com/JuliaLang/julia/issues/22064
[#22182]: https://github.com/JuliaLang/julia/issues/22182
[#22350]: https://github.com/JuliaLang/julia/issues/22350
[#22475]: https://github.com/JuliaLang/julia/issues/22475
[#22512]: https://github.com/JuliaLang/julia/issues/22512
[#22629]: https://github.com/JuliaLang/julia/issues/22629
[#22633]: https://github.com/JuliaLang/julia/issues/22633
[#22646]: https://github.com/JuliaLang/julia/issues/22646
[#22666]: https://github.com/JuliaLang/julia/issues/22666
[#22751]: https://github.com/JuliaLang/julia/issues/22751
[#22761]: https://github.com/JuliaLang/julia/issues/22761
[#22864]: https://github.com/JuliaLang/julia/issues/22864
[#22907]: https://github.com/JuliaLang/julia/issues/22907
[#23051]: https://github.com/JuliaLang/julia/issues/23051
[#23235]: https://github.com/JuliaLang/julia/issues/23235
[#23271]: https://github.com/JuliaLang/julia/issues/23271
[#23412]: https://github.com/JuliaLang/julia/issues/23412
[#23427]: https://github.com/JuliaLang/julia/issues/23427
[#23570]: https://github.com/JuliaLang/julia/issues/23570
[#23642]: https://github.com/JuliaLang/julia/issues/23642
[#23666]: https://github.com/JuliaLang/julia/issues/23666
[#23757]: https://github.com/JuliaLang/julia/issues/23757
[#23812]: https://github.com/JuliaLang/julia/issues/23812
[#23931]: https://github.com/JuliaLang/julia/issues/23931
[#24047]: https://github.com/JuliaLang/julia/issues/24047
[#24282]: https://github.com/JuliaLang/julia/issues/24282
[#24361]: https://github.com/JuliaLang/julia/issues/24361
[#24372]: https://github.com/JuliaLang/julia/issues/24372
[#24443]: https://github.com/JuliaLang/julia/issues/24443
[#24459]: https://github.com/JuliaLang/julia/issues/24459
[#24490]: https://github.com/JuliaLang/julia/issues/24490
[#24605]: https://github.com/JuliaLang/julia/issues/24605
[#24647]: https://github.com/JuliaLang/julia/issues/24647
[#24648]: https://github.com/JuliaLang/julia/issues/24648
[#24652]: https://github.com/JuliaLang/julia/issues/24652
[#24657]: https://github.com/JuliaLang/julia/issues/24657
[#24673]: https://github.com/JuliaLang/julia/issues/24673
[#24714]: https://github.com/JuliaLang/julia/issues/24714
[#24785]: https://github.com/JuliaLang/julia/issues/24785
[#24808]: https://github.com/JuliaLang/julia/issues/24808
[#24874]: https://github.com/JuliaLang/julia/issues/24874
[#25012]: https://github.com/JuliaLang/julia/issues/25012
[#25021]: https://github.com/JuliaLang/julia/issues/25021
[#25056]: https://github.com/JuliaLang/julia/issues/25056
[#25057]: https://github.com/JuliaLang/julia/issues/25057
[#25100]: https://github.com/JuliaLang/julia/issues/25100
[#25102]: https://github.com/JuliaLang/julia/issues/25102
[#25113]: https://github.com/JuliaLang/julia/issues/25113
[#25162]: https://github.com/JuliaLang/julia/issues/25162
[#25165]: https://github.com/JuliaLang/julia/issues/25165
[#25168]: https://github.com/JuliaLang/julia/issues/25168
[#25227]: https://github.com/JuliaLang/julia/issues/25227
[#25241]: https://github.com/JuliaLang/julia/issues/25241
[#25249]: https://github.com/JuliaLang/julia/issues/25249
[#25402]: https://github.com/JuliaLang/julia/issues/25402
[#25458]: https://github.com/JuliaLang/julia/issues/25458
[#25459]: https://github.com/JuliaLang/julia/issues/25459
[#25496]: https://github.com/JuliaLang/julia/issues/25496
[#25522]: https://github.com/JuliaLang/julia/issues/25522
[#25544]: https://github.com/JuliaLang/julia/issues/25544
[#25545]: https://github.com/JuliaLang/julia/issues/25545
[#25571]: https://github.com/JuliaLang/julia/issues/25571
[#25615]: https://github.com/JuliaLang/julia/issues/25615
[#25616]: https://github.com/JuliaLang/julia/issues/25616
[#25622]: https://github.com/JuliaLang/julia/issues/25622
[#25628]: https://github.com/JuliaLang/julia/issues/25628
[#25629]: https://github.com/JuliaLang/julia/issues/25629
[#25634]: https://github.com/JuliaLang/julia/issues/25634
[#25646]: https://github.com/JuliaLang/julia/issues/25646
[#25654]: https://github.com/JuliaLang/julia/issues/25654
[#25705]: https://github.com/JuliaLang/julia/issues/25705
[#25706]: https://github.com/JuliaLang/julia/issues/25706
[#25780]: https://github.com/JuliaLang/julia/issues/25780
[#24182]: https://github.com/JuliaLang/julia/issues/24182
[#24673]: https://github.com/JuliaLang/julia/issues/24673