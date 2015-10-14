# Compat Package for Julia

[![Build Status](https://travis-ci.org/JuliaLang/Compat.jl.svg?branch=master)](https://travis-ci.org/JuliaLang/Compat.jl)
[![Build status](https://ci.appveyor.com/api/projects/status/github/JuliaLang/Compat.jl?branch=master)](https://ci.appveyor.com/project/quinnj/compat-jl/branch/master)

[![Compat](http://pkg.julialang.org/badges/Compat_0.3.svg)](http://pkg.julialang.org/?pkg=Compat&ver=0.3)
[![Compat](http://pkg.julialang.org/badges/Compat_0.4.svg)](http://pkg.julialang.org/?pkg=Compat&ver=0.4)

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
after a `using Compat` statement to load Compat, simply use:

```
@compat ...Julia master syntax...
```

wherever you want to use syntax that differs in the latest Julia
`master` (the development version of Julia).

## Supported syntax

Currently, the `@compat` macro supports the following syntaxes:

* `@compat Dict(foo => bar, baz => qux)` - type-inferred `Dict` construction. (Also works for `DataStructures.OrderedDict`)

* `@compat Dict{Foo,Bar}(foo => bar, baz => qux)` - type-declared `Dict` construction. (Also works for `DataStructures.OrderedDict`)

* `@compat split(str, splitter; keywords...)` - the Julia 0.4-style keyword-based `split` function

* `@compat rsplit(str, splitter; keywords...)` - the Julia 0.4-style keyword-based `rsplit` function

* `@compat Float64(x)`, `@compat UInt8(x)`,  - the Julia 0.4-style numeric types constructor.

* `@compat Tuple{foo, bar}` - Julia 0.4-style tuple types.

* `@compat chol(A, Val{:U})` - Julia 0.4 type-stable cholesky factorizations (will not be type-stable on 0.3)

* `@compat f(t::Timer)` - mimic the Julia 0.4 Timer class

* `@compat Vector{Int}()`, `@compat Vector{UInt8}(n)`, `@compat Array{Float32}(2,2)` - Julia 0.4-style array constructors.

* `@compat Void` - `Nothing` on 0.3 (`Ptr{Void}` is not changed).

* `@compat Union{args...}` - `Union(args...)` on 0.3. [#11432](https://github.com/JuliaLang/julia/pull/11432)

## Type Aliases

* `typealias AbstractString String` - `String` has been renamed to `AbstractString` [#8872](https://github.com/JuliaLang/julia/pull/8872)

* `typealias AbstractFloat FloatingPoint` - `FloatingPoint` has been renamed to `AbstractFloat` [#12162](https://github.com/JuliaLang/julia/pull/12162)

* `typealias AssertionError ErrorException` - `AssertionError` was introduced in [#9734](https://github.com/JuliaLang/julia/pull/9734); before `@assert` threw an `ErrorException`

* For all unsigned integer types to their equivalents with uppercase `I`. [#8907](https://github.com/JuliaLang/julia/pull/8907)

* `Cstring` and `Cwstring` for `Ptr{UInt8}` and `Ptr{Cwchar_t}`, respectively:
  these should be used for passing NUL-terminated strings to `ccall`.  (In
  Julia 0.4, using these types also checks whether the string has embedded
  NUL characters [#10994](https://github.com/JuliaLang/julia/pull/10994).)

* `typealias Irrational MathConst` - `MathConst` has been renamed to `Irrational` [#11922](https://github.com/JuliaLang/julia/pull/11922)

* `typealias UDPSocket UdpSocket` - `UdpSocket` has been renamed to `UDPSocket` [#8175](https://github.com/JuliaLang/julia/pull/8175)

* `typealias Base64EncodePipe Base64Pipe` - `Base64Pipe` has been renamed to `Base64EncodePipe` [#9157](https://github.com/JuliaLang/julia/pull/9157)

* `typealias OutOfMemoryError MemoryError` - `MemoryError` has been renamed to `OutOfMemoryError` [#10503](https://github.com/JuliaLang/julia/pull/10503)

## New functions

* `eachindex`, as in `for i in eachindex(A)`, can be used in julia 0.3. This is the recommended way to iterate over each index in an `AbstractArray`. On julia 0.3 `eachindex` just returns `1:length(A)`, but in julia 0.4 it can return a more sophisticated iterator.

* `isdiag`, which tests whether a matrix is diagonal, can be used in julia 0.3.

* `keytype` and `valtype`, which return key and value type of Associative type, can be used in julia 0.3.

* `fma(x,y,z)` and `muladd(x,y,z)` can be used in Julia 0.3 for `x*y+z`.

* `Timer(timeout::Real, repeat::Real=0.0)` and `Timer(cb::Function, timeout::Real, repeat::Real=0.0)` allow julia 0.4-style Timers to be constructed and used.

* `__precompile__(iscompiled::Bool)` and `include_dependency(path::AbstractString)` allow
  Julia 0.4 precompilation information to be provided (with no effect in earlier versions).
  (However, to enable precompiling in 0.4, it is better to explicitly put `VERSION >= v"0.4.0-dev+6521" && __precompile__()` before your `module` statement, so that Julia knows to precompile before anything in your module is evaluated.)

* `isapprox(A, B)` for arrays ([JuliaLang/julia#12472](https://github.com/JuliaLang/julia/pull/12472)), and synonyms `≈` ([U+2248](http://www.fileformat.info/info/unicode/char/2248/index.htm), LaTeX `\approx`) and `≉` ([U+2249](http://www.fileformat.info/info/unicode/char/2249/index.htm), LaTeX `\napprox`) for `isapprox` and `!isapprox`, respectively.

## Renamed functions

* `itrunc`, `iround`, `iceil`, `ifloor` are now accessed via `trunc(T, x)`, etc. ([#9133](https://github.com/JuliaLang/julia/pull/9133)).  Truncated conversions between integer types are now `n % T` ([#8646](https://github.com/JuliaLang/julia/issues/8646)).

* `Base.Random.randmtzig_exprnd` is now `randexp` [#9144](https://github.com/JuliaLang/julia/pull/9144)

* `sizehint` is now `sizehint!` [#9278](https://github.com/JuliaLang/julia/pull/9278)

* `Base.IPv4` and `Base.IPv6` can now accept `String`s as constructor arguments [#9346](https://github.com/JuliaLang/julia/pull/9346)

* `randbool()` is now `rand(Bool)` and `randbool([dims])` is now `bitrand([dims])` [#9569](https://github.com/JuliaLang/julia/pull/9569)

* `beginswith` is now `startswith` [#9583](https://github.com/JuliaLang/julia/pull/9583)

* `|>`, `>>`, `.>`, and `.>>` are now `pipeline` ([#10211](https://github.com/JuliaLang/julia/pull/10211) and [#12739](https://github.com/JuliaLang/julia/pull/12739))

* `names(::DataType)` is now renamed to `fieldnames` [#10332](https://github.com/JuliaLang/julia/pull/10332)

* `parseint` and `parsefloat` are now `parse(T, ...)` [#10543](https://github.com/JuliaLang/julia/pull/10543); along the same line `BigFloat(s::String)` is now `parse(BigFloat,s)` [#10955](https://github.com/JuliaLang/julia/pull/10955).

* `convert(::Ptr{T}, x)` is now `Base.unsafe_convert` [#9986](https://github.com/JuliaLang/julia/pull/9986).
  Compat provides an unexported `Compat.unsafe_convert` method that is aliased to `Base.convert` on Julia 0.3 and
  `Base.unsafe_convert` on Julia 0.4.

* `gc_enable()` is now `gc_enable(true)` and `gc_disable()` is now `gc_enable(false)` [#11647](https://github.com/JuliaLang/julia/pull/11647)

* `base64` is now `base64encode` [#9157](https://github.com/JuliaLang/julia/pull/9157)

## New macros

* `@inline` and `@noinline` have been added. On 0.3, these are "no-ops," meaning they don't actually do anything.

## Other changes

* `Dict(ks, vs)` is now `Dict(zip(ks, vs))` [#8521](https://github.com/JuliaLang/julia/pull/8521)

* Libc and dynamic library-related functions have been moved to the Libc and Libdl modules [#10328](https://github.com/JuliaLang/julia/pull/10328)

* `zero(Ptr{T})` is now `Ptr{T}(0)` [#8909](https://github.com/JuliaLang/julia/pull/8909)

* The unexported macro `Base.@math_const` was renamed to `Base.@irrational`, accessible as `Compat.@irrational` on either 0.3 or 0.4 [#11922](https://github.com/JuliaLang/julia/pull/11922)

* `remotecall`, `remotecall_fetch`, `remotecall_wait`, and `remote_do` have the function to be executed remotely as the first argument in Julia 0.5. Loading `Compat` defines the same methods in older versions of Julia. [#13338](https://github.com/JuliaLang/julia/pull/13338)

## New types

* [`Nullable` types](http://julia.readthedocs.org/en/latest/manual/types/?highlight=nullable#nullable-types-representing-missing-values) and their associated operations.

* The parametric `Val{T}` ["value types"](http://julia.readthedocs.org/en/latest/manual/types/#value-types) can be used in julia 0.3.

## Developer tips

If you're adding additional compatibility code to this package, the [`bin/version.sh` script is useful for extracting the version number from a git commit SHA. For example, from the git repository of `julia`, run something like this:

```sh
bash $ /path/to/Compat/bin/version.sh e3aa57efbc6542efbcc7feac9b1309d628ac6f12
2418
```

This prints a number `XXXX`, and you can then test whether Julia
is at least this version by `VERSION >= v"0.4.0-dev+XXXX"` (assuming
it is a commit from the 0.4 development cycle).
