# Compat Package for Julia

[![Build Status](https://travis-ci.org/JuliaLang/Compat.jl.svg?branch=master)](https://travis-ci.org/JuliaLang/Compat.jl)

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

* `@compat Dict(foo => bar, baz => qux)` - type-inferred `Dict` construction.

* `@compat Dict{Foo,Bar}(foo => bar, baz => qux)` - type-declared `Dict` construction.

* `@compat split(str, splitter; keywords...)` - the Julia 0.4-style keyword-based `split` function

* `@compat rsplit(str, splitter; keywords...)` - the Julia 0.4-style keyword-based `rsplit` function

* `@compat Float64(x)`, `@compat UInt8(x)`,  - the Julia 0.4-style numeric types constructor.

* `@compat Tuple{foo, bar}` - Julia 0.4-style tuple types.

* `@compat chol(A, Val{:U})` - Julia 0.4 type-stable cholesky factorizations (will not be type-stable on 0.3)

* `@compat f(t::Timer)` - mimic the Julia 0.4 Timer class

## Type Aliases

* `typealias AbstractString String` - `String` has been renamed to `AbstractString` [#8872](https://github.com/JuliaLang/julia/pull/8872)

* `typealias AssertionError ErrorException` - `AssertionError` was introduced in [##9734](https://github.com/JuliaLang/julia/pull/#9734); before `@assert` threw an `ErrorException`

* For all unsigned integer types to their equivalents with uppercase `I`. [#8907](https://github.com/JuliaLang/julia/pull/8907)

* `Cstring` and `Cwstring` for `Ptr{Cchar}` and `Ptr{Cwchar_t}`, respectively:
  these should be used for passing NUL-terminated strings to `ccall`.  (In
  Julia 0.4, using these types also checks whether the string has embedded
  NUL characters [#10994](https://github.com/JuliaLang/julia/pull/10994).)

## New functions

* `eachindex`, as in `for i in eachindex(A)`, can be used in julia 0.3. This is the recommended way to iterate over each index in an `AbstractArray`. On julia 0.3 `eachindex` just returns `1:length(A)`, but in julia 0.4 it can return a more sophisticated iterator.

* `isdiag`, which tests whether a matrix is diagonal, can be used in julia 0.3.

* `keytype` and `valtype`, which return key and value type of Associative type, can be used in julia 0.3.

* `fma(x,y,z)` and `muladd(x,y,z)` can be used in Julia 0.3 for `x*y+z`.

* `Timer(timeout::Real, repeat::Real=0.0)` and `Timer(cb::Function, timeout::Real, repeat::Real=0.0)` allow julia 0.4-style Timers to be constructed and used.

## Renamed functions

* `itrunc`, `iround`, `iceil`, `ifloor` are now accessed via `trunc(T, x)`, etc. [#9133](https://github.com/JuliaLang/julia/pull/9133)

* `Base.Random.randmtzig_exprnd` is now `randexp` [#9144](https://github.com/JuliaLang/julia/pull/9144)

* `sizehint` is now `sizehint!` [#9278](https://github.com/JuliaLang/julia/pull/9278)

* `Base.IPv4` and `Base.IPv6` can now accept `String`s as constructor arguments [#9346](https://github.com/JuliaLang/julia/pull/9346)

* `randbool()` is now `rand(Bool)` and `randbool([dims])` is now `bitrand([dims])` [#9569](https://github.com/JuliaLang/julia/pull/9569)

* `beginswith` is now `startswith` [#9583](https://github.com/JuliaLang/julia/pull/9583)

* `|>`, `>>`, `.>`, and `.>>` are now `pipe` [#10211](https://github.com/JuliaLang/julia/pull/10211)

* `names(::DataType)` is now renamed to `fieldnames` [#10332](https://github.com/JuliaLang/julia/pull/10332)

* `parseint` and `parsefloat` are now `parse(T, ...)` [#10543](https://github.com/JuliaLang/julia/pull/10543); along the same line `BigFloat(s::String)` is now `parse(BigFloat,s)` [#10955](https://github.com/JuliaLang/julia/pull/10955).

* `convert(::Ptr{T}, x)` is now `Base.unsafe_convert` [#9986](https://github.com/JuliaLang/julia/pull/9986).
  Compat provides an unexported `Compat.unsafe_convert` method that is aliased to `Base.convert` on Julia 0.3 and
  `Base.unsafe_convert` on Julia 0.4.

## New macros

* `@inline` and `@noinline` have been added. On 0.3, these are "no-ops," meaning they don't actually do anything.

## Other changes

* `Dict(ks, vs)` is now `Dict(zip(ks, vs))` [#8521](https://github.com/JuliaLang/julia/pull/8521)

* Libc and dynamic library-related functions have been moved to the Libc and Libdl modules [#10328](https://github.com/JuliaLang/julia/pull/10328)

* `zero(Ptr{T})` is now `Ptr{T}(0)` [#8909](https://github.com/JuliaLang/julia/pull/8909)

## New types

* [`Nullable` types](http://julia.readthedocs.org/en/latest/manual/types/?highlight=nullable#nullable-types-representing-missing-values) and their associated operations.

* The parametric `Val{T}` ["value types"](http://julia.readthedocs.org/en/latest/manual/types/#value-types) can be used in julia 0.3.

## Developer tips

If you're adding additional compatibility code to this package, the following shell script is useful for extracting the version number from a git commit SHA:

```sh
#! /bin/bash
last_tag=$(git describe --tags --abbrev=0)
git rev-list $1 ^$last_tag | wc -l | sed -e 's/[^[:digit:]]//g'
```
This will print a number `XXXX`, and you can then test whether Julia
is at least this version by `VERSION >= v"0.4.0-dev+XXXX"` (assuming
it is a commit from the 0.4 development cycle).
