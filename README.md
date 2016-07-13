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
shortly after the `module` statement include lines like these:

```
using Compat
import Compat.String
```

and then as needed add

```
@compat ...compat syntax...
```

wherever you want to use syntax that differs in the latest Julia
`master` (the development version of Julia). The `compat syntax` is usually
the syntax on Julia `master`. However, in a few cases where this is not possible,
a slightly different syntax might be used.
Please check the list below for the specific syntax you need.

## Supported syntax

Currently, the `@compat` macro supports the following syntaxes:

* `@compat foo.:bar` - `foo.(:bar)` in 0.4 ([#15032]).

* `@compat f.(args...)` - `broadcast(f, args...)` in 0.4 ([#15032]).

* `@compat (a::B{T}){T}(c) = d` - the Julia 0.5-style call overload.

* `@compat Dict(foo => bar, baz => qux)` - type-inferred `Dict` construction. (Also works for `DataStructures.OrderedDict`)

* `@compat Dict{Foo,Bar}(foo => bar, baz => qux)` - type-declared `Dict` construction. (Also works for `DataStructures.OrderedDict`)

* `@compat(get(io, s, false))`, with `s` equal to `:limit`, `:compact` or `:multiline`, to detect the corresponding print settings (performs useful work only on Julia 0.5, defaults to `false` otherwise)

* `@compat split(str, splitter; keywords...)` - the Julia 0.4-style keyword-based `split` function

* `@compat rsplit(str, splitter; keywords...)` - the Julia 0.4-style keyword-based `rsplit` function

* `@compat Float64(x)`, `@compat UInt8(x)`,  - the Julia 0.4-style numeric types constructor.

* `@compat Tuple{foo, bar}` - Julia 0.4-style tuple types.

* `@compat chol(A, Val{:U})` - Julia 0.4 type-stable cholesky factorizations (will not be type-stable on 0.3)

* `@compat f(t::Timer)` - mimic the Julia 0.4 Timer class

* `@compat Vector{Int}()`, `@compat Vector{UInt8}(n)`, `@compat Array{Float32}(2,2)` - Julia 0.4-style array constructors.

* `@compat Void` - `Nothing` on 0.3 (`Ptr{Void}` is not changed).

* `@compat Union{args...}` - `Union(args...)` on 0.3. [#11432](https://github.com/JuliaLang/julia/pull/11432)

* `@compat withenv(f, "a" => a, "b" => b...)` on 0.3.

* `@compat import Base.show` and `@compat function show(args...)` for handling the deprecation of `writemime` in Julia 0.5 ([#16563]). See https://github.com/JuliaLang/Compat.jl/pull/219.

* `@compat @boundscheck checkbounds(...)` rewrites to unconditionally call `checkbounds(...)` in 0.4.  The 0.4-style two-argument form of `@boundscheck` is left unchanged.

## Type Aliases

* `String` has undergone multiple changes: in Julia 0.3 it was an abstract type and then got renamed to `AbstractString` in 0.4; in 0.5, `ASCIIString` and `ByteString` were deprecated, and `UTF8String` was renamed to the (now concrete) type `String`.

    Compat provides unexported `Compat.UTF8String` and `Compat.ASCIIString` type aliases which are equivalent to the same types from Base on Julia 0.3 and 0.4, but to `String` on Julia 0.5. In most cases, using these types by calling `import Compat: UTF8String, ASCIIString` should be enough. Though note that `Compat.ASCIIString` does **not** guarantee that the string only contains ASCII characters on Julia 0.5: call `isascii` to check if the string is pure ASCII if needed.

    Compat also provides an unexported `Compat.String` type which is equivalent to `ByteString` on Julia 0.3 and 0.4, and to `String` on Julia 0.5. This type should be used only in places where `ByteString` was used on Julia 0.3 and 0.4, i.e. where either `ASCIIString` or `UTF8String` should be accepted. It should **not** be used as the default type for variables or fields holding strings, as it introduces type-instability in Julia 0.3 and 0.4: use `Compat.UTF8String` or `Compat.ASCIIString` instead.

* `bytestring` has been replaced in most cases with additional `String` construction methods; for 0.3 compatibility, the usage involves replacing `bytestring(args...)` with `@compat String(args...)`. However, for converting a `Ptr{UInt8}` to a string, use the new `unsafe_string(...)` method to make a copy or `unsafe_wrap(String, ...)` to avoid a copy.

* `typealias AbstractString String` - `String` has been renamed to `AbstractString` in Julia 0.4 [#8872](https://github.com/JuliaLang/julia/pull/8872)

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

* `eachindex`, as in `for i in eachindex(A)`, can be used in Julia 0.3. This is the recommended way to iterate over each index in an `AbstractArray`. On Julia 0.3 `eachindex` just returns `1:length(A)`, but in Julia 0.4 it can return a more sophisticated iterator.

* `isdiag`, which tests whether a matrix is diagonal, can be used in Julia 0.3.

* `keytype` and `valtype`, which return key and value type of Associative type, can be used in Julia 0.3.

* `tryparse`, which is a variant on `Base.parse` that returns a `Nullable`, can be used in Julia 0.3.

* `fma(x,y,z)` and `muladd(x,y,z)` can be used in Julia 0.3 for `x*y+z`.

* `Timer(timeout::Real, repeat::Real=0.0)` and `Timer(cb::Function, timeout::Real, repeat::Real=0.0)` allow Julia 0.4-style Timers to be constructed and used.

* `__precompile__(iscompiled::Bool)` and `include_dependency(path::AbstractString)` allow
  Julia 0.4 precompilation information to be provided (with no effect in earlier versions).
  (However, to enable precompiling in 0.4, it is better to explicitly put `VERSION >= v"0.4.0-dev+6521" && __precompile__()` before your `module` statement, so that Julia knows to precompile before anything in your module is evaluated.)

* `isapprox(A, B)` for arrays ([JuliaLang/julia#12472](https://github.com/JuliaLang/julia/pull/12472)), and synonyms `≈` ([U+2248](http://www.fileformat.info/info/unicode/char/2248/index.htm), LaTeX `\approx`) and `≉` ([U+2249](http://www.fileformat.info/info/unicode/char/2249/index.htm), LaTeX `\napprox`) for `isapprox` and `!isapprox`, respectively.

* `withenv` can be used in Julia 0.3 (see [the 0.4 docs](http://docs.julialang.org/en/release-0.4/stdlib/base/#Base.withenv)). Note that you must prepend calls to `withenv` with `@compat` if you'd like to use it with the `=>` syntax.

* `foreach`, similar to `map` but when the return value is not needed ([#13744](https://github.com/JuliaLang/julia/pull/13774)).

* `walkdir`, returns an iterator that walks the directory tree of a directory. ([#13707](https://github.com/JuliaLang/julia/pull/13707))

* `allunique`, checks whether all elements in an iterable appear only once ([#15914](https://github.com/JuliaLang/julia/pull/15914)).
* `Base.promote_eltype_op` is available as `Compat.promote_eltype_op`; however, in Julia 0.3, results may be inaccurate.

## Renamed functions

* `pointer_to_array` and `pointer_to_string` have been replaced with `unsafe_wrap(Array, ...)` and `unsafe_wrap(String, ...)` respectively.

* `bytestring(::Ptr, ...)` has been replaced with `unsafe_string`.

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

* `super` is now `supertype` [#14338](https://github.com/JuliaLang/julia/pull/14338)

* `qr(A, pivot=b)` is now `qr(A, Val{b})`, likewise for `qrfact` and `qrfact!`

* `readall` and `readbytes` are now `readstring` and `read` [#14660](https://github.com/JuliaLang/julia/pull/14660)

* `get_bigfloat_precision` is now `precision(BigFloat)`, `set_precision` is `setprecision` and `with_bigfloat_precision` is now also `setprecision`
[#13232](https://github.com/JuliaLang/julia/pull/13232)

* `get_rounding` is now `rounding`. `set_rounding` and `with_rounding` are now `setrounding` [#13232](https://github.com/JuliaLang/julia/pull/13232)

*  `Base.tty_size` (which was not exported) is now `displaysize` in Julia 0.5.

* `Compat.LinAlg.checksquare` [#14601](https://github.com/JuliaLang/julia/pull/14601)

* `issym` is now `issymmetric` [#15192](https://github.com/JuliaLang/julia/pull/15192)

* `istext` is now `istextmime` [#15708](https://github.com/JuliaLang/julia/pull/15708)

* `symbol` is now `Symbol` [#16154](https://github.com/JuliaLang/julia/pull/16154); use `@compat Symbol(...)` if you need Julia 0.3 compatibility.

* `write(::IO, ::Ptr, len)` is now `unsafe_write` [#14766](https://github.com/JuliaLang/julia/pull/14766).

* `slice` is now `view`[#16972](https://github.com/JuliaLang/julia/pull/16972) do `import Compat.view` and then use `view` normally without the `@compat` macro.

## New macros

* `@static` has been added [#16219](https://github.com/JuliaLang/julia/pull/16219).

* `@inline` and `@noinline` have been added. On 0.3, these are "no-ops," meaning they don't actually do anything.

* `@functorize` (not present in any Julia version) takes a function (or operator) and turns it into a functor object if one is available in the used Julia version. E.g. something like `mapreduce(Base.AbsFun(), Base.MulFun(), x)` can now be written as `mapreduce(@functorize(abs), @functorize(*), x)`, and `f(::Base.AbsFun())` as `f(::typeof(@functorize(abs)))`, to work across different Julia versions. `Func{1}` can be written as `supertype(typeof(@functorize(abs)))` (and so on for `Func{2}`), which will fall back to `Function` on Julia 0.5.

## Other changes

* `Dict(ks, vs)` is now `Dict(zip(ks, vs))` [#8521](https://github.com/JuliaLang/julia/pull/8521)

* Libc and dynamic library-related functions have been moved to the Libc and Libdl modules [#10328](https://github.com/JuliaLang/julia/pull/10328)

* `zero(Ptr{T})` is now `Ptr{T}(0)` [#8909](https://github.com/JuliaLang/julia/pull/8909)

* The unexported macro `Base.@math_const` was renamed to `Base.@irrational`, accessible as `Compat.@irrational` on either 0.3 or 0.4 [#11922](https://github.com/JuliaLang/julia/pull/11922)

* `remotecall`, `remotecall_fetch`, `remotecall_wait`, and `remote_do` have the function to be executed remotely as the first argument in Julia 0.5. Loading `Compat` defines the same methods in older versions of Julia. [#13338](https://github.com/JuliaLang/julia/pull/13338)

* `Base.FS` is now `Base.Filesystem` [#12819](https://github.com/JuliaLang/julia/pull/12819).
  Compat provides an unexported `Compat.Filesystem` module that is aliased to
  `Base.FS` on Julia 0.3 and 0.4 and `Base.Filesystem` on Julia 0.5.

* `mktemp` and `mktempdir` now have variants which take a function as their first argument for automated cleanup. [#9017](https://github.com/JuliaLang/julia/pull/9017)

* `cov` and `cor` don't allow keyword arguments anymore. Loading Compat defines compatibility methods for the new API. [#13465](https://github.com/JuliaLang/julia/pull/13465)

* On versions of Julia that do not contain a Base.Threads module, Compat defines a Threads module containing a no-op `@threads` macro.

* `Base.SingleAsyncWork` is now `Base.AsyncCondition`
  Compat provides an unexported `Compat.AsyncCondition` type that is aliased to
  `Base.SingleAsyncWork` on Julia 0.3 and 0.4 and `Base.AsyncCondition` on Julia 0.5.

* `repeat` now accepts any `AbstractArray` [#14082](https://github.com/JuliaLang/julia/pull/14082): `Compat.repeat` supports this new API on Julia 0.3 and 0.4, and calls `Base.repeat` on 0.5.

* `OS_NAME` is now `Sys.KERNEL`. OS information available as `is_apple`, `is_bsd`, `is_linux`, `is_unix`, and `is_windows`. [16219](https://github.com/JuliaLang/julia/pull/16219)

## New types

* [`Nullable` types](http://julia.readthedocs.org/en/latest/manual/types/?highlight=nullable#nullable-types-representing-missing-values) and their associated operations.

* The parametric `Val{T}` ["value types"](http://julia.readthedocs.org/en/latest/manual/types/#value-types) can be used in Julia 0.3.

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

If you're adding additional compatibility code to this package, the [`bin/version.sh`](https://github.com/JuliaLang/Compat.jl/blob/master/bin/version.sh) script is useful for extracting the version number from a git commit SHA. For example, from the git repository of `julia`, run something like this:

```sh
bash $ /path/to/Compat/bin/version.sh a378b60fe483130d0d30206deb8ba662e93944da
0.5.0-dev+2023
```

This prints a version number corresponding to the specified commit of the form
`X.Y.Z-aaa+NNNN`, and you can then test whether Julia
is at least this version by `VERSION >= v"X.Y.Z-aaa+NNNN"`.

[#15032]: https://github.com/JuliaLang/julia/issues/15032
