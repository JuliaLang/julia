# Compat Package for Julia

[![Build Status](https://travis-ci.org/JuliaLang/Compat.jl.svg?branch=master)](https://travis-ci.org/JuliaLang/Compat.jl)
[![Build status](https://ci.appveyor.com/api/projects/status/github/JuliaLang/Compat.jl?branch=master)](https://ci.appveyor.com/project/quinnj/compat-jl/branch/master)

[![Compat](http://pkg.julialang.org/badges/Compat_0.3.svg)](http://pkg.julialang.org/?pkg=Compat&ver=0.3)
[![Compat](http://pkg.julialang.org/badges/Compat_0.4.svg)](http://pkg.julialang.org/?pkg=Compat&ver=0.4)
[![Compat](http://pkg.julialang.org/badges/Compat_0.5.svg)](http://pkg.julialang.org/?pkg=Compat&ver=0.5)
[![Compat](http://pkg.julialang.org/badges/Compat_0.6.svg)](http://pkg.julialang.org/?pkg=Compat&ver=0.6)

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

* `@compat(get(io, s, false))`, with `s` equal to `:limit`, `:compact` or `:multiline`, to detect the corresponding print settings (performs useful work only on Julia 0.5, defaults to `false` otherwise)

* `@compat import Base.show` and `@compat function show(args...)` for handling the deprecation of `writemime` in Julia 0.5 ([#16563]). See https://github.com/JuliaLang/Compat.jl/pull/219.

* `@compat @boundscheck checkbounds(...)` rewrites to unconditionally call `checkbounds(...)` in 0.4.  The 0.4-style two-argument form of `@boundscheck` is left unchanged.

## Type Aliases

* `String` has undergone multiple changes: in Julia 0.3 it was an abstract type and then got renamed to `AbstractString` in 0.4; in 0.5, `ASCIIString` and `ByteString` were deprecated, and `UTF8String` was renamed to the (now concrete) type `String`.

    Compat provides unexported `Compat.UTF8String` and `Compat.ASCIIString` type aliases which are equivalent to the same types from Base on Julia 0.4, but to `String` on Julia 0.5. In most cases, using these types by calling `import Compat: UTF8String, ASCIIString` should be enough. Though note that `Compat.ASCIIString` does **not** guarantee that the string only contains ASCII characters on Julia 0.5: call `isascii` to check if the string is pure ASCII if needed.

    Compat also provides an unexported `Compat.String` type which is equivalent to `ByteString` on Julia 0.4, and to `String` on Julia 0.5. This type should be used only in places where `ByteString` was used on Julia 0.4, i.e. where either `ASCIIString` or `UTF8String` should be accepted. It should **not** be used as the default type for variables or fields holding strings, as it introduces type-instability in Julia 0.4: use `Compat.UTF8String` or `Compat.ASCIIString` instead.

* `bytestring` has been replaced in most cases with additional `String` construction methods; for 0.4 compatibility, the usage involves replacing `bytestring(args...)` with `Compat.String(args...)`. However, for converting a `Ptr{UInt8}` to a string, use the new `unsafe_string(...)` method to make a copy or `unsafe_wrap(String, ...)` to avoid a copy.

## New functions

* `foreach`, similar to `map` but when the return value is not needed ([#13744](https://github.com/JuliaLang/julia/pull/13774)).

* `walkdir`, returns an iterator that walks the directory tree of a directory. ([#13707](https://github.com/JuliaLang/julia/pull/13707))

* `allunique`, checks whether all elements in an iterable appear only once ([#15914](https://github.com/JuliaLang/julia/pull/15914)).
* `Base.promote_eltype_op` is available as `Compat.promote_eltype_op`; however, in Julia 0.3, results may be inaccurate.

* [`normalize`](http://docs.julialang.org/en/latest/stdlib/linalg/?highlight=normalize#Base.normalize) and [`normalize!`](http://docs.julialang.org/en/latest/stdlib/linalg/?highlight=normalize#Base.normalize!), normalizes a vector with respect to the p-norm ([#13681](https://github.com/JuliaLang/julia/pull/13681))

* `redirect_stdout`, `redirect_stderr`, and `redirect_stdin` take an optional function as a first argument, `redirect_std*(f, stream)`, so that one may use `do` block syntax (as first available for Julia 0.6).

## Renamed functions

* `pointer_to_array` and `pointer_to_string` have been replaced with `unsafe_wrap(Array, ...)` and `unsafe_wrap(String, ...)` respectively.

* `bytestring(::Ptr, ...)` has been replaced with `unsafe_string`.

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

* `fieldoffsets` has been deprecated in favor of `fieldoffset`[#14777](https://github.com/JuliaLang/julia/pull/14777).

* `print_escaped` is now another method of `escape_string`, `print_unescaped` a method of `unescape_string`, and `print_joined` a method of `join`. [#16603](https://github.com/JuliaLang/julia/pull/16603)

* `writemime` has been merged into `show` [#16563](https://github.com/JuliaLang/julia/pull/16563). Note that to extend this function requires `@compat`; see the [Supported Syntax](#supported-syntax) section for more information.

## New macros

* `@static` has been added [#16219](https://github.com/JuliaLang/julia/pull/16219).

* `@functorize` (not present in any Julia version) takes a function (or operator) and turns it into a functor object if one is available in the used Julia version. E.g. something like `mapreduce(Base.AbsFun(), Base.MulFun(), x)` can now be written as `mapreduce(@functorize(abs), @functorize(*), x)`, and `f(::Base.AbsFun())` as `f(::typeof(@functorize(abs)))`, to work across different Julia versions. `Func{1}` can be written as `supertype(typeof(@functorize(abs)))` (and so on for `Func{2}`), which will fall back to `Function` on Julia 0.5.

* `Compat.@blasfunc` makes functionality of `Base.LinAlg.BLAS.@blasfunc` available on older Julia versions

* `@__DIR__` has been added [#18380](https://github.com/JuliaLang/julia/pull/18380)

* `@vectorize_1arg` and `@vectorize_2arg` are deprecated on Julia 0.6 in favor
  of the broadcast syntax [#17302](https://github.com/JuliaLang/julia/pull/17302).
  `Compat.@dep_vectorize_1arg` and `Compat.@dep_vectorize_2arg` are provided
  so that packages can still provide the deprecated definitions
  without causing a depwarn in the package itself before all the users
  are upgraded.

  Packages are expected to use this until all users of the deprecated
  vectorized function have migrated. These macros will be dropped when
  the support for `0.6` is dropped from `Compat`.

## Other changes

* `remotecall`, `remotecall_fetch`, `remotecall_wait`, and `remote_do` have the function to be executed remotely as the first argument in Julia 0.5. Loading `Compat` defines the same methods in older versions of Julia. [#13338](https://github.com/JuliaLang/julia/pull/13338)

* `Base.FS` is now `Base.Filesystem` [#12819](https://github.com/JuliaLang/julia/pull/12819).
  Compat provides an unexported `Compat.Filesystem` module that is aliased to
  `Base.FS` on Julia 0.4 and `Base.Filesystem` on Julia 0.5.

* `mktemp` and `mktempdir` now have variants which take a function as their first argument for automated cleanup. [#9017](https://github.com/JuliaLang/julia/pull/9017)

* `cov` and `cor` don't allow keyword arguments anymore. Loading Compat defines compatibility methods for the new API. [#13465](https://github.com/JuliaLang/julia/pull/13465)

* On versions of Julia that do not contain a Base.Threads module, Compat defines a Threads module containing a no-op `@threads` macro.

* `Base.SingleAsyncWork` is now `Base.AsyncCondition`
  Compat provides an unexported `Compat.AsyncCondition` type that is aliased to
  `Base.SingleAsyncWork` on Julia 0.4 and `Base.AsyncCondition` on Julia 0.5.

* `repeat` now accepts any `AbstractArray` [#14082](https://github.com/JuliaLang/julia/pull/14082): `Compat.repeat` supports this new API on Julia 0.4, and calls `Base.repeat` on 0.5.

* `OS_NAME` is now `Sys.KERNEL`. OS information available as `is_apple`, `is_bsd`, `is_linux`, `is_unix`, and `is_windows`. [16219](https://github.com/JuliaLang/julia/pull/16219)

* `cholfact`, `cholfact!`, and `chol` require that input is either `Hermitian`, `Symmetric`
or that the elements are perfectly symmetric or Hermitian on 0.5. Compat now defines methods
for `HermOrSym` such that using the new methods are backward compatible.

## New types

Currently, no new exported types are introduced by Compat.

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

[#15032]: https://github.com/JuliaLang/julia/issues/15032

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
