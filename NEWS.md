Julia v1.2 Release Notes
========================

New language features
---------------------

  * Argument splatting (`x...`) can now be used in calls to the `new` pseudo-function in
    constructors ([#30577]).

  * Objects created by calling `skipmissing` on an array can now be indexed using indices
    from the parent at non-missing positions. This allows functions such as
    `findall`, `findfirst`, `argmin`/`argmax` and `findmin`/`findmax` to work with these
    objects, returning the index of matching non-missing elements in the parent ([#31008]).

  * `inv(::Missing)` has now been added and returns `missing` ([#31451]).

  * `nextfloat(::BigFloat, n::Integer)` and `prevfloat(::BigFloat, n::Integer)` methods
    have been added ([#31310]).

  * Support for Unicode 12.0.0 ([#31561]).

Multi-threading changes
-----------------------

* The `Condition` type now has a thread-safe replacement, accessed as `Threads.Condition`.
    With that addition, task scheduling primitives such as `ReentrantLock` are now thread-safe ([#30061]).

  * It is possible to schedule and switch Tasks during `@threads` loops, and perform limited I/O ([#31438]).

Language changes
----------------
* Empty entries in `JULIA_DEPOT_PATH` are now expanded to default depot entries ([#31009]).
* `Enum` now behaves like a scalar when used in broadcasting ([#30670]).
* If a `pipeline` is specified with `append=true` set, but no redirection, an `ArgumentError`
is thrown, rather than a `ErrorException` ([#27900]).
* Functions that invoke commands (e.g. `run(::Cmd)`) now throw a `ProcessFailedException`
rather than an `ErrorException`, if those commands exit with non-zero exit code.
([#27900]).

Command-line option changes
---------------------------


New library functions
---------------------

* `getipaddrs()` function returns all the IP addresses of the local machine, with IPv4 addresses sorting before IPv6 addresses ([#30349, #30604])
* `getipaddr(addr_type)` and `getipaddrs(addr_type)` functions returns an IP address(es) of the desired type of the local machine ([#30604])
* Added `Base.hasproperty` and `Base.hasfield` ([#28850]).
* One argument `!=(x)`, `>(x)`, `>=(x)`, `<(x)`, `<=(x)` has been added for currying,
  similar to the existing `==(x)` and `isequal(x)` methods ([#30915]).

Standard library changes
------------------------

* The `extrema` function now accepts a function argument in the same manner as `minimum` and
  `maximum` ([#30323]).
* `hasmethod` can now check for matching keyword argument names ([#30712]).
* `startswith` and `endswith` now accept a `Regex` for the second argument ([#29790]).
* `retry` supports arbitrary callable objects ([#30382]).
* `filter` now supports `SkipMissing`-wrapped arrays ([#31235]).
* A no-argument construct to `Ptr{T}` has been added which constructs a null pointer ([#30919])
* `strip` now accepts a function argument in the same manner as `lstrip` and `rstrip` ([#31211])
* `mktempdir` now accepts a `prefix` keyword argument to customize the file name ([#31230], [#22922])
* `keytype` and `valtype` now work on `AbstractArray`, and return the `eltype` of `keys(...)` and
  `values(...)` respectively ([#27749]).
* `nextfloat(::BigFloat)` and `prevfloat(::BigFloat)` now returns a value with the same precision
  as their argument, which means that (in particular) `nextfloat(prevfloat(x)) == x` whereas
  previously this could result in a completely different value with a different precision ([#31310])
* `mapreduce` now accept multiple iterators, similar to `map` ([#31532]).

#### LinearAlgebra

* Added keyword arguments `rtol`, `atol` to `pinv` and `nullspace` ([#29998]).
* `UniformScaling` instances are now callable such that e.g. `I(3)` will produce a `Diagonal` matrix ([#30298]).
* Eigenvalues λ of general matrices are now sorted lexicographically by (Re λ, Im λ) ([#21598]).
* `one` for structured matrices (`Diagonal`, `Bidiagonal`, `Tridiagonal`, `Symtridiagonal`) now preserves
  structure and type. ([#29777])
* `diagm(v)` is now a shorthand for `diagm(0 => v)`. ([#31125]).

#### SparseArrays

* performance improvements for sparse matrix-matrix multiplication ([#30372]).
* Sparse vector outer products are more performant and maintain sparsity in products of the
  form `kron(u, v')`, `u * v'`, and `u .* v'` where `u` and `v` are sparse vectors or column
  views. ([#24980])

#### Dates

* Fixed `repr` such that it displays `DateTime` as it would be entered in Julia ([#30200]).

#### Statistics

* `quantile` now accepts in all cases collections whose `eltype` is not a subtype of `Number` ([#30938]).

#### Miscellaneous

* Since environment variables on Windows are case-insensitive, `ENV` now converts its keys
  to uppercase for display, iteration, and copying ([#30593]).

* Build system now prefers downloading prebuilt binary tarballs for most dependencies on
  supported systems, disable by setting `USE_BINARYBUILDER=0` at `make` time ([#31441]).

External dependencies
---------------------

* libgit2 has been updated to v0.27.7 ([#30584]).
* OpenBLAS has been updated to v0.3.5 ([#30583]).
* MbedTLS has been updated to v2.16.0 ([#30618]).
* libunwind has been updated to v1.3.1 ([#30724]).

Deprecated or removed
---------------------


<!--- generated by NEWS-update.jl: -->
[#21598]: https://github.com/JuliaLang/julia/issues/21598
[#22922]: https://github.com/JuliaLang/julia/issues/22922
[#24980]: https://github.com/JuliaLang/julia/issues/24980
[#28850]: https://github.com/JuliaLang/julia/issues/28850
[#29777]: https://github.com/JuliaLang/julia/issues/29777
[#29790]: https://github.com/JuliaLang/julia/issues/29790
[#29998]: https://github.com/JuliaLang/julia/issues/29998
[#30061]: https://github.com/JuliaLang/julia/issues/30061
[#30200]: https://github.com/JuliaLang/julia/issues/30200
[#30298]: https://github.com/JuliaLang/julia/issues/30298
[#30323]: https://github.com/JuliaLang/julia/issues/30323
[#30372]: https://github.com/JuliaLang/julia/issues/30372
[#30382]: https://github.com/JuliaLang/julia/issues/30382
[#30577]: https://github.com/JuliaLang/julia/issues/30577
[#30583]: https://github.com/JuliaLang/julia/issues/30583
[#30584]: https://github.com/JuliaLang/julia/issues/30584
[#30593]: https://github.com/JuliaLang/julia/issues/30593
[#30604]: https://github.com/JuliaLang/julia/issues/30604
[#30618]: https://github.com/JuliaLang/julia/issues/30618
[#30670]: https://github.com/JuliaLang/julia/issues/30670
[#30712]: https://github.com/JuliaLang/julia/issues/30712
[#30724]: https://github.com/JuliaLang/julia/issues/30724
[#30915]: https://github.com/JuliaLang/julia/issues/30915
[#30919]: https://github.com/JuliaLang/julia/issues/30919
[#30938]: https://github.com/JuliaLang/julia/issues/30938
[#31008]: https://github.com/JuliaLang/julia/issues/31008
[#31009]: https://github.com/JuliaLang/julia/issues/31009
[#31125]: https://github.com/JuliaLang/julia/issues/31125
[#31211]: https://github.com/JuliaLang/julia/issues/31211
[#31230]: https://github.com/JuliaLang/julia/issues/31230
[#31235]: https://github.com/JuliaLang/julia/issues/31235
[#31310]: https://github.com/JuliaLang/julia/issues/31310
[#31441]: https://github.com/JuliaLang/julia/issues/31441
[#31451]: https://github.com/JuliaLang/julia/issues/31451
[#31532]: https://github.com/JuliaLang/julia/issues/31532
