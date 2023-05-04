Julia v1.10 Release Notes
========================

New language features
---------------------

Language changes
----------------

* When a task forks a child, the parent task's task-local RNG (random number generator) is no longer affected. The seeding of child based on the parent task also takes a more disciplined approach to collision resistance, using a design based on the SplitMix and DotMix splittable RNG schemes ([#49110]).
* A new morespecific rule for methods resolves ambiguities containing Union{} in favor of
  the method defined explicitly to handle the Union{} argument. This makes it possible to
  define methods to explicitly handle Union{} without the ambiguities that commonly would
  result previously. This also lets the runtime optimize certain method lookups in a way
  that significantly improves load and inference times for heavily overloaded methods that
  dispatch on Types (such as traits and constructors).
* The "h bar" `ℏ` (`\hslash` U+210F) character is now treated as equivalent to `ħ` (`\hbar` U+0127).

Compiler/Runtime improvements
-----------------------------

* The `@pure` macro is now deprecated. Use `Base.@assume_effects :foldable` instead ([#48682]).
* The mark phase of the Garbage Collector is now multi-threaded ([#48600]).

Command-line option changes
---------------------------

* New option `--gcthreads` to set how many threads will be used by the Garbage Collector ([#48600]).
  The default is set to `N/2` where `N` is the amount of worker threads (`--threads`) used by Julia.

Multi-threading changes
-----------------------


Build system changes
--------------------


New library functions
---------------------
* `tanpi` is now defined. It computes tan(πx) more accurately than `tan(pi*x)` ([#48575]).
* `fourthroot(x)` is now defined in `Base.Math` and can be used to compute the fourth root of `x`.
   It can also be accessed using the unicode character `∜`, which can be typed by `\fourthroot<tab>` ([#48899]).

New library features
--------------------
* The `initialized=true` keyword assignment for `sortperm!` and `partialsortperm!`
  is now a no-op ([#47979]). It previously exposed unsafe behavior ([#47977]).
* `binomial(x, k)` now supports non-integer `x` ([#48124]).
* A `CartesianIndex` is now treated as a "scalar" for broadcasting ([#47044]).
* `printstyled` now supports italic output ([#45164]).

Standard library changes
------------------------

* `startswith` now supports seekable `IO` streams ([#43055])
* printing integral `Rational`s will skip the denominator in `Rational`-typed IO context (e.g. in `Arrays`) ([#45396])

#### Package Manager

* "Package Extensions": support for loading a piece of code based on other
  packages being loaded in the Julia session.
  This has similar applications as the Requires.jl package but also
  supports precompilation and setting compatibility.
* `Pkg.precompile` now accepts `timing` as a keyword argument which displays per package timing information for precompilation (e.g. `Pkg.precompile(timing=true)`)

#### LinearAlgebra

* `AbstractQ` no longer subtypes to `AbstractMatrix`. Moreover, `adjoint(Q::AbstractQ)`
  no longer wraps `Q` in an `Adjoint` type, but instead in an `AdjointQ`, that itself
  subtypes `AbstractQ`. This change accounts for the fact that typically `AbstractQ`
  instances behave like function-based, matrix-backed linear operators, and hence don't
  allow for efficient indexing. Also, many `AbstractQ` types can act on vectors/matrices
  of different size, acting like a matrix with context-dependent size. With this change,
  `AbstractQ` has a well-defined API that is described in detail in the
  [Julia documentation](https://docs.julialang.org/en/v1/stdlib/LinearAlgebra/#man-linalg-abstractq)
  ([#46196]).
* Adjoints and transposes of `Factorization` objects are no longer wrapped in `Adjoint`
  and `Transpose` wrappers, respectively. Instead, they are wrapped in
  `AdjointFactorization` and `TranposeFactorization` types, which themselves subtype
  `Factorization` ([#46874]).
* New functions `hermitianpart` and `hermitianpart!` for extracting the Hermitian
  (real symmetric) part of a matrix ([#31836]).
* The `norm` of the adjoint or transpose of an `AbstractMatrix` now returns the norm of the
  parent matrix by default, matching the current behaviour for `AbstractVector`s ([#49020]).

#### Printf
* Format specifiers now support dynamic width and precision, e.g. `%*s` and `%*.*g` ([#40105]).

#### Profile


#### Random


#### REPL


#### SuiteSparse


#### SparseArrays


#### Test


* The `@test_broken` macro (or `@test` with `broken=true`) now complains if the test expression returns a
  non-boolean value in the same way as a non-broken test. ([#47804])
* When a call to `@test` fails or errors inside a function, a larger stacktrace is now printed such that the location of the test within a `@testset` can be retrieved ([#49451])

#### Dates


#### Distributed


#### Unicode


#### DelimitedFiles


#### InteractiveUtils

 * `code_native` and `@code_native` now default to intel syntax instead of AT&T.
 * `@time_imports` now shows the timing of any module `__init__()`s that are run ([#49529])

Deprecated or removed
---------------------


External dependencies
---------------------


Tooling Improvements
--------------------


<!--- generated by NEWS-update.jl: -->
