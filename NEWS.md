Julia v1.11 Release Notes
========================

New language features
---------------------
* `public` is a new keyword. Symbols marked with `public` are considered public
  API. Symbols marked with `export` are now also treated as public API. The
  difference between `public` and `export` is that `public` names do not become
  available when `using` a package/module ([#50105]).
* `ScopedValue` implement dynamic scope with inheritance across tasks ([#50958]).
* The new macro `Base.Cartesian.@ncallkw` is analogous to `Base.Cartesian.@ncall`,
  but allows to add keyword arguments to the function call ([#51501]).
* Support for Unicode 15.1 ([#51799]).
* A new `AbstractString` type, `AnnotatedString`, is introduced that allows for
  regional annotations to be attached to an underlying string. This type is
  particularly useful for holding styling information, and is used extensively
  in the new `StyledStrings` standard library. There is also a new `AnnotatedChar`
  type, that is the equivalent new `AbstractChar` type.
* `Manifest.toml` files can now be renamed in the format `Manifest-v{major}.{minor}.toml`
  to be preferentially picked up by the given julia version. i.e. in the same folder,
  a `Manifest-v1.11.toml` would be used by v1.11 and `Manifest.toml` by every other julia
  version. This makes managing environments for multiple julia versions at the same time
  easier ([#43845]).

Language changes
----------------
* During precompilation, the `atexit` hooks now run before saving the output file. This
  allows users to safely tear down background state (such as closing Timers and sending
  disconnect notifications to heartbeat tasks) and cleanup other resources when the program
  wants to begin exiting.
* Code coverage and malloc tracking is no longer generated during the package precompilation stage.
  Further, during these modes pkgimage caches are now used for packages that are not being tracked.
  Meaning that coverage testing (the default for `julia-actions/julia-runtest`) will by default use
  pkgimage caches for all other packages than the package being tested, likely meaning faster test
  execution. ([#52123])

* Specifying a path in `JULIA_DEPOT_PATH` now results in the expansion of empty strings to
  omit the default user depot ([#51448]).

Compiler/Runtime improvements
-----------------------------
* Updated GC heuristics to count allocated pages instead of individual objects ([#50144]).
* A new `LazyLibrary` type is exported from `Libdl` for use in building chained lazy library
  loads, primarily to be used within JLLs ([#50074]).
* Added a support for annotating `Base.@assume_effects` on code block ([#52400]).

Command-line option changes
---------------------------

* The entry point for Julia has been standardized to `Main.main(ARGS)`. This must be explicitly opted into using the `@main` macro
(see the docstring for further details). When opted-in, and julia is invoked to run a script or expression
(i.e. using `julia script.jl` or `julia -e expr`), julia will subsequently run the `Main.main` function automatically.
This is intended to unify script and compilation workflows, where code loading may happen
in the compiler and execution of `Main.main` may happen in the resulting executable. For interactive use, there is no semantic
difference between defining a `main` function and executing the code directly at the end of the script ([50974]).
* The `--compiled-modules` and `--pkgimages` flags can now be set to `existing`, which will
  cause Julia to consider loading existing cache files, but not to create new ones ([#50586]
  and [#52573]).

Multi-threading changes
-----------------------

Build system changes
--------------------

New library functions
---------------------

* `in!(x, s::AbstractSet)` will return whether `x` is in `s`, and insert `x` in `s` if not.
* The new `Libc.mkfifo` function wraps the `mkfifo` C function on Unix platforms ([#34587]).
* `hardlink(src, dst)` can be used to create hard links ([#41639]).
* `diskstat(path=pwd())` can be used to return statistics about the disk ([#42248]).
* `copyuntil(out, io, delim)` and `copyline(out, io)` copy data into an `out::IO` stream ([#48273]).
* `eachrsplit(string, pattern)` iterates split substrings right to left.
* `Sys.username()` can be used to return the current user's username ([#51897]).
* `wrap(Array, m::Union{MemoryRef{T}, Memory{T}}, dims)` which is the safe counterpart to `unsafe_wrap` ([#52049]).

New library features
--------------------

* `invmod(n, T)` where `T` is a native integer type now computes the modular inverse of `n` in the modular integer ring that `T` defines ([#52180]).
* `invmod(n)` is an abbreviation for `invmod(n, typeof(n))` for native integer types ([#52180]).
* `replace(string, pattern...)` now supports an optional `IO` argument to
  write the output to a stream rather than returning a string ([#48625]).
* `sizehint!(s, n)` now supports an optional `shrink` argument to disable shrinking ([#51929]).
* New function `Docs.hasdoc(module, symbol)` tells whether a name has a docstring ([#52139]).
* New function `Docs.undocumented_names(module)` returns a module's undocumented public names ([#52413]).
* Passing an `IOBuffer` as a stdout argument for `Process` spawn now works as
  expected, synchronized with `wait` or `success`, so a `Base.BufferStream` is
  no longer required there for correctness to avoid data races ([#52461]).
* After a process exits, `closewrite` will no longer be automatically called on
  the stream passed to it. Call `wait` on the process instead to ensure the
  content is fully written, then call `closewrite` manually to avoid
  data-races. Or use the callback form of `open` to have all that handled
  automatically.

Standard library changes
------------------------

#### StyledStrings

* A new standard library for handling styling in a more comprehensive and structured way ([#49586]).
* The new `Faces` struct serves as a container for text styling information
  (think typeface, as well as color and decoration), and comes with a framework
  to provide a convenient, extensible (via `addface!`), and customisable (with a
  user's `Faces.toml` and `loadfaces!`) approach to
  styled content ([#49586]).
* The new `@styled_str` string macro provides a convenient way of creating a
  `AnnotatedString` with various faces or other attributes applied ([#49586]).

#### Package Manager

#### LinearAlgebra
* `cbrt(::AbstractMatrix{<:Real})` is now defined and returns real-valued matrix cube roots of real-valued matrices ([#50661]).
* `eigvals/eigen(A, bunchkaufman(B))` and `eigvals/eigen(A, lu(B))`, which utilize the Bunchkaufman (LDL) and LU decomposition of `B`,
   respectively, now efficiently compute the generalized eigenvalues (`eigen`: and eigenvectors) of `A` and `B`. Note: The second
   argument is the output of `bunchkaufman` or `lu` ([#50471]).
* There is now a specialized dispatch for `eigvals/eigen(::Hermitian{<:Tridiagonal})` which performs a similarity transformation to create a real symmetrix triagonal matrix, and solve that using the LAPACK routines ([#49546]).
* Structured matrices now retain either the axes of the parent (for `Symmetric`/`Hermitian`/`AbstractTriangular`/`UpperHessenberg`), or that of the principal diagonal (for banded matrices) ([#52480]).
* `bunchkaufman` and `bunchkaufman!` now work for any `AbstractFloat`, `Rational` and their complex variants. `bunchkaufman` now supports `Integer` types, by making an internal conversion to `Rational{BigInt}`. Added new function `inertia` that computes the inertia of the diagonal factor given by the `BunchKaufman` factorization object of a real symmetric or Hermitian matrix. For complex symmetric matrices, `inertia` only computes the number of zero eigenvalues of the diagonal factor ([#51487]).

#### Logging
* New `@create_log_macro` macro for creating new log macros like `@info`, `@warn` etc. For instance
  `@create_log_macro MyLog 1500 :magenta` will create `@mylog` to be used like `@mylog "hello"` which
  will show as `┌ MyLog: hello` etc. ([#52196])

#### Printf

#### Profile

#### Random
* `rand` now supports sampling over `Tuple` types ([#35856], [#50251]).
* `rand` now supports sampling over `Pair` types ([#28705]).
* When seeding RNGs provided by `Random`, negative integer seeds can now be used ([#51416]).
* Seedable random number generators from `Random` can now be seeded by a string, e.g.
  `seed!(rng, "a random seed")` ([#51527]).

#### REPL

* Tab complete hints now show in lighter text while typing in the repl. To disable
  set `Base.active_repl.options.hint_tab_completes = false` ([#51229]).
* Meta-M with an empty prompt now toggles the contextual module between the previous non-Main
  contextual module and Main so that switching back and forth is simple. ([#51616], [#52670])

#### SuiteSparse


#### SparseArrays

#### Test

#### Dates

#### Statistics

* Statistics is now an upgradeable standard library ([#46501]).

#### Distributed

* `pmap` now defaults to using a `CachingPool` ([#33892]).

#### Unicode


#### DelimitedFiles


#### InteractiveUtils

Deprecated or removed
---------------------


External dependencies
---------------------
* `tput` is no longer called to check terminal capabilities, it has been replaced with a pure-Julia terminfo parser ([#50797]).

Tooling Improvements
--------------------

* CI now performs limited automatic typo detection on all PRs. If you merge a PR with a
  failing typo CI check, then the reported typos will be automatically ignored in future CI
  runs on PRs that edit those same files ([#51704]).

<!--- generated by NEWS-update.jl: -->
