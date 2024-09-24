Julia v1.11 Release Notes
=========================

New language features
---------------------

* New `Memory` type that provides a lower-level container as an alternative to `Array`.
  `Memory` has less overhead and a faster constructor, making it a good choice for situations
  that do not need all the features of `Array` (e.g. multiple dimensions).
  Most of the `Array` type is now implemented in Julia on top of `Memory`, leading to
  significant speedups for several functions (e.g. `push!`) as well as more maintainable code ([#51319]).
* `public` is a new keyword. Symbols marked with `public` are considered public
  API. Symbols marked with `export` are now also treated as public API. The
  difference between `public` and `export` is that `public` names do not become
  available when `using` a package/module ([#50105]).
* `ScopedValue` implements dynamic scope with inheritance across tasks ([#50958]).
* `Manifest.toml` files can now be renamed in the format `Manifest-v{major}.{minor}.toml`
  to be preferentially picked up by the given julia version. i.e. in the same folder,
  a `Manifest-v1.11.toml` would be used by v1.11 and `Manifest.toml` by every other julia
  version. This makes managing environments for multiple julia versions at the same time
  easier ([#43845]).
* Support for Unicode 15.1 ([#51799]).

Language changes
----------------

* During precompilation, `atexit` hooks now run before saving the output file. This
  allows users to safely tear down background state (such as closing `Timer`s and sending
  disconnect notifications to heartbeat tasks) and cleanup other resources when the program
  wants to begin exiting.
* Code coverage and malloc tracking is no longer generated during the package precompilation stage.
  Further, during these modes pkgimage caches are now used for packages that are not being tracked.
  This means that coverage testing (the default for `julia-actions/julia-runtest`) will by default use
  pkgimage caches for all other packages than the package being tested, likely meaning faster test
  execution ([#52123]).
* Specifying a path in `JULIA_DEPOT_PATH` now results in the expansion of empty strings to
  omit the default user depot ([#51448]).
* Precompilation cache files are now relocatable and their validity is now verified through
  a content hash of their source files instead of their `mtime` ([#49866]).

Compiler/Runtime improvements
-----------------------------

* Updated GC heuristics to count allocated pages instead of individual objects ([#50144]).
* Added support for annotating `Base.@assume_effects` on code blocks ([#52400]).

Command-line option changes
---------------------------

* The entry point for Julia has been standardized to `Main.main(args)`. This must be explicitly opted into using the `@main` macro
  (see the docstring for further details). When opted-in, and `julia` is invoked to run a script or expression
  (i.e. using `julia script.jl` or `julia -e expr`), `julia` will subsequently run the `Main.main` function automatically.
  This is intended to unify script and compilation workflows, where code loading may happen
  in the compiler and execution of `Main.main` may happen in the resulting executable. For interactive use, there is no semantic
  difference between defining a `main` function and executing the code directly at the end of the script ([#50974]).
* The `--compiled-modules` and `--pkgimages` flags can now be set to `existing`, which will
  cause Julia to consider loading existing cache files, but not to create new ones ([#50586], [#52573]).
* The `--project` argument now accepts `@script` to give a path to a directory with a Project.toml relative to the passed script file. `--project=@script/foo` for the `foo` subdirectory. If no path is given after (i.e. `--project=@script`) then (like `--project=@.`) the directory and its parents are searched for a Project.toml ([#50864] and [#53352])

Multi-threading changes
-----------------------

* `Threads.@threads` now supports the `:greedy` scheduler, intended for non-uniform workloads ([#52096]).
* A new public (but unexported) struct `Base.Lockable{T, L<:AbstractLock}` makes it easy to bundle a resource and its lock together ([#52898]).

Build system changes
--------------------

* There is a new `Makefile` to build Julia and LLVM using the profile-guided and link-time optimizations (PGO and LTO) strategies, see `contrib/pgo-lto/Makefile` ([#45641]).

New library functions
---------------------

* Three new types around the idea of text with "annotations" (`Pair{Symbol, Any}`
  entries, e.g. `:lang => "en"` or `:face => :magenta`). These annotations
  are preserved across operations (e.g. string concatenation with `*`) when
  possible.
  * `AnnotatedString` is a new `AbstractString` type. It wraps an underlying
    string and allows for annotations to be attached to regions of the string.
    This type is used extensively in the new `StyledStrings` standard library to
    hold styling information.
  * `AnnotatedChar` is a new `AbstractChar` type. It wraps another char and
    holds a list of annotations that apply to it.
  * `AnnotatedIOBuffer` is a new `IO` type that mimics an `IOBuffer`, but has
    specialised `read`/`write` methods for annotated content. This can be
    thought of both as a "string builder" of sorts and also as glue between
    annotated and unannotated content.
* `in!(x, s::AbstractSet)` will return whether `x` is in `s`, and insert `x` in `s` if not ([#45156], [#51636]).
* The new `Libc.mkfifo` function wraps the `mkfifo` C function on Unix platforms ([#34587]).
* `logrange(start, stop; length)` makes a range of constant ratio, instead of constant step ([#39071])
* `copyuntil(out, io, delim)` and `copyline(out, io)` copy data into an `out::IO` stream ([#48273]).
* `eachrsplit(string, pattern)` iterates split substrings right to left ([#51646]).
* `Sys.username()` can be used to return the current user's username ([#51897]).
* `Sys.isreadable(), Sys.iswritable()` can be used to check if the current user has access permissions
  that permit reading and writing, respectively. ([#53320]).
* `GC.logging_enabled()` can be used to test whether GC logging has been enabled via `GC.enable_logging` ([#51647]).
* `IdSet` is now exported from Base and considered public ([#53262]).
* `@time` now reports a count of any lock conflicts where a `ReentrantLock` had to wait, plus a new macro
  `@lock_conflicts` which returns that count ([#52883]).
* The new macro `Base.Cartesian.@ncallkw` is analogous to `Base.Cartesian.@ncall`,
  but allows adding keyword arguments to the function call ([#51501]).
* New function `Docs.hasdoc(module, symbol)` tells whether a name has a docstring ([#52139]).
* New function `Docs.undocumented_names(module)` returns a module's undocumented public names ([#52413]).

New library features
--------------------

* `invmod(n, T)` where `T` is a native integer type now computes the modular inverse of `n` in the modular integer ring that `T` defines ([#52180]).
* `invmod(n)` is an abbreviation for `invmod(n, typeof(n))` for native integer types ([#52180]).
* `replace(string, pattern...)` now supports an optional `IO` argument to
  write the output to a stream rather than returning a string ([#48625]).
* New methods `allequal(f, itr)` and `allunique(f, itr)` taking a predicate function ([#47679]).
* `sizehint!(s, n)` now supports an optional `shrink` argument to disable shrinking ([#51929]).
* Passing an `IOBuffer` as a stdout argument for `Process` spawn now works as
  expected, synchronized with `wait` or `success`, so a `Base.BufferStream` is
  no longer required there for correctness to avoid data races ([#52461]).
* After a process exits, `closewrite` will no longer be automatically called on
  the stream passed to it. Call `wait` on the process instead to ensure the
  content is fully written, then call `closewrite` manually to avoid
  data races, or use the callback form of `open` to have all that handled
  automatically ([#52461]).
* `@timed` now additionally returns the elapsed compilation and recompilation time ([#52889]).
* `filter` can now act on a `NamedTuple` ([#50795]).
* `Iterators.cycle(iter, n)` runs over `iter` a fixed number of times, instead of forever ([#47354]).
* `zero(::AbstractArray)` now applies recursively, so `zero([[1,2],[3,4,5]])` now produces the additive identity `[[0,0],[0,0,0]]` rather than erroring ([#38064]).
* `include_dependency(path; track_content=true)` allows switching from using `mtime`
  to hashing of the precompilation dependency in order to restore relocatability of precompilation caches ([#51798]).

Standard library changes
------------------------

* The fallback method `write(::IO, ::AbstractArray)` used to recursively call `write` on each element,
  but now writes the in-memory representation of each value. For example, `write(io, 'a':'b')` now
  writes 4 bytes for each character, instead of writing the UTF-8 representation of each character.
  The new format is compatible with that used by `Array`, making it possible to use `read!` to get
  the data back ([#42593]).
* It's not possible to define `length` for stateful iterators in a generally consistent manner. The
  potential for silently incorrect results for `Stateful` iterators is addressed by deleting the
  `length(::Stateful)` method. The last type parameter of `Stateful` is gone, too. Issue: ([#47790]),
  PR: ([#51747]).

#### StyledStrings

* A new standard library for handling styling in a more comprehensive and structured way ([#49586]).
* The new `Faces` struct serves as a container for text styling information
  (think typeface, as well as color and decoration), and comes with a framework
  to provide a convenient, extensible (via `addface!`), and customisable (with a
  user's `Faces.toml` and `loadfaces!`) approach to
  styled content ([#49586]).
* The new `@styled_str` string macro provides a convenient way of creating a
  `AnnotatedString` with various faces or other attributes applied ([#49586]).

#### Libdl

* A new `LazyLibrary` type is exported from `Libdl` for use in building chained lazy library
  loads, primarily to be used within JLLs ([#50074]).

#### LinearAlgebra

* `cbrt(::AbstractMatrix{<:Real})` is now defined and returns real-valued matrix cube roots of real-valued matrices ([#50661]).
* `eigvals/eigen(A, bunchkaufman(B))` and `eigvals/eigen(A, lu(B))`, which utilize the Bunchkaufman (LDL) and LU decomposition of `B`,
   respectively, now efficiently compute the generalized eigenvalues (`eigen`: and eigenvectors) of `A` and `B`. Note: The second
   argument is the output of `bunchkaufman` or `lu` ([#50471]).
* There is now a specialized dispatch for `eigvals/eigen(::Hermitian{<:Tridiagonal})` which performs a similarity transformation to create a real symmetric tridiagonal matrix, and solve that using the LAPACK routines ([#49546]).
* Structured matrices now retain either the axes of the parent (for `Symmetric`/`Hermitian`/`AbstractTriangular`/`UpperHessenberg`), or that of the principal diagonal (for banded matrices) ([#52480]).
* `bunchkaufman` and `bunchkaufman!` now work for any `AbstractFloat`, `Rational` and their complex variants. `bunchkaufman` now supports `Integer` types, by making an internal conversion to `Rational{BigInt}`. Added new function `inertia` that computes the inertia of the diagonal factor given by the `BunchKaufman` factorization object of a real symmetric or Hermitian matrix. For complex symmetric matrices, `inertia` only computes the number of zero eigenvalues of the diagonal factor ([#51487]).
* Packages that specialize matrix-matrix `mul!` with a method signature of the form `mul!(::AbstractMatrix, ::MyMatrix, ::AbstractMatrix, ::Number, ::Number)` no longer encounter method ambiguities when interacting with `LinearAlgebra`. Previously, ambiguities used to arise when multiplying a `MyMatrix` with a structured matrix type provided by LinearAlgebra, such as `AbstractTriangular`, which used to necessitate additional methods to resolve such ambiguities. Similar sources of ambiguities have also been removed for matrix-vector `mul!` operations ([#52837]).
* `lu` and `issuccess(::LU)` now accept an `allowsingular` keyword argument. When set to `true`, a valid factorization with rank-deficient U factor will be treated as success instead of throwing an error. Such factorizations are now shown by printing the factors together with a "rank-deficient" note rather than printing a "Failed Factorization" message ([#52957]).

#### Random

* `rand` now supports sampling over `Tuple` types ([#35856], [#50251]).
* `rand` now supports sampling over `Pair` types ([#28705]).
* When seeding RNGs provided by `Random`, negative integer seeds can now be used ([#51416]).
* Seedable random number generators from `Random` can now be seeded by a string, e.g.
  `seed!(rng, "a random seed")` ([#51527]).

#### REPL

* Tab complete hints now show in lighter text while typing in the repl. To disable
  set `Base.active_repl.options.hint_tab_completes = false` interactively, or in startup.jl:
  ```
  if VERSION >= v"1.11.0-0"
    atreplinit() do repl
        repl.options.hint_tab_completes = false
    end
  end
  ``` ([#51229]).
* Meta-M with an empty prompt now toggles the contextual module between the previous non-Main
  contextual module and Main so that switching back and forth is simple ([#51616], [#52670]).

#### Dates

The undocumented function `adjust` is no longer exported but is now documented ([#53092]).

#### Statistics

* Statistics is now an upgradeable standard library ([#46501]).

#### Distributed

* `pmap` now defaults to using a `CachingPool` ([#33892]).

Deprecated or removed
---------------------

* `Base.map`, `Iterators.map`, and `foreach` lost their single-argument methods ([#52631]).

External dependencies
---------------------

* The libuv library has been updated from a base of v1.44.2 to v1.48.0 ([#49937]).
* `tput` is no longer called to check terminal capabilities; it has been replaced with a pure-Julia terminfo parser ([#50797]).
- The terminal info database, `terminfo`, is now vendored by default, providing a better
  REPL user experience when `terminfo` is not available on the system. Julia can be built
  without vendoring the database using the Makefile option `WITH_TERMINFO=0`. ([#55411])

Tooling Improvements
--------------------

* CI now performs limited automatic typo detection on all PRs. If you merge a PR with a
  failing typo CI check, then the reported typos will be automatically ignored in future CI
  runs on PRs that edit those same files ([#51704]).

<!--- generated by NEWS-update.jl: -->
[#28705]: https://github.com/JuliaLang/julia/issues/28705
[#33892]: https://github.com/JuliaLang/julia/issues/33892
[#34587]: https://github.com/JuliaLang/julia/issues/34587
[#35856]: https://github.com/JuliaLang/julia/issues/35856
[#38064]: https://github.com/JuliaLang/julia/issues/38064
[#39071]: https://github.com/JuliaLang/julia/issues/39071
[#42593]: https://github.com/JuliaLang/julia/issues/42593
[#43845]: https://github.com/JuliaLang/julia/issues/43845
[#45156]: https://github.com/JuliaLang/julia/issues/45156
[#45641]: https://github.com/JuliaLang/julia/issues/45641
[#46501]: https://github.com/JuliaLang/julia/issues/46501
[#47354]: https://github.com/JuliaLang/julia/issues/47354
[#47679]: https://github.com/JuliaLang/julia/issues/47679
[#48273]: https://github.com/JuliaLang/julia/issues/48273
[#48625]: https://github.com/JuliaLang/julia/issues/48625
[#49546]: https://github.com/JuliaLang/julia/issues/49546
[#49586]: https://github.com/JuliaLang/julia/issues/49586
[#49866]: https://github.com/JuliaLang/julia/issues/49866
[#49937]: https://github.com/JuliaLang/julia/issues/49937
[#50074]: https://github.com/JuliaLang/julia/issues/50074
[#50105]: https://github.com/JuliaLang/julia/issues/50105
[#50144]: https://github.com/JuliaLang/julia/issues/50144
[#50251]: https://github.com/JuliaLang/julia/issues/50251
[#50471]: https://github.com/JuliaLang/julia/issues/50471
[#50586]: https://github.com/JuliaLang/julia/issues/50586
[#50661]: https://github.com/JuliaLang/julia/issues/50661
[#50795]: https://github.com/JuliaLang/julia/issues/50795
[#50797]: https://github.com/JuliaLang/julia/issues/50797
[#50958]: https://github.com/JuliaLang/julia/issues/50958
[#50974]: https://github.com/JuliaLang/julia/issues/50974
[#51229]: https://github.com/JuliaLang/julia/issues/51229
[#51319]: https://github.com/JuliaLang/julia/issues/51319
[#51416]: https://github.com/JuliaLang/julia/issues/51416
[#51448]: https://github.com/JuliaLang/julia/issues/51448
[#51487]: https://github.com/JuliaLang/julia/issues/51487
[#51501]: https://github.com/JuliaLang/julia/issues/51501
[#51527]: https://github.com/JuliaLang/julia/issues/51527
[#51616]: https://github.com/JuliaLang/julia/issues/51616
[#51636]: https://github.com/JuliaLang/julia/issues/51636
[#51646]: https://github.com/JuliaLang/julia/issues/51646
[#51647]: https://github.com/JuliaLang/julia/issues/51647
[#51704]: https://github.com/JuliaLang/julia/issues/51704
[#51798]: https://github.com/JuliaLang/julia/issues/51798
[#51799]: https://github.com/JuliaLang/julia/issues/51799
[#51897]: https://github.com/JuliaLang/julia/issues/51897
[#51929]: https://github.com/JuliaLang/julia/issues/51929
[#52049]: https://github.com/JuliaLang/julia/issues/52049
[#52096]: https://github.com/JuliaLang/julia/issues/52096
[#52123]: https://github.com/JuliaLang/julia/issues/52123
[#52139]: https://github.com/JuliaLang/julia/issues/52139
[#52180]: https://github.com/JuliaLang/julia/issues/52180
[#52196]: https://github.com/JuliaLang/julia/issues/52196
[#52400]: https://github.com/JuliaLang/julia/issues/52400
[#52413]: https://github.com/JuliaLang/julia/issues/52413
[#52461]: https://github.com/JuliaLang/julia/issues/52461
[#52480]: https://github.com/JuliaLang/julia/issues/52480
[#52573]: https://github.com/JuliaLang/julia/issues/52573
[#52631]: https://github.com/JuliaLang/julia/issues/52631
[#52670]: https://github.com/JuliaLang/julia/issues/52670
[#52837]: https://github.com/JuliaLang/julia/issues/52837
[#52883]: https://github.com/JuliaLang/julia/issues/52883
[#52889]: https://github.com/JuliaLang/julia/issues/52889
[#52898]: https://github.com/JuliaLang/julia/issues/52898
[#52957]: https://github.com/JuliaLang/julia/issues/52957
[#53092]: https://github.com/JuliaLang/julia/issues/53092
[#53262]: https://github.com/JuliaLang/julia/issues/53262
[#53320]: https://github.com/JuliaLang/julia/issues/53320
