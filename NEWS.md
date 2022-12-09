Julia v1.8 Release Notes
========================

New language features
---------------------

* Mutable struct fields may now be annotated as `const` to prevent changing them after construction,
  providing for greater clarity and optimization ability of these objects ([#43305]).
* Type annotations can now be added to global variables to make accessing them type stable ([#43671]).
* Empty n-dimensional arrays can now be created using multiple semicolons inside square brackets,
  e.g. `[;;;]` creates a 0×0×0 `Array` ([#41618]).
* `try`-blocks can now optionally have an `else`-block which is executed right after the main body only if
  no errors were thrown ([#42211]).
* `@inline` and `@noinline` can now be placed within a function body, allowing one to annotate anonymous function ([#41312]).
* `@inline` and `@noinline` can now be applied to a function at callsite or block
  to enforce the involved function calls to be (or not to be) inlined ([#41328]).
* `∀`, `∃`, and `∄` are now allowed as identifier characters ([#42314]).
* Support for Unicode 14.0.0 ([#43443]).
* `Module(:name, false, false)` can be used to create a `module` that contains no names
  (it does not import `Base` or `Core` and does not contain a reference to itself) ([#40110], [#42154]).

Language changes
----------------

* Newly-created Task objects (`@spawn`, `@async`, etc.) now adopt the world age for methods from their parent
  Task upon creation, instead of using the global latest world at start. This is done to enable inference to
  eventually optimize these calls. Places that wish for the old behavior may use `Base.invokelatest` ([#41449]).
* Unbalanced Unicode bidirectional formatting directives are now disallowed within strings and comments,
  to mitigate the ["trojan source"](https://www.trojansource.codes) vulnerability ([#42918]).
* `Base.ifelse` is now defined as a generic function rather than a builtin one, allowing packages to
  extend its definition ([#37343]).
* Every assignment to a global variable now first goes through a call to `convert(Any, x)` (or `convert(T, x)`
  respectively if a type `T` has been declared for the global). This means great care should be taken
  to ensure the invariant `convert(Any, x) === x` always holds, as this change could otherwise lead to
  unexpected behavior ([#43671]).
* Builtin functions are now a bit more like generic functions, and can be enumerated with `methods` ([#43865]).

Compiler/Runtime improvements
-----------------------------

* Bootstrapping time has been improved by about 25% ([#41794]).
* The LLVM-based compiler has been separated from the run-time library into a new library,
  `libjulia-codegen`. It is loaded by default, so normal usage should see no changes.
  In deployments that do not need the compiler (e.g. system images where all needed code
  is precompiled), this library (and its LLVM dependency) can simply be excluded ([#41936]).
* Conditional type constraints are now be forwarded interprocedurally (i.e. propagated from caller to callee).
  This allows inference to understand e.g. `Base.ifelse(isa(x, Int), x, 0)` returns `::Int`-value
  even if the type of `x` is not known ([#42529]).
* Julia-level SROA (Scalar Replacement of Aggregates) has been improved: allowing elimination of
  `getfield` calls with constant global fields ([#42355]), enabling elimination of mutable structs with
  uninitialized fields ([#43208]), improving performance ([#43232]), and handling more nested `getfield`
  calls ([#43239]).
* Abstract call sites can now be inlined or statically resolved as long as the call site has a single
  matching method ([#43113]).
* Inference now tracks various effects such as side-effectful-ness and nothrow-ness on a per-specialization basis.
  Code heavily dependent on constant propagation should see significant compile-time performance improvements and
  certain cases (e.g. calls to uninlinable functions that are nevertheless effect free) should see runtime performance
  improvements. Effects may be overwritten manually with the `Base.@assume_effects` macro ([#43852]).
* Precompilation (with explicit `precompile` directives or representative workloads) now saves more type-inferred code,
  resulting in reduced time-to-first task for packages that use precompilation.  This change also eliminates the
  runtime performance degradation occasionally triggered by precompilation on older Julia versions. More specifically,
  any newly-inferred method/type combinations needed by your package--regardless of where those methods were
  defined--can now be cached in the precompile file, as long as they are inferrably called by a method owned by
  your package ([#43990]).

Command-line option changes
---------------------------

* The default behavior of observing `@inbounds` declarations is now an option via `auto` in `--check-bounds=yes|no|auto` ([#41551]).
* New option `--strip-metadata` to remove docstrings, source location information, and local
  variable names when building a system image ([#42513]).
* New option `--strip-ir` to remove the compiler's IR (intermediate representation) of source
  code when building a system image. The resulting image will only work if `--compile=all` is
  used, or if all needed code is precompiled ([#42925]).
* When the program file is `-` the code to be executed is read from standard in ([#43191]).
* In Linux and Windows, `--threads=auto` now tries to infer usable number of CPUs from the
  process affinity which is set typically in HPC and cloud environments ([#42340]).

Multi-threading changes
-----------------------

* `Threads.@threads` now defaults to a new `:dynamic` schedule option which is similar to the previous behavior except
  that iterations will be scheduled dynamically to available worker threads rather than pinned to each thread. This
  behavior is more composable with (possibly nested) `@spawn` and `@threads` loops ([#43919], [#44136]).

Build system changes
--------------------


New library functions
---------------------

* New function `eachsplit(str)` for iteratively performing `split(str)` ([#39245]).
* New function `allequal(itr)` for testing if all elements in an iterator are equal ([#43354]).
* `hardlink(src, dst)` can be used to create hard links ([#41639]).
* `setcpuaffinity(cmd, cpus)` can be used to set CPU affinity of sub-processes ([#42469]).
* `diskstat(path=pwd())` can be used to return statistics about the disk ([#42248]).
* New `@showtime` macro to show both the line being evaluated and the `@time` report ([#42431]).
* The `LazyString` and the `lazy"str"` macro were added to support delayed construction of error messages in error paths ([#33711]).

New library features
--------------------

* A known concurrency issue of `iterate` methods on `Dict` and other derived objects such
  as `keys(::Dict)`, `values(::Dict)`, and `Set` is fixed.  These methods of `iterate` can
  now be called on a dictionary or set shared by arbitrary tasks provided that there are no
  tasks mutating the dictionary or set ([#44534]).
* `@time` and `@timev` now take an optional description to allow annotating the source of time reports,
  e.g. `@time "Evaluating foo" foo()` ([#42431]).
* `range` accepts either `stop` or `length` as a sole keyword argument ([#39241]).
* `precision` and `setprecision` now accept a `base` keyword argument ([#42428]).
* TCP socket objects now expose `closewrite` functionality and support half-open mode usage ([#40783]).
* `extrema` now accepts an `init` keyword argument ([#36265], [#43604]).
* `Iterators.countfrom` now accepts any type that defines `+` ([#37747]).
* `@time` now separates out % time spent recompiling invalidated methods ([#45015]).

Standard library changes
------------------------

* Keys with value `nothing` are now removed from the environment in `addenv` ([#43271]).
* `Iterators.reverse` (and hence `last`) now supports `eachline` iterators ([#42225]).
* The `length` function on certain ranges of certain element types no longer checks for integer
  overflow in most cases. The new function `checked_length` is now available, which will try to use checked
  arithmetic to error if the result may be wrapping. Or use a package such as SaferIntegers.jl when
  constructing the range ([#40382]).
* Intersect returns a result with the eltype of the type-promoted eltypes of the two inputs ([#41769]).
* Iterating an `Iterators.Reverse` now falls back on reversing the eachindex iterator, if possible ([#43110]).

#### Package Manager

* New `⌃` and `⌅` indicators beside packages in `pkg> status` that have new versions available.
  `⌅` indicates when new versions cannot be installed ([Pkg#2906]).
* New `outdated::Bool` kwarg to `Pkg.status` (`--outdated` or `-o` in the REPL mode) to show
  information about packages not at the latest version ([Pkg#2284]).
* New `compat::Bool` kwarg to `Pkg.status` (`--compat` or `-c` in the REPL mode) to show any [compat]
  entries in the Project.toml ([Pkg#2702]).
* New `pkg> compat` (and `Pkg.compat`) mode for setting Project compat entries. Provides an interactive editor
  via `pkg> compat`, or direct entry manipulation via `pkg> Foo 0.4,0.5` which can load current entries via tab-completion.
  i.e. `pkg> compat Fo<TAB>` autocompletes to `pkg> Foo 0.4,0.5` so that the existing entry can be edited ([Pkg#2702]).
* Pkg now only tries to download packages from the package server in case the server tracks a registry that contains
  the package ([Pkg#2689]).
* `Pkg.instantiate` will now warn when a Project.toml is out of sync with a Manifest.toml. It does this by storing a hash
  of the project deps and compat entries (other fields are ignored) in the manifest when it is resolved, so that any change
  to the Project.toml deps or compat entries without a re-resolve can be detected ([Pkg#2815]).
* If `pkg> add` cannot find a package with the provided name it will now suggest similarly named packages that can be added ([Pkg#2985]).
* The julia version stored in the manifest no longer includes the build number i.e. master will now record as `1.9.0-DEV` ([Pkg#2995]).
* Interrupting a `pkg> test` will now be caught more reliably and exit back to the REPL gracefully ([Pkg#2933]).

#### InteractiveUtils

* New macro `@time_imports` for reporting any time spent importing packages and their dependencies, highlighting
  compilation and recompilation time as percentages per import ([#41612],[#45064]).

#### LinearAlgebra

* The BLAS submodule now supports the level-2 BLAS subroutine `spr!` ([#42830]).
* `cholesky[!]` now supports `LinearAlgebra.PivotingStrategy` (singleton type) values
  as its optional `pivot` argument: the default is `cholesky(A, NoPivot())` (vs.
  `cholesky(A, RowMaximum())`); the former `Val{true/false}`-based calls are deprecated ([#41640]).
* The standard library `LinearAlgebra.jl` is now completely independent of `SparseArrays.jl`,
  both in terms of the source code as well as unit testing ([#43127]). As a consequence,
  sparse arrays are no longer (silently) returned by methods from `LinearAlgebra` applied
  to `Base` or `LinearAlgebra` objects. Specifically, this results in the following breaking
  changes:
  * Concatenations involving special "sparse" matrices (`*diagonal`) now return dense matrices;
    As a consequence, the `D1` and `D2` fields of `SVD` objects, constructed upon `getproperty`
    calls are now dense matrices.
  * 3-arg `similar(::SpecialSparseMatrix, ::Type, ::Dims)` returns a dense zero matrix.
    As a consequence, products of bi-, tri- and symmetric tridiagonal matrices with each
    other result in dense output. Moreover, constructing 3-arg similar matrices of special
    "sparse" matrices of (nonstatic) matrices now fails for the lack of `zero(::Type{Matrix{T}})`.

#### Printf

* Now uses `textwidth` for formatting `%s` and `%c` widths ([#41085]).

#### Profile

* CPU profiling now records sample metadata including thread and task. `Profile.print()` has a new `groupby` kwarg that allows
  grouping by thread, task, or nested thread/task, task/thread, and `threads` and `tasks` kwargs to allow filtering.
  Further, percent utilization is now reported as a total or per-thread, based on whether the thread is idle or not at
  each sample. `Profile.fetch()` includes the new metadata by default. For backwards compatibility with external
  profiling data consumers, it can be excluded by passing `include_meta=false` ([#41742]).
* The new `Profile.Allocs` module allows memory allocations to be profiled. The stack trace, type, and size of each
  allocation is recorded, and a `sample_rate` argument allows a tunable amount of allocations to be skipped,
  reducing performance overhead ([#42768]).
* A fixed duration cpu profile can now be triggered by the user during running tasks without `Profile` being loaded
  first and the report will show during execution. On MacOS & FreeBSD press `ctrl-t` or raise a `SIGINFO`.
  For other platforms raise a `SIGUSR1` i.e. `% kill -USR1 $julia_pid`. Not currently available on windows ([#43179]).

#### REPL

* `RadioMenu` now supports optional `keybindings` to directly select options ([#41576]).
* ` ?(x, y` followed by TAB displays all methods that can be called
  with arguments `x, y, ...`. (The space at the beginning prevents entering help-mode.)
  `MyModule.?(x, y` limits the search to `MyModule`. TAB requires that at least one
  argument have a type more specific than `Any`; use SHIFT-TAB instead of TAB
  to allow any compatible methods ([#38791]).
* New `err` global variable in `Main` set when an expression throws an exception, akin to `ans`. Typing `err` reprints
  the exception information ([#40642]).

#### SparseArrays

* The code for SparseArrays has been moved from the Julia repo to the external
  repo at https://github.com/JuliaSparse/SparseArrays.jl. This is only a code
  movement and does not impact any usage ([#43813]).
* New sparse concatenation functions `sparse_hcat`, `sparse_vcat`, and `sparse_hvcat` return
  `SparseMatrixCSC` output independent from the types of the input arguments. They make
  concatenation behavior available, in which the presence of some special "sparse" matrix
  argument resulted in sparse output by multiple dispatch. This is no longer possible after
  making `LinearAlgebra.jl` independent from `SparseArrays.jl` ([#43127]).

#### Logging

* The standard log levels `BelowMinLevel`, `Debug`, `Info`, `Warn`, `Error`,
  and `AboveMaxLevel` are now exported from the Logging stdlib ([#40980]).

#### Unicode

* Added function `isequal_normalized` to check for Unicode equivalence without
  explicitly constructing normalized strings ([#42493]).
* The `Unicode.normalize` function now accepts a `chartransform` keyword that can
  be used to supply custom character mappings, and a `Unicode.julia_chartransform`
  function is provided to reproduce the mapping used in identifier normalization
  by the Julia parser ([#42561]).

#### Test

* `@test_throws "some message" triggers_error()` can now be used to check whether the displayed error text
  contains "some message" regardless of the specific exception type.
  Regular expressions, lists of strings, and matching functions are also supported ([#41888]).
* `@testset foo()` can now be used to create a test set from a given function. The name of the test set
  is the name of the called function. The called function can contain `@test` and other `@testset`
  definitions, including to other function calls, while recording all intermediate test results ([#42518]).
* `TestLogger` and `LogRecord` are now exported from the Test stdlib ([#44080]).

#### Distributed

* SSHManager now supports workers with csh/tcsh login shell, via `addprocs()` option `shell=:csh` ([#41485]).


Deprecated or removed
---------------------


External dependencies
---------------------


Tooling Improvements
---------------------

* `GC.enable_logging(true)` can be used to log each garbage collection, with the
  time it took and the amount of memory that was collected ([#43511]).

<!-- manually added -->
[Pkg#2284]: https://github.com/JuliaLang/Pkg.jl/issues/2284
[Pkg#2689]: https://github.com/JuliaLang/Pkg.jl/issues/2689
[Pkg#2702]: https://github.com/JuliaLang/Pkg.jl/issues/2702
[Pkg#2815]: https://github.com/JuliaLang/Pkg.jl/issues/2815
[Pkg#2906]: https://github.com/JuliaLang/Pkg.jl/issues/2906
[Pkg#2933]: https://github.com/JuliaLang/Pkg.jl/issues/2933
[Pkg#2985]: https://github.com/JuliaLang/Pkg.jl/issues/2985
[Pkg#2995]: https://github.com/JuliaLang/Pkg.jl/issues/2995

<!--- generated by NEWS-update.jl: -->
[#33711]: https://github.com/JuliaLang/julia/issues/33711
[#36265]: https://github.com/JuliaLang/julia/issues/36265
[#37343]: https://github.com/JuliaLang/julia/issues/37343
[#37747]: https://github.com/JuliaLang/julia/issues/37747
[#38791]: https://github.com/JuliaLang/julia/issues/38791
[#39241]: https://github.com/JuliaLang/julia/issues/39241
[#39245]: https://github.com/JuliaLang/julia/issues/39245
[#40110]: https://github.com/JuliaLang/julia/issues/40110
[#40382]: https://github.com/JuliaLang/julia/issues/40382
[#40642]: https://github.com/JuliaLang/julia/issues/40642
[#40783]: https://github.com/JuliaLang/julia/issues/40783
[#40980]: https://github.com/JuliaLang/julia/issues/40980
[#41085]: https://github.com/JuliaLang/julia/issues/41085
[#41312]: https://github.com/JuliaLang/julia/issues/41312
[#41328]: https://github.com/JuliaLang/julia/issues/41328
[#41449]: https://github.com/JuliaLang/julia/issues/41449
[#41485]: https://github.com/JuliaLang/julia/issues/41485
[#41551]: https://github.com/JuliaLang/julia/issues/41551
[#41576]: https://github.com/JuliaLang/julia/issues/41576
[#41612]: https://github.com/JuliaLang/julia/issues/41612
[#41618]: https://github.com/JuliaLang/julia/issues/41618
[#41639]: https://github.com/JuliaLang/julia/issues/41639
[#41640]: https://github.com/JuliaLang/julia/issues/41640
[#41742]: https://github.com/JuliaLang/julia/issues/41742
[#41769]: https://github.com/JuliaLang/julia/issues/41769
[#41794]: https://github.com/JuliaLang/julia/issues/41794
[#41888]: https://github.com/JuliaLang/julia/issues/41888
[#41936]: https://github.com/JuliaLang/julia/issues/41936
[#42154]: https://github.com/JuliaLang/julia/issues/42154
[#42211]: https://github.com/JuliaLang/julia/issues/42211
[#42225]: https://github.com/JuliaLang/julia/issues/42225
[#42248]: https://github.com/JuliaLang/julia/issues/42248
[#42314]: https://github.com/JuliaLang/julia/issues/42314
[#42355]: https://github.com/JuliaLang/julia/issues/42355
[#42428]: https://github.com/JuliaLang/julia/issues/42428
[#42431]: https://github.com/JuliaLang/julia/issues/42431
[#42469]: https://github.com/JuliaLang/julia/issues/42469
[#42493]: https://github.com/JuliaLang/julia/issues/42493
[#42513]: https://github.com/JuliaLang/julia/issues/42513
[#42518]: https://github.com/JuliaLang/julia/issues/42518
[#42529]: https://github.com/JuliaLang/julia/issues/42529
[#42561]: https://github.com/JuliaLang/julia/issues/42561
[#42768]: https://github.com/JuliaLang/julia/issues/42768
[#42830]: https://github.com/JuliaLang/julia/issues/42830
[#42918]: https://github.com/JuliaLang/julia/issues/42918
[#42925]: https://github.com/JuliaLang/julia/issues/42925
[#43110]: https://github.com/JuliaLang/julia/issues/43110
[#43113]: https://github.com/JuliaLang/julia/issues/43113
[#43127]: https://github.com/JuliaLang/julia/issues/43127
[#43179]: https://github.com/JuliaLang/julia/issues/43179
[#43191]: https://github.com/JuliaLang/julia/issues/43191
[#43208]: https://github.com/JuliaLang/julia/issues/43208
[#43232]: https://github.com/JuliaLang/julia/issues/43232
[#43239]: https://github.com/JuliaLang/julia/issues/43239
[#43271]: https://github.com/JuliaLang/julia/issues/43271
[#43305]: https://github.com/JuliaLang/julia/issues/43305
[#43354]: https://github.com/JuliaLang/julia/issues/43354
[#43443]: https://github.com/JuliaLang/julia/issues/43443
[#43511]: https://github.com/JuliaLang/julia/issues/43511
[#43604]: https://github.com/JuliaLang/julia/issues/43604
[#43671]: https://github.com/JuliaLang/julia/issues/43671
[#43813]: https://github.com/JuliaLang/julia/issues/43813
[#43852]: https://github.com/JuliaLang/julia/issues/43852
[#43865]: https://github.com/JuliaLang/julia/issues/43865
[#43919]: https://github.com/JuliaLang/julia/issues/43919
[#43990]: https://github.com/JuliaLang/julia/issues/43990
[#44080]: https://github.com/JuliaLang/julia/issues/44080
[#44136]: https://github.com/JuliaLang/julia/issues/44136
