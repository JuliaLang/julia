Julia v1.9 Release Notes
========================

New language features
---------------------

* It is now possible to assign to bindings in another module using `setproperty!(::Module, ::Symbol, x)` ([#44137]).
* Slurping in assignments is now also allowed in non-final position. This is handled via `Base.split_rest` ([#42902]).
* Character literals now support the same syntax allowed in string literals; i.e. the syntax can
  represent invalid UTF-8 sequences as allowed by the `Char` type ([#44989]).
* Support for Unicode 15 ([#47392]).
* Nested combinations of tuples and named tuples of symbols are now allowed as type parameters ([#46300]).
* New builtins `getglobal(::Module, ::Symbol[, order])` and `setglobal!(::Module, ::Symbol, x[, order])`
  for reading from and writing to globals. `getglobal` should now be preferred for accessing globals over
  `getfield` ([#44137]).

Language changes
----------------

* The `@invoke` macro introduced in 1.7 is now exported. Additionally, it now uses `Core.Typeof(x)`
  rather than `Any` when a type annotation is omitted for an argument `x` so that types passed
  as arguments are handled correctly ([#45807]).
* The `invokelatest` function and `@invokelatest` macro introduced in 1.7 are now exported ([#45831]).

Compiler/Runtime improvements
-----------------------------

* The known quadratic behavior of type inference is now fixed and inference uses less memory in general.
  Certain edge cases with auto-generated long functions (e.g. ModelingToolkit.jl with partial
  differential equations and large causal models) should see significant compile-time improvements ([#45276], [#45404]).
* Non-concrete call sites can now be union-split to be inlined or statically resolved even
  if there are multiple dispatch candidates. This may improve runtime performance in certain
  situations where object types are not fully known statically, by statically resolving
  `@nospecialize`-d call sites and avoiding excessive compilation ([#44512]).
* All uses of the `@pure` macro in `Base` have been replaced with the now-preferred `Base.@assume_effects` ([#44776]).
* `invoke(f, invokesig, args...)` calls to a less-specific method than would normally be chosen
  for `f(args...)` are no longer spuriously invalidated when loading package precompile files ([#46010]).

Command-line option changes
---------------------------

* In Linux and Windows, `--threads=auto` now tries to infer the usable number of CPUs from the
  process affinity which is set typically in HPC and cloud environments ([#42340]).
* `--math-mode=fast` is now a no-op ([#41638]). Users are encouraged to use the @fastmath macro instead, which has more well-defined semantics.
* The `--threads` command-line option now accepts `auto|N[,auto|M]` where `M` specifies the
  number of interactive threads to create (`auto` currently means 1) ([#42302]).
* New option `--heap-size-hint=<size>` suggests a size limit to invoke garbage collection more eagerly.
  The size may be specified in bytes, kilobytes (1000k), megabytes (300M), or gigabytes (1.5G) ([#45369]).

Multi-threading changes
-----------------------

* `Threads.@spawn` now accepts an optional first argument: `:default` or `:interactive`.
  An interactive task desires low latency and implicitly agrees to be short duration or to yield frequently.
  Interactive tasks will run on interactive threads, if any are specified when Julia is started ([#42302]).
* Threads started outside the Julia runtime (e.g. from C or Java) can now become able to call into Julia code
  by calling `jl_adopt_thread`. This is done automatically when entering Julia code via `cfunction` or a
  `@ccallable` entry point. As a consequence, the number of threads can now change during execution ([#46609]).

Build system changes
--------------------


New library functions
---------------------

* New function `Iterators.flatmap` ([#44792]).
* New helper `Splat(f)` which acts like `x -> f(x...)`, with pretty printing for
  inspecting which function `f` was originally wrapped ([#42717]).
* New `pkgversion(m::Module)` function to get the version of the package that loaded
  a given module, similar to `pkgdir(m::Module)` ([#45607]).
* New function `stack(x)` which generalises `reduce(hcat, x::Vector{<:Vector})` to any dimensionality,
  and allows any iterator of iterators. Method `stack(f, x)` generalises `mapreduce(f, hcat, x)` and
  is more efficient ([#43334]).
* New macro `@allocations` which is similar to `@allocated` except reporting the total number of allocations
  rather than the total size of memory allocated ([#47367]).

New library features
--------------------

* `RoundFromZero` now works for non-`BigFloat` types ([#41246]).
* `Dict` can be now shrunk manually by `sizehint!` ([#45004]).
* `@time` now separates out % time spent recompiling invalidated methods ([#45015]).

Standard library changes
------------------------

* A known concurrency issue in `iterate` methods on `Dict` and other derived objects such
  as `keys(::Dict)`, `values(::Dict)`, and `Set` is fixed. These methods of `iterate` can
  now be called on a dictionary or set shared by arbitrary tasks provided that there are no
  tasks mutating the dictionary or set ([#44534]).
* Predicate function negation `!f` now returns a composed function `(!) ∘ f` instead of an anonymous function ([#44752]).
* `eachslice` now works over multiple dimensions; `eachslice`, `eachrow` and `eachcol` return
  a `Slices` object, which allows dispatching to provide more efficient methods ([#32310]).
* `@kwdef` is now exported and added to the public API ([#46273]).
* An issue with order of operations in `fld1` is now fixed ([#28973]).
* Sorting is now always stable by default, as `QuickSort` was stabilized ([#45222]).

#### Package Manager

#### LinearAlgebra

* The methods `a / b` and `b \ a` with `a` a scalar and `b` a vector, which were equivalent to `a * pinv(b)`,
  have been removed due to the risk of confusion with elementwise division ([#44358]).
* We are now wholly reliant on libblastrampoline (LBT) for calling BLAS and LAPACK. OpenBLAS is shipped by default,
  but building the system image with other BLAS/LAPACK libraries is not supported. Instead, it is recommended that
  the LBT mechanism be used for swapping BLAS/LAPACK with vendor provided ones ([#44360]).
* `lu` supports a new pivoting strategy `RowNonZero()` that chooses the first non-zero pivot element, for use with
  new arithmetic types and for pedagogy ([#44571]).
* `normalize(x, p=2)` now supports any normed vector space `x`, including scalars ([#44925]).
* The default number of BLAS threads is now set to the number of CPU threads on ARM CPUs, and half the number
  of CPU threads on other architectures ([#45412], [#46085]).

#### Printf

* Error messages for bad format strings have been improved, to make it clearer what and where in the
  format string is wrong ([#45366]).

#### Profile

* New function `Profile.take_heap_snapshot(file)` that writes a file in Chrome's JSON-based `.heapsnapshot`
  format ([#46862]).

#### Random

* `randn` and `randexp` now work for any `AbstractFloat` type defining `rand` ([#44714]).

#### REPL

* `Alt-e` now opens the current input in an editor. The content (if modified) will be executed
  upon exiting the editor ([#33759]).
* The contextual module which is active in the REPL can be changed (it is `Main` by default),
  via the `REPL.activate(::Module)` function or via typing the module in the REPL and pressing
  the keybinding Alt-m ([#33872]).
* An "IPython mode" which mimics the behaviour of the prompts and storing the evaluated result in `Out` can be
  activated with `REPL.ipython_mode!()`. See the manual for how to enable this at startup ([#46474]).

#### SuiteSparse

* Code for the SuiteSparse solver wrappers has been moved to SparseArrays.jl. Solvers are now re-exported by
  SuiteSparse.jl.

#### SparseArrays

* SuiteSparse solvers are now available as submodules of SparseArrays (<https://github.com/JuliaSparse/SparseArrays.jl/pull/95>).
* UMFPACK (<https://github.com/JuliaSparse/SparseArrays.jl/pull/179>) and CHOLMOD (<https://github.com/JuliaSparse/SparseArrays.jl/pull/206>) thread safety are improved by
  avoiding globals and using locks. Multithreaded `ldiv!` of UMFPACK objects may now be performed safely.
* An experimental function `SparseArrays.allowscalar(::Bool)` allows scalar indexing of sparse arrays to be
  disabled or enabled. This function is intended to help find accidental scalar indexing of `SparseMatrixCSC`
  objects, which is a common source of performance issues (<https://github.com/JuliaSparse/SparseArrays.jl/pull/200>).

#### Test

* New fail-fast mode for testsets that will terminate the test run early if a failure or error occurs.
  Set either via the `@testset` kwarg `failfast=true` or by setting env var `JULIA_TEST_FAILFAST`
  to `"true"` e.g. for faster job failure in CI runs ([#45317]).

#### Distributed

* The package environment (active project, `LOAD_PATH`, `DEPOT_PATH`) is now propagated when adding *local* workers
  (e.g. with `addprocs(N::Int)` or through the `--procs=N` command line flag) ([#43270]).
* `addprocs` for local workers now accepts the `env` keyword argument for passing environment variables to worker
  processes. This was already supported for remote workers ([#43270]).

#### Unicode

* `graphemes(s, m:n)` returns a substring of the `m`-th to `n`-th graphemes in `s` ([#44266]).

#### DelimitedFiles

* DelimitedFiles has been moved out as a separate package. It now has to be explicitly installed to be used.

Deprecated or removed
---------------------

* Unexported `splat` is deprecated in favor of exported `Splat`, which has pretty printing of the wrapped function ([#42717]).

External dependencies
---------------------

* On Linux, now autodetects the system libstdc++ version, and automatically loads the system library if it is newer.
  The old behavior of loading the bundled libstdc++ regardless of the system version can be restored by setting the
  environment variable `JULIA_PROBE_LIBSTDCXX=0` ([#46976]).
* Removed `RPATH` from the julia binary. On Linux this may break libraries that have failed to set `RUNPATH`.

Tooling Improvements
--------------------

* Printing of `MethodError` and methods (such as from `methods(my_func)`) is now prettified and colored consistently
  with printing of methods in stacktraces ([#45069]).

<!--- generated by NEWS-update.jl: -->
[#28973]: https://github.com/JuliaLang/julia/issues/28973
[#32310]: https://github.com/JuliaLang/julia/issues/32310
[#33759]: https://github.com/JuliaLang/julia/issues/33759
[#33872]: https://github.com/JuliaLang/julia/issues/33872
[#41246]: https://github.com/JuliaLang/julia/issues/41246
[#41638]: https://github.com/JuliaLang/julia/issues/41638
[#42302]: https://github.com/JuliaLang/julia/issues/42302
[#42340]: https://github.com/JuliaLang/julia/issues/42340
[#42717]: https://github.com/JuliaLang/julia/issues/42717
[#42902]: https://github.com/JuliaLang/julia/issues/42902
[#43270]: https://github.com/JuliaLang/julia/issues/43270
[#43334]: https://github.com/JuliaLang/julia/issues/43334
[#44137]: https://github.com/JuliaLang/julia/issues/44137
[#44266]: https://github.com/JuliaLang/julia/issues/44266
[#44358]: https://github.com/JuliaLang/julia/issues/44358
[#44360]: https://github.com/JuliaLang/julia/issues/44360
[#44512]: https://github.com/JuliaLang/julia/issues/44512
[#44534]: https://github.com/JuliaLang/julia/issues/44534
[#44571]: https://github.com/JuliaLang/julia/issues/44571
[#44714]: https://github.com/JuliaLang/julia/issues/44714
[#44752]: https://github.com/JuliaLang/julia/issues/44752
[#44776]: https://github.com/JuliaLang/julia/issues/44776
[#44792]: https://github.com/JuliaLang/julia/issues/44792
[#44925]: https://github.com/JuliaLang/julia/issues/44925
[#44989]: https://github.com/JuliaLang/julia/issues/44989
[#45004]: https://github.com/JuliaLang/julia/issues/45004
[#45015]: https://github.com/JuliaLang/julia/issues/45015
[#45069]: https://github.com/JuliaLang/julia/issues/45069
[#45222]: https://github.com/JuliaLang/julia/issues/45222
[#45276]: https://github.com/JuliaLang/julia/issues/45276
[#45317]: https://github.com/JuliaLang/julia/issues/45317
[#45366]: https://github.com/JuliaLang/julia/issues/45366
[#45369]: https://github.com/JuliaLang/julia/issues/45369
[#45404]: https://github.com/JuliaLang/julia/issues/45404
[#45412]: https://github.com/JuliaLang/julia/issues/45412
[#45607]: https://github.com/JuliaLang/julia/issues/45607
[#45807]: https://github.com/JuliaLang/julia/issues/45807
[#45831]: https://github.com/JuliaLang/julia/issues/45831
[#46010]: https://github.com/JuliaLang/julia/issues/46010
[#46085]: https://github.com/JuliaLang/julia/issues/46085
[#46273]: https://github.com/JuliaLang/julia/issues/46273
[#46300]: https://github.com/JuliaLang/julia/issues/46300
[#46474]: https://github.com/JuliaLang/julia/issues/46474
[#46609]: https://github.com/JuliaLang/julia/issues/46609
[#46862]: https://github.com/JuliaLang/julia/issues/46862
[#46976]: https://github.com/JuliaLang/julia/issues/46976
[#47367]: https://github.com/JuliaLang/julia/issues/47367
[#47392]: https://github.com/JuliaLang/julia/issues/47392
