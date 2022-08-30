Julia v1.9 Release Notes
========================

New language features
---------------------

* It is now possible to assign to bindings in another module using `setproperty!(::Module, ::Symbol, x)`. ([#44137])
* Slurping in assignments is now also allowed in non-final position. This is
  handled via `Base.split_rest`. ([#42902])
* Character literals now support the same syntax allowed in string literals; i.e. the syntax can
  represent invalid UTF-8 sequences as allowed by the `Char` type ([#44989]).

Language changes
----------------

* New builtins `getglobal(::Module, ::Symbol[, order])` and `setglobal!(::Module, ::Symbol, x[, order])`
  for reading from and writing to globals. `getglobal` should now be preferred for accessing globals over
  `getfield`. ([#44137])
* The `@invoke` macro introduced in 1.7 is now exported. Additionally, it now uses `Core.Typeof(x)`
  rather than `Any` when a type annotation is omitted for an argument `x` so that types passed
  as arguments are handled correctly. ([#45807])
* The `invokelatest` function and `@invokelatest` macro introduced in 1.7 are now exported. ([#45831])

Compiler/Runtime improvements
-----------------------------

* The known quadratic behavior of type inference is now fixed and inference uses less memory in general.
  Certain edge cases with auto-generated long functions (e.g. ModelingToolkit.jl with partial
  differential equations and large causal models) should see significant compile-time improvements.
  ([#45276], [#45404])
* Non-concrete call sites can now be union-split to be inlined or statically-resolved even
  if there are multiple dispatch candidates. This may improve runtime performance in certain
  situations where object types are not fully known statically but mostly available at runtime
  (as like Julia-level type inference implementation itself) by statically resolving
  `@nospecialize`-d call sites and avoiding excessive compilation. ([#44512])
* All the previous usages of `@pure`-macro in `Base` has been replaced with the preferred
  `Base.@assume_effects`-based annotations. ([#44776])
* `invoke(f, invokesig, args...)` calls to a less-specific method than would normally be chosen
  for `f(args...)` are no longer spuriously invalidated when loading package precompile files. ([#46010])

Command-line option changes
---------------------------

* In Linux and Windows, `--threads=auto` now tries to infer usable number of CPUs from the
  process affinity which is set typically in HPC and cloud environments ([#42340]).
* `--math-mode=fast` is now a no-op ([#41638]). Users are encouraged to use the @fastmath macro instead, which has more well-defined semantics.
* The `--threads` command-line option now accepts `auto|N[,auto|M]` where `M` specifies the
  number of interactive threads to create (`auto` currently means 1) ([#42302]).
* New option `--heap-size-hint=<size>` gives a memory hint for triggering greedy garbage
  collection. The size might be specified in bytes, kilobytes(1000k), megabytes(300M),
  gigabytes(1.5G)

Multi-threading changes
-----------------------

* `Threads.@spawn` now accepts an optional first argument: `:default` or `:interactive`.
  An interactive task desires low latency and implicitly agrees to be short duration or to
  yield frequently. Interactive tasks will run on interactive threads, if any are specified
  when Julia is started ([#42302]).

Build system changes
--------------------


New library functions
---------------------

* `Iterators.flatmap` was added ([#44792]).
* New helper `Splat(f)` which acts like `x -> f(x...)`, with pretty printing for
  inspecting which function `f` was originally wrapped. ([#42717])
* New `pkgversion(m::Module)` function to get the version of the package that loaded
  a given module, similar to `pkgdir(m::Module)`. ([#45607])
* New function `stack(x)` which generalises `reduce(hcat, x::Vector{<:Vector})` to any dimensionality,
  and allows any iterators of iterators. Method `stack(f, x)` generalises `mapreduce(f, hcat, x)` and
  is efficient. ([#43334])

Library changes
---------------

* A known concurrency issue of `iterate` methods on `Dict` and other derived objects such
  as `keys(::Dict)`, `values(::Dict)`, and `Set` is fixed.  These methods of `iterate` can
  now be called on a dictionary or set shared by arbitrary tasks provided that there are no
  tasks mutating the dictionary or set ([#44534]).
* Predicate function negation `!f` now returns a composed function `(!) ∘ f` instead of an anonymous function ([#44752]).
* `RoundFromZero` now works for non-`BigFloat` types ([#41246]).
* `Dict` can be now shrunk manually by `sizehint!` ([#45004]).
* `@time` now separates out % time spent recompiling invalidated methods ([#45015]).
* `eachslice` now works over multiple dimensions; `eachslice`, `eachrow` and `eachcol` return
  a `Slices` object, which allows dispatching to provide more efficient methods ([#32310]).
* `@kwdef` is now exported and added to the public API ([#46273])

Standard library changes
------------------------

#### Package Manager

#### LinearAlgebra

* The methods `a / b` and `b \ a` with `a` a scalar and `b` a vector,
  which were equivalent to `a * pinv(b)`, have been removed due to the
  risk of confusion with elementwise division ([#44358]).
* We are now wholly reliant on libblastrampoline (LBT) for calling
  BLAS and LAPACK. OpenBLAS is shipped by default, but building the
  system image with other BLAS/LAPACK libraries is not
  supported. Instead, it is recommended that the LBT mechanism be used
  for swapping BLAS/LAPACK with vendor provided ones. ([#44360])
* `lu` now supports a new pivoting strategy `RowNonZero()` that chooses
   the first non-zero pivot element, for use with new arithmetic types and for pedagogy ([#44571]).
* `normalize(x, p=2)` now supports any normed vector space `x`, including scalars ([#44925]).

#### Markdown

#### Printf

* Error messages for bad format strings have been improved, to make it clearer what & where in the
  format string is wrong. ([#45366])

#### Random

* `randn` and `randexp` now work for any `AbstractFloat` type defining `rand` ([#44714]).

#### REPL

* `Meta-e` now opens the current input in an editor. The content (if modified) will be
  executed upon existing the editor.

* The contextual module which is active at the REPL can be changed (it is `Main` by default),
  via the `REPL.activate(::Module)` function or via typing the module in the REPL and pressing
  the keybinding Alt-m ([#33872]).

#### SparseArrays

#### Test
* New fail-fast mode for testsets that will terminate the test run early if a failure or error occurs.
  Set either via the `@testset` kwarg `failfast=true` or by setting env var `JULIA_TEST_FAILFAST`
  to `"true"` i.e. in CI runs to request the job failure be posted eagerly when issues occur ([#45317])

#### Dates

#### Downloads

#### Statistics

#### Sockets

#### Tar

#### Distributed

* The package environment (active project, `LOAD_PATH`, `DEPOT_PATH`) are now propagated
  when adding *local* workers (e.g. with `addprocs(N::Int)` or through the `--procs=N`
  command line flag) ([#43270]).
* `addprocs` for local workers now accept the `env` keyword argument for passing
  environment variables to the workers processes. This was already supported for
  remote workers ([#43270]).

#### UUIDs

#### Unicode

* `graphemes(s, m:n)` returns a substring of the `m`-th to `n`-th graphemes in `s` ([#44266]).

#### Mmap

#### DelimitedFiles

* DelimitedFiles has been promoted from being a standard library to a separate package. It now has to be explicitly installed to be used.


Deprecated or removed
---------------------

* Unexported `splat` is deprecated in favor of exported `Splat`, which has pretty printing of the wrapped function. ([#42717])

External dependencies
---------------------


Tooling Improvements
---------------------

* Printing of `MethodError` and methods (such as from `methods(my_func)`) are now prettified and color consistent with printing of methods
  in stacktraces. ([#45069])

<!--- generated by NEWS-update.jl: -->
