Julia v1.12 Release Notes
========================

New language features
---------------------

- New option `--trim` creates smaller binaries by removing code that was not proven to be reachable from
  the entry points. Entry points can be marked using `Base.Experimental.entrypoint` ([#55047]).
- A new keyword argument `usings::Bool` has been added to `names`. By using this, we can now
  find all the names available in module `A` by `names(A; all=true, imported=true, usings=true)`. ([#54609])
- the `@atomic(...)` macro family supports now the reference assignment syntax, e.g.
  `@atomic :monotonic v[3] += 4` modifies `v[3]` atomically with monotonic ordering semantics. ([#54707])
  The supported syntax allows
  - atomic fetch (`x = @atomic v[3]`),
  - atomic set (`@atomic v[3] = 4`),
  - atomic modify (`@atomic v[3] += 2`),
  - atomic set once (`@atomiconce v[3] = 2`),
  - atomic swap (`x = @atomicswap v[3] = 2`), and
  - atomic replace (`x = @atomicreplace v[3] 2=>5`).
- New option `--task-metrics=yes` to enable the collection of per-task timing information,
  which can also be enabled/disabled at runtime with `Base.Experimental.task_metrics(::Bool)`. ([#56320])
  The available metrics are:
  - actual running time for the task (`Base.Experimental.task_running_time_ns`), and
  - wall-time for the task (`Base.Experimental.task_wall_time_ns`).
- Support for Unicode 16 ([#56925]).

Language changes
----------------

 - When methods are replaced with exactly equivalent ones, the old method is no
   longer deleted implicitly simultaneously, although the new method does take
   priority and become more specific than the old method. Thus if the new
   method is deleted later, the old method will resume operating. This can be
   useful to mocking frameworks (such as in SparseArrays, Pluto, and Mocking,
   among others), as they do not need to explicitly restore the old method.
   While inference and compilation still must be repeated with this, it also
   may pave the way for inference to be able to intelligently re-use the old
   results, once the new method is deleted. ([#53415])

 - Macro expansion will no longer eagerly recurse into `Expr(:toplevel)`
   expressions returned from macros. Instead, macro expansion of `:toplevel`
   expressions will be delayed until evaluation time. This allows a later
   expression within a given `:toplevel` expression to make use of macros
   defined earlier in the same `:toplevel` expression. ([#53515])

 - Trivial infinite loops (like `while true; end`) are no longer undefined
   behavior. Infinite loops that actually do things (e.g. have side effects
   or sleep) were never and are still not undefined behavior. ([#52999])

 - It is now an error to mark a symbol as both `public` and `export`ed.
   ([#53664])

Compiler/Runtime improvements
-----------------------------

- Generated LLVM IR now uses actual pointer types instead of passing pointers as integers.
  This affects `llvmcall`: Inline LLVM IR should be updated to use `i8*` or `ptr` instead of
  `i32` or `i64`, and remove unneeded `ptrtoint`/`inttoptr` conversions. For compatibility,
  IR with integer pointers is still supported, but generates a deprecation warning. ([#53687])

- A new exception `FieldError` is now introduced to raise/handle `getfield` exceptions. Previously `getfield` exception was captured by fallback generic exception `ErrorException`. Now that `FieldError` is more specific `getfield` related exceptions that can occur should use `FieldError` exception instead. ([#54504])

Command-line option changes
---------------------------

* The `-m/--module` flag can be passed to run the `main` function inside a package with a set of arguments.
  This `main` function should be declared using `@main` to indicate that it is an entry point. ([#52103])
* Enabling or disabling color text in Julia can now be controlled with the
  [`NO_COLOR`](https://no-color.org/) or [`FORCE_COLOR`](https://force-color.org/) environment
  variables. These variables are also honored by Julia's build system ([#53742], [#56346]).
* `--project=@temp` starts Julia with a temporary environment. ([#51149])
* New `--trace-compile-timing` option to report how long each method reported by `--trace-compile` took
  to compile, in ms. ([#54662])
* `--trace-compile` now prints recompiled methods in yellow or with a trailing comment if color is not supported ([#55763])
* New `--trace-dispatch` option to report methods that are dynamically dispatched ([#55848]).

Multi-threading changes
-----------------------

* New types are defined to handle the pattern of code that must run once per process, called
  a `OncePerProcess{T}` type, which allows defining a function that should be run exactly once
  the first time it is called, and then always return the same result value of type `T`
  every subsequent time afterwards. There are also `OncePerThread{T}` and `OncePerTask{T}` types for
  similar usage with threads or tasks. ([#55793])

Build system changes
--------------------

* There are new `Makefile`s to build Julia and LLVM using the Binary Optimization and Layout Tool (BOLT), see  `contrib/bolt` and `contrib/pgo-lto-bolt` ([#54107]).

New library functions
---------------------

* `logrange(start, stop; length)` makes a range of constant ratio, instead of constant step ([#39071])
* The new `isfull(c::Channel)` function can be used to check if `put!(c, some_value)` will block. ([#53159])
* `waitany(tasks; throw=false)` and `waitall(tasks; failfast=false, throw=false)` which wait multiple tasks at once ([#53341]).
* `uuid7()` creates an RFC 9652 compliant UUID with version 7 ([#54834]).
* `insertdims(array; dims)` allows to insert singleton dimensions into an array which is the inverse operation to `dropdims`. ([#45793])
* The new `Fix` type is a generalization of `Fix1/Fix2` for fixing a single argument ([#54653]).

New library features
--------------------

* `escape_string` takes additional keyword arguments `ascii=true` (to escape all
  non-ASCII characters) and `fullhex=true` (to require full 4/8-digit hex numbers
  for u/U escapes, e.g. for C compatibility) ([#55099]).
* `tempname` can now take a suffix string to allow the file name to include a suffix and include that suffix in
  the uniquing checking ([#53474])
* `RegexMatch` objects can now be used to construct `NamedTuple`s and `Dict`s ([#50988])
* `Lockable` is now exported ([#54595])
* `Base.require_one_based_indexing` and `Base.has_offset_axes` are now public ([#56196])
* New `ltruncate`, `rtruncate` and `ctruncate` functions for truncating strings to text width, accounting for char widths ([#55351])
* `isless` (and thus `cmp`, sorting, etc.) is now supported for zero-dimensional `AbstractArray`s ([#55772])
* `invoke` now supports passing a Method instead of a type signature making this interface somewhat more flexible for certain uncommon use cases ([#56692]).
* `Timer(f, ...)` will now match the stickiness of the parent task when creating timer tasks, which can be overridden
  by the new `spawn` kwarg. This avoids the issue where sticky tasks i.e. `@async` make their parent sticky ([#56745])
* `invoke` now supports passing a CodeInstance instead of a type, which can enable
certain compiler plugin workflows ([#56660]).
* `sort` now supports `NTuple`s ([#54494])
* `map!(f, A)` now stores the results in `A`, like `map!(f, A, A)`. or `A .= f.(A)` ([#40632]).

Standard library changes
------------------------

* `gcdx(0, 0)` now returns `(0, 0, 0)` instead of `(0, 1, 0)` ([#40989]).
* `fd` returns a `RawFD` instead of an `Int` ([#55080]).

#### StyledStrings

#### JuliaSyntaxHighlighting

* A new standard library for applying syntax highlighting to Julia code, this
  uses `JuliaSyntax` and `StyledStrings` to implement a `highlight` function
  that creates an `AnnotatedString` with syntax highlighting applied. ([#51810])

#### Package Manager

#### LinearAlgebra

* `rank` can now take a `QRPivoted` matrix to allow rank estimation via QR factorization ([#54283]).
* Added keyword argument `alg` to `eigen`, `eigen!`, `eigvals` and `eigvals!` for self-adjoint
  matrix types (i.e., the type union `RealHermSymComplexHerm`) that allows one to switch
  between different eigendecomposition algorithms ([#49355]).
* Added a generic version of the (unblocked) pivoted Cholesky decomposition
  (callable via `cholesky[!](A, RowMaximum())`) ([#54619]).
* The number of default BLAS threads now respects process affinity, instead of
  using total number of logical threads available on the system ([#55574]).
* A new function `zeroslike` is added that is used to generate the zero elements for matrix-valued banded matrices.
  Custom array types may specialize this function to return an appropriate result ([#55252]).
* The matrix multiplication `A * B` calls `matprod_dest(A, B, T::Type)` to generate the destination.
  This function is now public ([#55537]).
* The function `haszero(T::Type)` is used to check if a type `T` has a unique zero element defined as `zero(T)`.
  This is now public ([#56223]).
* A new function `diagview` is added that returns a view into a specific band of an `AbstractMatrix` ([#56175]).

#### Logging

#### Printf

#### Profile

* `Profile.take_heap_snapshot` takes a new keyword argument, `redact_data::Bool`,
  that is `true` by default. When set, the contents of Julia objects are not emitted
  in the heap snapshot. This currently only applies to strings. ([#55326])
* `Profile.print()` now colors Base/Core/Package modules similarly to how they are in stacktraces.
  Also paths, even if truncated, are now clickable in terminals that support URI links
  to take you to the specified `JULIA_EDITOR` for the given file & line number. ([#55335])

#### Random

#### REPL

- Using the new `usings=true` feature of the `names()` function, REPL completions can now
  complete names that have been explicitly `using`-ed. ([#54610])
- REPL completions can now complete input lines like `[import|using] Mod: xxx|` e.g.
  complete `using Base.Experimental: @op` to `using Base.Experimental: @opaque`. ([#54719])
- the REPL will now warn if it detects a name is being accessed from a module which does not define it (nor has a submodule which defines it),
  and for which the name is not public in that module. For example, `map` is defined in Base, and executing `LinearAlgebra.map`
  in the REPL will now issue a warning the first time occurs. ([#54872])
- When an object is printed automatically (by being returned in the REPL), its display is now truncated after printing 20 KiB.
  This does not affect manual calls to `show`, `print`, and so forth. ([#53959])
- Backslash completions now print the respective glyph or emoji next to each matching backslash shortcode. ([#54800])

#### SuiteSparse

#### SparseArrays

#### Test

* A failing `DefaultTestSet` now prints to screen the random number generator (RNG) of the failed test, to help reproducing a stochastic failure which only depends on the state of the RNG.
  It is also possible seed a test set by passing the `rng` keyword argument to `@testset`:
  ```julia
  using Test, Random
  @testset rng=Xoshiro(0x2e026445595ed28e, 0x07bb81ac4c54926d, 0x83d7d70843e8bad6, 0xdbef927d150af80b, 0xdbf91ddf2534f850) begin
      @test rand() == 0.559472630416976
  end
  ```

#### Dates

#### Statistics

#### Distributed

#### Unicode

#### DelimitedFiles

#### InteractiveUtils

* New macros `@trace_compile` and `@trace_dispatch` for running an expression with
  `--trace-compile=stderr --trace-compile-timing` and `--trace-dispatch=stderr` respectively enabled.
  ([#55915])

Deprecated or removed
---------------------

External dependencies
---------------------

- The terminal info database, `terminfo`, is now vendored by default, providing a better
  REPL user experience when `terminfo` is not available on the system. Julia can be built
  without vendoring the database using the Makefile option `WITH_TERMINFO=0`. ([#55411])

Tooling Improvements
--------------------

- A wall-time profiler is now available for users who need a sampling profiler that captures tasks regardless of their scheduling or running state. This type of profiler enables profiling of I/O-heavy tasks and helps detect areas of heavy contention in the system ([#55889]).

<!--- generated by NEWS-update.jl: -->
