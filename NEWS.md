Julia v1.14 Release Notes
=========================

New language features
---------------------

* It is now possible to control which version of the Julia syntax will be used to parse a package by setting the
  `compat.julia` or `syntax.julia_version` key in Project.toml. This feature is similar to the notion of "editions"
  in other language ecosystems and will allow non-breaking evolution of Julia syntax in future versions.
  See the "Syntax Versioning" section in the code loading documentation ([#60018]).
* `ᵅ` (U+U+1D45), `ᵋ` (U+1D4B), `ᶲ` (U+1DB2), `˱` (U+02F1), `˲` (U+02F2), and `ₔ` (U+2094) can now also be used as
  operator suffixes, accessible as `\^alpha`, `\^epsilon`, `\^ltphi`, `\_<`, `\_>`, and `\_schwa` at the REPL
  ([#60285]).
* The `@label` macro can now create labeled blocks that can be exited early with `break name [value]`. Use
  `@label name expr` for named blocks or `@label expr` for anonymous blocks. Anonymous `@label` blocks
  participate in the default break scope: a plain `break` or `break _` exits the innermost breakable scope,
  whether it is a loop or an `@label` block. The `continue` statement also supports labels with
  `continue name` to continue a labeled loop ([#60481]).
* `typegroup` blocks allow defining mutually recursive struct types that reference each other in their
  field types. All types in the group are resolved atomically at the end of the block ([#60569]).
* Primitive types with non-byte-multiple logical widths can now be defined ([#61359]).
* Introduced explicitly wrapping arithmetic operators `+%`, `-%`, `*%` to annotate arithmetic operations
  that are semantically safe to wrap/overflow. Their behavior is currently identical to the default `+`, `-`, `*`
  operators. However, in a future version, there may be opt-in support to detect unannotated wrapping
  in the default operators ([#50790]).

* `@sync`, `Threads.@threads` and `Experimental.@sync` blocks now scope a cancellation source
  (see `Base.CancellationTokenSource`) over their children, so cancelling an enclosing scope
  reaches everything spawned within, and the blocks' teardown awaits internal tasks per the
  requested cancellation severity ([#60281]).
* Task cancellation is now supported, organized around cancellation tokens:
  `Base.CancellationTokenSource` is a level-triggered, tree-structured cancellation scope
  (cancelling a source cancels its whole subtree, at monotonically escalating severities), and
  `Base.CancellationToken` is its observe/wait view. The token governing a computation is carried
  as a scoped value (`Base.CANCEL_TOKEN`, established with the standard `ScopedValues` API) that
  propagates to child tasks; blocking operations
  (`wait`, `lock`, Channel operations, `sleep`, stream and command I/O, Sockets, FileWatching,
  ...) accept a `cancel` keyword argument defaulting to the scoped token and throw a
  `Base.CancellationRequest` while it is cancelled. Cancellation is uniformly level-triggered:
  cleanup code that must block under a cancelled scope shields itself with `cancel = nothing`
  (or by scoping `Base.CANCEL_TOKEN => nothing` over a whole block). Compute-bound code can opt into cancellation with the
  `Base.@cancel_check` cancellation point.
  A long-running foreign call can be made cancellable with
  `@ccall cancel_handler=(fn, state) ...`: cancelling the governing token runs the
  C-callable `fn(state, severity)` on the thread executing the call, signal-handler-style,
  so it can tell the library to return early (the pending cancellation is then thrown at
  the next cancellation point). Calls into libraries audited for asynchronous unwinding
  can be annotated `@ccall reset_safe=true ...` instead, letting a cancellation unwind
  the foreign computation at an arbitrary instruction; `BigInt` (GMP) arithmetic uses
  this, so checkless bignum loops now cancel cleanly at the first ^C.
  In interactive sessions, ^C now cancels the current evaluation's cancellation scope
  (instead of throwing an `InterruptException` into whatever code happened to be running),
  and a fresh ^C epoch is re-armed at each prompt; a script that catches a ^C
  cancellation continues under the cancelled scope unless it re-arms one itself
  (`ScopedValues.@with Base.CANCEL_TOKEN => Base.sigint_new_episode!() ...`) ([#60281]).

Language changes
----------------

* `Type{T} <: S` now holds only if every type `==` to `T` is an instance of `S`, fixing a
  long-standing soundness hole where e.g. `Type{Int} <: DataType` held even though types like
  `Tuple{S} where S<:Int` are `==` (and `isa`) their canonical spelling without being `DataType`s.
  In particular `Type{T}` is no longer a subtype of any single kind: use a union of kinds instead
  (e.g. `Type{Int} <: Union{DataType,UnionAll}` holds). `isa` and dispatch of type *values* are
  unaffected, and a method on `Type{Int}` remains more specific than one on `DataType`
  ([#33136], [#62141]).

Compiler/Runtime improvements
-----------------------------

* Type inference now refines field types through conditional checks and call signatures.
  For example, after `if !isnothing(x.field)`, inference knows `x.field` is not `nothing` within the branch.
  Similarly, after a call like `func(x.field)` where `func(::Int)` is the only matching method, inference
  refines `x.field` to `Int`. This works for immutable struct fields and `const` fields of mutable structs.
  Mutable (non-`const`) fields are not supported due to the lack of per-object memory effect tracking;
  for those, the recommended pattern remains storing the field value in a local variable before the check
  (e.g. `val = x.field; if !isnothing(val) ... end`) ([#41199], [#47574]).
* Stack traces now show full method signatures with argument types for inlined frames, matching the display
  of non-inlined frames ([#53925]).
* Stack traces of errors raised while loading code no longer show the internals of the
  code loading machinery, which are collapsed to the single frame that entered loading.
  Frames for user code that runs during loading are unaffected. Set the
  `JULIA_STACKTRACE_FULL_LOADING` environment variable to `true` to show them ([#52988]).
* Parallel package precompilation now coordinates CPU usage across both the precompile worker processes and
  the LLVM threads each spawns to compile its native image, sharing a single thread budget so idle cores are
  filled during the long tail without oversubscribing the machine when many packages compile at once. The total
  budget can be set with the new `JULIA_PRECOMPILE_THREADS` environment variable ([#61958]).
* Coverage reports now include code executed by the interpreter, such as top-level statements and method
  bodies run with `--compile=min`. Consequently, LCOV output and `.cov` files may contain source lines that
  were absent in earlier releases ([#62514]).
* Coverage and allocation tracking use separate unordered atomic loads and stores. This avoids the atomic
  read-modify-write overhead reported in [#62424] while keeping concurrent accesses well-defined; execution
  counts may still be inaccurate when the same source line runs on multiple threads ([#62724]).
* `--code-coverage=user` no longer includes inlined Base methods whose module cannot be recovered from debug
  information. This prevents coverage from writing `.cov` files for Base sources into the Julia installation
  ([#62514]).
* Coverage now records only whether each source line ran by default, and reports a count of 1 for executed
  lines in `.cov` files and LCOV tracefiles. Use `--code-coverage-mode=count` to collect execution counts
  instead. The default `hit` mode avoids the load and increment at each instrumentation point ([#62724]).
* A process collecting coverage with `--code-coverage=user`, `all` or `@<path>` now precompiles package
  images with coverage counters compiled in and keeps using that code, instead of recompiling package
  code as it runs (with `@<path>`, only the packages under the tracked path are instrumented). The
  instrumentation is part of the cache identity (see `Base.CacheFlags`), so the instrumented variants
  live next to the plain ones in the depot and can be cached across CI runs.

Command-line option changes
---------------------------

* `-P <project>` is now a shorthand for `--project <project>` ([#59867]).
* `--code-coverage=@<path>` and `--track-allocation=@<path>` now restrict tracking to the specified file or
  directory tree. For example, `@/src/Foo` tracks `/src/Foo/x.jl`, but not `/src/Foobar/x.jl`. Specifying the
  filesystem root as `@/` tracks every absolute path. `Base.is_file_tracked` now returns `false` when Julia was
  not started with either `@<path>` option ([#62514]).

Multi-threading changes
-----------------------

* The return type of `fetch(::Task)` is now inferred precisely when inference can determine the code
  the task was created to run (for example `fetch(Threads.@spawn f(x))`), instead of always being
  `Any`. Correspondingly, assigning to the `result` field of a `Task` via property syntax
  (`t.result = v`) now throws an error: the result of a task is determined by the return value of its
  code, and the runtime and the compiler now rely on this correspondence. To pass a value to a
  suspended task, use `schedule(t, val)` or `yieldto(t, val)` ([#59221]).
* New functions `Threads.atomic_fence_heavy` and `Threads.atomic_fence_light` provide support for
  asymmetric atomic fences, speeding up atomic synchronization where one side of the synchronization
  runs significantly less often than the other ([#60311]).
* `Threads.@threads` now supports array comprehensions with syntax like `@threads [f(i) for i in 1:n]`,
  filtered comprehensions like `@threads [f(i) for i in 1:n if condition(i)]`, typed comprehensions
  like `@threads Float64[f(i) for i in 1:n]`, and multi-dimensional comprehensions like
  `@threads [f(i,j) for i in 1:n, j in 1:m]` (preserves dimensions). All scheduling options
  (`:static`, `:dynamic`, `:greedy`) are supported. Results preserve element order for `:static`
  and `:dynamic` scheduling; `:greedy` does not guarantee order. Non-indexable iterators are
  also supported ([#59019]).
* The task scheduler now avoids O(nthreads) wake overhead on every `@spawn`, significantly reducing
  threading overhead particularly on highly oversubscribed machines. Benchmarks show up to 1000x
  reduction in spawn time in such scenarios ([#61826]).
* `Threads.Atomic` now supports the reference form of the `@atomic`, `@atomicswap`, `@atomicreplace`,
  and `@atomiconce` macros (e.g. `@atomic a[]`, `@atomic a[] = v`, `@atomic a[] += 1`), which allows
  the memory ordering to be specified explicitly and makes atomic read-modify-write operations
  syntactically clear ([#62382]).

Build system changes
--------------------

New library functions
---------------------

* `tap(f)` creates a function that calls `f(x)` for side effects and returns `x` ([#61340]).
* `unsplat(f)` creates a function that bundles its arguments into a tuple and passes them to `f`;
  it is the inverse of `splat` ([#62714]).
* `Base.set_binding_visibility!` sets the declared visibility (`:none`, `:public`, or `:export`) of a name
  in a module, allowing an `export` or `public` declaration to be retracted programmatically ([#62131]).
* `Base.generating_output()` has been made `public` (but not exported) to allow checking whether the current
  process is performing compilation for a pkgimage/sysimage ([#61224]).
* `Base.isfieldatomic(t, s)` has been made `public` (but not exported); it reports whether a field `s` of a
  type `t` is declared `@atomic`.
* `Base.raw_substring` is an unexported, public constructor to build a `SubString` without checking for
  valid string indices.
* `Base.unannotate(::AnnotatedString)` returns the underlying un-annotated string of the input string.
* `Base.include_mapexprs(mod)` is an unexported, public function returning the non-identity
  `mapexpr` functions used by `include(mapexpr, …)` calls while loading the package rooted at
  `mod`, keyed by `(including_module, absolute_path)`. The table is stored inside the package
  image, so it survives precompilation; revision tools (e.g. Revise) use it to re-apply the
  original transform when an `include(mapexpr, …)`-ed file is edited.

New library features
--------------------

* `IOContext` supports a new boolean `hexunsigned` option that allows for printing unsigned integers in
  decimal instead of hexadecimal ([#60267]).
* `lazy"..."` strings now support a flag `lazy"..."c` that adds `compact` and `limit` flags to the
  `IOContext` for final output-string generation ([#61887]).
* The `StringView` type wraps an `AbstractVector{UInt8}` and interprets it as a UTF-8 encoded string,
  superseding the [StringViews.jl](https://github.com/JuliaStrings/StringViews.jl) package ([#60526]).
* Package precompilation now supports running precompilation in
  a background task and has new interactive keyboard controls:
  `c` to cleanly cancel immediately, `d` to detach, `i` for a profile peek,
  `v` to toggle verbose mode showing elapsed time, CPU%, and memory usage, and `?` for help ([#60943]).
* Instances of an `Enum` can now be given their own docstrings within the `@enum` definition ([#61955]).
* New methods `readdir(path, DirEntry)` and `readdir(::DirEntry, DirEntry)` return directory contents
  along with the type of the entries in a vector of new `DirEntry` objects to provide more efficient `isfile`
  etc. checks. `readdir(::DirEntry)` accepts a `DirEntry` as input and, like `readdir(::AbstractString)`,
  returns a `Vector{String}` of names. `DirEntry` is exported from `Base` ([#55358]).
* New public but unexported function `Base.unsetindex!` unsets the reference from an array
  or a `MemoryRef` to its value, making it as if it was uninitialized.
* Calls to `wait` on one-shot `Timer`s that have already triggered no longer throw `EOFError`. Previously
  only the first `wait` returned and subsequent `wait` calls would throw ([#62539])
* When the display height is too small to show any array entries, the `text/plain` array display
  (used e.g. by the REPL and when logging values with `@info` etc.) now shows as many entries as
  fit on a single line, truncated to the display width, instead of showing no data at all ([#62543]).
* The element type of broadcast expressions now uses regular inference machinery rather than an idiosyncratic
  heuristic. This can help fused or empty broadcasts infer to more precise element types ([#62564]).

Standard library changes
------------------------

* `codepoint(c)` now succeeds for overlong encodings.  `Base.ismalformed`, `Base.isoverlong`, and
  `Base.show_invalid` are now `public` and documented (but not exported) ([#55152]).

#### JuliaSyntaxHighlighting

#### LinearAlgebra

#### Markdown

* Support "raw" or "inline" HTML inside Markdown data ([#60629], [#60632], [#60732]).
* Support autolinks for email addresses ([#60570]).
* Many improvements and bugfixes for rendering Markdown lists in a terminal ([#55456], [#60519]).
* Strikethrough text via `~strike~` or `~~through~~` is now supported by the Markdown parser ([#60537]).
* Many, many bug fixes and minor tweaks; overall behavior is now much closer to CommonMark ([#59977], [#60502]).

#### Profile

#### Random

#### REPL

#### SharedArrays

* `close(::SharedArray)` eagerly releases the shared-memory mappings referenced through the
  array on all processes, e.g. so the file backing a file-backed `SharedArray` can be deleted
  immediately ([#62488]).

#### Test
* Pressing `^C` twice at an empty `julia>` prompt now cancels all still-running
  work started by earlier evaluations (e.g. a runaway `@async` task spewing
  output): each REPL evaluation runs under its own cancellation source, linked
  under one session-level source that the repeated press cancels ([#47839]).

#### Test

* `@test`, `@test_throws`, and `@test_broken` now support a `context` keyword argument that provides
  additional information displayed on test failure. This is useful for debugging which specific case failed
  in parameterized tests ([#60501]).
* `@test_throws`, `@test_warn`, `@test_nowarn`, `@test_logs`, and `@test_deprecated` now support
  `broken` and `skip` keyword arguments for consistency with `@test` ([#60543]).
* New functions `detect_closure_boxes` and `detect_closure_boxes_all` find methods that allocate `Core.Box`
  in their lowered code, which can indicate performance issues from captured variables in closures ([#60478]).

* `detect_unbound_args` now uses a conservative rule derived from how subtyping assigns values
  to static parameters, instead of older heuristics. It detects previously missed
  possibly-unbound parameters (such as `f(::Type{<:T}) where {T}`, which leaves `T`
  unbound when called with `Union{}`, or `f(::Vector{<:T}) where {T}` with a
  `Vector{Union{}}` argument), and no longer reports methods whose problematic calls are
  all shadowed by more specific methods (such as a `f(::Type{Union{}})` fallback), or
  whose lowered bodies never read the possibly-unbound parameters. Parameters left
  unbound only by calls with `Union{}` type parameters are reported only with the new
  `ambiguous_bottom=true` keyword argument, as for `detect_ambiguities` ([#62405]).

#### Dates

* `unix2datetime` now accepts a keyword argument `localtime=true` to use the host system's local time zone instead of UTC ([#50296]).

#### InteractiveUtils

* `less`/`@less` and `edit`/`@edit` are now supported for documented variables ([#53539]).
* A new `@methods` macro lists all methods applicable to a call expression, using the types of
  the given arguments, e.g. `@methods isvalid('a', 1)` or `@methods isvalid(::AbstractChar, ::Integer)` ([#62311]).

#### Dates

#### TOML

* The parsing functions (`TOML.parsefile`, `TOML.parse`, and their `try` variants) can now capture
  the comments of a document into a `TOML.Comments` object via the new `comments` keyword argument,
  and `TOML.print` can write them back out via its new `comments` keyword argument. This allows
  modifying a TOML file without losing its comments ([#62672]).

External dependencies
---------------------

Tooling Improvements
--------------------

Deprecated or removed
---------------------

* Storing into a `Threads.Atomic` with the plain `a[] = v` form (i.e. `setindex!`) is deprecated in
  favor of `@atomic a[] = v`. The plain form makes read-modify-write expressions such as `a[] += 1`
  look atomic even though they expand to a separate, non-atomic load and store; use `@atomic a[] += 1`
  or `Threads.atomic_add!` for an atomic update. Reading with `a[]` is unchanged ([#62382]).

<!--- generated by NEWS-update.jl: -->
