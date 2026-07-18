Julia v1.14 Release Notes
========================

New language features
---------------------

  - It is now possible to control which version of the Julia syntax will be used to parse a package by setting the
    `compat.julia` or `syntax.julia_version` key in Project.toml. This feature is similar to the notion of "editions"
    in other language ecosystems and will allow non-breaking evolution of Julia syntax in future versions.
    See the "Syntax Versioning" section in the code loading documentation ([#60018]).
  - `ᵅ` (U+U+1D45), `ᵋ` (U+1D4B), `ᶲ` (U+1DB2), `˱` (U+02F1), `˲` (U+02F2), and `ₔ` (U+2094) can now also be used as
    operator suffixes, accessible as `\^alpha`, `\^epsilon`, `\^ltphi`, `\_<`, `\_>`, and `\_schwa` at the REPL
    ([#60285]).
  - The `@label` macro can now create labeled blocks that can be exited early with `break name [value]`. Use
    `@label name expr` for named blocks or `@label expr` for anonymous blocks. Anonymous `@label` blocks
    participate in the default break scope: a plain `break` or `break _` exits the innermost breakable scope,
    whether it is a loop or an `@label` block. The `continue` statement also supports labels with
    `continue name` to continue a labeled loop ([#60481]).
  - `typegroup` blocks allow defining mutually recursive struct types that reference each other in their
    field types. All types in the group are resolved atomically at the end of the block ([#60569]).

Language changes
----------------

  - `Type{T} <: S` now holds only if every type `==` to `T` is an instance of `S`, fixing a
    long-standing soundness hole where e.g. `Type{Int} <: DataType` held even though types like
    `Tuple{S} where S<:Int` are `==` (and `isa`) their canonical spelling without being `DataType`s.
    In particular `Type{T}` is no longer a subtype of any single kind: use a union of kinds instead
    (e.g. `Type{Int} <: Union{DataType,UnionAll}` holds). `isa` and dispatch of type *values* are
    unaffected, and a method on `Type{Int}` remains more specific than one on `DataType`
    ([#33136], [#62141]).
  - Introduced explicitly wrapping arithmetic operators `+%`, `-%`, `*%` to annotate arithmetic operations
    that are semantically safe to wrap/overflow. Their behavior is currently identical to the default `+`, `-`, `*`
    operators. However, in a future version, there may be opt-in support to detect unannotated wrapping
    in the default operators ([#50790]).

Compiler/Runtime improvements
-----------------------------

  - Type inference now refines field types through conditional checks and call signatures.
    For example, after `if !isnothing(x.field)`, inference knows `x.field` is not `nothing`
    within the branch. Similarly, after a call like `func(x.field)` where `func(::Int)` is
    the only matching method, inference refines `x.field` to `Int`.
    This works for immutable struct fields and `const` fields of mutable structs.
    Mutable (non-`const`) fields are not supported due to the lack of per-object memory
    effect tracking; for those, the recommended pattern remains storing the field value in
    a local variable before the check (e.g. `val = x.field; if !isnothing(val) ... end`)
    ([#41199], [#47574]).
  - Stack traces now show full method signatures with argument types for inlined
    frames, matching the display of non-inlined frames ([#53925]).
  - Parallel package precompilation now coordinates CPU usage across both the
    precompile worker processes and the LLVM threads each spawns to compile its
    native image, sharing a single thread budget so idle cores are filled during
    the long tail without oversubscribing the machine when many packages compile
    at once. The total budget can be set with the new `JULIA_PRECOMPILE_THREADS`
    environment variable ([#61958]).

Command-line option changes
---------------------------

  - `-P <project>` is now a shorthand for `--project <project>` ([#59867]).

Multi-threading changes
-----------------------

  - New functions `Threads.atomic_fence_heavy` and `Threads.atomic_fence_light` provide support for
    asymmetric atomic fences, speeding up atomic synchronization where one side of the synchronization
    runs significantly less often than the other ([#60311]).
  - `Threads.@threads` now supports array comprehensions with syntax like `@threads [f(i) for i in 1:n]`,
    filtered comprehensions like `@threads [f(i) for i in 1:n if condition(i)]`, typed comprehensions
    like `@threads Float64[f(i) for i in 1:n]`, and multi-dimensional comprehensions like
    `@threads [f(i,j) for i in 1:n, j in 1:m]` (preserves dimensions). All scheduling options
    (`:static`, `:dynamic`, `:greedy`) are supported. Results preserve element order for `:static`
    and `:dynamic` scheduling; `:greedy` does not guarantee order. Non-indexable iterators are
    also supported. ([#59019])
  - The task scheduler now avoids O(nthreads) wake overhead on every `@spawn`, significantly reducing
    threading overhead particularly on highly oversubscribed machines. Benchmarks show up to 1000x
    reduction in spawn time in such scenarios ([#61826]).

Build system changes
--------------------

New library functions
---------------------

* `tap(f)` creates a function that calls `f(x)` for side effects and returns `x`. ([#61340]).
* `Base.set_binding_visibility!` sets the declared visibility (`:none`, `:public`, or
  `:export`) of a name in a module, allowing an `export` or `public` declaration to be
  retracted programmatically ([#62131]).
* `Base.generating_output()` has been made `public` (but not exported) to allow
  checking whether the current process is performing compilation for a
  pkgimage/sysimage ([#61224]).
- `Base.raw_substring` is an unexported, public constructor to build a `SubString`
  without checking for valid string indices.
- `Base.unannotate(::AnnotatedString)` returns the underlying un-annotated string
  of the input string.
- `Base.include_mapexprs(mod)` is an unexported, public function returning the non-identity
  `mapexpr` functions used by `include(mapexpr, …)` calls while loading the package rooted at
  `mod`, keyed by `(including_module, absolute_path)`. The table is stored inside the package
  image, so it survives precompilation; revision tools (e.g. Revise) use it to re-apply the
  original transform when an `include(mapexpr, …)`-ed file is edited.

New library features
--------------------

* `IOContext` supports a new boolean `hexunsigned` option that allows for
  printing unsigned integers in decimal instead of hexadecimal ([#60267]).
* `lazy"..."` strings now support a flag `lazy"..."c` that adds `compact` and `limit` flags
  to the `IOContext` for final output-string generation ([#61887]).
* The `StringView` type wraps an `AbstractVector{UInt8}` and interprets it as a UTF-8 encoded string,
  superseding the [StringViews.jl](https://github.com/JuliaStrings/StringViews.jl) package ([#60526]).
* Package precompilation now supports running precompilation in
  a background task and has new interactive keyboard controls:
  `c` to cleanly cancel immediately, `d` to detach, `i` for a profile peek,
  `v` to toggle verbose mode showing elapsed time, CPU%, and memory usage, and `?` for help. ([#60943]).
* Instances of an `Enum` can now be given their own docstrings within the `@enum` definition ([#61955]).
* New methods `readdir(path, DirEntry)` and `readdir(::DirEntry, DirEntry)` return directory contents
  along with the type of the entries in a vector of new `DirEntry` objects to provide more efficient `isfile`
  etc. checks. `readdir(::DirEntry)` accepts a `DirEntry` as input and, like `readdir(::AbstractString)`,
  returns a `Vector{String}` of names. `DirEntry` is exported from `Base` ([#55358]).

Standard library changes
------------------------

* `codepoint(c)` now succeeds for overlong encodings.  `Base.ismalformed`, `Base.isoverlong`, and
  `Base.show_invalid` are now `public` and documented (but not exported) ([#55152]).

#### JuliaSyntaxHighlighting

#### LinearAlgebra

#### Markdown

* Support "raw" or "inline" HTML inside Markdown data ([#60629], [#60632], [#60732])
* Support autolinks for email addresses (#60570)
* Many improvements and bugfixes for rendering Markdown lists in a terminal ([#55456], [#60519])
* Strikethrough text via `~strike~` or `~~through~~` is now supported by the Markdown parser. ([#60537])
* Many, many bug fixes and minor tweaks; overall behavior is now much closer to CommonMark ([#59977], [#60502])

#### Profile

#### Random

#### REPL

#### Test

* `@test`, `@test_throws`, and `@test_broken` now support a `context` keyword argument
  that provides additional information displayed on test failure. This is useful for
  debugging which specific case failed in parameterized tests ([#60501]).

* `@test_throws`, `@test_warn`, `@test_nowarn`, `@test_logs`, and `@test_deprecated` now support
  `broken` and `skip` keyword arguments for consistency with `@test` ([#60543]).

* New functions `detect_closure_boxes` and `detect_closure_boxes_all` find methods that
  allocate `Core.Box` in their lowered code, which can indicate performance issues from
  captured variables in closures.

#### Dates

* `unix2datetime` now accepts a keyword argument `localtime=true` to use the host system's local time zone instead of UTC ([#50296]).

#### InteractiveUtils

* `less`/`@less` and `edit`/`@edit` are now supported for documented variables ([#53539]).
* A new `@methods` macro lists all methods applicable to a call expression, using the types of
  the given arguments, e.g. `@methods isvalid('a', 1)` or `@methods isvalid(::AbstractChar, ::Integer)`.

#### Dates

External dependencies
---------------------

Tooling Improvements
--------------------

Deprecated or removed
---------------------

<!--- generated by NEWS-update.jl: -->
