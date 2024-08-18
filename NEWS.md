Julia v1.12 Release Notes
========================

New language features
---------------------

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

 - Macro expansion will no longer eagerly recurse into into `Expr(:toplevel)`
   expressions returned from macros. Instead, macro expansion of `:toplevel`
   expressions will be delayed until evaluation time. This allows a later
   expression within a given `:toplevel` expression to make use of macros
   defined earlier in the same `:toplevel` expression. ([#53515])

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
  This `main` function should be declared using `@main` to indicate that it is an entry point.
* Enabling or disabling color text in Julia can now be controlled with the
[`NO_COLOR`](https://no-color.org/) or [`FORCE_COLOR`](https://force-color.org/) environment
variables. ([#53742]).
* `--project=@temp` starts Julia with a temporary environment.
* New `--trace-compile-timing` option to report how long each method reported by `--trace-compile` took
  to compile, in ms. ([#54662])

Multi-threading changes
-----------------------

Build system changes
--------------------

* There are new `Makefile`s to build Julia and LLVM using the Binary Optimization and Layout Tool (BOLT), see  `contrib/bolt` and `contrib/pgo-lto-bolt` ([#54107]).

New library functions
---------------------

* `logrange(start, stop; length)` makes a range of constant ratio, instead of constant step ([#39071])
* The new `isfull(c::Channel)` function can be used to check if `put!(c, some_value)` will block. ([#53159])
* `waitany(tasks; throw=false)` and `waitall(tasks; failfast=false, throw=false)` which wait multiple tasks at once ([#53341]).
* `uuid7()` creates an RFC 9652 compliant UUID with version 7 ([#54834]).
* `insertdims(array; dims)` allows to insert singleton dimensions into an array which is the inverse operation to `dropdims`
* The new `Fix` type is a generalization of `Fix1/Fix2` for fixing a single argument ([#54653]).

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
* `@timed` now additionally returns the elapsed compilation and recompilation time ([#52889])
* `escape_string` takes additional keyword arguments `ascii=true` (to escape all
  non-ASCII characters) and `fullhex=true` (to require full 4/8-digit hex numbers
  for u/U escapes, e.g. for C compatibility) [#55099]).
* `filter` can now act on a `NamedTuple` ([#50795]).
* `tempname` can now take a suffix string to allow the file name to include a suffix and include that suffix in
  the uniquing checking ([#53474])
* `RegexMatch` objects can now be used to construct `NamedTuple`s and `Dict`s ([#50988])
* `Lockable` is now exported ([#54595])
* New `ltruncate`, `rtruncate` and `ctruncate` functions for truncating strings to text width, accounting for char widths ([#55351])

Standard library changes
------------------------

* `gcdx(0, 0)` now returns `(0, 0, 0)` instead of `(0, 1, 0)` ([#40989]).
* `fd` returns a `RawFD` instead of an `Int` ([#55080]).

#### StyledStrings

#### JuliaSyntaxHighlighting

* A new standard library for applying syntax highlighting to Julia code, this
  uses `JuliaSyntax` and `StyledStrings` to implement a `highlight` function
  that creates an `AnnotatedString` with syntax highlighting applied.

#### Package Manager

#### LinearAlgebra

* `rank` can now take a `QRPivoted` matrix to allow rank estimation via QR factorization ([#54283]).
* Added keyword argument `alg` to `eigen`, `eigen!`, `eigvals` and `eigvals!` for self-adjoint
  matrix types (i.e., the type union `RealHermSymComplexHerm`) that allows one to switch
  between different eigendecomposition algorithms ([#49355]).
* Added a generic version of the (unblocked) pivoted Cholesky decomposition
  (callable via `cholesky[!](A, RowMaximum())`) ([#54619]).

#### Logging

#### Printf

#### Profile

#### Random

#### REPL

- Using the new `usings=true` feature of the `names()` function, REPL completions can now
  complete names that have been explicitly `using`-ed. ([#54610])
- REPL completions can now complete input lines like `[import|using] Mod: xxx|` e.g.
  complete `using Base.Experimental: @op` to `using Base.Experimental: @opaque`. ([#54719])
- the REPL will now warn if it detects a name is being accessed from a module which does not define it (nor has a submodule which defines it),
  and for which the name is not public in that module. For example, `map` is defined in Base, and executing `LinearAlgebra.map`
  in the REPL will now issue a warning the first time occurs. ([#54872])

#### SuiteSparse

#### SparseArrays

#### Test

#### Dates

#### Statistics

#### Distributed

#### Unicode

#### DelimitedFiles

#### InteractiveUtils

Deprecated or removed
---------------------

External dependencies
---------------------

- The terminal info database, `terminfo`, is now vendored by default, providing a better
  REPL user experience when `terminfo` is not available on the system. Julia can be built
  without vendoring the database using the Makefile option `WITH_TERMINFO=0`. ([#55411])

Tooling Improvements
--------------------

<!--- generated by NEWS-update.jl: -->
