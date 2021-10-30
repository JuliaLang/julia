Julia v1.8 Release Notes
========================


New language features
---------------------

* `Module(:name, false, false)` can be used to create a `module` that contains no names (it does not import `Base` or `Core` and does not contain a reference to itself). ([#40110, #42154])
* `@inline` and `@noinline` annotations can be used within a function body to give an extra
  hint about the inlining cost to the compiler. ([#41312])
* `@inline` and `@noinline` annotations can now be applied to a function callsite or block
  to enforce the involved function calls to be (or not to be) inlined. ([#41312])
* The default behavior of observing `@inbounds` declarations is now an option via `auto` in `--check-bounds=yes|no|auto` ([#41551])
* New function `eachsplit(str)` for iteratively performing `split(str)`.
* `∀`, `∃`, and `∄` are now allowed as identifier characters ([#42314]).
* `try`-blocks can now optionally have an `else`-block which is executed right after the main body only if
  no errors were thrown. ([#42211])

Language changes
----------------

* Newly created Task objects (`@spawn`, `@async`, etc.) now adopt the world-age for methods from their parent
  Task upon creation, instead of using the global latest world at start. This is done to enable inference to
  eventually optimize these calls. Places that wish for the old behavior may use `Base.invokelatest`. ([#41449])
* `@time` and `@timev` now take an optional description to allow annotating the source of time reports.
  i.e. `@time "Evaluating foo" foo()` ([#42431])
* New `@showtime` macro to show both the line being evaluated and the `@time` report ([#42431])

Compiler/Runtime improvements
-----------------------------

* The LLVM-based compiler has been separated from the run-time library into a new library,
  `libjulia-codegen`. It is loaded by default, so normal usage should see no changes.
  In deployments that do not need the compiler (e.g. system images where all needed code
  is precompiled), this library (and its LLVM dependency) can simply be excluded ([#41936]).

Command-line option changes
---------------------------


Multi-threading changes
-----------------------


Build system changes
--------------------


New library functions
---------------------

* `hardlink(src, dst)` can be used to create hard links. ([#41639])
* `diskstat(path=pwd())` can be used to return statistics about the disk. ([#42248])

New library features
--------------------

* `@test_throws "some message" triggers_error()` can now be used to check whether the displayed error text
  contains "some message" regardless of the specific exception type.
  Regular expressions, lists of strings, and matching functions are also supported. ([#41888])
* `@testset foo()` can now be used to create a test set from a given function. The name of the test set
  is the name of the called function. The called function can contain `@test` and other `@testset`
  definitions, including to other function calls, while recording all intermediate test results. ([#42518])

Standard library changes
------------------------

* `range` accepts either `stop` or `length` as a sole keyword argument ([#39241])
* `precision` and `setprecision` now accept a `base` keyword ([#42428]).
* The `length` function on certain ranges of certain specific element types no longer checks for integer
  overflow in most cases. The new function `checked_length` is now available, which will try to use checked
  arithmetic to error if the result may be wrapping. Or use a package such as SaferIntegers.jl when
  constructing the range. ([#40382])
* TCP socket objects now expose `closewrite` functionality and support half-open mode usage ([#40783]).
* Intersect returns a result with the eltype of the type-promoted eltypes of the two inputs ([#41769]).
* `Iterators.countfrom` now accepts any type that defines `+`. ([#37747])

#### InteractiveUtils
* A new macro `@time_imports` for reporting any time spent importing packages and their dependencies ([#41612])

#### Package Manager

#### LinearAlgebra

#### Markdown

#### Printf
* Now uses `textwidth` for formatting `%s` and `%c` widths ([#41085]).

#### Profile
* Profiling now records sample metadata including thread and task. `Profile.print()` has a new `groupby` kwarg that allows
  grouping by thread, task, or nested thread/task, task/thread, and `threads` and `tasks` kwargs to allow filtering.
  Further, percent utilization is now reported as a total or per-thread, based on whether the thread is idle or not at
  each sample. `Profile.fetch()` by default strips out the new metadata to ensure backwards compatibility with external
  profiling data consumers, but can be included with the `include_meta` kwarg. ([#41742])

#### Random

#### REPL
* `RadioMenu` now supports optional `keybindings` to directly select options ([#41576]).
* ` ?(x, y` followed by TAB displays all methods that can be called
  with arguments `x, y, ...`. (The space at the beginning prevents entering help-mode.)
  `MyModule.?(x, y` limits the search to `MyModule`. TAB requires that at least one
  argument have a type more specific than `Any`; use SHIFT-TAB instead of TAB
  to allow any compatible methods.

#### SparseArrays

#### Dates

#### Downloads

#### Statistics

#### Sockets

#### Tar

#### Distributed

#### UUIDs

#### Mmap

#### DelimitedFiles

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


Deprecated or removed
---------------------


External dependencies
---------------------


Tooling Improvements
---------------------


<!--- generated by NEWS-update.jl: -->
