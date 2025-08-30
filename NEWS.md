Julia v1.13 Release Notes
========================

New language features
---------------------

  - New `Base.@acquire` macro for a non-closure version of `Base.acquire(f, s::Base.Semaphore)`, like `@lock`. ([#56845])
  - New `nth` function to access the `n`-th element of a generic iterable. ([#56580])
  - New `@__FUNCTION__` macro to refer to the innermost enclosing function. ([#58940])
  - The character U+1F8B2 🢲 (RIGHTWARDS ARROW WITH LOWER HOOK), newly added by Unicode 16,
    is now a valid operator with arrow precedence, accessible as `\hookunderrightarrow` at the REPL.
    ([JuliaLang/JuliaSyntax.jl#525], [#57143])

Language changes
----------------
* `mod(x::AbstractFloat, -Inf)` now returns `x` (as long as `x` is finite), this aligns with C standard and
is considered a bug fix ([#47102])

  - The `hash` algorithm and its values have changed. Most `hash` specializations will remain correct and require no action. Types that reimplement the core hashing logic independently, such as some third-party string packages do, may require a migration to the new algorithm. ([#57509])

* Indexless `getindex` and `setindex!` (i.e. `A[]`) on `ReinterpretArray` now correctly throw a `BoundsError` when there is more than one element. ([#58814])

Compiler/Runtime improvements
-----------------------------

Command-line option changes
---------------------------

* The option `--sysimage-native-code=no` has been deprecated.
* The `JULIA_CPU_TARGET` environment variable now supports a `sysimage` keyword to match (or extend) the CPU target used to build the current system image ([#58970]).
* The `--code-coverage=all` option now automatically throws away sysimage caches so that code coverage can be accurately measured on methods within the sysimage. It is thrown away after startup (and after startup.jl), before any user code is executed ([#59234])

Multi-threading changes
-----------------------

* A new `AbstractSpinLock` is defined with `SpinLock <: AbstractSpinLock` ([#55944]).
* A new `PaddedSpinLock <: AbstractSpinLock` is defined.  It has extra padding to avoid false sharing ([#55944]).
* New types are defined to handle the pattern of code that must run once per process, called
  a `OncePerProcess{T}` type, which allows defining a function that should be run exactly once
  the first time it is called, and then always return the same result value of type `T`
  every subsequent time afterwards. There are also `OncePerThread{T}` and `OncePerTask{T}` types for
  similar usage with threads or tasks. ([#TBD])

Build system changes
--------------------

New library functions
---------------------

* `ispositive(::Real)` and `isnegative(::Real)` are provided for performance and convenience ([#53677]).
* Exporting function `fieldindex` to get the index of a struct's field ([#58119]).
* `Base.donotdelete` is now public. It prevents deadcode elemination of its arguments ([#55774]).
* `Sys.sysimage_target()` returns the CPU target string used to build the current system image ([#58970]).
* `Iterators.findeach` is a lazy version of `findall` ([#54124])

New library features
--------------------

* `fieldoffset` now also accepts the field name as a symbol as `fieldtype` already did ([#58100]).
* `sort(keys(::Dict))` and `sort(values(::Dict))` now automatically collect, they previously threw ([#56978]).
* `Base.AbstractOneTo` is added as a supertype of one-based axes, with `Base.OneTo` as its subtype ([#56902]).
* `takestring!(::IOBuffer)` removes the content from the buffer, returning the content as a `String`.
* The `macroexpand` (with default true) and the new `macroexpand!` (with default false)
  functions now support a `legacyscope` boolean keyword argument to control whether to run
  the legacy scope resolution pass over the result. The legacy scope resolution code has
  known design bugs and will be disabled by default in a future version. Users should
  migrate now by calling `legacyscope=false` or using `macroexpand!`. This may often require
  fixes to the code calling `macroexpand` with `Meta.unescape` and `Meta.reescape` or by
  updating tests to expect `hygienic-scope` or `escape` markers might appear in the result.
* `Base.ScopedValues.LazyScopedValue{T}` is introduced for scoped values that compute their default using a
  `OncePerProcess{T}` callback, allowing for lazy initialization of the default value. `AbstractScopedValue` is
  now the abstract base type for both `ScopedValue` and `LazyScopedValue`. ([#59372])

Standard library changes
------------------------

#### JuliaSyntaxHighlighting

#### LinearAlgebra

#### Profile

#### Random

* `randperm!` and `randcycle!` now support non-`Array` `AbstractArray` inputs, assuming they are mutable and their indices are one-based ([#58596]).

* `shuffle` now may take an argument of `NTuple` value ([#56906]).

#### REPL

* The display of `AbstractChar`s in the main REPL mode now includes LaTeX input information like what is shown in help mode ([#58181]).
* Display of repeated frames and cycles in stack traces has been improved by bracketing them in the trace and treating them consistently ([#55841]).

#### Test

* Test failures when using the `@test` macro now show evaluated arguments for all function calls ([#57825], [#57839]).
* Transparent test sets (`@testset let`) now show context when tests error ([#58727]).
* `@test_throws` now supports a three-argument form `@test_throws ExceptionType pattern expr` to test both exception type and message pattern in one call ([#59117]).
* The testset stack was changed to use `ScopedValue` rather than task local storage ([#53462]).

#### InteractiveUtils

* Introspection utilities such as `@code_typed`, `@which` and `@edit` now accept type annotations as substitutes for values, recognizing forms such as `f(1, ::Float64, 3)` or even `sum(::Vector{T}; init = ::T) where {T<:Real}`. Type-annotated variables as in `f(val::Int; kw::Float64)` are not evaluated if the type annotation provides the necessary information, making this syntax compatible with signatures found in stacktraces ([#57909], [#58222]).
* Code introspection macros such as `@code_lowered` and `@code_typed` now have a much better support for broadcasting expressions, including broadcasting assignments of the form `x .+= f(y)` ([#58349]).

External dependencies
---------------------

Tooling Improvements
--------------------

<!--- generated by NEWS-update.jl: -->
