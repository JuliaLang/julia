Julia v1.13 Release Notes
=========================

New language features
---------------------

* New `@__FUNCTION__` macro to refer to the innermost enclosing function ([#58940]).
* The character U+1F8B2 🢲 (RIGHTWARDS ARROW WITH LOWER HOOK), newly added by Unicode 16,
  is now a valid operator with arrow precedence, accessible as `\hookunderrightarrow` at the REPL
  ([JuliaLang/JuliaSyntax.jl#525], [#57143]).
* Support for Unicode 17 ([#59534]).

Language changes
----------------

* The `hash` algorithm and its values have changed for certain types, most notably `AbstractString`. Any `hash` specializations for equal types to those that changed, such as some third-party string packages, may need to be deleted ([#57509], [#59691]).
* The `hash(::AbstractString)` function is now a zero-copy / zero-cost function, based upon providing a correct implementation of the `codeunit` and `iterate` functions. Third-party string packages should migrate to the new algorithm by deleting their existing overrides of the `hash` function ([#59691]).

Command-line option changes
---------------------------

* The option `--sysimage-native-code=no` has been deprecated.
* The `JULIA_CPU_TARGET` environment variable now supports a `sysimage` keyword to match (or extend) the CPU target used to build the current system image ([#58970]).
* The `--code-coverage=all` option now automatically throws away sysimage caches so that code coverage can be accurately measured on methods within the sysimage. It is thrown away after startup (and after startup.jl), before any user code is executed ([#59234]).
* New `--trace-eval` command-line option to show expressions being evaluated during top-level evaluation. Supports `--trace-eval=loc` or just `--trace-eval` (show location only), `--trace-eval=full` (show full expressions), and `--trace-eval=no` (disable tracing). Also adds `Base.TRACE_EVAL` global control that takes priority over the command-line option and can be set to `:no`, `:loc`, `:full`, or `nothing` (to use command-line setting) ([#57137]).
* Julia now automatically enables verbose debugging options (`--trace-eval` and `JULIA_TEST_VERBOSE`) when CI debugging has been triggered. i.e. via the "debug logging" UI toggle is enabled on github actions re-runs. Other platforms are supported too ([#59551]).

Multi-threading changes
-----------------------

* A new `AbstractSpinLock` is defined with `SpinLock <: AbstractSpinLock` ([#55944]).
* A new `PaddedSpinLock <: AbstractSpinLock` is defined. It has extra padding to avoid false sharing ([#55944]).

New library functions
---------------------

* `Base.@acquire` macro for a non-closure version of `Base.acquire(f, s::Base.Semaphore)`, like `@lock` ([#56845]).
* `nth` function to access the `n`-th element of a generic iterable ([#56580]).
* `ispositive(::Real)` and `isnegative(::Real)` are provided for performance and convenience ([#53677]).
* The `fieldindex` function (to get the index of a struct's field) is now exported ([#58119]).
* `Base.donotdelete` is now public. It prevents dead code elimination of its arguments ([#55774]).
* `Sys.sysimage_target()` returns the CPU target string used to build the current system image ([#58970]).
* `Iterators.findeach` is a lazy version of `findall` ([#54124]).

New library features
--------------------

* `fieldoffset` now also accepts the field name as a symbol as `fieldtype` already did ([#58100]).
* `sort(keys(::Dict))` and `sort(values(::Dict))` now automatically collect; they previously threw ([#56978]).
* `Base.AbstractOneTo` is added as a supertype of one-based axes, with `Base.OneTo` as its subtype ([#56902]).
* `takestring!(::IOBuffer)` removes the content from the buffer, returning the content as a `String`.
* `chopprefix` and `chopsuffix` can now also accept an `AbstractChar` as the prefix/suffix to remove.
* The `macroexpand` (with default true) and the new `macroexpand!` (with default false)
  functions now support a `legacyscope` boolean keyword argument to control whether to run
  the legacy scope resolution pass over the result. The legacy scope resolution code has
  known design bugs and will be disabled by default in a future version. Users should
  migrate now by calling `legacyscope=false` or using `macroexpand!`. This may often require
  fixes to the code calling `macroexpand` with `Meta.unescape` and `Meta.reescape` or by
  updating tests to expect `hygienic-scope` or `escape` markers might appear in the result.
* `Base.ScopedValues.LazyScopedValue{T}` is introduced for scoped values that compute their default using a
  `OncePerProcess{T}` callback, allowing for lazy initialization of the default value. `AbstractScopedValue` is
  now the abstract base type for both `ScopedValue` and `LazyScopedValue` ([#59372]).
* New `Base.active_manifest()` function to return the path of the active manifest, like `Base.active_project()`.
  Also can return the manifest that would be used for a given project file ([#57937]).

Standard library changes
------------------------

* `mod(x::AbstractFloat, -Inf)` now returns `x` (as long as `x` is finite). This aligns with the C standard and is considered a bug fix ([#47102]).
* Indexless `getindex` and `setindex!` (i.e. `A[]`) on `ReinterpretArray` now correctly throw a `BoundsError` when there is more than one element ([#58814]).
* `randperm!` and `randcycle!` now support non-`Array` `AbstractArray` inputs, assuming they are mutable and their indices are one-based ([#58596]).
* `shuffle` now accepts `NTuple` arguments ([#56906]).

#### REPL

* The Julia REPL now supports bracketed paste on Windows, which should significantly speed up pasting large code blocks into the REPL ([#59825]).
* The REPL now provides syntax highlighting for input as you type. See the REPL docs for more info about customization.
* The REPL now supports automatic insertion of closing brackets, parentheses, and quotes. See the REPL docs for more info about customization.
* History searching has been rewritten to use a new interactive modal dialogue, using a fzf-like style.
* The display of `AbstractChar`s in the main REPL mode now includes LaTeX input information like what is shown in help mode ([#58181]).
* Display of repeated frames and cycles in stack traces has been improved by bracketing them in the trace and treating them consistently ([#55841]).
* The superscript character U+107A5 𐞥 (MODIFIER LETTER SMALL Q), which was already supported in the language, can now be accessed at the REPL with `\^q` ([#59544]).

#### Test

* `Test` now supports the `JULIA_TEST_VERBOSE` environment variable. When set to `true`,
  it enables verbose testset entry/exit messages with timing information and sets the default `verbose=true`
  for `DefaultTestSet` to show detailed hierarchical test summaries ([#59295]).
* Test failures when using the `@test` macro now show evaluated arguments for all function calls ([#57825], [#57839]).
* Transparent test sets (`@testset let`) now show context when tests error ([#58727]).
* `@test_throws` now supports a three-argument form `@test_throws ExceptionType pattern expr` to test both exception type and message pattern in one call ([#59117]).
* The testset stack was changed to use `ScopedValue` rather than task local storage ([#53462]).

#### InteractiveUtils

* Introspection utilities such as `@code_typed`, `@which` and `@edit` now accept type annotations as substitutes for values, recognizing forms such as `f(1, ::Float64, 3)` or even `sum(::Vector{T}; init = ::T) where {T<:Real}`. Type-annotated variables as in `f(val::Int; kw::Float64)` are not evaluated if the type annotation provides the necessary information, making this syntax compatible with signatures found in stacktraces ([#57909], [#58222]).
* Code introspection macros such as `@code_lowered` and `@code_typed` now have a much better support for broadcasting expressions, including broadcasting assignments of the form `x .+= f(y)` ([#58349]).

#### Dates

* `isoweekdate`, `isoyear`, `weeksinyear` are now implemented and exported for week based calendars, following [ISO week date](https://en.wikipedia.org/wiki/ISO_week_date) ([#48507]).

External dependencies
---------------------

* 7-Zip updated from p7zip v17.06 to upstream 7-Zip v25.01. On Windows, the full 7z.exe/7z.dll bundle is replaced with standalone 7za.exe, which supports fewer formats but unifies cross-platform behavior ([#60025]).

Deprecated or removed
---------------------

* The method `merge(combine::Callable, d::AbstractDict...)` is now deprecated to favor `mergewith` instead ([#59775]).

Deprecated or removed
---------------------

* The method `merge(combine::Callable, d::AbstractDict...)` is now deprecated to favor `mergewith` instead ([#59775]).

<!--- generated by NEWS-update.jl: -->
[#47102]: https://github.com/JuliaLang/julia/issues/47102
[#48507]: https://github.com/JuliaLang/julia/issues/48507
[#53462]: https://github.com/JuliaLang/julia/issues/53462
[#53677]: https://github.com/JuliaLang/julia/issues/53677
[#54124]: https://github.com/JuliaLang/julia/issues/54124
[#55774]: https://github.com/JuliaLang/julia/issues/55774
[#55841]: https://github.com/JuliaLang/julia/issues/55841
[#55944]: https://github.com/JuliaLang/julia/issues/55944
[#56580]: https://github.com/JuliaLang/julia/issues/56580
[#56845]: https://github.com/JuliaLang/julia/issues/56845
[#56902]: https://github.com/JuliaLang/julia/issues/56902
[#56906]: https://github.com/JuliaLang/julia/issues/56906
[#56978]: https://github.com/JuliaLang/julia/issues/56978
[#57137]: https://github.com/JuliaLang/julia/issues/57137
[#57143]: https://github.com/JuliaLang/julia/issues/57143
[#57509]: https://github.com/JuliaLang/julia/issues/57509
[#57825]: https://github.com/JuliaLang/julia/issues/57825
[#57839]: https://github.com/JuliaLang/julia/issues/57839
[#57909]: https://github.com/JuliaLang/julia/issues/57909
[#57937]: https://github.com/JuliaLang/julia/issues/57937
[#58100]: https://github.com/JuliaLang/julia/issues/58100
[#58119]: https://github.com/JuliaLang/julia/issues/58119
[#58181]: https://github.com/JuliaLang/julia/issues/58181
[#58222]: https://github.com/JuliaLang/julia/issues/58222
[#58349]: https://github.com/JuliaLang/julia/issues/58349
[#58596]: https://github.com/JuliaLang/julia/issues/58596
[#58727]: https://github.com/JuliaLang/julia/issues/58727
[#58814]: https://github.com/JuliaLang/julia/issues/58814
[#58940]: https://github.com/JuliaLang/julia/issues/58940
[#58970]: https://github.com/JuliaLang/julia/issues/58970
[#59117]: https://github.com/JuliaLang/julia/issues/59117
[#59234]: https://github.com/JuliaLang/julia/issues/59234
[#59295]: https://github.com/JuliaLang/julia/issues/59295
[#59372]: https://github.com/JuliaLang/julia/issues/59372
[#59534]: https://github.com/JuliaLang/julia/issues/59534
[#59544]: https://github.com/JuliaLang/julia/issues/59544
[#59551]: https://github.com/JuliaLang/julia/issues/59551
[#59691]: https://github.com/JuliaLang/julia/issues/59691
[#59775]: https://github.com/JuliaLang/julia/issues/59775
[#59825]: https://github.com/JuliaLang/julia/issues/59825
[#60025]: https://github.com/JuliaLang/julia/issues/60025
