Julia v1.1 Release Notes
==========================

New language features
---------------------

  * An *exception stack* is maintained on each task to make exception handling
    more robust and enable root cause analysis. The stack may be accessed using
    the experimental function `Base.catch_stack` ([#28878]).
  * The experimental macro `Base.@locals` returns a dictionary of current local variable names
    and values ([#29733]).

Language changes
----------------

  * Parser inputs ending with a comma are now consistently treated as incomplete.
    Previously they were sometimes parsed as tuples, depending on whitespace ([#28506]).
  * Spaces were accidentally allowed in broadcast call syntax, e.g. `f. (x)`. They are now
    disallowed, consistent with normal function call syntax ([#29781]).
  * Big integer literals and command syntax (backticks) are now parsed with the name of
    the macro (`@int128_str`, `@uint128_str`, `@big_str`, `@cmd`) qualified to refer
    to the `Core` module ([#29968]).
  * Using the same name for both a local variable and a static parameter is now an error instead
    of a warning ([#29429]).
  * `findall(in(b), a)` now returns a `CartesianIndex` when `a` is a matrix or a higher-dimensional array,
    for consistency with other `findall` methods. Use `LinearIndices(a)[findall(in(b), a)]` to get
    the old behavior, or `CartesianIndices(a)[findall(in(b), a)]` to get the new behavior
    on previous Julia versions ([#30226]).
  * `findmin(::BitArray)` and `findmax(::BitArray)` now return a `CartesianIndex` when `a` is a matrix
    or a higher-dimensional array, for consistency with for other array types.
    Use `LinearIndices(a)[findmin(a)[2]]` to get the old behavior, or `CartesianIndices(a)[findmin(a)[2]]`
    to get the new behavior on previous Julia versions ([#30102]).
  * Method signatures such as
    `f(::Type{T}, ::T) where {T <: X}` and
    `f(::Type{X}, ::Any)`
    are now considered ambiguous. Previously a bug caused the first one to be considered more specific ([#30160]).

Command-line option changes
---------------------------

  * When a script run in interactive mode (`-i`) throws an error, the REPL now starts after
    the error is displayed. Previously the REPL only started if the script completed without
    error ([#21233]).

New library functions
---------------------

  * `splitpath(p::String)` function, which is the opposite of `joinpath(parts...)`: it splits a filepath into its components ([#28156]).
  * `isnothing(::Any)` function, to check whether something is a `Nothing`, returns a `Bool` ([#29679]).
  * `getpid(::Process)` method ([#24064]).
  * `eachrow`, `eachcol` and `eachslice` functions provide efficient iterators over slices of arrays ([#29749]).
  * `fieldtypes(T::Type)` which return the declared types of the field in type T ([#29600]).
  * `uuid5` has been added to the `UUIDs` standard library ([#28761]).
  * Predicate functions `Sys.isfreebsd`, `Sys.isopenbsd`, `Sys.isnetbsd`, and `Sys.isdragonfly` for
    detecting BSD systems have been added ([#30249]).
  * Internal `Base.disable_library_threading` that sets libraries to use one thread.
    It executes function hooks that have been registered with
    `Base.at_disable_library_threading` ([#30004]).

Standard library changes
------------------------

  * `CartesianIndices` can now be constructed from two `CartesianIndex`es `I` and `J` with `I:J` ([#29440]).
  * `CartesianIndices` support broadcasting arithmetic (+ and -) with a `CartesianIndex` ([#29890]).
  * `copy!` support for arrays, dicts, and sets has been moved to Base from the Future package ([#29173]).
  * Channels now convert inserted values (like containers) instead of requiring types to match ([#29092]).
  * `range` can accept the stop value as a positional argument, e.g. `range(1,10,step=2)` ([#28708]).
  * `diff` now supports arrays of arbitrary dimensionality and can operate over any dimension ([#29827]).
  * The constructor `BigFloat(::BigFloat)` now respects the global precision setting and always
    returns a `BigFloat` with precision equal to `precision(BigFloat)` ([#29127]). The optional
    `precision` argument to override the global setting is now a keyword instead of positional
    argument ([#29157]).
  * The use of scientific notation when printing `BigFloat` values is now consistent with other floating point
    types ([#29211]).
  * `Regex` now behave like a scalar when used in broadcasting ([#29913]).
  * `Char` now behaves like a read-only 0-dimensional array ([#29819]).
  * `parse` now allows strings representing integer 0 and 1 for type `Bool` ([#29980]).
  * `Base.tail` now works on named tuples ([#29595]).
  * The process id is appended to malloc log files in order to track memory allocations of
    multiple processes ([#29969]).
  * `Base.julia_cmd` now propagates the `--inline=(yes|no)` flag ([#29858]).
  * `Base.@kwdef` can now be used for parametric structs, and for structs with supertypes ([#29316]).
  * `merge(::NamedTuple, ::NamedTuple...)` can now be used with more than 2 `NamedTuple`s ([#29259]).
  * `Future.copy!` has been moved to `Base` ([#29178]).
  * New `ncodeunits(c::Char)` method as a fast equivalent to `ncodeunits(string(c))` ([#29153]).
  * New `sort!(::AbstractArray; dims)` method that can sort the array along the `dims` dimension ([#28902]).
  * `range` now accept `stop` as a positional argument ([#28708]).
  * `get(A::AbstractArray, (), default)` now returns the result of `A[]` if it can instead of always
    returning an empty array ([#30270]).
  * `parse(Bool, str)` is now supported ([#29997]).
  * `copyto!(::AbstractMatrix, ::UniformScaling)` supports rectangular matrices now ([#28790]).
  * In `put!(c::Channel{T}, v)`, `v` now gets converted to `T` as `put!` is being called ([#29092]).
  * `current_project()` now searches the parent directories of a Git repository for a `Project.toml` file.
    This also affects the behavior of the `--project` command line option when using the default
    `--project=@.` ([#29108]).
  * The `spawn` API is now more flexible and supports taking IOBuffer directly as a I/O stream,
    converting to a system pipe as needed ([#30278]).

#### Dates
  * New `DateTime(::Date, ::Time)` constructor ([#29754]).
  * `TimeZone` now behave like a scalar when used in broadcasting ([#30159]).

#### InteractiveUtils
  * `edit` can now be called on a module to edit the file that defines it ([#29636]).
  * All compiler-reflection tools (i.e. the `code_` class of functions and macros) now print accurate
    line number and inlining information in a common style, and take an optional parameter (debuginfo=:default)
    to control the verbosity of the metadata shown ([#29893]).

#### LinearAlgebra
  * `isdiag` and `isposdef` for `Diagonal` and `UniformScaling` ([#29638]).
  * `mul!`, `rmul!` and `lmul!` methods for `UniformScaling` ([#29506]).
  * `Symmetric` and `Hermitian` matrices now preserve the wrapper when scaled with a number ([#29469]).
  * Exponentiation operator `^` now supports raising a `Irrational` to an `AbstractMatrix` power ([#29782]).

#### Random
  * `randperm` and `randcycle` now use the type of their argument to determine the element type of
    the returned array ([#29670]).
  * A new method `rand(::Tuple)` implements sampling from the values of a tuple ([#25278]).
  * `serialize` and `deserialize` now accept a filename argument, like `write` and `read` ([#30151]).

#### SparseArrays
  * `sprandn` now supports specifying the output element type ([#30083]).

#### Statistics
  * `mean` and `var` now handles the empty case ([#29033]).

External dependencies
---------------------

  * 7zip (bundled with Julia on Windows) has been upgraded from version 16.04 to 18.05 ([#30035]).
  * Busybox is no longer bundled with Julia on Windows ([#30022]).
  * OpenBLAS has been upgraded from 0.3.2 to 0.3.3 ([#29845]).
  * The source code for Pkg is no longer included in JuliaLang/julia. Pkg is instead
    downloaded during the build process ([#29615]).
  * LLVM has been upgraded to 6.0.1 and support for LLVM < 6.0 has been dropped ([#28745], [#28696]).

Deprecated or removed
---------------------

  * `one(i::CartesianIndex)` should be replaced with `oneunit(i::CartesianIndex)` ([#29442]).
  * The internal array `Base.Grisu.DIGITS` is deprecated; new code should use `Base.Grisu.getbuf()`
    to get an appropriate task-local buffer and pass it to `grisu()` instead ([#29907]).
  * The internal function `Base._default_type(T)` has been removed. Calls to it should be
    replaced with just the argument `T` ([#29739]).
  * `peakflops` has been scheduled to move from `InteractiveUtils` to `LinearAlgebra`
    but is already now available as `LinearAlgebra.peakflops` ([#29978]).

<!--- generated by NEWS-update.jl: -->
[#21233]: https://github.com/JuliaLang/julia/issues/21233
[#24064]: https://github.com/JuliaLang/julia/issues/24064
[#25278]: https://github.com/JuliaLang/julia/issues/25278
[#28156]: https://github.com/JuliaLang/julia/issues/28156
[#28506]: https://github.com/JuliaLang/julia/issues/28506
[#28696]: https://github.com/JuliaLang/julia/issues/28696
[#28708]: https://github.com/JuliaLang/julia/issues/28708
[#28745]: https://github.com/JuliaLang/julia/issues/28745
[#28761]: https://github.com/JuliaLang/julia/issues/28761
[#28790]: https://github.com/JuliaLang/julia/issues/28790
[#28878]: https://github.com/JuliaLang/julia/issues/28878
[#28902]: https://github.com/JuliaLang/julia/issues/28902
[#29033]: https://github.com/JuliaLang/julia/issues/29033
[#29092]: https://github.com/JuliaLang/julia/issues/29092
[#29108]: https://github.com/JuliaLang/julia/issues/29108
[#29127]: https://github.com/JuliaLang/julia/issues/29127
[#29153]: https://github.com/JuliaLang/julia/issues/29153
[#29157]: https://github.com/JuliaLang/julia/issues/29157
[#29173]: https://github.com/JuliaLang/julia/issues/29173
[#29178]: https://github.com/JuliaLang/julia/issues/29178
[#29211]: https://github.com/JuliaLang/julia/issues/29211
[#29259]: https://github.com/JuliaLang/julia/issues/29259
[#29316]: https://github.com/JuliaLang/julia/issues/29316
[#29429]: https://github.com/JuliaLang/julia/issues/29429
[#29440]: https://github.com/JuliaLang/julia/issues/29440
[#29442]: https://github.com/JuliaLang/julia/issues/29442
[#29469]: https://github.com/JuliaLang/julia/issues/29469
[#29506]: https://github.com/JuliaLang/julia/issues/29506
[#29595]: https://github.com/JuliaLang/julia/issues/29595
[#29600]: https://github.com/JuliaLang/julia/issues/29600
[#29615]: https://github.com/JuliaLang/julia/issues/29615
[#29636]: https://github.com/JuliaLang/julia/issues/29636
[#29638]: https://github.com/JuliaLang/julia/issues/29638
[#29670]: https://github.com/JuliaLang/julia/issues/29670
[#29679]: https://github.com/JuliaLang/julia/issues/29679
[#29733]: https://github.com/JuliaLang/julia/issues/29733
[#29739]: https://github.com/JuliaLang/julia/issues/29739
[#29749]: https://github.com/JuliaLang/julia/issues/29749
[#29754]: https://github.com/JuliaLang/julia/issues/29754
[#29781]: https://github.com/JuliaLang/julia/issues/29781
[#29782]: https://github.com/JuliaLang/julia/issues/29782
[#29819]: https://github.com/JuliaLang/julia/issues/29819
[#29827]: https://github.com/JuliaLang/julia/issues/29827
[#29845]: https://github.com/JuliaLang/julia/issues/29845
[#29858]: https://github.com/JuliaLang/julia/issues/29858
[#29890]: https://github.com/JuliaLang/julia/issues/29890
[#29893]: https://github.com/JuliaLang/julia/issues/29893
[#29907]: https://github.com/JuliaLang/julia/issues/29907
[#29913]: https://github.com/JuliaLang/julia/issues/29913
[#29968]: https://github.com/JuliaLang/julia/issues/29968
[#29969]: https://github.com/JuliaLang/julia/issues/29969
[#29978]: https://github.com/JuliaLang/julia/issues/29978
[#29980]: https://github.com/JuliaLang/julia/issues/29980
[#29997]: https://github.com/JuliaLang/julia/issues/29997
[#30022]: https://github.com/JuliaLang/julia/issues/30022
[#30035]: https://github.com/JuliaLang/julia/issues/30035
[#30083]: https://github.com/JuliaLang/julia/issues/30083
[#30159]: https://github.com/JuliaLang/julia/issues/30159
[#30249]: https://github.com/JuliaLang/julia/issues/30249
