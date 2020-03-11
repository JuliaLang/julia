Julia v1.5 Release Notes
========================

New language features
---------------------
* Macro calls `@foo {...}` can now also be written `@foo{...}` (without the space) ([#34498]).
* `⨟` is now parsed as a binary operator with times precedence. It can be entered in the REPL
  with `\bbsemi` followed by <kbd>TAB</kbd> ([#34722]).

* `±` and `∓` are now unary operators as well, like `+` or `-`. Attention has to be paid in
  macros and matrix constructors, which are whitespace sensitive, because expressions like
  `[a ±b]` now get parsed as `[a ±(b)]` instead of `[±(a, b)]`. ([#34200])

* Passing an identifier `x` by itself as a keyword argument or named tuple element
  is equivalent to `x=x`, implicitly using the name of the variable as the keyword
  or named tuple field name.
  Similarly, passing an `a.b` expression uses `b` as the keyword or field name ([#29333]).

* Packages can now provide custom hints to help users resolve errors by using the
  `register_error_hint` function. Packages that define custom exception types
  can support hints by calling `show_error_hints` from their `showerror` method. ([#35094])

* Support for Unicode 13.0.0 (via utf8proc 2.5) ([#35282]).

* The compiler optimization level can now be set per-module using the experimental macro
  `Base.Experimental.@optlevel n`. For code that is not performance-critical, setting
  this to 0 or 1 can provide significant latency improvements ([#34896]).

Language changes
----------------

* The interactive REPL now uses "soft scope" for top-level expressions: an assignment inside a
  scope block such as a `for` loop automatically assigns to a global variable if one has been
  defined already. This matches the behavior of Julia versions 0.6 and prior, as well as
  [IJulia](https://github.com/JuliaLang/IJulia.jl).
  Note that this only affects expressions interactively typed or pasted directly into the
  default REPL ([#28789], [#33864]).

* Outside of the REPL (e.g. in a file), assigning to a variable within a top-level scope
  block is considered ambiguous if a global variable with the same name exists.
  A warning is given if that happens, to alert you that the code will work differently
  than in the REPL.
  A new command line option `--warn-scope` controls this warning ([#33864]).

* Converting arbitrary tuples to `NTuple`, e.g. `convert(NTuple, (1, ""))` now gives an error,
  where it used to be incorrectly allowed. This is because `NTuple` refers only to homogeneous
  tuples (this meaning has not changed) ([#34272]).

* In docstrings, a level-1 markdown header "Extended help" is now
  interpreted as a marker dividing "brief help" from "extended help."
  The REPL help mode only shows the brief help (the content before the
  "Extended help" header) by default; prepend the expression with '?'
  (in addition to the one that enters the help mode) to see the full
  docstring. ([#25930])

* The syntax `(;)` (which was deprecated in v1.4) now creates an empty named tuple ([#30115]).

* `@inline` macro can now be applied to short-form anonymous functions ([#34953]).

* In triple-quoted string literals, whitespace stripping is now done before processing
  escape sequences instead of after. For example, the syntax
  ```
  """
    a\n b"""
  ```
  used to yield the string " a\nb", since the single space before `b` set the indent level.
  Now the result is "a\n b", since the space before `b` is no longer considered to occur
  at the start of a line. The old behavior is considered a bug ([#35001]).

* The line number of function definitions is now added by the parser as an
  additional `LineNumberNode` at the start of each function body ([#35138]).

Command-line option changes
---------------------------

* Deprecation warnings are no longer shown by default. i.e. if the `--depwarn=...` flag is
  not passed it defaults to `--depwarn=no`. The warnings are printed from tests run by
  `Pkg.test()`. ([#35362]).

* Color now defaults to on when stdout and stderr are TTYs ([#34347])

Command-line option changes
---------------------------

  * `-t N`, `--threads N` starts Julia with `N` threads. This option takes precedence over
    `JULIA_NUM_THREADS`. The specified number of threads also propagates to worker
    processes spawned using the `-p`/`--procs` or `--machine-file` command line arguments.
    In order to set number of threads for worker processes spawned with `addprocs` use the
    `exeflags` keyword argument, e.g. `` addprocs(...; exeflags=`--threads 4`) `` ([#35108]).

Multi-threading changes
-----------------------


Build system changes
--------------------


New library functions
---------------------
* The `@ccall` macro has been added added to Base. It is a near drop-in replacement for `ccall` with more Julia-like syntax. It also wraps the new `foreigncall` API for varargs of different types, though it lacks the capability to specify an LLVM calling convention. ([#32748])
* New functions `mergewith` and `mergewith!` supersede `merge` and `merge!` with `combine`
  argument.  They don't have the restriction for `combine` to be a `Function` and also
  provide one-argument method that returns a closure.  The old methods of `merge` and
  `merge!` are still available for backward compatibility ([#34296]).
* The new `isdisjoint` function indicates whether two collections are disjoint ([#34427]).
* Add function `ismutable` and deprecate `isimmutable` to check whether something is mutable.([#34652])
* `include` now accepts an optional `mapexpr` first argument to transform the parsed
  expressions before they are evaluated ([#34595]).
* New function `bitreverse` for reversing the order of bits in a fixed-width integer ([#34791]).
* New function `bitrotate(x, k)` for rotating the bits in a fixed-width integer ([#33937]).
* One argument methods `startswith(x)` and `endswith(x)` have been added, returning partially-applied versions of the functions, similar to existing methods like `isequal(x)` ([#33193]).
* New function `contains(haystack, needle)` and its one argument partially applied form have been added, it acts like `occursin(needle, haystack)`([#35132]).

New library features
--------------------
* Function composition now works also on one argument `∘(f) = f` (#34251)
* `@NamedTuple{key1::Type1, ...}` macro for convenient `NamedTuple` declarations ([#34548]).

* `isapprox` (or `≈`) now has a one-argument "curried" method `isapprox(x)` which returns a function, like `isequal` (or `==`) ([#32305]).
* `Ref{NTuple{N,T}}` can be passed to `Ptr{T}`/`Ref{T}` `ccall` signatures ([#34199])
* `x::Signed % Unsigned` and `x::Unsigned % Signed` are supported for integer bitstypes.
* `signed(unsigned_type)` is supported for integer bitstypes, `unsigned(signed_type)` has been supported.
* `accumulate`, `cumsum`, and `cumprod` now support `Tuple` ([#34654]) and arbitrary iterators ([#34656]).
* In `splice!` with no replacement, values to be removed can now be specified with an
  arbitrary iterable (instead of a `UnitRange`) ([#34524]).
* `open` for files now accepts a keyword argument `lock` controlling whether file operations
  will acquire locks for safe multi-threaded access. Setting it to `false` provides better
  performance when only one thread will access the file.

Standard library changes
------------------------
* A 1-d `Zip` iterator (where `Base.IteratorSize` is `Base.HasShape{1}()`) with defined length of `n` has now also size of `(n,)` (instead of throwing an error with truncated iterators) ([#29927]).
* The `@timed` macro now returns a `NamedTuple` ([#34149])
* New `supertypes(T)` function returns a tuple of all supertypes of `T` ([#34419]).
* Sorting-related functions such as `sort` that take the keyword arguments `lt`, `rev`, `order`
  and `by` now do not discard `order` if `by` or `lt` are passed. In the former case, the
  order from `order` is used to compare the values of `by(element)`. In the latter case,
  any order different from `Forward` or `Reverse` will raise an error about the
  ambiguity.
* `close` on a file (`IOStream`) can now throw an exception if an error occurs when trying
  to flush buffered data to disk ([#35303]).

#### LinearAlgebra
* The BLAS submodule now supports the level-2 BLAS subroutine `hpmv!` ([#34211]).
* `normalize` now supports multidimensional arrays ([#34239])
* `lq` factorizations can now be used to compute the minimum-norm solution to under-determined systems ([#34350]).
* The BLAS submodule now supports the level-2 BLAS subroutine `spmv!` ([#34320]).
* The BLAS submodule now supports the level-1 BLAS subroutine `rot!` ([#35124]).
* New generic `rotate!(x, y, c, s)` and `reflect!(x, y, c, s)` functions ([#35124]).

#### Markdown


#### Random

* `randn!(::MersenneTwister, ::Array{Float64})` is faster, and as a result, for a given state of the RNG,
  the corresponding generated numbers have changed ([#35078]).

#### REPL


#### SparseArrays
* `lu!` accepts `UmfpackLU` as an argument to make use of its symbolic factorization.
* The `trim` keyword argument for the functions `fkeep!`, `tril!`, `triu!`,
  `droptol!`,`dropzeros!` and `dropzeros` has been removed in favour of always
  trimming. Calling these with `trim=false` could result in invalid sparse
  arrays.

#### Dates
* The `eps` function now accepts `TimeType` types ([#31487]).

#### Statistics


#### Sockets

#### Distributed
* `launch_on_machine` now supports and parses ipv6 square-bracket notation ([#34430])

Deprecated or removed
---------------------

External dependencies
---------------------

Tooling Improvements
---------------------



<!--- generated by NEWS-update.jl: -->
