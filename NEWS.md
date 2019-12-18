Julia v1.4 Release Notes
========================

New language features
---------------------

* Structs with all isbits and isbitsunion fields are now stored inline in arrays ([#32448]).

* `import` now allows quoted symbols, e.g. `import Base.:+` ([#33158]).

* Function composition now supports multiple functions: `∘(f, g, h) = f ∘ g ∘ h`
and splatting `∘(fs...)` for composing an iterable collection of functions ([#33568]).

* Functions `gcd`, `lcm`, and `gcdx` now support `Rational` arguments ([#33910]).

* `a[begin]` can now be used to address the first element of an integer-indexed collection `a`.
  The index is computed by `firstindex(a)` ([#33946]).

Language changes
----------------

* Calling `show` or `repr` on an `undef`/`UndefInitializer()` array initializer now shows valid Julia code ([#33211]).

* Calling `show` or `repr` on a 0-dimensional `AbstractArray` now shows valid code for creating an equivalent 0-dimensional array, instead of only showing the contained value. ([#33206])

* The syntax `(;)`, which used to parse as an empty block expression, is deprecated.
  In the future it will indicate an empty named tuple ([#30115]).

Multi-threading changes
-----------------------

* Values can now be interpolated into `@async` and `@spawn` via `$`, which copies the value directly into the constructed
underlying closure. ([#33119])

Build system changes
--------------------
* Windows build installer has switched to Inno Setup. Installer command line parameters have thus changed. For example, to extract the installer to a specific directory, the command line parameter is now `/DIR=x:\dirname`. Use `julia-installer.exe /?` to list all new command line parameters.

New library functions
---------------------

* The `splitpath` function now accepts any `AbstractString` whereas previously it only accepted paths of type `String` ([#33012]).
* The `tempname` function now takes an optional `parent::AbstractString` argument to give it a directory in which to attempt to produce a temporary path name ([#33090]).
* The `tempname` function now takes a `cleanup::Bool` keyword argument defaulting to `true`, which causes the process to try to ensure that any file or directory at the path returned by `tempname` is deleted upon process exit ([#33090]).
* The `readdir` function now takes a `join::Bool` keyword argument defaulting to `false`, which when set causes `readdir` to join its directory argument with each listed name ([#33113]).
* `readdir` output is now guaranteed to be sorted. The `sort` keyword allows opting out of sorting to get names in OS-native order ([#33542]).
* The new `only(x)` function returns the one-and-only element of a collection `x`, and throws an `ArgumentError` if `x` contains zero or multiple elements. ([#33129])
* `takewhile` and `dropwhile` have been added to the Iterators submodule ([#33437]).
* `accumulate` has been added to the Iterators submodule.
* `filter` can now act on a `Tuple` ([#32968]).
* There is a now an `evalpoly` (generated) function meant to take the role of the `@evalpoly` macro. The function is just as efficient as the macro while giving added flexibility, so it should be preferred over `@evalpoly`. `evalpoly` takes a list of coefficients as a tuple, so where one might write `@evalpoly(x, p1, p2, p3)` one would instead write `evalpoly(x, (p1, p2, p3))`.


Standard library changes
------------------------

* The methods of `mktemp` and `mktempdir` which take a function body to pass temporary paths to no longer throw errors if the path is already deleted when the function body returns ([#33091]).
* `div` now accepts a rounding mode as the third argument, consistent with the corresponding argument to `rem`. Support for rounding division, by passing one of the RoundNearest modes to this function, was added. For future compatibility, library authors should now extend this function, rather than extending the two-argument `fld`/`cld`/`div` directly. ([#33040])
* Verbose `display` of `Char` (`text/plain` output) now shows the codepoint value in standard-conforming `"U+XXXX"` format ([#33291]).

* `Iterators.partition` now uses views (or smartly re-computed ranges) for partitions of all `AbstractArray`s ([#33533]).

* Sets are now displayed less compactly in the REPL, as a column of elements, like vectors
  and dictionaries ([#33300]).

* `methods` now accepts passing a module (or a list thereof) to filter methods defined in it ([#33403]).

#### Libdl

#### LinearAlgebra

* `qr` and `qr!` functions support `blocksize` keyword argument ([#33053]).

* `dot` now admits a 3-argument method `dot(x, A, y)` to compute generalized dot products `dot(x, A*y)`, but without computing and storing the intermediate result `A*y` ([#32739]).

* `ldlt` and non-pivoted `lu` now throw a new `ZeroPivotException` type ([#33372]).

* `cond(A, p)` with `p=1` or `p=Inf` now computes the exact condition number instead of an estimate ([#33547]).

* `UniformScaling` objects may now be exponentiated such that `(a*I)^x = a^x * I`.

#### Markdown

* Tables now have the `align` attribute set when `show`n as HTML ([#33849]).

#### Random

* `AbstractRNG`s now behave like scalars when used in broadcasting ([#33213]).

* Products involving sparse arrays now allow more general sparse `eltype`s, such as `StaticArrays` ([#33205])

* The performance of `rand(::Tuple)` is improved in some cases ([#32208]). As a consequence, the
  stream of generated values produced for a given seed has changed.

#### REPL

* The attributes of the implicit `IOContext` used by the REPL to display objects can be
  modified by the user (experimental feature) ([#29249]).

#### SparseArrays

#### Dates

#### Statistics

#### Sockets


Deprecated or removed
---------------------


External dependencies
---------------------

Tooling Improvements
---------------------


<!--- generated by NEWS-update.jl: -->
