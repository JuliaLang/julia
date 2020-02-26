Julia v1.4 Release Notes
========================

New language features
---------------------

* Structs with all isbits and isbitsunion fields are now stored inline in arrays ([#32448]).
* `import` now allows quoted symbols, e.g. `import Base.:+` ([#33158]).
* `a[begin]` can now be used to address the first element of an integer-indexed collection `a`.
  The index is computed by `firstindex(a)` ([#33946]).

Language changes
----------------

* The syntax `(;)`, which used to parse as an empty block expression, is deprecated.
  In the future it will indicate an empty named tuple ([#30115]).

Multi-threading changes
-----------------------

* Values can now be interpolated into `@async` and `@spawn` via `$`, which copies the value directly into the constructed
  underlying closure ([#33119]).

Build system changes
--------------------

* Windows build installer has switched to Inno Setup. Installer command line parameters have thus changed. For example, to extract the installer to a specific directory, the command line parameter is now `/DIR=x:\dirname`. Use `julia-installer.exe /?` to list all new command line parameters.

New library functions
---------------------

* The new `only(x)` function returns the one-and-only element of a collection `x`, and throws an `ArgumentError` if `x` contains zero or multiple elements ([#33129]).
* `takewhile` and `dropwhile` have been added to the Iterators submodule ([#33437]).
* `accumulate` has been added to the Iterators submodule ([#34033]).
* There is a now an `evalpoly` function meant to take the role of the `@evalpoly` macro. The function is just as efficient as the macro while giving added flexibility, so it should be preferred over `@evalpoly`. `evalpoly` takes a list of coefficients as a tuple, so where one might write `@evalpoly(x, p1, p2, p3)` one would instead write `evalpoly(x, (p1, p2, p3))`.

New library features
--------------------

* Function composition now supports multiple functions: `∘(f, g, h) = f ∘ g ∘ h`
  and splatting `∘(fs...)` for composing an iterable collection of functions ([#33568]).
* Functions `gcd`, `lcm`, and `gcdx` now support `Rational` arguments ([#33910]).
* The `splitpath` function now accepts any `AbstractString` whereas previously it only accepted paths of type `String` ([#33012]).
* `filter` can now act on a `Tuple` ([#32968]).
* The `tempname` function now takes an optional `parent::AbstractString` argument to give it a directory in which to attempt to produce a temporary path name ([#33090]).
* The `tempname` function now takes a `cleanup::Bool` keyword argument defaulting to `true`, which causes the process to try to ensure that any file or directory at the path returned by `tempname` is deleted upon process exit ([#33090]).
* The `readdir` function now takes a `join::Bool` keyword argument defaulting to `false`, which when set causes `readdir` to join its directory argument with each listed name ([#33113]).
* `div` now accepts a rounding mode as the third argument, consistent with the corresponding argument to `rem`. Support for rounding division, by passing one of the RoundNearest modes to this function, was added. For future compatibility, library authors should now extend this function, rather than extending the two-argument `fld`/`cld`/`div` directly ([#33040]).
* `methods` now accepts a module (or a list thereof) to filter methods defined in it ([#33403]).

Standard library changes
------------------------

* Calling `show` or `repr` on an `undef`/`UndefInitializer()` array initializer now shows valid Julia code ([#33211]).
* Calling `show` or `repr` on a 0-dimensional `AbstractArray` now shows valid code for creating an equivalent 0-dimensional array, instead of only showing the contained value ([#33206]).
* `readdir` output is now guaranteed to be sorted. The `sort` keyword allows opting out of sorting to get names in OS-native order ([#33542]).
* The methods of `mktemp` and `mktempdir` that take a function to pass temporary paths to no longer throw errors if the path is already deleted when the function returns ([#33091]).
* Verbose `display` of `Char` (`text/plain` output) now shows the codepoint value in standard-conforming `"U+XXXX"` format ([#33291]).
* `Iterators.partition` now uses views (or smartly re-computed ranges) for partitions of all `AbstractArray`s ([#33533]).
* Sets are now displayed less compactly in the REPL, as a column of elements, like vectors
  and dictionaries ([#33300]).
* `delete!` on `WeakKeyDict`s now returns the `WeakKeyDict` itself instead of the underlying `Dict` used for implementation

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
* The performance of `rand(::Tuple)` is improved in some cases ([#32208]). As a consequence, the
  stream of generated values produced for a given seed has changed.

#### REPL

* The attributes of the implicit `IOContext` used by the REPL to display objects can be
  modified by the user (experimental feature) ([#29249]).

#### SparseArrays

* The return value of `zero(x::AbstractSparseArray)` has no stored zeros anymore ([#31835]).
  Previously, it would have stored zeros wherever `x` had them. This makes the operation
  constant time instead of `O(<number of stored values>)`.
* Products involving sparse arrays now allow more general sparse `eltype`s, such as `StaticArrays` ([#33205])

<!--- generated by NEWS-update.jl: -->
[#29249]: https://github.com/JuliaLang/julia/issues/29249
[#30115]: https://github.com/JuliaLang/julia/issues/30115
[#31835]: https://github.com/JuliaLang/julia/issues/31835
[#32208]: https://github.com/JuliaLang/julia/issues/32208
[#32448]: https://github.com/JuliaLang/julia/issues/32448
[#32739]: https://github.com/JuliaLang/julia/issues/32739
[#32968]: https://github.com/JuliaLang/julia/issues/32968
[#33012]: https://github.com/JuliaLang/julia/issues/33012
[#33040]: https://github.com/JuliaLang/julia/issues/33040
[#33053]: https://github.com/JuliaLang/julia/issues/33053
[#33090]: https://github.com/JuliaLang/julia/issues/33090
[#33091]: https://github.com/JuliaLang/julia/issues/33091
[#33113]: https://github.com/JuliaLang/julia/issues/33113
[#33119]: https://github.com/JuliaLang/julia/issues/33119
[#33129]: https://github.com/JuliaLang/julia/issues/33129
[#33158]: https://github.com/JuliaLang/julia/issues/33158
[#33205]: https://github.com/JuliaLang/julia/issues/33205
[#33206]: https://github.com/JuliaLang/julia/issues/33206
[#33211]: https://github.com/JuliaLang/julia/issues/33211
[#33213]: https://github.com/JuliaLang/julia/issues/33213
[#33291]: https://github.com/JuliaLang/julia/issues/33291
[#33300]: https://github.com/JuliaLang/julia/issues/33300
[#33372]: https://github.com/JuliaLang/julia/issues/33372
[#33403]: https://github.com/JuliaLang/julia/issues/33403
[#33437]: https://github.com/JuliaLang/julia/issues/33437
[#33533]: https://github.com/JuliaLang/julia/issues/33533
[#33542]: https://github.com/JuliaLang/julia/issues/33542
[#33547]: https://github.com/JuliaLang/julia/issues/33547
[#33568]: https://github.com/JuliaLang/julia/issues/33568
[#33849]: https://github.com/JuliaLang/julia/issues/33849
[#33910]: https://github.com/JuliaLang/julia/issues/33910
[#33946]: https://github.com/JuliaLang/julia/issues/33946
[#34033]: https://github.com/JuliaLang/julia/issues/34033

Julia v1.3 Release Notes
========================

New language features
---------------------

* Support for Unicode 12.1.0 ([#32002]).
* Methods can now be added to an abstract type ([#31916]).
* Support for unicode bold digits and double-struck digits 0 through 9 as valid identifiers ([#32838]).
* Added the syntax `var"#str#"` for printing and parsing non-standard variable names ([#32408]).

Language changes
----------------


Multi-threading changes
-----------------------

* New experimental `Threads.@spawn` macro that runs a task on any available thread ([#32600]).
* All system-level I/O operations (e.g. files and sockets) are now thread-safe.
  This does not include subtypes of `IO` that are entirely in-memory, such as `IOBuffer`,
  although it specifically does include `BufferStream`.
  ([#32309], [#32174], [#31981], [#32421]).
* The global random number generator (`GLOBAL_RNG`) is now thread-safe (and thread-local) ([#32407]).
* New `Channel(f::Function, spawn=true)` keyword argument to schedule the created Task on
  any available thread, matching the behavior of `Threads.@spawn` ([#32872]).
* Simplified the `Channel` constructor, which is now easier to read and more idiomatic julia.
  Use of the keyword arguments `csize` and `ctype` is now discouraged ([#30855], [#32818]).

Build system changes
--------------------


New library functions
---------------------

* `findfirst`, `findlast`, `findnext` and `findprev` now accept a character as first argument
  to search for that character in a string passed as the second argument ([#31664]).
* New `findall(pattern, string)` method where `pattern` is a string or regex ([#31834]).
* `count(pattern, string)` gives the number of things `findall` would match ([#32849]).
* `istaskfailed` is now documented and exported, like its siblings `istaskdone` and `istaskstarted` ([#32300]).
* `RefArray` and `RefValue` objects now accept index `CartesianIndex()` in  `getindex` and `setindex!` ([#32653])
* Added `sincosd(x)` to simultaneously compute the sine and cosine of `x`, where `x` is in degrees ([#30134]).
* The function `nonmissingtype`, which removes `Missing` from type unions, is now exported ([#31562]).

Standard library changes
------------------------

* `Pkg` won't clobber pre-compilation files as often when switching environments ([#32651])
* `Pkg` can now download and install binary artifacts through the `Pkg.Artifacts`
   submodule and supporting functions. ([#32918])
* When `wait` (or `@sync`, or `fetch`) is called on a failing `Task`, the exception is propagated as a
  `TaskFailedException` wrapping the task.
  This makes it possible to see the location of the original failure inside the task (as well as the
  location of the `wait` call, as before) ([#32814]).
* `Regex` can now be multiplied (`*`) and exponentiated (`^`), like strings ([#23422]).
* `Cmd` interpolation (``` `$(x::Cmd) a b c` ``` where) now propagates `x`'s process flags
  (environment, flags, working directory, etc) if `x` is the first interpolant and errors
  otherwise ([#24353]).
* Zero-dimensional arrays are now consistently preserved in the return values of mathematical
  functions that operate on the array(s) as a whole (and are not explicitly broadcasted across their elements).
  Previously, the functions  `+`, `-`, `*`, `/`, `conj`, `real` and `imag` returned the unwrapped element
  when operating over zero-dimensional arrays ([#32122]).
* `IPAddr` subtypes now behave like scalars when used in broadcasting ([#32133]).
* `Pair` is now treated as a scalar for broadcasting ([#32209]).
* `clamp` can now handle missing values ([#31066]).
* `empty` now accepts a `NamedTuple` ([#32534]).
* `mod` now accepts a unit range as the second argument to easily perform offset modular arithmetic to ensure the result is inside the range ([#32628]).
* `nothing` can now be `print`ed, and interpolated into strings etc. as the string `"nothing"`. It is still not permitted to be interpolated into Cmds (i.e. ``echo `$(nothing)` `` will still error without running anything.) ([#32148])
* When `open` is called with a function, command, and keyword argument (e.g. ```open(`ls`, read=true) do f ...```)
  it now correctly throws a `ProcessFailedException` like other similar calls ([#32193]).
* `mktemp` and `mktempdir` now try, by default, to remove temporary paths they create before the process exits ([#32851]).
* Added argument `keep` to `unescape_string` ([#27125]).

#### Libdl

* `dlopen()` can now be invoked in `do`-block syntax, similar to `open()`.

#### LinearAlgebra

* The BLAS submodule no longer exports `dot`, which conflicts with that in LinearAlgebra ([#31838]).
* `diagm` and `spdiagm` now accept optional `m,n` initial arguments to specify a size ([#31654]).
* `Hessenberg` factorizations `H` now support efficient shifted solves `(H+µI) \ b` and determinants, and use a specialized tridiagonal factorization for Hermitian matrices. There is also a new `UpperHessenberg` matrix type ([#31853]).
* Added keyword argument `alg` to `svd` and `svd!` that allows one to switch between different SVD algorithms ([#31057]).
* Five-argument `mul!(C, A, B, α, β)` now implements inplace multiplication fused with addition _C = A B α + C β_ ([#23919]).

#### SparseArrays

* `SparseMatrixCSC(m,n,colptr,rowval,nzval)` perform consistency checks for arguments:
  `colptr` must be properly populated and lengths of `colptr`, `rowval`, and `nzval`
  must be compatible with `m`, `n`, and `eltype(colptr)`.
* `sparse(I, J, V, m, n)` verifies lengths of `I`, `J`, `V` are equal and compatible with
  `eltype(I)` and `m`, `n`.

#### Dates

* `DateTime` and `Time` formatting/parsing now supports 12-hour clocks with AM/PM via `I` and `p` codes, similar to `strftime` ([#32308]).
* Fixed `repr` such that it displays `Time` as it would be entered in Julia ([#32103]).

#### Statistics

* `mean` now accepts both a function argument and a `dims` keyword ([#31576]).

#### Sockets

* `Sockets.recvfrom` now returns both host and port as an InetAddr ([#32729]).
* Added `InetAddr` constructor from `AbstractString`, representing IP address, and `Integer`,
  representing port number ([#31459]).

#### Miscellaneous

* `foldr` and `mapfoldr` now work on any iterator that supports `Iterators.reverse`, not just arrays ([#31781]).

Deprecated or removed
---------------------

* `@spawn expr` from the `Distributed` standard library should be replaced with `@spawnat :any expr` ([#32600]).
* `Threads.Mutex` and `Threads.RecursiveSpinLock` have been removed; use `ReentrantLock` (preferred) or
  `Threads.SpinLock` instead ([#32875]).

External dependencies
---------------------

Tooling Improvements
---------------------

* The `ClangSA.jl` static analysis package has been imported, which makes use of
  the clang static analyzer to validate GC invariants in Julia's C code. The analysis
  may be run using `make -C src analyzegc`.

<!--- generated by NEWS-update.jl: -->
[#23422]: https://github.com/JuliaLang/julia/issues/23422
[#23919]: https://github.com/JuliaLang/julia/issues/23919
[#24353]: https://github.com/JuliaLang/julia/issues/24353
[#27125]: https://github.com/JuliaLang/julia/issues/27125
[#30134]: https://github.com/JuliaLang/julia/issues/30134
[#30855]: https://github.com/JuliaLang/julia/issues/30855
[#31057]: https://github.com/JuliaLang/julia/issues/31057
[#31066]: https://github.com/JuliaLang/julia/issues/31066
[#31459]: https://github.com/JuliaLang/julia/issues/31459
[#31562]: https://github.com/JuliaLang/julia/issues/31562
[#31576]: https://github.com/JuliaLang/julia/issues/31576
[#31654]: https://github.com/JuliaLang/julia/issues/31654
[#31664]: https://github.com/JuliaLang/julia/issues/31664
[#31781]: https://github.com/JuliaLang/julia/issues/31781
[#31834]: https://github.com/JuliaLang/julia/issues/31834
[#31838]: https://github.com/JuliaLang/julia/issues/31838
[#31853]: https://github.com/JuliaLang/julia/issues/31853
[#31916]: https://github.com/JuliaLang/julia/issues/31916
[#31981]: https://github.com/JuliaLang/julia/issues/31981
[#32002]: https://github.com/JuliaLang/julia/issues/32002
[#32103]: https://github.com/JuliaLang/julia/issues/32103
[#32122]: https://github.com/JuliaLang/julia/issues/32122
[#32133]: https://github.com/JuliaLang/julia/issues/32133
[#32148]: https://github.com/JuliaLang/julia/issues/32148
[#32174]: https://github.com/JuliaLang/julia/issues/32174
[#32193]: https://github.com/JuliaLang/julia/issues/32193
[#32209]: https://github.com/JuliaLang/julia/issues/32209
[#32300]: https://github.com/JuliaLang/julia/issues/32300
[#32308]: https://github.com/JuliaLang/julia/issues/32308
[#32309]: https://github.com/JuliaLang/julia/issues/32309
[#32407]: https://github.com/JuliaLang/julia/issues/32407
[#32408]: https://github.com/JuliaLang/julia/issues/32408
[#32421]: https://github.com/JuliaLang/julia/issues/32421
[#32534]: https://github.com/JuliaLang/julia/issues/32534
[#32600]: https://github.com/JuliaLang/julia/issues/32600
[#32628]: https://github.com/JuliaLang/julia/issues/32628
[#32653]: https://github.com/JuliaLang/julia/issues/32653
[#32729]: https://github.com/JuliaLang/julia/issues/32729
[#32814]: https://github.com/JuliaLang/julia/issues/32814
[#32818]: https://github.com/JuliaLang/julia/issues/32818
[#32838]: https://github.com/JuliaLang/julia/issues/32838
[#32849]: https://github.com/JuliaLang/julia/issues/32849
[#32851]: https://github.com/JuliaLang/julia/issues/32851
[#32872]: https://github.com/JuliaLang/julia/issues/32872
[#32875]: https://github.com/JuliaLang/julia/issues/32875

Julia v1.2 Release Notes
========================

New language features
---------------------

* Argument splatting (`x...`) can now be used in calls to the `new` pseudo-function in
  constructors ([#30577]).
* Support for Unicode 12.0.0 ([#31561]).
* Added `⋆` (`\star`) as unary operator ([#31604]).

Language changes
----------------

* Empty entries in `JULIA_DEPOT_PATH` are now expanded to default depot entries ([#31009]).

Multi-threading changes
-----------------------

* The `Condition` type now has a thread-safe replacement, accessed as `Threads.Condition`.
  With that addition, task scheduling primitives such as `ReentrantLock` are now thread-safe ([#30061]).
* It is possible to schedule and switch Tasks during `@threads` loops, and perform limited I/O ([#31438]).

Build system changes
--------------------

* The build system now prefers downloading prebuilt binary tarballs for most dependencies on
  supported systems, disable by setting `USE_BINARYBUILDER=0` at `make` time ([#31441]).

New library functions
---------------------

* `getipaddrs()` function returns all the IP addresses of the local machine, with IPv4 addresses sorting before IPv6 addresses ([#30349, #30604]).
* `getipaddr(addr_type)` and `getipaddrs(addr_type)` functions returns an IP address(es) of the desired type of the local machine ([#30604]).
* Added `Base.hasproperty` and `Base.hasfield` ([#28850]).
* One argument `!=(x)`, `>(x)`, `>=(x)`, `<(x)`, `<=(x)` have been added, returning partially-applied
  versions of the functions, similar to the existing `==(x)` and `isequal(x)` methods ([#30915]).

Standard library changes
------------------------

* `Enum` now behaves like a scalar when used in broadcasting ([#30670]).
* If a `pipeline` is specified with `append=true` set, but no redirection, an `ArgumentError`
  is thrown, rather than a `ErrorException` ([#27900]).
* Functions that invoke commands (e.g. `run(::Cmd)`) now throw a `ProcessFailedException`
  rather than an `ErrorException`, if those commands exit with non-zero exit code ([#27900]).
* The `extrema` function now accepts a function argument in the same manner as `minimum` and
  `maximum` ([#30323]).
* `hasmethod` can now check for matching keyword argument names ([#30712]).
* `startswith` and `endswith` now accept a `Regex` for the second argument ([#29790]).
* `retry` supports arbitrary callable objects ([#30382]).
* A no-argument constructor for `Ptr{T}` has been added which constructs a null pointer ([#30919]).
* `strip` now accepts a function argument in the same manner as `lstrip` and `rstrip` ([#31211]).
* `mktempdir` now accepts a `prefix` keyword argument to customize the file name ([#31230], [#22922]).
* `keytype` and `valtype` now work on `AbstractArray`, and return the `eltype` of `keys(...)` and
  `values(...)` respectively ([#27749]).
* `nextfloat(::BigFloat)` and `prevfloat(::BigFloat)` now returns a value with the same precision
  as their argument, which means that (in particular) `nextfloat(prevfloat(x)) == x` whereas
  previously this could result in a completely different value with a different precision ([#31310]).
* `mapreduce` now accepts multiple iterators, similar to `map` ([#31532]).
* `filter` now supports `SkipMissing`-wrapped arrays ([#31235]).
* Objects created by calling `skipmissing` on an array can now be indexed using indices
  from the parent at non-missing positions. This allows functions such as
  `findall`, `findfirst`, `argmin`/`argmax` and `findmin`/`findmax` to work with these
  objects, returning the index of matching non-missing elements in the parent ([#31008]).
* `inv(::Missing)` has now been added and returns `missing` ([#31451]).
* `nextfloat(::BigFloat, n::Integer)` and `prevfloat(::BigFloat, n::Integer)` methods
  have been added ([#31310]).

#### LinearAlgebra
* Added keyword arguments `rtol`, `atol` to `pinv` and `nullspace` ([#29998]).
* `UniformScaling` instances are now callable such that e.g. `I(3)` will produce a `Diagonal` matrix ([#30298]).
* Eigenvalues λ of general matrices are now sorted lexicographically by (Re λ, Im λ) ([#21598]).
* `one` for structured matrices (`Diagonal`, `Bidiagonal`, `Tridiagonal`, `Symtridiagonal`) now preserves
  structure and type ([#29777]).
* `diagm(v)` is now a shorthand for `diagm(0 => v)` ([#31125]).

#### SparseArrays
* Performance improvements for sparse matrix-matrix multiplication ([#30372]).
* Sparse vector outer products are more performant and maintain sparsity in products of the
  form `kron(u, v')`, `u * v'`, and `u .* v'` where `u` and `v` are sparse vectors or column
  views ([#24980]).
* The `sprand` function is now 2 to 5 times faster ([#30494]). As a consequence of this change, the random stream of matrices produced with `sprand` and `sprandn` has changed.

#### Sockets

* `getipaddrs` returns IP addresses in the order provided by libuv ([#32260]).
* `getipaddr` prefers to return the first `IPv4` interface address provided by libuv ([#32260]).

#### Dates
* Fixed `repr` such that it displays `DateTime` as it would be entered in Julia ([#30200]).

#### Statistics
* `quantile` now accepts in all cases collections whose `eltype` is not a subtype of `Number` ([#30938]).

#### Miscellaneous
* Since environment variables on Windows are case-insensitive, `ENV` now converts its keys
  to uppercase for display, iteration, and copying ([#30593]).

External dependencies
---------------------

* libgit2 has been updated to v0.27.7 ([#30584]).
* OpenBLAS has been updated to v0.3.5 ([#30583]).
* MbedTLS has been updated to v2.16.0 ([#30618]).
* libunwind has been updated to v1.3.1 ([#30724]).

<!--- generated by NEWS-update.jl: -->
[#21598]: https://github.com/JuliaLang/julia/issues/21598
[#22922]: https://github.com/JuliaLang/julia/issues/22922
[#24980]: https://github.com/JuliaLang/julia/issues/24980
[#27749]: https://github.com/JuliaLang/julia/issues/27749
[#27900]: https://github.com/JuliaLang/julia/issues/27900
[#28850]: https://github.com/JuliaLang/julia/issues/28850
[#29777]: https://github.com/JuliaLang/julia/issues/29777
[#29790]: https://github.com/JuliaLang/julia/issues/29790
[#29998]: https://github.com/JuliaLang/julia/issues/29998
[#30061]: https://github.com/JuliaLang/julia/issues/30061
[#30200]: https://github.com/JuliaLang/julia/issues/30200
[#30298]: https://github.com/JuliaLang/julia/issues/30298
[#30323]: https://github.com/JuliaLang/julia/issues/30323
[#30372]: https://github.com/JuliaLang/julia/issues/30372
[#30382]: https://github.com/JuliaLang/julia/issues/30382
[#30494]: https://github.com/JuliaLang/julia/issues/30494
[#30577]: https://github.com/JuliaLang/julia/issues/30577
[#30583]: https://github.com/JuliaLang/julia/issues/30583
[#30584]: https://github.com/JuliaLang/julia/issues/30584
[#30593]: https://github.com/JuliaLang/julia/issues/30593
[#30604]: https://github.com/JuliaLang/julia/issues/30604
[#30618]: https://github.com/JuliaLang/julia/issues/30618
[#30670]: https://github.com/JuliaLang/julia/issues/30670
[#30712]: https://github.com/JuliaLang/julia/issues/30712
[#30724]: https://github.com/JuliaLang/julia/issues/30724
[#30915]: https://github.com/JuliaLang/julia/issues/30915
[#30919]: https://github.com/JuliaLang/julia/issues/30919
[#30938]: https://github.com/JuliaLang/julia/issues/30938
[#31008]: https://github.com/JuliaLang/julia/issues/31008
[#31009]: https://github.com/JuliaLang/julia/issues/31009
[#31125]: https://github.com/JuliaLang/julia/issues/31125
[#31211]: https://github.com/JuliaLang/julia/issues/31211
[#31230]: https://github.com/JuliaLang/julia/issues/31230
[#31235]: https://github.com/JuliaLang/julia/issues/31235
[#31310]: https://github.com/JuliaLang/julia/issues/31310
[#31438]: https://github.com/JuliaLang/julia/issues/31438
[#31441]: https://github.com/JuliaLang/julia/issues/31441
[#31451]: https://github.com/JuliaLang/julia/issues/31451
[#31532]: https://github.com/JuliaLang/julia/issues/31532
[#31561]: https://github.com/JuliaLang/julia/issues/31561
[#31604]: https://github.com/JuliaLang/julia/issues/31604
[#32260]: https://github.com/JuliaLang/julia/issues/32260

Julia v1.1 Release Notes
========================

New language features
---------------------

  * An *exception stack* is maintained on each task to make exception handling
    more robust and enable root cause analysis. The stack may be accessed using
    the experimental function `Base.catch_stack` ([#28878]).
  * The experimental macro `Base.@locals` returns a dictionary of current local variable names
    and values ([#29733]).
  * Binary `~` can now be dotted, as in `x .~ y` ([#30341]).

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
    or a higher-dimensional array, for consistency with other array types.
    Use `LinearIndices(a)[findmin(a)[2]]` to get the old behavior, or `CartesianIndices(a)[findmin(a)[2]]`
    to get the new behavior on previous Julia versions ([#30102]).
  * Method signatures such as
    `f(::Type{T}, ::T) where {T <: X}` and
    `f(::Type{X}, ::Any)`
    are now considered ambiguous. Previously a bug caused the first one to be considered more specific in
    some cases ([#30160]).

Command-line option changes
---------------------------

  * When a script run in interactive mode (`-i`) throws an error, the REPL now starts after
    the error is displayed. Previously the REPL only started if the script completed without
    error ([#21233]).

New library functions
---------------------

  * `splitpath(p::String)` function, which is the opposite of `joinpath(parts...)`: it splits a filepath
    into its components ([#28156]).
  * `isnothing(::Any)` predicate, to check whether the argument is `nothing`. ([#29679]).
  * `getpid(::Process)` method ([#24064]).
  * `eachrow`, `eachcol` and `eachslice` functions provide efficient iterators over slices of arrays ([#29749]).
  * `fieldtypes(T::Type)` which returns the declared types of the field in type T ([#29600]).
  * `uuid5` has been added to the `UUIDs` standard library ([#28761]).
  * Predicates `Sys.isfreebsd`, `Sys.isopenbsd`, `Sys.isnetbsd`, and `Sys.isdragonfly` for
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
  * `Regex` now behaves like a scalar when used in broadcasting ([#29913]).
  * `Char` now behaves like a read-only 0-dimensional array ([#29819]).
  * `parse` now allows strings representing integer 0 and 1 for type `Bool` ([#29980]).
  * `Base.tail` now works on named tuples ([#29595]).
  * The process id is appended to malloc log files in order to track memory allocations of
    multiple processes ([#29969]).
  * `Base.julia_cmd` now propagates the `--inline=(yes|no)` flag ([#29858]).
  * `Base.@kwdef` can now be used for parametric structs, and for structs with supertypes ([#29316]).
  * `merge(::NamedTuple, ::NamedTuple...)` can now be used with more than 2 `NamedTuple`s ([#29259]).
  * New `ncodeunits(c::Char)` method as a fast equivalent to `ncodeunits(string(c))` ([#29153]).
  * New `sort!(::AbstractArray; dims)` method that can sort the array along the `dims` dimension ([#28902]).
  * `range` now accepts `stop` as a positional argument ([#28708]).
  * `get(A::AbstractArray, (), default)` now returns `A[]` instead of an empty array ([#30270]).
  * `parse(Bool, str)` is now supported ([#29997]).
  * `copyto!(::AbstractMatrix, ::UniformScaling)` now supports rectangular matrices ([#28790]).
  * `current_project()` now searches the parent directories of a Git repository for a `Project.toml` file.
    This also affects the behavior of the `--project` command line option when using the default
    `--project=@.` ([#29108]).
  * The `spawn` API is now more flexible and supports taking IOBuffer directly as an I/O stream,
    converting to a system pipe as needed ([#30278]).

#### Dates
  * New `DateTime(::Date, ::Time)` constructor ([#29754]).
  * `TimeZone` now behaves like a scalar when used in broadcasting ([#30159]).

#### InteractiveUtils
  * `edit` can now be called on a module to edit the file that defines it ([#29636]).
  * All compiler-reflection tools (i.e. the `code_` class of functions and macros) now print accurate
    line number and inlining information in a common style, and take an optional parameter (debuginfo=:default)
    to control the verbosity of the metadata shown ([#29893]).

#### LinearAlgebra
  * `isdiag` and `isposdef` for `Diagonal` and `UniformScaling` ([#29638]).
  * `mul!`, `rmul!` and `lmul!` methods for `UniformScaling` ([#29506]).
  * `Symmetric` and `Hermitian` matrices now preserve the wrapper when scaled with a number ([#29469]).
  * Exponentiation operator `^` now supports raising an `Irrational` to an `AbstractMatrix` power ([#29782]).
  * Added keyword arguments `rtol`, `atol` to `rank` ([#29926]).

#### Random
  * `randperm` and `randcycle` now use the type of their argument to determine the element type of
    the returned array ([#29670]).
  * A new method `rand(::Tuple)` implements sampling from the values of a tuple ([#25278]).
  * `serialize` and `deserialize` now accept a filename argument, like `write` and `read` ([#30151]).

#### SparseArrays
  * `sprandn` now supports specifying the output element type ([#30083]).

#### Statistics
  * `mean` and `var` now handle more kinds of empty inputs ([#29033]).

External dependencies
---------------------

  * 7zip (bundled with Julia on Windows) has been upgraded from version 16.04 to 18.05 ([#30035]).
  * Busybox is no longer bundled with Julia on Windows ([#30022]).
  * OpenBLAS has been upgraded from 0.3.2 to 0.3.3 ([#29845]).
  * The source code for Pkg is no longer included in JuliaLang/julia. Pkg is instead
    downloaded during the build process ([#29615]).
  * LLVM has been upgraded to 6.0.1 and support for LLVM < 6.0 has been dropped ([#28745], [#28696]).
  * Pkg has been upgraded to version 1.1 ([#30342]).

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
[#29926]: https://github.com/JuliaLang/julia/issues/29926
[#29968]: https://github.com/JuliaLang/julia/issues/29968
[#29969]: https://github.com/JuliaLang/julia/issues/29969
[#29978]: https://github.com/JuliaLang/julia/issues/29978
[#29980]: https://github.com/JuliaLang/julia/issues/29980
[#29997]: https://github.com/JuliaLang/julia/issues/29997
[#30004]: https://github.com/JuliaLang/julia/issues/30004
[#30022]: https://github.com/JuliaLang/julia/issues/30022
[#30035]: https://github.com/JuliaLang/julia/issues/30035
[#30083]: https://github.com/JuliaLang/julia/issues/30083
[#30102]: https://github.com/JuliaLang/julia/issues/30102
[#30151]: https://github.com/JuliaLang/julia/issues/30151
[#30159]: https://github.com/JuliaLang/julia/issues/30159
[#30160]: https://github.com/JuliaLang/julia/issues/30160
[#30226]: https://github.com/JuliaLang/julia/issues/30226
[#30249]: https://github.com/JuliaLang/julia/issues/30249
[#30270]: https://github.com/JuliaLang/julia/issues/30270
[#30278]: https://github.com/JuliaLang/julia/issues/30278
[#30341]: https://github.com/JuliaLang/julia/issues/30341
[#30342]: https://github.com/JuliaLang/julia/issues/30342

Julia v1.0.0 Release Notes
==========================

Julia v1.0 is identical to the v0.7 release, with the exception that
it removes all deprecations and deprecation related warnings. When
upgrading a codebase from v0.6, the process is to first get the code
to work on v0.7, and fix all the deprecation warnings. Once the code
runs on v0.7 without warnings, it should be good to run on v1.0.

Refer to the [Release Notes for
v0.7](https://github.com/JuliaLang/julia/blob/master/HISTORY.md) for a
detailed list of changes from Julia v0.6.

Standard Library Changes
------------------------

* The `Libdl` module's methods `dlopen()` and `dlsym()` have gained a
  `throw_error` keyword argument, replacing the now-deprecated `dlopen_e()`
  and `dlsym_e()` methods.  When `throw_error` is `false`, failure to locate
  a shared library or symbol will return `nothing` rather than `C_NULL`.
  ([#28888])

Deprecated or removed
---------------------

* The old package manager (now called `OldPkg`) has been moved to a
  separate repository at https://github.com/JuliaArchive/OldPkg.jl ([#27930])

<!--- generated by NEWS-update.jl: -->
[#27930]: https://github.com/JuliaLang/julia/issues/27930
[#28888]: https://github.com/JuliaLang/julia/issues/28888

Julia v0.7.0 Release Notes
==========================

New language features
---------------------

  * Local variables can be tested for being defined
    using the new `@isdefined variable` macro ([#22281]).

  * Destructuring in function arguments: when an expression such as `(x, y)` is used as
    a function argument name, the argument is unpacked into local variables `x` and `y`
    as in the assignment `(x, y) = arg` ([#6614]).

  * Named tuples, with the syntax `(a=1, b=2)`. These behave very similarly to tuples,
    except components can also be accessed by name using dot syntax `t.a` ([#22194]).

  * Keyword argument containers (`kw` in `f(; kw...)`) are now based on named tuples. Dictionary
    functions like `haskey` and indexing can be used on them, and name-value pairs can be
    iterated using `pairs(kw)`. `kw` can no longer contain multiple entries for the same
    argument name ([#4916]).

  * Custom infix operators can now be defined by appending Unicode
    combining marks, primes, and sub/superscripts to other operators.
    For example, `+̂ₐ″` is parsed as an infix operator with the same
    precedence as `+` ([#22089]).

  * The macro call syntax `@macroname[args]` is now available and is parsed
    as `@macroname([args])` ([#23519]).

  * The construct `if @generated ...; else ...; end` can be used to provide both
    `@generated` and normal implementations of part of a function. Surrounding code
    will be common to both versions ([#23168]).

  * Added `⟂` (`\perp`) operator with comparison precedence ([#24404]).

  * The `missing` singleton object (of type `Missing`) has been added to represent
    missing values ([#24653]). It propagates through standard operators and mathematical functions,
    and implements three-valued logic, similar to SQLs `NULL` and R's `NA`.

  * Field access via dot-syntax can now be overloaded by adding methods to
    `Base.getproperty` and `Base.setproperty!` ([#1974]), optionally along with
    a corresponding `Base.propertynames` method for reflection ([#25311]).

  * Values for `Enum`s can now be specified inside of a `begin` block when using the
    `@enum` macro ([#25424]).

  * Keyword arguments can be required: if a default value is omitted, then an
    exception is thrown if the caller does not assign the keyword a value ([#25830]).

  * The pair operator `=>` is now broadcastable as `.=>` which was previously a parsing error ([#27447])

Language changes
----------------

  * The syntax for parametric methods, `function f{T}(x::T)`, has been
    changed to `function f(x::T) where {T}` ([#11310]).

  * The fallback constructor that calls `convert` is deprecated. Instead, new types should
    prefer to define constructors, and add `convert` methods that call those constructors
    only as necessary ([#15120]).

  * The syntax `1.+2` is deprecated, since it is ambiguous: it could mean either
    `1 .+ 2` (the current meaning) or `1. + 2` ([#19089]).

  * Mutable structs with no fields are no longer singletons; it is now possible to make
    multiple instances of them that can be distinguished by `===` ([#25854]).
    Zero-size immutable structs are still singletons.

  * In string and character literals, backslash `\` may no longer
    precede unrecognized escape characters ([#22800]).

  * Juxtaposing binary, octal, and hexadecimal literals is deprecated, since it can lead to
    confusing code such as `0xapi == 0xa * pi` ([#16356]).

  * Numeric literal juxtaposition now has slighty lower precedence than unary operators,
    so for example `√2x` parses as `(√2) * x` ([#27641]).

  * Declaring arguments as `x::ANY` to avoid specialization has been replaced
    by `@nospecialize x`. ([#22666]).

    This can also be used in global scope, to apply to all subsequent method definitions
    in the module (until `@specialize`). ([#28065])

  * Keyword argument default values are now evaluated in successive scopes ---
    the scope for each expression includes only previous keyword arguments, in
    left-to-right order ([#17240]).

  * The parsing of `1<<2*3` as `1<<(2*3)` is deprecated, and will change to
    `(1<<2)*3` in a future version ([#13079]).

  * The parsing of `<|` is now right associative. `|>` remains left associative ([#24153]).

  * `:` now parses like other operators, as a call to a function named `:`, instead of
    calling `colon` ([#25947]).

  * `{ }` expressions now use `braces` and `bracescat` as expression heads instead
    of `cell1d` and `cell2d`, and parse similarly to `vect` and `vcat` ([#8470]).

  * Nested `if` expressions that arise from the keyword `elseif` now use `elseif`
    as their expression head instead of `if` ([#21774]).

  * `let` blocks now parse the same as `for` loops; the first argument is either an
    assignment or `block` of assignments, and the second argument is a block of
    statements ([#21774]).

  * `do` syntax now parses to an expression with head `:do`, instead of as a function
    call ([#21774]).

  * Parsed and lowered forms of type definitions have been synchronized with their
    new keywords ([#23157]). Expression heads are renamed as follows:

    + `type`           => `struct`

    + `bitstype`       => `primitive` (order of arguments is also reversed, to match syntax)

    + `composite_type` => `struct_type`

    + `bits_type`      => `primitive_type`

  * The `global` keyword now only introduces a new binding if one doesn't already exist
    in the module.
    This means that assignment to a global (`global sin = 3`) may now throw the error:
    "cannot assign variable Base.sin from module Main", rather than emitting a warning.
    Additionally, the new bindings are now created before the statement is executed.
    For example, `f() = (global sin = "gluttony"; nothing)` will now resolve which module
    contains `sin` eagerly, rather than delaying that decision until `f` is run. ([#22984]).

  * `global const` declarations may no longer appear inside functions ([#12010]).

  * Uninitialized `BitArray` constructors of the form `BitArray[{N}](shape...)` have been
    deprecated in favor of equivalents accepting `undef` (an alias for
    `UndefInitializer()`) as their first argument, as in
    `BitArray[{N}](undef, shape...)`. For example, `BitVector(3)` is now
    `BitVector(undef, 3)`, `BitMatrix((2, 4))` is now
    `BitMatrix(undef, (2, 4))`, and `BitArray{3}(11, 13, 17)` is now
    `BitArray{3}(undef, 11, 14, 17)` ([#24785]).

  * Dispatch rules have been simplified:
    method matching is now determined exclusively by subtyping;
    the rule that method type parameters must also be captured has been removed.
    Instead, attempting to access the unconstrained parameters will throw an `UndefVarError`.
    Linting in package tests is recommended to confirm that the set of methods
    which might throw `UndefVarError` when accessing the static parameters
    (`need_to_handle_undef_sparam = Set{Any}(m.sig for m in Test.detect_unbound_args(Base, recursive=true))`)
    is equal (`==`) to some known set (`expected = Set()`). ([#23117])

  * `const` declarations on local variables were previously ignored. They now give a
    warning, so that this syntax can be disallowed or given a new meaning in a
    future version ([#5148]).

  * Placing an expression after `catch`, as in `catch f(x)`, is deprecated.
    Use `catch; f(x)` instead ([#19987]).

  * In `for i = ...`, if a local variable `i` already existed it would be overwritten
    during the loop. This behavior is deprecated, and in the future `for` loop variables
    will always be new variables local to the loop ([#22314]).
    The old behavior of overwriting an existing variable is available via `for outer i = ...`.

  * In `for i in x`, `x` used to be evaluated in a new scope enclosing the `for` loop.
    Now it is evaluated in the scope outside the `for` loop.

  * In `for i in x, j in y`, all variables now have fresh bindings on each iteration of the
    innermost loop. For example, an assignment to `i` will not be visible on the next `j`
    loop iteration ([#330]).

  * Variable bindings local to `while` loop bodies are now freshly allocated on each loop iteration,
    matching the behavior of `for` loops.

  * Prefix `&` for by-reference arguments to `ccall` has been deprecated in favor of
    `Ref` argument types ([#6080]).

  * The constructor `Ref(x::T)` now always returns a `Ref{T}` ([#21527]).

  * All line numbers in ASTs are represented by `LineNumberNode`s; the `:line` expression
    head is no longer used. `QuoteNode`s are also consistently used for quoted symbols instead
    of the `:quote` expression head (though `:quote` `Expr`s are still used for quoted
    expressions) ([#23885]).

  * The `+` and `-` methods for `Number` and `UniformScaling` are not ambiguous anymore since `+`
    and `-` no longer do automatic broadcasting. Hence, the methods for `UniformScaling` and `Number` are
    no longer deprecated ([#23923]).

  * The keyword `importall` is deprecated. Use `using` and/or individual `import` statements
    instead ([#22789]).

  * `reduce(+, [...])` and `reduce(*, [...])` no longer widen the iterated over arguments to
    system word size. `sum` and `prod` still preserve this behavior. ([#22825])

  * Like `_`, variable names consisting only of underscores can be assigned,
    but accessing their values is deprecated ([#24221]).

  * Raw string literal escaping rules have been changed to make it possible to write all strings.
    The rule is that backslashes escape both quotes and other backslashes, but only when a sequence
    of backslashes precedes a quote character. Thus, 2n backslashes followed by a quote encodes n
    backslashes and the end of the literal while 2n+1 backslashes followed by a quote encodes n
    backslashes followed by a quote character ([#22926]).

  * `reprmime(mime, x)` has been renamed to `repr(mime, x)`, and along with `repr(x)`
    and `sprint` it now accepts an optional `context` keyword for `IOContext` attributes.
    `stringmime` has been moved to the Base64 stdlib package ([#25990]).

  * The syntax `(x...)` for constructing a tuple is deprecated; use `(x...,)` instead ([#24452]).

  * Non-parenthesized interpolated variables in strings, e.g. `"$x"`, must be followed
    by a character that will never be an allowed identifier character (currently
    operators, space/control characters, or common punctuation characters) ([#25231]).

  * The syntax `using A.B` can now only be used when `A.B` is a module, and the syntax
    `using A: B` can only be used for adding single bindings ([#8000]).

  * `=>` now has its own precedence level, giving it strictly higher precedence than
    `=` and `,` ([#25391]).

  * The conditions under which unary operators followed by `(` are parsed as prefix function
    calls have changed ([#26154]).

  * `begin` is disallowed inside indexing expressions, in order to enable the syntax
    `a[begin]` (for selecting the first element) in the future ([#23354]).

  * Underscores for `_italics_` and `__bold__` are now supported by the Base Markdown
    parser. ([#25564])

  * `…` (`\dots`) and `⁝` (`\tricolon`) are now parsed as binary operators ([#26262]).

  * Assignment syntax (`a=b`) inside square bracket expressions (e.g. `A[...]`, `[x, y]`)
    is deprecated. It will likely be reclaimed in a later version for passing keyword
    arguments. Note this does not affect updating operators like `+=` ([#25631]).

  * `try` blocks without `catch` or `finally` are no longer allowed. An explicit empty
    `catch` block should be written instead ([#27554]).

  * `AbstractArray` types that use unconventional (not 1-based) indexing can now support
    `size`, `length`, and `@inbounds`. To optionally enforce conventional indices,
    you can `@assert !has_offset_axes(A)`.

  * Module pre-compilation is now the default for code loading. Adding a
    `__precompile__()` declaration is no longer necessary, although
    `__precompile__(false)` can still be used to opt-out ([#26991]).

Breaking changes
----------------

This section lists changes that do not have deprecation warnings.

  * The package manager `Pkg` has been replaced with a new one. See the manual entries on
    "Code Loading" and "Pkg" for documentation.

  * `replace(s::AbstractString, pat=>repl)` for function `repl` arguments formerly
    passed a substring to `repl` in all cases.  It now passes substrings for
    string patterns `pat`, but a `Char` for character patterns (when `pat` is a
    `Char`, collection of `Char`, or a character predicate) ([#25815]).

  * `readuntil` now does *not* include the delimiter in its result, matching the
    behavior of `readline`. Pass `keep=true` to get the old behavior ([#25633]).

  * `lu` methods now return decomposition objects such as `LU` rather than
    tuples of arrays or tuples of numbers ([#26997], [#27159], [#27212]).

  * `schur` methods now return decomposition objects such as `Schur` and
    `GeneralizedSchur` rather than tuples of arrays ([#26997], [#27159], [#27212]).

  * `lq` methods now return decomposition objects such as `LQ`
    rather than tuples of arrays ([#26997], [#27159], [#27212]).

  * `qr` methods now return decomposition objects such as `QR`, `QRPivoted`,
    and `QRCompactWY` rather than tuples of arrays ([#26997], [#27159], [#27212]).

  * `svd` methods now return decomposition objects such as `SVD` and
    `GeneralizedSVD` rather than tuples of arrays or tuples of numbers ([#26997], [#27159], [#27212]).

  * `countlines` now always counts the last non-empty line even if it does not
    end with EOL, matching the behavior of `eachline` and `readlines` ([#25845]).

  * `getindex(s::String, r::UnitRange{Int})` now throws `StringIndexError` if `last(r)`
    is not a valid index into `s` ([#22572]).

  * `ntuple(f, n::Integer)` throws `ArgumentError` if `n` is negative.
    Previously an empty tuple was returned ([#21697]).

  * `⋮`, `⋱`, `⋰`, and `⋯` are now parsed as binary operators, not ordinary
    identifiers.  `≔`, `≕`, and `⩴` now parse with assignment rather than comparison
    precedence ([#26262]).

  * Juxtaposing string literals (e.g. `"x"y`) is now a syntax error ([#20575]).

  * `finalizer(function, object)` now returns `object` rather than `nothing` ([#24679]).

  * The constructor of `SubString` now checks if the requested view range
    is defined by valid indices in the parent `AbstractString` ([#22511]).

  * Macro calls with `for` expressions are now parsed as generators inside
    function argument lists ([#18650]). Examples:

    + `sum(@inbounds a[i] for i = 1:n)` used to give a syntax error, but is now
      parsed as `sum(@inbounds(a[i]) for i = 1:n)`.

    + `sum(@m x for i = 1:n end)` used to parse the argument to `sum` as a 2-argument
      call to macro `@m`, but now parses it as a generator plus a syntax error
      for the dangling `end`.

  * `@__DIR__` returns the current working directory rather than `nothing` when not run
    from a file ([#21759]).

  * `@__FILE__` and `@__DIR__` return information relative to the file that it was parsed from,
    rather than from the task-local `SOURCE_PATH` global when it was expanded.

  * All macros receive an extra argument `__source__::LineNumberNode` which describes the
    parser location in the source file for the `@` of the macro call.
    It can be accessed as a normal argument variable in the body of the macro.
    This is implemented by inserting an extra leading argument into the
    `Expr(:macrocall, :@name, LineNumberNode(...), args...)`
    surface syntax. ([#21746])

  * Passing the same keyword argument multiple times is now a syntax error ([#16937]).

  * `getsockname` on a `TCPSocket` now returns the locally bound address and port
    of the socket. Previously the address of the remote endpoint was being
    returned ([#21825]).

  * The `~/.juliarc.jl` file has been moved to `~/.julia/config/startup.jl` and
    `/etc/julia/juliarc.jl` file has been renamed to `/etc/julia/startup.jl` ([#26161]).

  * Using `ARGS` within `startup.jl` files or within a .jl file loaded with `--load` will no
    longer contain the script name as the first argument. Instead, the script name will be
    assigned to `PROGRAM_FILE`. ([#22092])

  * The format for a `ClusterManager` specifying the cookie on the command line is now
    `--worker=<cookie>`. `--worker <cookie>` will not work as it is now an optional argument.

  * The representation of `CartesianRange` has changed to a
    tuple-of-AbstractUnitRanges; the `start` and `stop` fields are no
    longer present. Use `first(R)` and `last(R)` to obtain
    start/stop. ([#20974])

  * The `Diagonal`, `Bidiagonal`, `Tridiagonal` and `SymTridiagonal` type definitions have
    changed from `Diagonal{T}`, `Bidiagonal{T}`, `Tridiagonal{T}` and `SymTridiagonal{T}`
    to `Diagonal{T,V<:AbstractVector{T}}`, `Bidiagonal{T,V<:AbstractVector{T}}`,
    `Tridiagonal{T,V<:AbstractVector{T}}` and `SymTridiagonal{T,V<:AbstractVector{T}}`
    respectively ([#22718], [#22925], [#23035], [#23154]).

  * The immediate supertype of `BitArray` is now simply `AbstractArray`. `BitArray` is no longer
    considered a subtype of `DenseArray` and `StridedArray` ([#25858]).

  * When called with an argument that contains `NaN` elements, `findmin` and `findmax` now return the
    first `NaN` found and its corresponding index. Previously, `NaN` elements were ignored.
    The new behavior matches that of `min`, `max`, `minimum`, and `maximum`.

  * `isapprox(x,y)` now tests `norm(x-y) <= max(atol, rtol*max(norm(x), norm(y)))`
    rather than `norm(x-y) <= atol + ...`, and `rtol` defaults to zero
    if an `atol > 0` is specified ([#22742]).

  * Spaces are no longer allowed between `@` and the name of a macro in a macro call ([#22868]).

  * Juxtaposition of a non-literal with a macro call (`x@macro`) is no longer valid syntax ([#22868]).

  * On a cluster, all files are now loaded from the local file system rather than node 1 ([#22588]).
    To load the same file everywhere from node 1, one possible alternative is to broadcast a call to `include_string`:
    `@everywhere include_string(Main, $(read("filename", String)), "filename")`.
    Improving upon this API is left as an opportunity for packages.

  * `randperm(n)` and `randcycle(n)` now always return a `Vector{Int}` (independent of
    the type of `n`). Use the corresponding mutating functions `randperm!` and `randcycle!`
    to control the array type ([#22723]).

  * Hermitian now ignores any imaginary components in the diagonal instead of checking
    the diagonal. ([#17367])

  * Worker-worker connections are setup lazily for an `:all_to_all` topology. Use keyword
    arg `lazy=false` to force all connections to be setup during a `addprocs` call. ([#22814])

  * In `joinpath(a, b)` on Windows, if the drive specifications of `a` and `b` do not match,
    `joinpath` now returns `b` instead of throwing an `ArgumentError`. `joinpath(path...)` is
    defined to be left associative, so if any argument has a drive path which does not match
    the drive of the join of the preceding paths, the prior ones are dropped. ([#20912])

  * `^(A::AbstractMatrix{<:Integer}, p::Integer)` now throws a `DomainError`
    if `p < 0`, unless `A == one(A)` or `A == -one(A)` (same as for
    `^(A::Integer, p::Integer)`) ([#23366]).

  * `^(A::AbstractMatrix{<:Integer}, p::Integer)` now promotes the element type in the same
    way as `^(A::Integer, p::Integer)`. This means, for instance, that `[1 1; 0 1]^big(1)`
    will return a `Matrix{BigInt}` instead of a `Matrix{Int}` ([#23366]).

  * The element type of the input is now preserved in `unique`. Previously the element type
    of the output was shrunk to fit the union of the type of each element in the input.
    ([#22696])

  * The `promote` function now raises an error if its arguments are of different types
    and if attempting to convert them to a common type fails to change any of their types.
    This avoids stack overflows in the common case of definitions like
    `f(x, y) = f(promote(x, y)...)` ([#22801]).

  * `indmin` and `indmax` have been renamed to `argmin` and `argmax`, respectively ([#25654]).

  * `findmin`, `findmax`, `argmin`, and `argmax` used to always return linear indices.
    They now return `CartesianIndex`es for all but 1-d arrays, and in general return
    the `keys` of indexed collections (e.g. dictionaries) ([#22907]).

  * The `openspecfun` library is no longer built and shipped with Julia, as it is no longer
    used internally ([#22390]).

  * All loaded packages used to have bindings in `Main` (e.g. `Main.Package`). This is no
    longer the case; now bindings will only exist for packages brought into scope by
    typing `using Package` or `import Package` ([#17997]).

  * The rules for mixed-signedness integer arithmetic (e.g. `Int32(1) + UInt64(1)`) have been
    simplified: if the arguments have different sizes (in bits), then the type of the larger
    argument is used. If the arguments have the same size, the unsigned type is used ([#9292]).

  * All command line arguments passed via `-e`, `-E`, and `-L` will be executed in the order
    given on the command line ([#23665]).

  * `I` now yields `UniformScaling{Bool}(true)` rather than `UniformScaling{Int64}(1)`
    to better preserve types in operations involving `I` ([#24396]).

  * The return type of `reinterpret` has changed to `ReinterpretArray`. `reinterpret` on sparse
    arrays has been discontinued.

  * `Base.find_in_path` is now `Base.find_package` or `Base.find_source_file` ([#24320]).

  * `finalizer` now takes functions or pointers as its first argument, and the object being
    finalized as its second (rather than the reverse). For the majority of use cases
    deprecation warnings will be triggered. However, deprecation warnings will not trigger where
    (1) the callable argument is not a subtype of `Function`; or (2) both arguments are
    `Function`s or `Ptr{Cvoid}`s ([#24605]).

  * The `kill` function now throws errors on user error (e.g. on permission
    errors), but returns successfully if the process had previously exited.
    Its return value has been removed. Use the `process_running` function
    to determine if a process has already exited.

  * The logging system has been redesigned - `info` and `warn` are deprecated
    and replaced with the logging macros `@info`, `@warn`, `@debug` and
    `@error`.  The `logging` function is also deprecated and replaced with
    `AbstractLogger` and the functions from the new standard `Logging` library.
    ([#24490])

  * The `RevString` type has been removed from the language; `reverse(::String)` returns
    a `String` with code points (or fragments thereof) in reverse order. In general,
    `reverse(s)` should return a string of the same type and encoding as `s` with code
    points in reverse order; any string type overrides `reverse` to return a different
    type of string must also override `reverseind` to compute reversed indices correctly.

  * `eachindex(A, B...)` now requires that all inputs have the same number of elements.
    When the chosen indexing is Cartesian, they must have the same axes.

  * `AbstractRange` objects are now considered as equal to other `AbstractArray` objects
    by `==` and `isequal` if all of their elements are equal ([#16401]).
    This has required changing the hashing algorithm: ranges now use an O(N) fallback
    instead of a O(1) specialized method unless they define the `Base.RangeStepStyle`
    trait; see its documentation for details. Types which support subtraction (operator
    `-`) must now implement `widen` for hashing to work inside heterogeneous arrays.

  * `findn(x::AbstractArray)` has been deprecated in favor of `findall(!iszero, x)`, which
    now returns cartesian indices for multidimensional arrays (see below, [#25532]).

  * Broadcasting operations are no longer fused into a single operation by Julia's parser.
    Instead, a lazy `Broadcasted` object is created to represent the fused expression and
    then realized with `copy(bc::Broadcasted)` or `copyto!(dest, bc::Broadcasted)`
    to evaluate the wrapper. Consequently, package authors generally need to specialize
    `copy` and `copyto!` methods rather than `broadcast` and `broadcast!`. This also allows
    for more customization and control of fused broadcasts. See the
    [Interfaces chapter](https://docs.julialang.org/en/latest/manual/interfaces/#man-interfaces-broadcasting-1)
    for more information.

  * `find` has been renamed to `findall`. `findall`, `findfirst`, `findlast`, `findnext`
    now take and/or return the same type of indices as `keys`/`pairs` for `AbstractArray`,
    `AbstractDict`, `AbstractString`, `Tuple` and `NamedTuple` objects ([#24774], [#25545]).
    In particular, this means that they use `CartesianIndex` objects for matrices
    and higher-dimensional arrays instead of linear indices as was previously the case.
    Use `LinearIndices(a)[findall(f, a)]` and similar constructs to compute linear indices.

  * The `find*` functions, i.e. `findnext`, `findprev`, `findfirst`,
    and `findlast`, as well as `indexin`, now return `nothing` when no match is found rather
    than `0` or `0:-1` ([#25472], [#25662], [#26149])

  * The `Base.HasShape` iterator trait has gained a type parameter `N` indicating the
    number of dimensions, which must correspond to the length of the tuple returned by
    `size` ([#25655]).

 * `AbstractSet` objects are now considered equal by `==` and `isequal` if all of their
    elements are equal ([#25368]). This has required changing the hashing algorithm
    for `BitSet`.

  * the default behavior of `titlecase` is changed in two ways ([#23393]):
    + characters not starting a word are converted to lowercase;
      a new keyword argument `strict` is added which
      allows to get the old behavior when it's `false`.
    + any non-letter character is considered as a word separator;
      to get the old behavior (only "space" characters are considered as
      word separators), use the keyword `wordsep=isspace`.

  * `writedlm` in the standard library module DelimitedFiles now writes numeric values
    using `print` rather than `print_shortest` ([#25745]).

  * The `tempname` function used to create a file on Windows but not on other
    platforms. It now never creates a file ([#9053]).

  * The `fieldnames` and `propertynames` functions now return a tuple rather than
    an array ([#25725]).

  * `indexin` now returns the first rather than the last matching index ([#25998]).

  * `parse(::Type, ::Char)` now uses a default base of 10, like other number parsing
    methods, instead of 36 ([#26576]).

  * `isequal` for `Ptr`s now compares element types; `==` still compares only addresses
    ([#26858]).

  * `widen` on 8- and 16-bit integer types now widens to 16- and 32-bit types, respectively. ([#28045]).

  * `mv`,`cp`, `touch`, `mkdir`, `mkpath`, `chmod` and `chown` now return the path that was created/modified
    rather than `nothing` ([#27071]).

  * Regular expressions now default to UCP mode. Escape sequences such as `\w`
    will now match based on unicode character properties, e.g. `r"\w+"` will
    match `café` (not just `caf`). Add the `a` modifier (e.g. `r"\w+"a`) to
    restore the previous behavior ([#27189]).

  * `@sync` now waits only for *lexically* enclosed (i.e. visible directly in the source
    text of its argument) `@async` expressions. If you need to wait for a task created by
    a called function `f`, have `f` return the task and put `@async wait(f(...))` within
    the `@sync` block.
    This change makes `@schedule` redundant with `@async`, so `@schedule` has been
    deprecated ([#27164]).

  * `norm(A::AbstractMatrix, p=2)` computes no longer the operator/matrix norm but the `norm` of `A`
    as for other iterables, i.e. as if it were a vector. Especially, `norm(A::AbstractMatrix)` is the
    Frobenius norm. To compute the operator/matrix norm, use the new function `opnorm` ([#27401]).

  * `dot(u, v)` now acts recursively. Instead of `sum(u[i]' * v[i] for i in ...)`, it computes
    `sum(dot(u[i], v[i]) for i in ...)`, similarly to `vecdot` before ([#27401]).

  * `Sys.CPU_CORES` has been renamed to `Sys.CPU_THREADS`; it still gives the number
    of "logical cores" (including hyperthreading) rather than the number of physical
    cores present on the CPU. Similarly, the environment variable `JULIA_CPU_CORES` is
    deprecated in favor of `JULIA_CPU_THREADS` ([#27856]).

  * `WeakKeyDict` does not convert keys on insertion anymore (#24941).

Library improvements
--------------------

  * The function `thisind(s::AbstractString, i::Integer)` returns the largest valid index
    less or equal than `i` in the string `s` or `0` if no such index exists ([#24414]).

  * Support for Unicode 11 ([#28266]).

  * `Char` is now a subtype of `AbstractChar`, and most of the functions that
    take character arguments now accept any `AbstractChar` ([#26286]).

  * `pathof(module)` returns the path a module was imported from ([#28310]).

  * `bytes2hex` now accepts an optional `io` argument to output to a hexadecimal stream
    without allocating a `String` first ([#27121]).

  * `String(array)` now accepts an arbitrary `AbstractVector{UInt8}`. For `Vector`
    inputs, it "steals" the memory buffer, leaving them with an empty buffer which
    is guaranteed not to be shared with the `String` object. For other types of vectors
    (in particular immutable vectors), a copy is made and the input is not truncated ([#26093]).

  * `Irrational` is now a subtype of `AbstractIrrational` ([#24245]).

  * Introduced the `empty` function, the functional pair to `empty!` which returns a new,
    empty container ([#24390]).

  * Jump to first/last history entries in the REPL via "Alt-<" and "Alt->" ([#22829]).

  * REPL LaTeX-like tab completions have been simplified for several Unicode characters,
    e.g. `𝔸` is now `\bbA` rather than `\BbbA` ([#25980]).

  * The function `chop` now accepts two arguments `head` and `tail` allowing to specify
    number of characters to remove from the head and tail of the string ([#24126]).

  * `get(io, :color, false)` can now be used to query whether a stream `io` supports
    [ANSI color codes](https://en.wikipedia.org/wiki/ANSI_escape_code) ([#25067]),
    rather than using the undocumented `Base.have_color` global flag.

  * `print_with_color` has been deprecated in favor of
    `printstyled([io], xs...; bold=false, color=:normal)` for printing styled text ([#25522]).

  * Functions `first` and `last` now accept `nchar` argument for `AbstractString`.
    If this argument is used they return a string consisting of first/last `nchar`
    characters from the original string ([#23960]).

  * Expressions `x^-n` where `n` is an *integer literal* now correspond to `inv(x)^n`.
    For example, `x^-1` is now essentially a synonym for `inv(x)`, and works
    in a type-stable way even if `typeof(x) != typeof(inv(x))` ([#24240]).

  * New `Iterators.reverse(itr)` for reverse-order iteration ([#24187]).  Iterator
    types `T` can implement `start` etc. for `Iterators.Reverse{T}` to support this.

  * The functions `nextind` and `prevind` now accept `nchar` argument that indicates
    the number of characters to move ([#23805]).

  * The functions `strip`, `lstrip` and `rstrip` now return `SubString` ([#22496]).

  * The functions `strwidth` and `charwidth` have been merged into `textwidth`([#20816]).

  * The functions `base` and `digits` digits now accept a negative
    base (like `ndigits` did) ([#21692]).

  * The function `randn` now accepts complex arguments (`Complex{T <: AbstractFloat}`)
    ([#21973]).

  * `parse(Complex{T}, string)` can parse complex numbers in some common formats ([#24713]).

  * The function `rand` can now pick up random elements from strings, associatives
    and sets ([#22228], [#21960], [#18155], [#22224]).

  * It's now possible to specify the characters to pick from in the `randstring` function ([#22222]).

  * Allow multidimensional arrays in `shuffle` and `shuffle!` functions ([#22226]).

  * Method lists are now printed as a numbered list. In addition, the source code of a
    method can be opened in an editor by entering the corresponding number in the REPL
    and pressing `^Q` ([#22007]).

  * `getpeername` on a `TCPSocket` returns the address and port of the remote
    endpoint of the TCP connection ([#21825]).

  * `resize!` and `sizehint!` methods no longer over-reserve memory when the
    requested array size is more than double of its current size ([#22038]).

  * The `crc32c` function for CRC-32c checksums is now exported ([#22274]).

  * `eye(::Type{Diagonal{T}}, m::Integer)` has been deprecated in favor of
    `Diagonal{T}(I, m)` ([#24415]).

  * The output of `versioninfo` is now controlled with keyword arguments ([#21974]).

  * The function `LibGit2.set_remote_url` now always sets both the fetch and push URLs for a
    git repo. Additionally, the argument order was changed to be consistent with the git
    command line tool ([#22062]).

  * Added `unique!` which is an inplace version of `unique` ([#20549]).

  * `@test isequal(x, y)` and `@test isapprox(x, y)` now prints an evaluated expression when
    the test fails ([#22296]).

  * Uses of `Val{c}` in `Base` has been replaced with `Val{c}()`, which is now easily
    accessible via the efficient constructor `Val(c)`. Functions are defined as
    `f(::Val{c}) = ...` and called by `f(Val(c))`. Notable affected functions include:
    `ntuple`, `Base.literal_pow`, `sqrtm`, `lufact`, `lufact!`, `qrfact`, `qrfact!`,
    `cholfact`, `cholfact!`, `_broadcast!`, `reshape`, `cat` and `cat_t`.

  * A new `@macroexpand1` macro for non recursive macro expansion ([#21662]).

  * `Char`s can now be concatenated with `String`s and/or other `Char`s using `*` ([#22532]).

  * `Diagonal`, `Bidiagonal`, `Tridiagonal` and `SymTridiagonal` are now parameterized on
    the type of the wrapped vectors, allowing `Diagonal`, `Bidiagonal`, `Tridiagonal` and
    `SymTridiagonal` matrices with arbitrary `AbstractVector`s
    ([#22718], [#22925], [#23035], [#23154]).

  * Mutating versions of `randperm` and `randcycle` have been added:
    `randperm!` and `randcycle!` ([#22723]).

  * `BigFloat` random numbers can now be generated ([#22720]).

  * The efficiency of random generation for MersenneTwister RNGs has been improved for
    integers, `Float64` and ranges; as a result, given a seed, the produced stream of numbers
    has changed ([#27560], [#25277], [#25197], [#25058], [#25047]).

  * REPL Undo via Ctrl-/ and Ctrl-_

  * `diagm` now accepts several diagonal index/vector `Pair`s ([#24047]).

  * `isequal`, `==`, and `in` have one argument "curried" forms. For example `isequal(x)`
    returns a function that compares its argument to `x` using `isequal` ([#26436]).

  * `reinterpret` now works on any AbstractArray using the new `ReinterpretArray` type.
    This supersedes the old behavior of reinterpret on Arrays. As a result, reinterpreting
    arrays with different alignment requirements (removed in 0.6) is once again allowed ([#23750]).

  * The `keys` of an `Associative` are now an `AbstractSet`. `Base.KeyIterator{<:Associative}`
    has been changed to `KeySet{K, <:Associative{K}} <: AbstractSet{K}` ([#24580]).

  * New function `ncodeunits(s::AbstractString)` gives the number of code units in a string.
    The generic definition is constant time but calls `lastindex(s)` which may be inefficient.
    Therefore custom string types may want to define direct `ncodeunits` methods.

  * `reverseind(s::AbstractString, i::Integer)` now has an efficient generic fallback, so
    custom string types do not need to provide their own efficient definitions. The generic
    definition relies on `ncodeunits` however, so for optimal performance you may need to
    define a custom method for that function.

  * The global RNG is being re-seeded with its own seed at the beginning of each `@testset`,
    and have its original state restored at the end ([#24445]). This is breaking for testsets
    relying implicitly on the global RNG being in a specific state.

  * `permutedims(m::AbstractMatrix)` is now short for `permutedims(m, (2,1))`, and is now a
    more convenient way of making a "shallow transpose" of a 2D array. This is the
    recommended approach for manipulating arrays of data, rather than the recursively
    defined, linear-algebra function `transpose`. Similarly,
    `permutedims(v::AbstractVector)` will create a row matrix ([#24839]).

  * A new `replace(A, old=>new)` function is introduced to replace `old` by `new` in
    collection `A`. There is also another method with a different API, and
    a mutating variant, `replace!` ([#22324], [#25697], [#26206], [#27944]).

  * Adding integers to `CartesianIndex` objects is now deprecated. Instead of
    `i::Int + x::CartesianIndex`, use `i*one(x) + x` ([#26284]).

  * `CartesianRange` changes ([#24715]):
    - Inherits from `AbstractArray`, and linear indexing can be used to provide
      linear-to-cartesian conversion ([#24715])
    - It has a new constructor taking an array

  * several missing set-like operations have been added ([#23528]):
    `union`, `intersect`, `symdiff`, `setdiff` are now implemented for
    all collections with arbitrary many arguments, as well as the
    mutating counterparts (`union!` etc.). The performance is also
    much better in many cases. Note that this change is slightly
    breaking: all the non-mutating functions always return a new
    object even if only one argument is passed. Moreover the semantics
    of `intersect` and `symdiff` is changed for vectors:
    + `intersect` doesn't preserve the multiplicity anymore (use `filter` for
      the old behavior)
    + `symdiff` has been made consistent with the corresponding methods for
      other containers, by taking the multiplicity of the arguments into account.
      Use `unique` to get the old behavior.

  * The `linearindices` function has been deprecated in favor of the new
    `LinearIndices` type, which additionally provides conversion from
    cartesian indices to linear indices using the normal indexing operation.
    ([#24715], [#26775]).

  * `IdDict{K,V}` replaces `ObjectIdDict`.  It has type parameters
    like other `AbstractDict` subtypes and its constructors mirror the
    ones of `Dict`. ([#25210])

  * `IOBuffer` can take the `sizehint` keyword argument to suggest a capacity of
    the buffer ([#25944]).

  * `lstrip` and `rstrip` now accept a predicate function that defaults to `isspace`
    ([#27309]).

  * `trunc`, `floor`, `ceil`, and `round` specify `digits`, `sigdigits` and `base` using
    keyword arguments. ([#26156], [#26670])

  * `Sys.which()` provides a cross-platform method to find executable files, similar to
    the Unix `which` command. ([#26559])

  * Added an optimized method of `vecdot` for taking the Frobenius inner product
    of sparse matrices. ([#27470])

  * Added an optimized method of `kron` for taking the tensor product of two
    `Diagonal` matrices. ([27581])

  * An official API for extending `rand` is now defined ([#23964], [#25002]).

  * The constructor `MersenneTwister()` is re-enabled, producing a randomly initialized RNG
    (similar to `Random.seed!(MersenneTwister(0))`) ([#21909]).

  * `BitSet` can now store any `Int` (instead of only positive ones) ([#25029]).

  * The initial element `v0` in `reduce(op, v0, itr)` has been replaced with an `init`
    optional keyword argument, as in `reduce(op, itr; init=v0)`. Similarly for `foldl`,
    `foldr`, `mapreduce`, `mapfoldl`, `mapfoldr`, `accumulate` and `accumulate!`.
    ([#27711], [#27859])

Compiler/Runtime improvements
-----------------------------

  * The inlining heuristic now models the approximate runtime cost of
    a method (using some strongly-simplifying assumptions). Functions
    are inlined unless their estimated runtime cost substantially
    exceeds the cost of setting up and issuing a subroutine
    call. ([#22210], [#22732])

  * Inference recursion-detection heuristics are now more precise,
    allowing them to be triggered less often, but being more aggressive when they
    are triggered to drive the inference computation to a solution ([#23912]).

  * Inference now propagates constants inter-procedurally, and can compute
    various constants expressions at compile-time ([#24362]).

  * The LLVM SLP Vectorizer optimization pass is now enabled at the default
    optimization level.

Deprecated or removed
---------------------

  * The `JULIA_HOME` environment variable has been renamed to `JULIA_BINDIR` and
    `Base.JULIA_HOME` has been moved to `Sys.BINDIR` ([#20899]).

  * The keyword `immutable` is fully deprecated to `struct`, and
    `type` is fully deprecated to `mutable struct` ([#19157], [#20418]).

  * `lufact`, `schurfact`, `lqfact`, `qrfact`, `ldltfact`, `svdfact`,
    `bkfact`, `hessfact`, `eigfact`, and `cholfact` have respectively been
    deprecated to `lu`, `schur`, `lq`, `qr`, `ldlt`, `svd`, `bunchkaufman`,
    `hessenberg`, `eigen`, and `cholesky` ([#26997], [#27159], [#27212]).

  * `lufact!`, `schurfact!`, `lqfact!`, `qrfact!`, `ldltfact!`, `svdfact!`,
    `bkfact!`, `hessfact!`, and `eigfact!` have respectively been deprecated to
    `lu!`, `schur!`, `lq!`, `qr!`, `ldlt!`, `svd!`, `bunchkaufman!`,
    `hessenberg!`, and `eigen!` ([#26997], [#27159], [#27212]).

  * `eig(A[, args...])` has been deprecated in favor of `eigen(A[, args...])`.
    Whereas the former returns a tuple of arrays, the latter returns an `Eigen` object.
    So for a direct replacement, use `(eigen(A[, args...])...,)`. But going forward,
    consider using the direct result of `eigen(A[, args...])` instead, either
    destructured into its components (`vals, vecs = eigen(A[, args...])`) or
    as an `Eigen` object (`X = eigen(A[, args...])`) ([#26997], [#27159], [#27212]).

  * `eig(A::AbstractMatrix, B::AbstractMatrix)` and `eig(A::Number, B::Number)`
    have been deprecated in favor of `eigen(A, B)`. Whereas the former each return
    a tuple of arrays, the latter returns a `GeneralizedEigen` object. So for a direct
    replacement, use `(eigen(A, B)...,)`. But going forward, consider using the
    direct result of `eigen(A, B)` instead, either destructured into its components
    (`vals, vecs = eigen(A, B)`), or as a `GeneralizedEigen` object
    (`X = eigen(A, B)`) ([#26997], [#27159], [#27212]).

  * `ordschur(T::StridedMatrix{Ty}, Z::StridedMatrix{Ty}, select::Union{Vector{Bool},BitVector})`
    and `ordschur(S::StridedMatrix{Ty}, T::StridedMatrix{Ty}, Q::StridedMatrix{Ty},
    Z::StridedMatrix{Ty}, select::Union{Vector{Bool},BitVector})` and their respective
    inplace versions have been deprecated.
    Use `ordschur(schur::Schur, select::Union{Vector{Bool},BitVector})` and
    `ordschur(gschur::GeneralizedSchur, select::Union{Vector{Bool},BitVector})` instead
    ([#28155]).

  * Indexing into multidimensional arrays with more than one index but fewer indices than there are
    dimensions is no longer permitted when those trailing dimensions have lengths greater than 1.
    Instead, reshape the array or add trailing indices so the dimensionality and number of indices
    match ([#14770], [#23628]).

  * The use of a positional dimension argument has largely been deprecated in favor of a
    `dims` keyword argument. This includes the functions `sum`, `prod`, `maximum`,
    `minimum`, `all`, `any`, `findmax`, `findmin`, `mean`, `varm`, `std`, `var`, `cov`,
    `cor`, `median`, `mapreducedim`, `reducedim`, `sort`, `accumulate`, `accumulate!`,
    `cumsum`, `cumsum!`, `cumprod`, `cumprod!`, `flipdim`, `dropdims`, and `cat` ([#25501], [#26660], [#27100]).

  * `indices(a)` and `indices(a,d)` have been deprecated in favor of `axes(a)` and
    `axes(a, d)` ([#25057]).

  * `EnvHash` has been renamed to `EnvDict` ([#24167]).

  * Uninitialized `Array` constructors of the form
    `Array[{T,N}](shape...)` have been deprecated in favor of equivalents
    accepting `undef` (an alias for `UndefInitializer()`) as their first argument,
    as in `Array[{T,N}](undef, shape...)`. For example,
    `Vector(3)` is now `Vector(undef, 3)`, `Matrix{Int}((2, 4))` is now,
    `Matrix{Int}(undef, (2, 4))`, and `Array{Float32,3}(11, 13, 17)` is now
    `Array{Float32,3}(undef, 11, 13, 17)` ([#24781]).

  * Previously `setindex!(A, x, I...)` (and the syntax `A[I...] = x`) supported two
    different modes of operation when supplied with a set of non-scalar indices `I`
    (e.g., at least one index is an `AbstractArray`) depending upon the value of `x`
    on the right hand side. If `x` is an `AbstractArray`, its _contents_ are copied
    elementwise into the locations in `A` selected by `I` and it must have the same
    number of elements as `I` selects locations. Otherwise, if `x` is not an
    `AbstractArray`, then its _value_ is implicitly broadcast to all locations to
    all locations in `A` selected by `I`. This latter behavior—implicitly broadcasting
    "scalar"-like values across many locations—is now deprecated in favor of explicitly
    using the broadcasted assignment syntax `A[I...] .= x` or `fill!(view(A, I...), x)`
    ([#26347]).

  * `broadcast_getindex(A, I...)` and `broadcast_setindex!(A, v, I...)` are deprecated in
    favor of `getindex.((A,), I...)` and `setindex!.((A,), v, I...)`, respectively ([#27075]).

  * `LinAlg.fillslots!` has been renamed `LinAlg.fillstored!` ([#25030]).

  * `fill!(A::Diagonal, x)` and `fill!(A::AbstractTriangular, x)` have been deprecated
    in favor of `Base.LinAlg.fillstored!(A, x)` ([#24413]).

  * `eye` has been deprecated in favor of `I` and `Matrix` constructors. Please see the
    deprecation warnings for replacement details ([#24438]).

  * `zeros(D::Diagonal[, opts...])` has been deprecated ([#24654]).

  * Using Bool values directly as indices is now deprecated and will be an error in the future. Convert
    them to `Int` before indexing if you intend to access index `1` for `true` and `0` for `false`.

  * `slicedim(A, d, i)` has been deprecated in favor of `copy(selectdim(A, d, i))`. The new
    `selectdim` function now always returns a view into `A`; in many cases the `copy` is
    not necessary. Previously, `slicedim` on a vector `V` over dimension `d=1` and scalar
    index `i` would return the just selected element (unless `V` was a `BitVector`). This
    has now been made consistent: `selectdim` now always returns a view into the original
    array, with a zero-dimensional view in this specific case ([#26009]).

  * `whos` has been renamed `varinfo`, and now returns a markdown table instead of printing
    output ([#12131]).

  * Uninitialized `RowVector` constructors of the form `RowVector{T}(shape...)` have been
    deprecated in favor of equivalents accepting `undef` (an alias for
    `UndefInitializer()`) as their first argument, as in
    `RowVector{T}(undef, shape...)`. For example, `RowVector{Int}(3)` is now
    `RowVector{Int}(undef, 3)`, and `RowVector{Float32}((1, 4))` is now
    `RowVector{Float32}(undef, (1, 4))` ([#24786]).

  * `writecsv(io, a; opts...)` has been deprecated in favor of
    `writedlm(io, a, ','; opts...)` ([#23529]).

  * The method `srand(rng, filename, n=4)` has been deprecated ([#21359]).

  * `readcsv(io[, T::Type]; opts...)` has been deprecated in favor of
    `readdlm(io, ','[, T]; opts...)` ([#23530]).

  * `sparse(s::UniformScaling, m::Integer)` has been deprecated in favor of the
    three-argument equivalent `sparse(s::UniformScaling, m, n)` ([#24472]).

  * The `cholfact`/`cholfact!` methods that accepted an `uplo` symbol have been deprecated
    in favor of using `Hermitian` (or `Symmetric`) views ([#22187], [#22188]).

  * The `thin` keyword argument for orthogonal decomposition methods has
    been deprecated in favor of `full`, which has the opposite meaning:
    `thin == true` if and only if `full == false` ([#24279]).

  * `isposdef(A::AbstractMatrix, UL::Symbol)` and `isposdef!(A::AbstractMatrix, UL::Symbol)`
    have been deprecated in favor of `isposdef(Hermitian(A, UL))` and `isposdef!(Hermitian(A, UL))`
    respectively ([#22245]).

  * The `bkfact`/`bkfact!` methods that accepted `uplo` and `issymmetric` symbols have been deprecated
    in favor of using `Hermitian` (or `Symmetric`) views ([#22605]).

  * The function `current_module` is deprecated and replaced with `@__MODULE__`.
    This caused the deprecation of some reflection methods (such as `macroexpand` and
    `isconst`), which now require a module argument. And it caused the bugfix of other
    default arguments to use the Main module (including `whos`, `which`)  ([#22064]).

  * `expand(ex)` and `expand(module, ex)` have been deprecated in favor of
    `Meta.lower(module, ex)` ([#22064], [#24278]).

  * `ones(A::AbstractArray[, opts...])` and `zeros(A::AbstractArray[, opts...])` methods
    have been deprecated. For `zeros(A)`, consider `zero(A)`. For `ones(A)` or `zeros(A)`,
    consider `ones(size(A))`, `zeros(size(A))`, `fill(v, size(A))` for `v` an appropriate
    one or zero, `fill!(copy(A), {1|0})`, `fill!(similar(A), {1|0})`, or any of the preceding
    with different element type and/or shape depending on `opts...`. Where strictly
    necessary, consider `fill!(similar(A[, opts...]), {one(eltype(A)) | zero(eltype(A))})`.
    For an algebraic multiplicative identity, consider `one(A)` ([#24656]).

  * The `similar(dims->f(..., dims...), [T], axes...)` method to add offset array support
    to a function `f` that would otherwise create a non-offset array has been deprecated.
    Instead, call `f(..., axes...)` directly and, if needed, the offset array implementation
    should add offset axis support to the function `f` directly ([#26733]).

  * The functions `ones` and `zeros` used to accept any objects as dimensional arguments,
    implicitly converting them to `Int`s.  This is now deprecated; only `Integer`s or
    `AbstractUnitRange`s are accepted as arguments.  Instead, convert the arguments before
    calling `ones` or `zeros` ([#26733]).

  * The variadic `size(A, dim1, dim2, dims...)` method to return a tuple of multiple
    dimension lengths of `A` has been deprecated ([#26862]).

  * The `Operators` module is deprecated. Instead, import required operators explicitly
    from `Base`, e.g. `import Base: +, -, *, /` ([#22251]).

  * Bindings to the FFTW library have been removed from Base. The DFT framework for building FFT
    implementations is now in AbstractFFTs.jl, the bindings to the FFTW library are in FFTW.jl,
    and the Base signal processing functions which used FFTs are now in DSP.jl ([#21956]).

  * The `corrected` positional argument to `cov` has been deprecated in favor of
    a keyword argument with the same name ([#21709]).

  * Omitting spaces around the `?` and the `:` tokens in a ternary expression has been deprecated.
    Ternaries must now include some amount of whitespace, e.g. `x ? a : b` rather than
    `x?a:b` ([#22523] and [#22712]).

  * `?` can no longer be used as an identifier name ([#22712])

  * The method `replace(s::AbstractString, pat, r, [count])` is deprecated
    in favor of `replace(s::AbstractString, pat => r; [count])` ([#25165]).
    Moreover, `count` cannot be negative anymore (use `typemax(Int)` instead ([#22325]).

  * `read(io, type, dims)` is deprecated to `read!(io, Array{type}(undef, dims))` ([#21450]).

  * `read(::IO, ::Ref)` is now a method of `read!`, since it mutates its `Ref` argument ([#21592]).

  * `nb_available` is now `bytesavailable` ([#25634]).

  * `skipchars(io::IO, predicate; linecomment=nothing)` is deprecated in favor of
    `skipchars(predicate, io::IO; linecomment=nothing)` ([#25667]).

  * `Bidiagonal` constructors now use a `Symbol` (`:U` or `:L`) for the upper/lower
    argument, instead of a `Bool` or a `Char` ([#22703]).

  * `Bidiagonal`, `Tridiagonal` and `SymTridiagonal` constructors that automatically
    converted the input vectors to the same type are deprecated in favor of explicit
    conversion ([#22925], [#23035], [#23154].

  * Calling `nfields` on a type to find out how many fields its instances have is deprecated.
    Use `fieldcount` instead. Use `nfields` only to get the number of fields in a specific object ([#22350]).

  * `fieldnames` now operates only on types. To get the names of fields in an object, use
    `fieldnames(typeof(x))` ([#22350]).

  * `InexactError`, `DomainError`, and `OverflowError` now take
    arguments. `InexactError(func::Symbol, type, -3)` now prints as
    "ERROR: InexactError: func(type, -3)", `DomainError(val,
    [msg])` prints as "ERROR: DomainError with val:\nmsg",
    and `OverflowError(msg)` prints as "ERROR: OverflowError: msg".
    ([#20005], [#22751], [#22761])

  * The operating system identification functions: `is_linux`, `is_bsd`, `is_apple`, `is_unix`,
    and `is_windows`, have been deprecated in favor of `Sys.islinux`, `Sys.isbsd`, `Sys.isapple`,
    `Sys.isunix`, and `Sys.iswindows`, respectively ([#22182]).

  * The forms of `read`, `readstring`, and `eachline` that accepted both a `Cmd` object and an
    input stream are deprecated. Use e.g. `read(pipeline(stdin, cmd))` instead ([#22762]).

  * The unexported type `AbstractIOBuffer` has been renamed to `GenericIOBuffer` ([#17360] [#22796]).

  * `IOBuffer(data::AbstractVector{UInt8}, read::Bool, write::Bool, maxsize::Integer)`,
    `IOBuffer(read::Bool, write::Bool)`, and `IOBuffer(maxsize::Integer)` are
    deprecated in favor of constructors taking keyword arguments ([#25872]).

  * `Display` has been renamed to `AbstractDisplay` ([#24831]).

  * Remaining vectorized methods over `SparseVector`s, particularly `floor`, `ceil`,
    `trunc`, `round`, and most common transcendental functions such as `exp`, `log`, and
    `sin` variants, have been deprecated in favor of dot-syntax ([#22961]).

  * The method `String(io::IOBuffer)` is deprecated to `String(take!(copy(io)))` ([#21438]).

  * The function `readstring` is deprecated in favor of `read(io, String)` ([#22793])

  * The function `showall` is deprecated. Showing entire values is the default, unless an
    `IOContext` specifying `:limit=>true` is in use ([#22847]).

  * `issubtype` has been deprecated in favor of `<:` (which used to be an alias for `issubtype`).

  * Calling `write` on non-isbits arrays is deprecated in favor of explicit loops or
    `serialize` ([#6466]).

  * The default `startup.jl` file on Windows has been removed. Now must explicitly include the
    full path if you need access to executables or libraries in the `Sys.BINDIR` directory, e.g.
    `joinpath(Sys.BINDIR, "7z.exe")` for `7z.exe` ([#21540]).

  * `sqrtm` has been deprecated in favor of `sqrt` ([#23504]).

  * `expm` has been deprecated in favor of `exp` ([#23233]).

  * `logm` has been deprecated in favor of `log` ([#23505]).

  * `full` has been deprecated in favor of more specific, better defined alternatives.
    On structured matrices `A`, consider instead `Matrix(A)`, `Array(A)`,
    `SparseMatrixCSC(A)`, or `sparse(A)`. On sparse arrays `S`, consider instead
    `Vector(S)`, `Matrix(S)`, or `Array(S)` as appropriate. On factorizations `F`,
    consider instead `Matrix(F)`, `Array(F)`, `AbstractMatrix(F)`, or `AbstractArray(F)`.
    On implicit orthogonal factors `Q`, consider instead `Matrix(Q)` or `Array(Q)`; for
    implicit orthogonal factors that can be recovered in square or truncated form,
    see the deprecation message for square recovery instructions. On `Symmetric`,
    `Hermitian`, or `AbstractTriangular` matrices `A`, consider instead `Matrix(S)`,
    `Array(S)`, `SparseMatrixCSC(S)`, or `sparse(S)`. On `Symmetric` matrices `A`
    particularly, consider instead `LinAlg.copytri!(copy(parent(A)), A.uplo)`. On
    `Hermitian` matrices `A` particularly, consider instead
    `LinAlg.copytri!(copy(parent(A)), A.uplo, true)`. On `UpperTriangular` matrices `A`
    particularly, consider instead `triu!(copy(parent(A)))`. On `LowerTriangular` matrices
    `A` particularly, consider instead `tril!(copy(parent(A)))` ([#24250]).

  * `speye` has been deprecated in favor of `I`, `sparse`, and `SparseMatrixCSC`
    constructor methods ([#24356]).

  * Calling `union` with no arguments is deprecated; construct an empty set with an appropriate
    element type using `Set{T}()` instead ([#23144]).

  * Vectorized `DateTime`, `Date`, and `format` methods have been deprecated in favor of
    dot-syntax ([#23207]).

  * `Base.cpad` has been removed; use an appropriate combination of `rpad` and `lpad`
    instead ([#23187]).

  * `ctranspose` and `ctranspose!` have been deprecated in favor of `adjoint` and `adjoint!`,
    respectively ([#23235]).

  * `filter` and `filter!` on dictionaries now pass a single `key=>value` pair to the
    argument function, instead of two arguments ([#17886]).

  * `rol`, `rol!`, `ror`, and `ror!` have been deprecated in favor of specialized methods for
    `circshift`/`circshift!` ([#23404]).

  * `Base.SparseArrays.SpDiagIterator` has been removed ([#23261]).

  * The function `cfunction`, has been deprecated in favor of a macro form `@cfunction`.
    Most existing uses can be upgraded simply by adding a `@`.
    The new syntax now additionally supports allocating closures at runtime,
    for dealing with C APIs that don't provide a separate `void* env`-type callback
    argument. ([#26486])

  * `diagm(v::AbstractVector, k::Integer=0)` has been deprecated in favor of
    `diagm(k => v)` ([#24047]).

  * `diagm(x::Number)` has been deprecated in favor of `fill(x, 1, 1)` ([#24047]).

  * `diagm(A::SparseMatrixCSC)` has been deprecated in favor of
    `spdiagm(sparsevec(A))` ([#23341]).

  * `diagm(A::BitMatrix)` has been deprecated, use `diagm(0 => vec(A))` or
    `BitMatrix(Diagonal(vec(A)))` instead ([#23373], [#24047]).

  * `ℯ` (written as `\mscre<TAB>` or `\euler<TAB>`) is now the only (by default) exported
    name for Euler's number, and the type has changed from `Irrational{:e}` to
    `Irrational{:ℯ}` ([#23427]).

  * The mathematical constants `π`, `pi`, `ℯ`, `e`, `γ`, `eulergamma`, `catalan`, `φ` and
    `golden` have been moved from `Base` to a new module; `Base.MathConstants`.
    Only `π`, `pi` and `ℯ` are now exported by default from `Base` ([#23427]).

  * `eu` (previously an alias for `ℯ`) has been deprecated in favor of `ℯ` (or `MathConstants.e`) ([#23427]).

  * `GMP.gmp_version()`, `GMP.GMP_VERSION`, `GMP.gmp_bits_per_limb()`, and `GMP.GMP_BITS_PER_LIMB`
    have been renamed to `GMP.version()`, `GMP.VERSION`, `GMP.bits_per_limb()`, and `GMP.BITS_PER_LIMB`,
    respectively. Similarly, `MPFR.get_version()`, has been renamed to `MPFR.version()` ([#23323]). Also,
    `LinAlg.LAPACK.laver()` has been renamed to `LinAlg.LAPACK.version()` and now returns a `VersionNumber`.

  * `select`, `select!`, `selectperm` and `selectperm!` have been renamed respectively to
    `partialsort`, `partialsort!`, `partialsortperm` and `partialsortperm!` ([#23051]).

  * The `Range` abstract type has been renamed to `AbstractRange` ([#23570]).

  * `map` on dictionaries previously operated on `key=>value` pairs. This behavior is deprecated,
    and in the future `map` will operate only on values ([#5794]).

  * `map` on sets previously returned a `Set`, possibly changing the order or number of elements. This
    behavior is deprecated and in the future `map` will preserve order and number of elements ([#26980]).

  * Previously, broadcast defaulted to treating its arguments as scalars if they were not
    arrays. This behavior is deprecated, and in the future `broadcast` will default to
    iterating over all its arguments. Wrap arguments you wish to be treated as scalars with
    `Ref()` or a 1-tuple. Package developers can choose to allow a non-iterable type `T` to
    always behave as a scalar by implementing `broadcastable(x::T) = Ref(x)` ([#26212]).

  * Automatically broadcasted `+` and `-` for `array + scalar`, `scalar - array`, and so-on have
    been deprecated due to inconsistency with linear algebra. Use `.+` and `.-` for these operations
    instead ([#22880], [#22932]).

  * `flipbits!(B)` is deprecated in favor of using in-place broadcast to negate each element:
    `B .= .!B` ([#27067]).

  * `isleaftype` is deprecated in favor of the simpler predicates `isconcretetype` and `isdispatchtuple`.
    Concrete types are those that might equal `typeof(x)` for some `x`;
    `isleaftype` included some types for which this is not true. Those are now categorized more precisely
    as "dispatch tuple types" and "!has_free_typevars" (not exported). ([#17086], [#25496])

  * `contains(eq, itr, item)` is deprecated in favor of `any` with a predicate ([#23716]).

  * `spdiagm(x::AbstractVector)` has been deprecated in favor of `sparse(Diagonal(x))`
    alternatively `spdiagm(0 => x)` ([#23757]).

  * `spdiagm(x::AbstractVector, d::Integer)` and `spdiagm(x::Tuple{<:AbstractVector}, d::Tuple{<:Integer})`
    have been deprecated in favor of `spdiagm(d => x)` and `spdiagm(d[1] => x[1], d[2] => x[2], ...)`
    respectively. The new `spdiagm` implementation now always returns a square matrix ([#23757]).

  * `spones(A::AbstractSparseArray)` has been deprecated in favor of
    `LinAlg.fillstored!(copy(A), 1)` ([#25037]).

  * Constructors for `LibGit2.UserPasswordCredentials` and `LibGit2.SSHCredentials` which take a
    `prompt_if_incorrect` argument are deprecated. Instead, prompting behavior is controlled using
    the `allow_prompt` keyword in the `LibGit2.CredentialPayload` constructor ([#23690]).

  * `gradient` is deprecated and will be removed in the next release ([#23816]).

  * The timing functions `tic`, `toc`, and `toq` are deprecated in favor of `@time` and `@elapsed`
    ([#17046]).

  * Methods of `findfirst`, `findnext`, `findlast`, and `findprev` that accept a value to
    search for are deprecated in favor of passing a predicate ([#19186], [#10593]).

  * `find` functions now operate only on booleans by default. To look for non-zeros, use
    `x->x!=0` or `!iszero` ([#23120]).

  * The ability of `reinterpret` to yield `Array`s of different type than the underlying storage
    has been removed. The `reinterpret` function is still available, but now returns a
    `ReinterpretArray`. The three argument form of `reinterpret` that implicitly reshapes
    has been deprecated ([#23750]).

  * `bits` has been deprecated in favor of `bitstring` ([#24281], [#24263]).

  * `num2hex` and `hex2num` have been deprecated in favor of `reinterpret` combined with `parse`/`hex` ([#22088]).

  * `copy!` is deprecated for `AbstractSet` and `AbstractDict`, with the intention to re-enable
    it with a cleaner meaning in a future version ([#24844]).

  * `copy!` (resp. `unsafe_copy!`) is deprecated for `AbstractArray` and is renamed `copyto!`
    (resp. `unsafe_copyto!`); it will be re-introduced with a different meaning in a future
    version ([#24808]).

  * `a:b` is deprecated for constructing a `StepRange` when `a` and `b` have physical units
    (Dates and Times). Use `a:s:b`, where `s = Dates.Day(1)` or `s = Dates.Second(1)`.

  * `trues(A::AbstractArray)` and `falses(A::AbstractArray)` are deprecated in favor of
    `trues(size(A))` and `falses(size(A))` respectively ([#24595]).

  * `workspace` is discontinued, check out [Revise.jl](https://github.com/timholy/Revise.jl)
    for an alternative workflow ([#25046]).

  * `cumsum`, `cumprod`, `accumulate`, their mutating versions, and `diff` all now require a `dim`
    argument instead of defaulting to using the first dimension unless there is only
    one dimension ([#24684], [#25457]).

  * The `sum_kbn` and `cumsum_kbn` functions have been moved to the
    [KahanSummation](https://github.com/JuliaMath/KahanSummation.jl) package ([#24869]).

  * `isnumber` has been renamed to `isnumeric` ([#25021]).

  * `isalpha` has been renamed to `isletter` ([#26932]).

  * `is_assigned_char` and `normalize_string` have been renamed to `isassigned` and
    `normalize`, and moved to the new `Unicode` standard library module.
    `graphemes` has also been moved to that module ([#25021]).

  * Sparse array functionality has moved to the `SparseArrays` standard library module ([#25249]).

  * Linear algebra functionality, and specifically the `LinAlg` module has moved to the
    `LinearAlgebra` standard library module ([#25571]).

  * `@printf` and `@sprintf` have been moved to the `Printf` standard library ([#23929],[#25056]).

  * The `Libdl` module has moved to the `Libdl` standard library module ([#25459]).

  * The aliases `Complex32`, `Complex64` and `Complex128` have been deprecated in favor of `ComplexF16`,
    `ComplexF32` and `ComplexF64` respectively ([#24647]).

  * `Base.parentindexes` and `SharedArrays.localindexes` have been renamed to `parentindices`
    and `localindices`, respectively. Similarly, the `indexes` field in the `SubArray` type
    has been renamed to `indices` without deprecation ([#25088]).

  * `Associative` has been deprecated in favor of `AbstractDict` ([#25012]).

  * `Void` has been renamed back to `Nothing` with an alias `Cvoid` for use when calling C
    with a return type of `Cvoid` or a return or argument type of `Ptr{Cvoid}` ([#25162]).

  * `Nullable{T}` has been deprecated and moved to the Nullables package ([#23642]). Use
    `Union{T, Nothing}` instead, or `Union{Some{T}, Nothing}` if `nothing` is a possible
    value (i.e. `Nothing <: T`). `isnull(x)` can be replaced with `x === nothing` and
    `unsafe_get`/`get` can be dropped or replaced with `coalesce`.
    `NullException` has been removed.

  * `unshift!` and `shift!` have been renamed to `pushfirst!` and `popfirst!` ([#23902])

  * `ipermute!` has been deprecated in favor of `invpermute!` ([#25168]).

  * `CartesianRange` has been renamed `CartesianIndices` ([#24715]).

  * `sub2ind` and `ind2sub` are deprecated in favor of using `CartesianIndices` and `LinearIndices` ([#24715]).

  * `getindex(F::Factorization, s::Symbol)` (usually seen as e.g. `F[:Q]`) is deprecated
    in favor of dot overloading (`getproperty`) so factors should now be accessed as e.g.
    `F.Q` instead of `F[:Q]` ([#25184]).

  * `search` and `rsearch` have been deprecated in favor of `findfirst`/`findnext` and
    `findlast`/`findprev` respectively, in combination with curried `isequal` and `in`
    predicates for some methods ([#24673]).

  * `search(buf::IOBuffer, delim::UInt8)` has been deprecated in favor of either `occursin(delim, buf)`
    (to test containment) or `readuntil(buf, delim)` (to read data up to `delim`) ([#26600]).

  * `ismatch(regex, str)` has been deprecated in favor of `occursin(regex, str)` ([#26283]).

  * `matchall` has been deprecated in favor of `collect(m.match for m in eachmatch(r, s))` ([#26071]).

  * `similar(::Associative)` has been deprecated in favor of `empty(::Associative)`, and
    `similar(::Associative, ::Pair{K, V})` has been deprecated in favour of
    `empty(::Associative, K, V)` ([#24390]).

  * `findin(a, b)` has been deprecated in favor of `findall(in(b), a)` ([#24673]).

  * `module_name` has been deprecated in favor of a new, general `nameof` function. Similarly,
    the unexported `Base.function_name` and `Base.datatype_name` have been deprecated in favor
    of `nameof` methods ([#25622]).

  * The module `Random.dSFMT` is renamed `Random.DSFMT` ([#25567]).

  * `Random.RandomDevice(unlimited::Bool)` (on non-Windows systems) is deprecated in favor of
    `Random.RandomDevice(; unlimited=unlimited)` ([#25668]).

  * The generic implementations of `strides(::AbstractArray)` and `stride(::AbstractArray, ::Int)`
     have been deprecated. Subtypes of `AbstractArray` that implement the newly introduced strided
     array interface should define their own `strides` method ([#25321]).

  * `module_parent`, `Base.datatype_module`, and `Base.function_module` have been deprecated
    in favor of `parentmodule` ([#TODO]).

  * `rand(t::Tuple{Vararg{Int}})` is deprecated in favor of `rand(Float64, t)` or `rand(t...)`;
    `rand(::Tuple)` will have another meaning in the future ([#25429], [#25278]).

  * `randjump`, which produced an array, is deprecated in favor of the
    scalar version `Future.randjump` used with `accumulate` ([#27746]).

  * The `assert` function (and `@assert` macro) have been documented that they are not guaranteed to run under various optimization levels and should therefore not be used to e.g. verify passwords.

  * `ObjectIdDict` has been deprecated in favor of `IdDict{Any,Any}` ([#25210]).

  * `gc` and `gc_enable` have been deprecated in favor of `GC.gc` and `GC.enable` ([#25616]).

  * `Base.@gc_preserve` has been deprecated in favor of `GC.@preserve` ([#25616]).

  * `print_shortest` has been discontinued, but is still available in the `Base.Grisu`
    submodule ([#25745]).

  * `scale!` has been deprecated in favor of `mul!`, `lmul!`, and `rmul!` ([#25701], [#25812]).

  * The `remove_destination` keyword argument to `cp`, `mv`, and the unexported `cptree`
    has been renamed to `force` ([#25979]).

  * `contains` has been deprecated in favor of a more general `occursin` function, which
    takes its arguments in reverse order from `contains` ([#26283]).

  * `Regex` objects are no longer callable. Use `occursin` instead ([#26283]).

  * The methods of `range` based on positional arguments have been deprecated in favor of
    keyword arguments ([#25896]).

  * `linspace` has been deprecated in favor of `range` with `stop` and `length` keyword
    arguments ([#25896]).

  * `LinSpace` has been renamed to `LinRange` ([#25896]).

  * `logspace` has been deprecated to its definition ([#25896]).

  * `endof(a)` has been renamed to `lastindex(a)`, and the `end` keyword in indexing expressions now
    lowers to either `lastindex(a)` (in the case with only one index) or `lastindex(a, d)` (in cases
    where there is more than one index and `end` appears at dimension `d`) ([#23554], [#25763]).

  * `DateTime()`, `Date()`, and `Time()` have been deprecated, instead use `DateTime(1)`, `Date(1)`
    and `Time(0)` respectively ([#23724]).

  * The fallback method `^(x, p::Integer)` is deprecated. If your type relied on this definition,
    add a method such as `^(x::MyType, p::Integer) = Base.power_by_squaring(x, p)` ([#23332]).

  * `DevNull`, `STDIN`, `STDOUT`, and `STDERR` have been renamed to `devnull`, `stdin`, `stdout`,
    and `stderr`, respectively ([#25786]).

  * `wait` and `fetch` on `Task` now resemble the interface of `Future`.

  * `showcompact(io, x...)` has been deprecated in favor of
    `show(IOContext(io, :compact => true), x...)` ([#26080]).
    Use `sprint(show, x..., context=:compact => true)` instead of `sprint(showcompact, x...)`.

  * `isupper`, `islower`, `ucfirst` and `lcfirst` have been deprecated in favor of `isuppercase`,
    `islowercase`, `uppercasefirst` and `lowercasefirst`, respectively ([#26442]).

  * `signif` has been deprecated in favor of the `sigdigits` keyword argument to `round`.

  * `Base.IntSet` has been deprecated in favor of `Base.BitSet` ([#24282]).

  * `setrounding` has been deprecated for `Float32` and `Float64`, as the behaviour was too unreliable ([#26935]).

  * `gamma`, `lgamma`, `beta`, `lbeta` and `lfact` have been moved to
    [SpecialFunctions.jl](https://github.com/JuliaMath/SpecialFunctions.jl) ([#27459], [#27473]).

  * `atan2` is now a 2-argument method of `atan` ([#27248]).

  * The functions `eigs` and `svds` have been moved to the `Arpack.jl` package ([#27616]).

  * `vecdot` and `vecnorm` are deprecated in favor of `dot` and `norm`, respectively ([#27401]).

  * `clipboard` has been moved to the `InteractiveUtils` standard library package
    (along with other utilities mostly used at the interactive prompt, such as `edit`
    and `less`) ([#27635]).

  * `ndigits(n, b, [pad])` is deprecated in favor of `ndigits(n, base=b, pad=pad)` ([#27908]).

  * `squeeze` is deprecated in favor of `dropdims`.

  * `srand` is deprecated in favor of the unexported `Random.seed!` ([#27726]).

  * `realmin`/`realmax` are deprecated in favor of `floatmin`/`floatmax` ([#28302]).

  * `sortrows`/`sortcols` have been deprecated in favor of the more general `sortslices`.

  * `nextpow2`/`prevpow2` have been deprecated in favor of the more general `nextpow`/`prevpow` functions.

Command-line option changes
---------------------------

  * New option `--warn-overwrite={yes|no}` to control the warning for overwriting method
    definitions. The default is `no` ([#23002]).

  * New option `--banner={yes,no}` allows suppressing or forcing the printing of the
    startup banner, overriding the default behavior (banner in REPL, no banner otherwise).
    The `--quiet` option implies `--banner=no` even in REPL mode but can be overridden by
    passing `--quiet` together with `--banner=yes` ([#23342]).

  * The option `--precompiled` has been renamed to `--sysimage-native-code` ([#23054]).

  * The option `--compilecache` has been renamed to `--compiled-modules` ([#23054]).

<!--- generated by NEWS-update.jl: -->
[#330]: https://github.com/JuliaLang/julia/issues/330
[#1974]: https://github.com/JuliaLang/julia/issues/1974
[#4916]: https://github.com/JuliaLang/julia/issues/4916
[#5148]: https://github.com/JuliaLang/julia/issues/5148
[#5794]: https://github.com/JuliaLang/julia/issues/5794
[#6080]: https://github.com/JuliaLang/julia/issues/6080
[#6466]: https://github.com/JuliaLang/julia/issues/6466
[#6614]: https://github.com/JuliaLang/julia/issues/6614
[#8000]: https://github.com/JuliaLang/julia/issues/8000
[#8470]: https://github.com/JuliaLang/julia/issues/8470
[#9053]: https://github.com/JuliaLang/julia/issues/9053
[#9292]: https://github.com/JuliaLang/julia/issues/9292
[#10593]: https://github.com/JuliaLang/julia/issues/10593
[#11310]: https://github.com/JuliaLang/julia/issues/11310
[#12010]: https://github.com/JuliaLang/julia/issues/12010
[#12131]: https://github.com/JuliaLang/julia/issues/12131
[#13079]: https://github.com/JuliaLang/julia/issues/13079
[#14770]: https://github.com/JuliaLang/julia/issues/14770
[#15120]: https://github.com/JuliaLang/julia/issues/15120
[#16356]: https://github.com/JuliaLang/julia/issues/16356
[#16401]: https://github.com/JuliaLang/julia/issues/16401
[#16937]: https://github.com/JuliaLang/julia/issues/16937
[#17046]: https://github.com/JuliaLang/julia/issues/17046
[#17086]: https://github.com/JuliaLang/julia/issues/17086
[#17240]: https://github.com/JuliaLang/julia/issues/17240
[#17360]: https://github.com/JuliaLang/julia/issues/17360
[#17367]: https://github.com/JuliaLang/julia/issues/17367
[#17886]: https://github.com/JuliaLang/julia/issues/17886
[#17997]: https://github.com/JuliaLang/julia/issues/17997
[#18155]: https://github.com/JuliaLang/julia/issues/18155
[#18650]: https://github.com/JuliaLang/julia/issues/18650
[#19089]: https://github.com/JuliaLang/julia/issues/19089
[#19157]: https://github.com/JuliaLang/julia/issues/19157
[#19186]: https://github.com/JuliaLang/julia/issues/19186
[#19987]: https://github.com/JuliaLang/julia/issues/19987
[#20005]: https://github.com/JuliaLang/julia/issues/20005
[#20418]: https://github.com/JuliaLang/julia/issues/20418
[#20549]: https://github.com/JuliaLang/julia/issues/20549
[#20575]: https://github.com/JuliaLang/julia/issues/20575
[#20816]: https://github.com/JuliaLang/julia/issues/20816
[#20899]: https://github.com/JuliaLang/julia/issues/20899
[#20912]: https://github.com/JuliaLang/julia/issues/20912
[#20974]: https://github.com/JuliaLang/julia/issues/20974
[#21359]: https://github.com/JuliaLang/julia/issues/21359
[#21438]: https://github.com/JuliaLang/julia/issues/21438
[#21450]: https://github.com/JuliaLang/julia/issues/21450
[#21527]: https://github.com/JuliaLang/julia/issues/21527
[#21540]: https://github.com/JuliaLang/julia/issues/21540
[#21592]: https://github.com/JuliaLang/julia/issues/21592
[#21662]: https://github.com/JuliaLang/julia/issues/21662
[#21692]: https://github.com/JuliaLang/julia/issues/21692
[#21697]: https://github.com/JuliaLang/julia/issues/21697
[#21709]: https://github.com/JuliaLang/julia/issues/21709
[#21746]: https://github.com/JuliaLang/julia/issues/21746
[#21759]: https://github.com/JuliaLang/julia/issues/21759
[#21774]: https://github.com/JuliaLang/julia/issues/21774
[#21825]: https://github.com/JuliaLang/julia/issues/21825
[#21909]: https://github.com/JuliaLang/julia/issues/21909
[#21956]: https://github.com/JuliaLang/julia/issues/21956
[#21960]: https://github.com/JuliaLang/julia/issues/21960
[#21973]: https://github.com/JuliaLang/julia/issues/21973
[#21974]: https://github.com/JuliaLang/julia/issues/21974
[#22007]: https://github.com/JuliaLang/julia/issues/22007
[#22038]: https://github.com/JuliaLang/julia/issues/22038
[#22062]: https://github.com/JuliaLang/julia/issues/22062
[#22064]: https://github.com/JuliaLang/julia/issues/22064
[#22088]: https://github.com/JuliaLang/julia/issues/22088
[#22089]: https://github.com/JuliaLang/julia/issues/22089
[#22092]: https://github.com/JuliaLang/julia/issues/22092
[#22182]: https://github.com/JuliaLang/julia/issues/22182
[#22187]: https://github.com/JuliaLang/julia/issues/22187
[#22188]: https://github.com/JuliaLang/julia/issues/22188
[#22194]: https://github.com/JuliaLang/julia/issues/22194
[#22210]: https://github.com/JuliaLang/julia/issues/22210
[#22222]: https://github.com/JuliaLang/julia/issues/22222
[#22224]: https://github.com/JuliaLang/julia/issues/22224
[#22226]: https://github.com/JuliaLang/julia/issues/22226
[#22228]: https://github.com/JuliaLang/julia/issues/22228
[#22245]: https://github.com/JuliaLang/julia/issues/22245
[#22251]: https://github.com/JuliaLang/julia/issues/22251
[#22274]: https://github.com/JuliaLang/julia/issues/22274
[#22281]: https://github.com/JuliaLang/julia/issues/22281
[#22296]: https://github.com/JuliaLang/julia/issues/22296
[#22314]: https://github.com/JuliaLang/julia/issues/22314
[#22324]: https://github.com/JuliaLang/julia/issues/22324
[#22325]: https://github.com/JuliaLang/julia/issues/22325
[#22350]: https://github.com/JuliaLang/julia/issues/22350
[#22390]: https://github.com/JuliaLang/julia/issues/22390
[#22496]: https://github.com/JuliaLang/julia/issues/22496
[#22511]: https://github.com/JuliaLang/julia/issues/22511
[#22523]: https://github.com/JuliaLang/julia/issues/22523
[#22532]: https://github.com/JuliaLang/julia/issues/22532
[#22572]: https://github.com/JuliaLang/julia/issues/22572
[#22588]: https://github.com/JuliaLang/julia/issues/22588
[#22605]: https://github.com/JuliaLang/julia/issues/22605
[#22666]: https://github.com/JuliaLang/julia/issues/22666
[#22696]: https://github.com/JuliaLang/julia/issues/22696
[#22703]: https://github.com/JuliaLang/julia/issues/22703
[#22712]: https://github.com/JuliaLang/julia/issues/22712
[#22718]: https://github.com/JuliaLang/julia/issues/22718
[#22720]: https://github.com/JuliaLang/julia/issues/22720
[#22723]: https://github.com/JuliaLang/julia/issues/22723
[#22732]: https://github.com/JuliaLang/julia/issues/22732
[#22742]: https://github.com/JuliaLang/julia/issues/22742
[#22751]: https://github.com/JuliaLang/julia/issues/22751
[#22761]: https://github.com/JuliaLang/julia/issues/22761
[#22762]: https://github.com/JuliaLang/julia/issues/22762
[#22789]: https://github.com/JuliaLang/julia/issues/22789
[#22793]: https://github.com/JuliaLang/julia/issues/22793
[#22796]: https://github.com/JuliaLang/julia/issues/22796
[#22800]: https://github.com/JuliaLang/julia/issues/22800
[#22801]: https://github.com/JuliaLang/julia/issues/22801
[#22814]: https://github.com/JuliaLang/julia/issues/22814
[#22825]: https://github.com/JuliaLang/julia/issues/22825
[#22829]: https://github.com/JuliaLang/julia/issues/22829
[#22847]: https://github.com/JuliaLang/julia/issues/22847
[#22868]: https://github.com/JuliaLang/julia/issues/22868
[#22880]: https://github.com/JuliaLang/julia/issues/22880
[#22907]: https://github.com/JuliaLang/julia/issues/22907
[#22925]: https://github.com/JuliaLang/julia/issues/22925
[#22926]: https://github.com/JuliaLang/julia/issues/22926
[#22932]: https://github.com/JuliaLang/julia/issues/22932
[#22961]: https://github.com/JuliaLang/julia/issues/22961
[#22984]: https://github.com/JuliaLang/julia/issues/22984
[#23002]: https://github.com/JuliaLang/julia/issues/23002
[#23035]: https://github.com/JuliaLang/julia/issues/23035
[#23051]: https://github.com/JuliaLang/julia/issues/23051
[#23054]: https://github.com/JuliaLang/julia/issues/23054
[#23117]: https://github.com/JuliaLang/julia/issues/23117
[#23120]: https://github.com/JuliaLang/julia/issues/23120
[#23144]: https://github.com/JuliaLang/julia/issues/23144
[#23154]: https://github.com/JuliaLang/julia/issues/23154
[#23157]: https://github.com/JuliaLang/julia/issues/23157
[#23168]: https://github.com/JuliaLang/julia/issues/23168
[#23187]: https://github.com/JuliaLang/julia/issues/23187
[#23207]: https://github.com/JuliaLang/julia/issues/23207
[#23233]: https://github.com/JuliaLang/julia/issues/23233
[#23235]: https://github.com/JuliaLang/julia/issues/23235
[#23261]: https://github.com/JuliaLang/julia/issues/23261
[#23323]: https://github.com/JuliaLang/julia/issues/23323
[#23332]: https://github.com/JuliaLang/julia/issues/23332
[#23341]: https://github.com/JuliaLang/julia/issues/23341
[#23342]: https://github.com/JuliaLang/julia/issues/23342
[#23354]: https://github.com/JuliaLang/julia/issues/23354
[#23366]: https://github.com/JuliaLang/julia/issues/23366
[#23373]: https://github.com/JuliaLang/julia/issues/23373
[#23393]: https://github.com/JuliaLang/julia/issues/23393
[#23404]: https://github.com/JuliaLang/julia/issues/23404
[#23427]: https://github.com/JuliaLang/julia/issues/23427
[#23504]: https://github.com/JuliaLang/julia/issues/23504
[#23505]: https://github.com/JuliaLang/julia/issues/23505
[#23519]: https://github.com/JuliaLang/julia/issues/23519
[#23528]: https://github.com/JuliaLang/julia/issues/23528
[#23529]: https://github.com/JuliaLang/julia/issues/23529
[#23530]: https://github.com/JuliaLang/julia/issues/23530
[#23554]: https://github.com/JuliaLang/julia/issues/23554
[#23570]: https://github.com/JuliaLang/julia/issues/23570
[#23628]: https://github.com/JuliaLang/julia/issues/23628
[#23642]: https://github.com/JuliaLang/julia/issues/23642
[#23665]: https://github.com/JuliaLang/julia/issues/23665
[#23690]: https://github.com/JuliaLang/julia/issues/23690
[#23716]: https://github.com/JuliaLang/julia/issues/23716
[#23724]: https://github.com/JuliaLang/julia/issues/23724
[#23750]: https://github.com/JuliaLang/julia/issues/23750
[#23757]: https://github.com/JuliaLang/julia/issues/23757
[#23805]: https://github.com/JuliaLang/julia/issues/23805
[#23816]: https://github.com/JuliaLang/julia/issues/23816
[#23885]: https://github.com/JuliaLang/julia/issues/23885
[#23902]: https://github.com/JuliaLang/julia/issues/23902
[#23912]: https://github.com/JuliaLang/julia/issues/23912
[#23923]: https://github.com/JuliaLang/julia/issues/23923
[#23929]: https://github.com/JuliaLang/julia/issues/23929
[#23960]: https://github.com/JuliaLang/julia/issues/23960
[#23964]: https://github.com/JuliaLang/julia/issues/23964
[#24047]: https://github.com/JuliaLang/julia/issues/24047
[#24126]: https://github.com/JuliaLang/julia/issues/24126
[#24153]: https://github.com/JuliaLang/julia/issues/24153
[#24167]: https://github.com/JuliaLang/julia/issues/24167
[#24187]: https://github.com/JuliaLang/julia/issues/24187
[#24221]: https://github.com/JuliaLang/julia/issues/24221
[#24240]: https://github.com/JuliaLang/julia/issues/24240
[#24245]: https://github.com/JuliaLang/julia/issues/24245
[#24250]: https://github.com/JuliaLang/julia/issues/24250
[#24263]: https://github.com/JuliaLang/julia/issues/24263
[#24278]: https://github.com/JuliaLang/julia/issues/24278
[#24279]: https://github.com/JuliaLang/julia/issues/24279
[#24281]: https://github.com/JuliaLang/julia/issues/24281
[#24282]: https://github.com/JuliaLang/julia/issues/24282
[#24320]: https://github.com/JuliaLang/julia/issues/24320
[#24356]: https://github.com/JuliaLang/julia/issues/24356
[#24362]: https://github.com/JuliaLang/julia/issues/24362
[#24390]: https://github.com/JuliaLang/julia/issues/24390
[#24396]: https://github.com/JuliaLang/julia/issues/24396
[#24404]: https://github.com/JuliaLang/julia/issues/24404
[#24413]: https://github.com/JuliaLang/julia/issues/24413
[#24414]: https://github.com/JuliaLang/julia/issues/24414
[#24415]: https://github.com/JuliaLang/julia/issues/24415
[#24438]: https://github.com/JuliaLang/julia/issues/24438
[#24445]: https://github.com/JuliaLang/julia/issues/24445
[#24452]: https://github.com/JuliaLang/julia/issues/24452
[#24472]: https://github.com/JuliaLang/julia/issues/24472
[#24490]: https://github.com/JuliaLang/julia/issues/24490
[#24580]: https://github.com/JuliaLang/julia/issues/24580
[#24595]: https://github.com/JuliaLang/julia/issues/24595
[#24605]: https://github.com/JuliaLang/julia/issues/24605
[#24647]: https://github.com/JuliaLang/julia/issues/24647
[#24653]: https://github.com/JuliaLang/julia/issues/24653
[#24654]: https://github.com/JuliaLang/julia/issues/24654
[#24656]: https://github.com/JuliaLang/julia/issues/24656
[#24673]: https://github.com/JuliaLang/julia/issues/24673
[#24679]: https://github.com/JuliaLang/julia/issues/24679
[#24684]: https://github.com/JuliaLang/julia/issues/24684
[#24713]: https://github.com/JuliaLang/julia/issues/24713
[#24715]: https://github.com/JuliaLang/julia/issues/24715
[#24774]: https://github.com/JuliaLang/julia/issues/24774
[#24781]: https://github.com/JuliaLang/julia/issues/24781
[#24785]: https://github.com/JuliaLang/julia/issues/24785
[#24786]: https://github.com/JuliaLang/julia/issues/24786
[#24808]: https://github.com/JuliaLang/julia/issues/24808
[#24831]: https://github.com/JuliaLang/julia/issues/24831
[#24839]: https://github.com/JuliaLang/julia/issues/24839
[#24844]: https://github.com/JuliaLang/julia/issues/24844
[#24869]: https://github.com/JuliaLang/julia/issues/24869
[#25002]: https://github.com/JuliaLang/julia/issues/25002
[#25012]: https://github.com/JuliaLang/julia/issues/25012
[#25021]: https://github.com/JuliaLang/julia/issues/25021
[#25029]: https://github.com/JuliaLang/julia/issues/25029
[#25030]: https://github.com/JuliaLang/julia/issues/25030
[#25037]: https://github.com/JuliaLang/julia/issues/25037
[#25046]: https://github.com/JuliaLang/julia/issues/25046
[#25047]: https://github.com/JuliaLang/julia/issues/25047
[#25056]: https://github.com/JuliaLang/julia/issues/25056
[#25057]: https://github.com/JuliaLang/julia/issues/25057
[#25058]: https://github.com/JuliaLang/julia/issues/25058
[#25067]: https://github.com/JuliaLang/julia/issues/25067
[#25088]: https://github.com/JuliaLang/julia/issues/25088
[#25162]: https://github.com/JuliaLang/julia/issues/25162
[#25165]: https://github.com/JuliaLang/julia/issues/25165
[#25168]: https://github.com/JuliaLang/julia/issues/25168
[#25184]: https://github.com/JuliaLang/julia/issues/25184
[#25197]: https://github.com/JuliaLang/julia/issues/25197
[#25210]: https://github.com/JuliaLang/julia/issues/25210
[#25231]: https://github.com/JuliaLang/julia/issues/25231
[#25249]: https://github.com/JuliaLang/julia/issues/25249
[#25277]: https://github.com/JuliaLang/julia/issues/25277
[#25278]: https://github.com/JuliaLang/julia/issues/25278
[#25311]: https://github.com/JuliaLang/julia/issues/25311
[#25321]: https://github.com/JuliaLang/julia/issues/25321
[#25368]: https://github.com/JuliaLang/julia/issues/25368
[#25391]: https://github.com/JuliaLang/julia/issues/25391
[#25424]: https://github.com/JuliaLang/julia/issues/25424
[#25429]: https://github.com/JuliaLang/julia/issues/25429
[#25457]: https://github.com/JuliaLang/julia/issues/25457
[#25459]: https://github.com/JuliaLang/julia/issues/25459
[#25472]: https://github.com/JuliaLang/julia/issues/25472
[#25496]: https://github.com/JuliaLang/julia/issues/25496
[#25501]: https://github.com/JuliaLang/julia/issues/25501
[#25522]: https://github.com/JuliaLang/julia/issues/25522
[#25532]: https://github.com/JuliaLang/julia/issues/25532
[#25545]: https://github.com/JuliaLang/julia/issues/25545
[#25564]: https://github.com/JuliaLang/julia/issues/25564
[#25567]: https://github.com/JuliaLang/julia/issues/25567
[#25571]: https://github.com/JuliaLang/julia/issues/25571
[#25616]: https://github.com/JuliaLang/julia/issues/25616
[#25622]: https://github.com/JuliaLang/julia/issues/25622
[#25631]: https://github.com/JuliaLang/julia/issues/25631
[#25633]: https://github.com/JuliaLang/julia/issues/25633
[#25634]: https://github.com/JuliaLang/julia/issues/25634
[#25654]: https://github.com/JuliaLang/julia/issues/25654
[#25655]: https://github.com/JuliaLang/julia/issues/25655
[#25662]: https://github.com/JuliaLang/julia/issues/25662
[#25667]: https://github.com/JuliaLang/julia/issues/25667
[#25668]: https://github.com/JuliaLang/julia/issues/25668
[#25697]: https://github.com/JuliaLang/julia/issues/25697
[#25701]: https://github.com/JuliaLang/julia/issues/25701
[#25725]: https://github.com/JuliaLang/julia/issues/25725
[#25745]: https://github.com/JuliaLang/julia/issues/25745
[#25763]: https://github.com/JuliaLang/julia/issues/25763
[#25786]: https://github.com/JuliaLang/julia/issues/25786
[#25812]: https://github.com/JuliaLang/julia/issues/25812
[#25815]: https://github.com/JuliaLang/julia/issues/25815
[#25830]: https://github.com/JuliaLang/julia/issues/25830
[#25845]: https://github.com/JuliaLang/julia/issues/25845
[#25854]: https://github.com/JuliaLang/julia/issues/25854
[#25858]: https://github.com/JuliaLang/julia/issues/25858
[#25872]: https://github.com/JuliaLang/julia/issues/25872
[#25896]: https://github.com/JuliaLang/julia/issues/25896
[#25944]: https://github.com/JuliaLang/julia/issues/25944
[#25947]: https://github.com/JuliaLang/julia/issues/25947
[#25979]: https://github.com/JuliaLang/julia/issues/25979
[#25980]: https://github.com/JuliaLang/julia/issues/25980
[#25990]: https://github.com/JuliaLang/julia/issues/25990
[#25998]: https://github.com/JuliaLang/julia/issues/25998
[#26009]: https://github.com/JuliaLang/julia/issues/26009
[#26071]: https://github.com/JuliaLang/julia/issues/26071
[#26080]: https://github.com/JuliaLang/julia/issues/26080
[#26093]: https://github.com/JuliaLang/julia/issues/26093
[#26149]: https://github.com/JuliaLang/julia/issues/26149
[#26154]: https://github.com/JuliaLang/julia/issues/26154
[#26156]: https://github.com/JuliaLang/julia/issues/26156
[#26161]: https://github.com/JuliaLang/julia/issues/26161
[#26206]: https://github.com/JuliaLang/julia/issues/26206
[#26212]: https://github.com/JuliaLang/julia/issues/26212
[#26262]: https://github.com/JuliaLang/julia/issues/26262
[#26283]: https://github.com/JuliaLang/julia/issues/26283
[#26284]: https://github.com/JuliaLang/julia/issues/26284
[#26286]: https://github.com/JuliaLang/julia/issues/26286
[#26347]: https://github.com/JuliaLang/julia/issues/26347
[#26436]: https://github.com/JuliaLang/julia/issues/26436
[#26442]: https://github.com/JuliaLang/julia/issues/26442
[#26486]: https://github.com/JuliaLang/julia/issues/26486
[#26559]: https://github.com/JuliaLang/julia/issues/26559
[#26576]: https://github.com/JuliaLang/julia/issues/26576
[#26600]: https://github.com/JuliaLang/julia/issues/26600
[#26660]: https://github.com/JuliaLang/julia/issues/26660
[#26670]: https://github.com/JuliaLang/julia/issues/26670
[#26733]: https://github.com/JuliaLang/julia/issues/26733
[#26775]: https://github.com/JuliaLang/julia/issues/26775
[#26858]: https://github.com/JuliaLang/julia/issues/26858
[#26862]: https://github.com/JuliaLang/julia/issues/26862
[#26932]: https://github.com/JuliaLang/julia/issues/26932
[#26935]: https://github.com/JuliaLang/julia/issues/26935
[#26980]: https://github.com/JuliaLang/julia/issues/26980
[#26997]: https://github.com/JuliaLang/julia/issues/26997
[#27067]: https://github.com/JuliaLang/julia/issues/27067
[#27071]: https://github.com/JuliaLang/julia/issues/27071
[#27075]: https://github.com/JuliaLang/julia/issues/27075
[#27100]: https://github.com/JuliaLang/julia/issues/27100
[#27121]: https://github.com/JuliaLang/julia/issues/27121
[#27159]: https://github.com/JuliaLang/julia/issues/27159
[#27164]: https://github.com/JuliaLang/julia/issues/27164
[#27189]: https://github.com/JuliaLang/julia/issues/27189
[#27212]: https://github.com/JuliaLang/julia/issues/27212
[#27248]: https://github.com/JuliaLang/julia/issues/27248
[#27309]: https://github.com/JuliaLang/julia/issues/27309
[#27401]: https://github.com/JuliaLang/julia/issues/27401
[#27447]: https://github.com/JuliaLang/julia/issues/27447
[#27459]: https://github.com/JuliaLang/julia/issues/27459
[#27470]: https://github.com/JuliaLang/julia/issues/27470
[#27473]: https://github.com/JuliaLang/julia/issues/27473
[#27554]: https://github.com/JuliaLang/julia/issues/27554
[#27560]: https://github.com/JuliaLang/julia/issues/27560
[#27616]: https://github.com/JuliaLang/julia/issues/27616
[#27635]: https://github.com/JuliaLang/julia/issues/27635
[#27641]: https://github.com/JuliaLang/julia/issues/27641
[#27711]: https://github.com/JuliaLang/julia/issues/27711
[#27726]: https://github.com/JuliaLang/julia/issues/27726
[#27746]: https://github.com/JuliaLang/julia/issues/27746
[#27856]: https://github.com/JuliaLang/julia/issues/27856
[#27859]: https://github.com/JuliaLang/julia/issues/27859
[#27908]: https://github.com/JuliaLang/julia/issues/27908
[#27944]: https://github.com/JuliaLang/julia/issues/27944
[#28045]: https://github.com/JuliaLang/julia/issues/28045
[#28065]: https://github.com/JuliaLang/julia/issues/28065
[#28155]: https://github.com/JuliaLang/julia/issues/28155
[#28266]: https://github.com/JuliaLang/julia/issues/28266
[#28302]: https://github.com/JuliaLang/julia/issues/28302

Julia v0.6.0 Release Notes
==========================

New language features
---------------------

  * New type system capabilities ([#8974], [#18457])

    + Type parameter constraints can refer to previous parameters, e.g.
      `type Foo{R<:Real, A<:AbstractArray{R}}`. Can also be used in method definitions.

    + New syntax `Array{T} where T<:Integer`, indicating a union of types over all
      specified values of `T` (represented by a `UnionAll` type). This provides behavior
      similar to parametric methods or `typealias`, but can be used anywhere a type is
      accepted. This syntax can also be used in method definitions, e.g.
      `function inv(M::Matrix{T}) where T<:AbstractFloat`.
      Anonymous functions can have type parameters via the syntax
      `((x::Array{T}) where T<:Real) -> 2x`.

    + Implicit type parameters, e.g. `Vector{<:Real}` is equivalent to
      `Vector{T} where T<:Real`, and similarly for `Vector{>:Int}` ([#20414]).

    + Much more accurate subtype and type intersection algorithms. Method sorting and
      identification of equivalent and ambiguous methods are improved as a result.

Language changes
----------------

  * "Inner constructor" syntax for parametric types is deprecated. For example,
    in this definition:
    ```
    type Foo{T,S<:Real}
        x
        Foo(x) = new(x)
    end
    ```
    the syntax `Foo(x) = new(x)` actually defined a constructor for `Foo{T,S}`,
    i.e. the case where the type parameters are specified. For clarity, this
    definition now must be written as `Foo{T,S}(x) where {T,S<:Real} = new(x)`
    ([#11310], [#20308]).

  * The keywords used to define types have changed ([#19157], [#20418]).

    + `immutable` changes to `struct`

    + `type` changes to `mutable struct`

    + `abstract` changes to `abstract type ... end`

    + `bitstype 32 Char` changes to `primitive type Char 32 end`

    In 0.6, `immutable` and `type` are still allowed as synonyms without a deprecation
    warning.

  * Multi-line and single-line nonstandard command literals have been added. A
    nonstandard command literal is like a nonstandard string literal, but the
    syntax uses backquotes (``` ` ```) instead of double quotes, and the
    resulting macro called is suffixed with `_cmd`. For instance, the syntax
    ``` q`xyz` ``` is equivalent to `@q_cmd "xyz"` ([#18644]).

  * Nonstandard string and command literals can now be qualified with their
    module. For instance, `Base.r"x"` is now parsed as `Base.@r_str "x"`.
    Previously, this syntax parsed as an implicit multiplication ([#18690]).

  * For every binary operator `⨳`, `a .⨳ b` is now automatically equivalent to
    the `broadcast` call `(⨳).(a, b)`.  Hence, one no longer defines methods
    for `.*` etcetera.  This also means that "dot operations" automatically
    fuse into a single loop, along with other dot calls `f.(x)` ([#17623]).
    Similarly for unary operators ([#20249]).

  * Newly defined methods are no longer callable from the same dynamic runtime
    scope they were defined in ([#17057]).

  * `isa` is now parsed as an infix operator with the same precedence as `in`
    ([#19677]).

  * `@.` is now parsed as `@__dot__`, and can be used to add dots to
    every function call, operator, and assignment in an expression ([#20321]).

  * The identifier `_` can be assigned, but accessing its value is deprecated,
    allowing this syntax to be used in the future for discarding values ([#9343],
    [#18251], [#20328]).

  * The `typealias` keyword is deprecated, and should be replaced with
    `Vector{T} = Array{T,1}` or a `const` assignment ([#20500]).

  * Experimental feature: `x^n` for integer literals `n` (e.g. `x^3`
    or `x^-3`) is now lowered to `Base.literal_pow(^, x, Val{n})`, to enable
    compile-time specialization for literal integer exponents ([#20530], [#20889]).

Breaking changes
----------------

This section lists changes that do not have deprecation warnings.

  * `readline`, `readlines` and `eachline` return lines without line endings by default.
    You *must* use `readline(s, chomp=false)`, etc. to get the old behavior where
    returned lines include trailing end-of-line character(s) ([#19944]).

  * `String`s no longer have a `.data` field (as part of a significant performance
    improvement). Use `Vector{UInt8}(str)` to access a string as a byte array.
    However, allocating the `Vector` object has overhead. You can also use
    `codeunit(str, i)` to access the `i`th byte of a `String`.
    Use `sizeof(str)` instead of `length(str.data)`, and `pointer(str)` instead of
    `pointer(str.data)` ([#19449]).

  * Operations between `Float16` and `Integers` now return `Float16` instead of `Float32` ([#17261]).

  * Keyword arguments are processed left-to-right: if the same keyword is specified more than
    once, the rightmost occurrence takes precedence ([#17785]).

  * The `lgamma(z)` function now uses a different (more standard) branch cut
    for `real(z) < 0`, which differs from `log(gamma(z))` by multiples of 2π
    in the imaginary part ([#18330]).

  * `broadcast` now handles tuples, and treats any argument that is not a tuple
    or an array as a "scalar" ([#16986]).

  * `broadcast` now produces a `BitArray` instead of `Array{Bool}` for
    functions yielding a boolean result.  If you want `Array{Bool}`, use
    `broadcast!` or `.=` ([#17623]).

  * Broadcast `A[I...] .= X` with entirely scalar indices `I` is deprecated as
    its behavior will change in the future.  Use `A[I...] = X` instead.

  * Operations like `.+` and `.*` on `Range` objects are now generic
    `broadcast` calls (see [above](#language-changes)) and produce an `Array`.
    If you want a `Range` result, use `+` and `*`, etcetera ([#17623]).

  * `broadcast` now treats `Ref` (except for `Ptr`) arguments as 0-dimensional
    arrays ([#18965]).

  * `broadcast` now handles missing data (`Nullable`s) allowing operations to
    be lifted over mixtures of `Nullable`s and scalars, as if the `Nullable`
    were like an array with zero or one element ([#16961], [#19787]).

  * The runtime now enforces when new method definitions can take effect ([#17057]).
    The flip-side of this is that new method definitions should now reliably actually
    take effect, and be called when evaluating new code ([#265]).

  * The array-scalar methods of `/`, `\`, `*`, `+`, and `-` now follow broadcast promotion
    rules. (Likewise for the now-deprecated array-scalar methods of `div`, `mod`, `rem`,
    `&`, `|`, and `xor`; see "Deprecated or removed" below.) ([#19692]).

  * `broadcast!(f, A)` now calls `f()` for each element of `A`, rather than doing `fill!(A, f())` ([#19722]).

  * `rmprocs` now throws an exception if requested workers have not been completely
    removed before `waitfor` seconds. With a `waitfor=0`, `rmprocs` returns immediately
    without waiting for worker exits.

  * `quadgk` has been moved from Base into a separate package ([#19741]).

  * The `Collections` module has been removed, and all functions defined therein have been
    moved to the `DataStructures` package ([#19800]).

  * The `RepString` type has been moved to the
    [LegacyStrings.jl package](https://github.com/JuliaArchive/LegacyStrings.jl).

  * In macro calls with parentheses, e.g. `@m(a=1)`, assignments are now parsed as
    `=` expressions, instead of as `kw` expressions ([#7669]).

  * When used as an infix operator, `~` is now parsed as a call to an ordinary operator
    with assignment precedence, instead of as a macro call ([#20406]).

  * (µ "micro" and ɛ "latin epsilon") are considered equivalent to
    the corresponding Greek characters in identifiers.  `\varepsilon`
    now tab-completes to U+03B5 (greek small letter epsilon) ([#19464]).

  * `retry` now inputs the keyword arguments `delays` and `check` instead of
    `n` and `max_delay`.  The previous functionality can be achieved setting
    `delays` to `ExponentialBackOff` ([#19331]).

  * `transpose(::AbstractVector)` now always returns a `RowVector` view of the input (which is a
     special 1×n-sized `AbstractMatrix`), not a `Matrix`, etc. In particular, for
     `v::AbstractVector` we now have `(v.').' === v` and `v.' * v` is a scalar ([#19670]).

  * Parametric types with "unspecified" parameters, such as `Array`, are now represented
    as `UnionAll` types instead of `DataType`s ([#18457]).

  * `Union` types have two fields, `a` and `b`, instead of a single `types` field.
    The empty type `Union{}` is represented by a singleton of type `TypeofBottom` ([#18457]).

  * The type `NTuple{N}` now refers to tuples where every element has the same type
    (since it is shorthand for `NTuple{N,T} where T`). To get the old behavior of matching
    any tuple, use `NTuple{N,Any}` ([#18457]).

  * `FloatRange` has been replaced by `StepRangeLen`, and the internal
    representation of `LinSpace` has changed. Aside from changes in
    the internal field names, this leads to several differences in
    behavior ([#18777]):

    + Both `StepRangeLen` and `LinSpace` can represent ranges of
      arbitrary object types---they are no longer limited to
      floating-point numbers.

    + For ranges that produce `Float64`, `Float32`, or `Float16`
      numbers, `StepRangeLen` can be used to produce values with
      little or no roundoff error due to internal arithmetic that is
      typically twice the precision of the output result.

    + To take advantage of this precision, `linspace(start, stop,
      len)` now returns a range of type `StepRangeLen` rather than
      `LinSpace` when `start` and `stop` are
      `FloatNN`. `LinSpace(start, stop, len)` always returns a
      `LinSpace`.

    + `StepRangeLen(a, step, len)` constructs an ordinary-precision range
      using the values and types of `a` and `step` as given, whereas
      `range(a, step, len)` will attempt to match inputs `a::FloatNN`
      and `step::FloatNN` to rationals and construct a `StepRangeLen`
      that internally uses twice-precision arithmetic.  These two
      outcomes exhibit differences in both precision and speed.

  * `A=>B` expressions are now parsed as calls instead of using `=>` as the
    expression head ([#20327]).

  * The `count` function no longer sums non-boolean values ([#20404])

  * The generic `getindex(::AbstractString, ::AbstractVector)` method's signature has been
    tightened to `getindex(::AbstractString, ::AbstractVector{<:Integer})`. Consequently,
    indexing into `AbstractString`s with non-`AbstractVector{<:Integer}` `AbstractVector`s
    now throws a `MethodError` in the absence of an appropriate specialization.
    (Previously such cases failed less explicitly with the exception of
    `AbstractVector{Bool}`, which now throws an `ArgumentError` noting that
    logical indexing into strings is not supported.)  ([#20248])

  * Bessel, Hankel, Airy, error, Dawson, eta, zeta, digamma, inverse digamma,
    trigamma, and polygamma special functions have been moved from Base to
    the
    [SpecialFunctions.jl package](https://github.com/JuliaMath/SpecialFunctions.jl)
    ([#20427]).  Note that `airy`, `airyx` and `airyprime` have been deprecated
    in favor of more specific functions (`airyai`, `airybi`, `airyaiprime`,
    `airybiprimex`, `airyaix`, `airybix`, `airyaiprimex`, `airybiprimex`)
    ([#18050]).

  * When a macro is called in the module in which that macro is defined, global variables
    in the macro are now correctly resolved in the macro definition environment. Breakage
    from this change commonly manifests as undefined variable errors that do not occur
    under 0.5. Fixing such breakage typically requires sprinkling additional `esc`s in
    the offending macro ([#15850]).

  * `write` on an `IOBuffer` now returns a signed integer in order to be
    consistent with other buffers ([#20609]).

  * The `<:Integer` division fallback `/(::Integer, ::Integer)`, which formerly
    inappropriately took precedence over other division methods for some
    mixed-integer-type division calls, has been removed ([#19779]).

  * `@async`, `@spawn`, `@spawnat`, `@fetch` and `@fetchfrom` no longer implicitly
    localize variables. Previously, the expression would be wrapped in an implicit
    `let` block  ([#19594]).

  * `parse` no longer accepts IPv4 addresses including leading zeros, octal, or hexadecimal.
    Convert IPv4 addresses including octal or hexadecimal to decimal, and remove leading
    zeros in decimal addresses ([#19811]).

  * Closures shipped for remote execution via `@spawn` or `remotecall` now automatically
    serialize globals defined under Main. For details, please refer to the paragraph
    on "Global variables" under the "Parallel computing" chapter in the manual ([#19594]).

  * `homedir` now determines the user's home directory via `libuv`'s `uv_os_homedir`,
    rather than from environment variables ([#19636]).

  * Workers now listen on an ephemeral port assigned by the OS. Previously workers would
    listen on the first free port available from 9009 ([#21818]).


Library improvements
--------------------

  * A new `@views` macro was added to convert a whole expression or block of code to
    use views for all slices ([#20164]).

  * `max`, `min`, and related functions (`minmax`, `maximum`, `minimum`, `extrema`)
     now return `NaN` for `NaN` arguments ([#12563]).

  * `oneunit(x)` function to return a dimensionful version of `one(x)`
    (which is clarified to mean a dimensionless quantity if `x` is dimensionful) ([#20268]).

  * The `chop` and `chomp` functions now return a `SubString` ([#18339]).

  * Numbered stackframes printed in stacktraces can now be opened in an editor by
    entering the corresponding number in the REPL and pressing `^Q` ([#19680]).

  * The REPL now supports something called *prompt pasting* ([#17599]).
    This activates when pasting text that starts with `julia> ` into the REPL.
    In that case, only expressions starting with `julia> ` are parsed, the rest are removed.
    This makes it possible to paste a chunk of code that has been copied from a REPL session
    without having to scrub away prompts and outputs.
    This can be disabled or enabled at will with `Base.REPL.enable_promptpaste(::Bool)`.

  * The function `print_with_color` can now take a color
    represented by an integer between 0 and 255 inclusive
    as its first argument ([#18473]). For a number-to-color mapping, please refer to
    [this chart](https://upload.wikimedia.org/wikipedia/commons/1/15/Xterm_256color_chart.svg).
    It is also possible to use numbers as colors in environment variables that customizes colors in the REPL.
    For example, to get orange warning messages, simply set `ENV["JULIA_WARN_COLOR"] = 208`.
    Please note that not all terminals support 256 colors.

  * The function `print_with_color` no longer prints text in bold by default ([#18628]).
    Instead, the function now take a keyword argument `bold::Bool`
    which determines whether to print in bold or not. On some terminals, printing a color in non bold
    results in slightly darker colors being printed than when printing in bold.
    Therefore, light versions of the colors are now supported.
    For the available colors see the help entry on `print_with_color`.

  * The default text style for REPL input and answers has been changed from bold to normal ([#11250]).
    They can be changed back to bold by setting the environment variables
    `JULIA_INPUT_COLOR` and `JULIA_ANSWER_COLOR` to `"bold"`.
    For example, one way of doing this is adding `ENV["JULIA_INPUT_COLOR"] = :bold`
    and `ENV["JULIA_ANSWER_COLOR"] = :bold` to the `.juliarc.jl` file. See the
    [manual section on customizing colors](https://docs.julialang.org/en/latest/manual/interacting-with-julia#Customizing-Colors-1)
    for more information.

  * The default color for info messages has been changed from blue to cyan
    ([#18442]), and for warning messages from red to yellow ([#18453]).  This
    can be changed back to the original colors by setting the environment
    variables `JULIA_INFO_COLOR` to `"blue"` and `JULIA_WARN_COLOR` to `"red"`.

  * Iteration utilities that wrap iterators and return other iterators (`enumerate`, `zip`, `rest`,
    `countfrom`, `take`, `drop`, `cycle`, `repeated`, `product`, `flatten`, `partition`) have been
    moved to the module `Base.Iterators` ([#18839]).

  * BitArrays can now be constructed from arbitrary iterables, in particular from generator expressions,
    e.g. `BitArray(isodd(x) for x = 1:100)` ([#19018]).

  * `hcat`, `vcat`, and `hvcat` now work with `UniformScaling` objects, so
    you can now do e.g. `[A I]` and it will concatenate an appropriately sized
    identity matrix ([#19305]).

  * New `accumulate` and `accumulate!` functions were added, which generalize `cumsum` and `cumprod`.
    Also known as a [scan](https://en.wikipedia.org/wiki/Prefix_sum) operation ([#18931]).

  * `reshape` now allows specifying one dimension with a `Colon()` (`:`) for the new shape, in which case
    that dimension's length will be computed such that its product with all the other dimensions is equal
    to the length of the original array ([#19919]).

  * The new `to_indices` function provides a uniform interface for index conversions,
    taking an array and a tuple of indices as arguments and returning a tuple of
    integers and/or arrays of supported scalar indices. It will throw an `ArgumentError`
    for any unsupported indices, and the returned arrays should be iterated over (and
    not indexed into) to support more efficient logical indexing ([#19730]).

    + Using colons (`:`) to represent a collection of indices is deprecated. They now must be
      explicitly converted to a specialized array of integers with the `to_indices` function.
      As a result, the type of `SubArray`s that represent views over colon indices has changed.

    + Logical indexing is now more efficient. Logical arrays are converted by `to_indices` to
      a lazy, iterable collection of indices that doesn't support indexing. A deprecation
      provides indexing support with O(n) lookup.

    + The performance of indexing with `CartesianIndex`es is also improved in many situations.

  * A new `titlecase` function was added, to capitalize the first character of each word within a string ([#19469]).

  * `any` and `all` now always short-circuit, and `mapreduce` never short-circuits ([#19543]).
    That is, not every member of the input iterable will be visited if a `true` (in the case of `any`) or
    `false` (in the case of `all`) value is found, and `mapreduce` will visit all members of the iterable.

  * Additional methods for `ones` and `zeros` functions were added
    to support the same signature as the `similar` function ([#19635]).

  * `count` now has a `count(itr)` method equivalent to `count(identity, itr)` ([#20403]).

  * Methods for `map` and `filter` with `Nullable` arguments have been implemented;
    the semantics are as if the `Nullable` were a container with zero or one elements ([#16961]).

  * New `@test_warn` and `@test_nowarn` macros were added in the `Base.Test` module to
    test for the presence or absence of warning messages ([#19903]).

  * `logging` can now be used to redirect `info`, `warn`, and `error` messages
    either universally or on a per-module/function basis ([#16213]).

  * New function `Base.invokelatest(f, args...)` to call the latest version
    of a function in circumstances where an older version may be called
    instead (e.g. in a function calling `eval`) ([#19784]).

  * A new `iszero(x)` function was added, to quickly check whether `x` is zero
    (or is all zeros, for an array) ([#19950]).

  * `notify` now returns a count of tasks woken up ([#19841]).

  * A new nonstandard string literal `raw"..."` was added,
    for creating strings with no interpolation or unescaping ([#19900]).

  * A new `Dates.Time` type was added that supports representing the time of day
    with up to nanosecond resolution ([#12274]).

  * Raising one or negative one to a negative integer power formerly threw a `DomainError`.
    One raised to any negative integer power now yields one, negative one raised to any
    negative even integer power now yields one, and negative one raised to any negative
    odd integer power now yields negative one. Similarly, raising `true` to any negative
    integer power now yields `true` rather than throwing a `DomainError` ([#18342]).

  * A new `@macroexpand` macro was added as a convenient alternative to the `macroexpand` function ([#18660]).

  * `invoke` now supports keyword arguments ([#20345]).

  * A new `ConjArray` type was added, as a wrapper type for lazy complex conjugation of arrays.
    Currently, it is used by default for the new `RowVector` type only, and
    enforces that both `transpose(vec)` and `ctranspose(vec)` are views not copies ([#20047]).

  * `rem` now accepts a `RoundingMode` argument via `rem(x, y, r::RoundingMode)`, yielding
    `x - y*round(x/y, r)` without intermediate rounding. In particular, `rem(x, y, RoundNearest)`
    yields a value in the interval `[-abs(y)/2, abs(y)/2]`), which corresponds to the IEE754
    `remainder` function. Similarly, `rem2pi(x, r::RoundingMode)` now exists as well, yielding
    `rem(x, 2pi, r::RoundingMode)` but with greater accuracy ([#10946]).

  * `map[!]` and `broadcast[!]` now have dedicated methods for sparse/structured
    vectors/matrices. Specifically, `map[!]` and `broadcast[!]` over combinations including
    one or more `SparseVector`, `SparseMatrixCSC`, `Diagonal`, `Bidiagonal`, `Tridiagonal`,
    or `SymTridiagonal`, and any number of `broadcast` scalars, `Vector`s, or `Matrix`s,
    now efficiently yield `SparseVector`s or `SparseMatrix`s as appropriate ([#19239],
    [#19371], [#19518], [#19438], [#19690], [#19724], [#19926], [#19934], [#20009]).

  * The operators `!` and `∘` (`\circ<tab>` at the REPL and in most code editors) now
    respectively perform predicate function negation and function composition. For example,
    `map(!iszero, (0, 1))` is now equivalent to `map(x -> !iszero(x), (0, 1))` and
    `map(uppercase ∘ hex, 250:255)` is now equivalent to
    `map(x -> uppercase(hex(x)), 250:255)` ([#17155]).

  * `enumerate` now supports the two-argument form `enumerate(::IndexStyle, iterable)`.
    This form allows specification of the returned indices' style. For example,
    `enumerate(IndexLinear, iterable)` yields linear indices and
    `enumerate(IndexCartesian, iterable)` yields cartesian indices ([#16378]).

Compiler/Runtime improvements
-----------------------------

  * `ccall` is now implemented as a macro,
    removing the need for special code-generator support for `Intrinsics` ([#18754]).

  * `ccall` gained limited support for a `llvmcall` calling-convention.
    This can replace many uses of `llvmcall` with a simpler, shorter declaration ([#18754]).

  * All `Intrinsics` are now `Builtin` functions instead and have proper error checking
    and fall-back static compilation support ([#18754]).

Deprecated or removed
---------------------

  * `ipermutedims(A::AbstractArray, p)` has been deprecated in favor of
    `permutedims(A, invperm(p))` ([#18891]).

  * Linear indexing is now only supported when there is exactly one
    non-cartesian index provided. Allowing a trailing index at dimension `d` to
    linearly access the higher dimensions from array `A` (beyond `size(A, d)`)
    has been deprecated as a stricter constraint during bounds checking.
    Instead, `reshape` the array such that its dimensionality matches the
    number of indices ([#20079]).

  * `Multimedia.@textmime "mime"` has been deprecated. Instead define
    `Multimedia.istextmime(::MIME"mime") = true` ([#18441]).

  * `isdefined(a::Array, i::Int)` has been deprecated in favor of `isassigned` ([#18346]).

  * The three-argument `SubArray` constructor (which accepts `dims::Tuple` as its third
    argument) has been deprecated in favor of the two-argument equivalent (the
    `dims::Tuple` argument being superfluous) ([#19259]).

  * `is` has been deprecated in favor of `===` (which used to be an alias for `is`) ([#17758]).

  * Ambiguous methods for addition and subtraction between `UniformScaling`s and `Number`s,
    for example `(+)(J::UniformScaling, x::Number)`, have been deprecated in favor of
    unambiguous, explicit equivalents, for example `J.λ + x` ([#17607]).

  * `num` and `den` have been deprecated in favor of `numerator` and `denominator` respectively ([#19233],[#19246]).

  * `delete!(ENV::EnvDict, k::AbstractString, def)` has been deprecated in favor of
    `pop!(ENV, k, def)`. Be aware that `pop!` returns `k` or `def`, whereas `delete!`
    returns `ENV` or `def` ([#18012]).

  * infix operator `$` has been deprecated in favor of infix `⊻` or function `xor` ([#18977]).

  * The single-argument form of `write` (`write(x)`, with implicit `STDOUT` output stream),
    has been deprecated in favor of the explicit equivalent `write(STDOUT, x)` ([#17654]).

  * `Dates.recur` has been deprecated in favor of `filter` ([#19288])

  * A number of ambiguous `convert` operations between `Number`s (especially `Real`s)
    and `Date`, `DateTime`, and `Period` types have been deprecated in favor of
    unambiguous `convert` and explicit constructor calls. Additionally, ambiguous colon
    construction of `<:Period` ranges without step specification, for example
    `Dates.Hour(1):Dates.Hour(2)`, has been deprecated in favor of such construction
    including step specification, for example `Dates.Hour(1):Dates.Hour(1):Dates.Hour(2)`
    ([#19920]).

  * `cummin` and `cummax` have been deprecated in favor of `accumulate` ([#18931]).

  * The `Array` constructor syntax `Array(T, dims...)` has been deprecated
    in favor of the forms `Array{T,N}(dims...)` (where `N` is known, or
    particularly `Vector{T}(dims...)` for `N = 1` and `Matrix{T}(dims...)` for `N = 2`),
    and `Array{T}(dims...)` (where `N` is not known). Likewise for `SharedArray`s ([#19989]).

  * `sumabs` and `sumabs2` have been deprecated in favor of `sum(abs, x)` and `sum(abs2, x)`, respectively.
    `maxabs` and `minabs` have similarly been deprecated in favor of `maximum(abs, x)` and `minimum(abs, x)`.
    Likewise for the in-place counterparts of these functions ([#19598]).

  * The array-reducing form of `isinteger` (`isinteger(x::AbstractArray)`) has been
    deprecated in favor of `all(isinteger, x)` ([#19925]).

  * `produce`, `consume` and iteration over a Task object have been deprecated in favor of
    using Channels for inter-task communication  ([#19841]).

  * The `negate` keyword has been deprecated from all functions in the `Dates` adjuster
    API (`adjust`, `tonext`, `toprev`, `Date`, `Time`, and `DateTime`). Instead use
    predicate function negation via the `!` operator
    (see [Library Improvements](#library-improvements)) ([#20213]).

  * `@test_approx_eq x y` has been deprecated in favor of `@test isapprox(x,y)` or `@test x ≈ y` ([#4615]).

  * `Matrix()` and `Matrix{T}()` have been deprecated in favor of the explicit forms
    `Matrix(0, 0)` and `Matrix{T}(0, 0)` ([#20330]).

  * Vectorized functions have been deprecated in favor of dot syntax ([#17302], [#17265],
    [#18558], [#19711], [#19712], [#19791], [#19802], [#19931], [#20543], [#20228]).

  *  All methods of character predicates (`isalnum`, `isalpha`, `iscntrl`, `isdigit`,
     `isnumber`, `isgraph`, `islower`, `isprint`, `ispunct`, `isspace`, `isupper`,
     `isxdigit`) that accept `AbstractStrings` have been deprecated in favor of `all`.
     For example, `isnumber("123")` should now be expressed `all(isnumber, "123")`
     ([#20342]).

  * A few names related to indexing traits have been changed: `LinearIndexing` and
    `linearindexing` have been deprecated in favor of `IndexStyle`. `LinearFast` has
    been deprecated in favor of `IndexLinear`, and `LinearSlow` has been deprecated in
    favor of `IndexCartesian` ([#16378]).

  * The two-argument forms of `map` (`map!(f, A)`) and `asyncmap!` (`asyncmap!(f, A)`)
    have been deprecated in anticipation of future semantic changes ([#19721]).

  * `unsafe_wrap(String, ...)` has been deprecated in favor of `unsafe_string` ([#19449]).

  * `zeros` and `ones` methods accepting an element type as the first argument and an
    array as the second argument, for example `zeros(Float64, [1, 2, 3])`, have been
    deprecated in favor of equivalent methods with the second argument instead the
    size of the array, for example `zeros(Float64, size([1, 2, 3]))` ([#21183]).

  * `Base.promote_eltype_op` has been deprecated ([#19669], [#19814], [#19937]).

  * `isimag` has been deprecated ([#19949]).

  * The tuple-of-types form of `invoke`, `invoke(f, (types...), ...)`, has been deprecated
    in favor of the tuple-type form `invoke(f, Tuple{types...}, ...)` ([#18444]).

  * `Base._promote_array_type` has been deprecated ([#19766]).

  * `broadcast_zpreserving` has been deprecated ([#19533], [#19720]).

  * Methods allowing indexing of tuples by `AbstractArray`s with more than one dimension
    have been deprecated. (Indexing a tuple by such a higher-dimensional `AbstractArray`
    should yield a tuple with more than one dimension, but tuples are one-dimensional.)
    ([#19737]).

  * `@test_approx_eq a b` has been deprecated in favor of `@test a ≈ b` (or,
    equivalently, `@test ≈(a, b)` or `@test isapprox(a, b)`).
    `@test_approx_eq_eps` has been deprecated in favor of new `@test` syntax:
    `@test` now supports the syntax `@test f(args...) key=val ...` for
    `@test f(args..., key=val...)`. This syntax allows, for example, writing
    `@test a ≈ b atol=c` in place of `@test ≈(a, b, atol=c)` (and hence
    `@test_approx_eq_eps a b c`) ([#19901]).

  * `takebuf_array` has been deprecated in favor of `take!`, and `takebuf_string(x)`
    has been deprecated in favor of `String(take!(x))` ([#19088]).

  * `convert` methods from `Diagonal` and `Bidiagonal` to subtypes of
    `AbstractTriangular` have been deprecated ([#17723]).

  * `Base.LinAlg.arithtype` has been deprecated. If you were using `arithtype` within a
    `promote_op` call, instead use `promote_op(Base.LinAlg.matprod, Ts...)`. Otherwise,
    consider defining equivalent functionality locally ([#18218]).

  * Special characters (`#{}()[]<>|&*?~;`) should now be quoted in commands. For example,
    ``` `export FOO=1\;` ``` should replace ``` `export FOO=1;` ``` and
    ``` `cd $dir '&&' $thingie` ``` should replace ``` `cd $dir && $thingie` ``` ([#19786]).

  * Zero-argument `Channel` constructors (`Channel()`, `Channel{T}()`) have been deprecated
    in favor of equivalents accepting an explicit `Channel` size
    (`Channel(2)`, `Channel{T}(2)`) ([#18832]).

  * The zero-argument constructor `MersenneTwister()` has been
    deprecated in favor of the explicit `MersenneTwister(0)` ([#16984]).

  * `Base.promote_type(op::Type, Ts::Type...)` has been removed as part of an overhaul
    of `broadcast`'s promotion mechanism. If you need the functionality of that
    `Base.promote_type` method, consider defining it locally via
    `Core.Compiler.return_type(op, Tuple{Ts...})` ([#18642]).

  * `bitbroadcast` has been deprecated in favor of `broadcast`, which now produces a
    `BitArray` instead of `Array{Bool}` for functions yielding a boolean result ([#19771]).

  * To complete the deprecation of histogram-related functions, `midpoints` has been
    deprecated. Instead use the
    [StatsBase.jl package](https://github.com/JuliaStats/StatsBase.jl)'s
    `midpoints` function ([#20058]).

  * Passing a type argument to `LibGit2.cat` has been deprecated in favor of a simpler,
    two-argument method for `LibGit2.cat` ([#20435]).

  * The `LibGit2.owner` function for finding the repository which owns a given Git object
    has been deprecated in favor of `LibGit2.repository` ([#20135]).

  * The `LibGit2.GitAnyObject` type has been renamed to `LibGit2.GitUnknownObject` to
    clarify its intent ([#19935]).

  * The `LibGit2.GitOid` type has been renamed to `LibGit2.GitHash` for clarity ([#19878]).

  * Finalizing `LibGit2` objects with `finalize` has been deprecated in favor of using `close`
    ([#19660]).

  * Parsing string dates from a `Dates.DateFormat` object has been deprecated as part of a
    larger effort toward faster, more extensible date parsing ([#20952]).

Command-line option changes
---------------------------

  * In `polly` builds (`USE_POLLY := 1`), the new flag `--polly={yes|no}` controls whether
    `@polly` declarations are respected. (With `--polly=no`, `@polly` declarations are
    ignored.) This flag is also available in non-`polly` builds (`USE_POLLY := 0`),
    but has no effect ([#18159]).

Julia v0.5.0 Release Notes
==========================

New language features
---------------------

  * Generator expressions: `f(i) for i in 1:n` ([#4470]). This returns an iterator
    that computes the specified values on demand. This is useful for computing, e.g.
    `sum(f(i) for i in 1:n)` without creating an intermediate array of values.

  * Generators and comprehensions support filtering using `if` ([#550]) and nested
    iteration using multiple `for` keywords ([#4867]).

  * Fused broadcasting syntax: ``f.(args...)`` is equivalent to ``broadcast(f, args...)`` ([#15032]),
    and nested `f.(g.(args...))` calls are fused into a single `broadcast` loop ([#17300]).
    Similarly, the syntax `x .= ...` is equivalent to a `broadcast!(identity, x, ...)`
    call and fuses with nested "dot" calls; also, `x .+= y` and similar is now
    equivalent to `x .= x .+ y`, rather than `x = x .+ y` ([#17510]).

  * Macro expander functions are now generic, so macros can have multiple definitions
    (e.g. for different numbers of arguments, or optional arguments) ([#8846], [#9627]).
    However note that the argument types refer to the syntax tree representation, and not
    to the types of run time values.

  * Varargs functions like `foo{T}(x::T...)` may now restrict the number
    of such arguments using `foo{T,N}(x::Vararg{T,N})` ([#11242]).

  * `x ∈ X` is now a synonym for `x in X` in `for` loops and comprehensions,
    as it already was in comparisons ([#13824]).

  * The `PROGRAM_FILE` global is now available for determining the name of the running script ([#14114]).

  * The syntax `x.:sym` (e.g. `Base.:+`) is now supported, while using `x.(:sym)`
    or `x.(i)` for field access are deprecated in favor of `getfield` ([#15032]).

  * Function return type syntax `function f()::T` has been added ([#1090]). Values returned
    from a function with such a declaration will be converted to the specified type `T`.

  * Many more operators now support `.` prefixes (e.g. `.≤`) ([#17393]).  However,
    users are discouraged from overloading these, since they are mainly parsed
    in order to implement backwards compatibility with planned automatic
    broadcasting of dot operators in Julia 0.6 ([#16285]).  Explicitly qualified
    operator names like `Base.≤` should now use `Base.:≤` (prefixed by `@compat`
    if you need 0.4 compatibility via the `Compat` package).

  * User-extensible bounds check elimination is now possible with the new
    `@boundscheck` macro ([#14474]). This macro marks bounds checking code blocks,
    which the compiler may remove when encountered inside an `@inbounds` call.

Experimental language features
------------------------------

  * Support for
    [multi-threading](https://docs.julialang.org/en/latest/manual/parallel-computing/#multi-threading-experimental).
    Loops with independent iterations can be easily parallelized with the
    `Threads.@threads` macro.

  * Support for arrays with indexing starting at values different from 1.
    The array types are expected to be defined in packages, but now
    Julia provides an API for writing generic algorithms for arbitrary
    indexing schemes ([#16260]).

Language changes
----------------

  * Each function and closure now has its own type. The captured variables of a closure
    are fields of its type. `Function` is now an abstract type, and is the default
    supertype of functions and closures. All functions, including anonymous functions,
    are generic and support all features (e.g. keyword arguments). Instead of adding
    methods to `call`, methods are added by type using the syntax
    `(::ftype)(...) = ...`. `call` is deprecated ([#13412]). A significant result of
    this language change is that higher order functions can be specialized on their
    function arguments, leading to much faster functional programming, typically as
    fast as if function arguments were manually inlined. See below for details.

  * Square brackets and commas (e.g. `[x, y]`) no longer concatenate arrays, and always
    simply construct a vector of the provided values. If `x` and `y` are arrays,
    `[x, y]` will be an array of arrays ([#3737], [#2488], [#8599]).

  * `using` and `import` are now case-sensitive even on case-insensitive filesystems
    (common on Mac and Windows) ([#13542]).

  * Relational algebra symbols are now allowed as infix operators ([#8036]):
    `⨝`, `⟕`, `⟖`, `⟗` for joins and `▷` for anti-join.

  * A warning is always given when a method is overwritten; previously, this was done
    only when the new and old definitions were in separate modules ([#14759]).

  * The `if` keyword cannot be followed immediately by a line break ([#15763]).

  * Juxtaposition of numeric literals ending in `.` (e.g. `1.x`) is no longer
    allowed ([#15731]).

  * The built-in `NTuple` type has been removed; `NTuple{N,T}` is now
    implemented internally as `Tuple{Vararg{T,N}}` ([#11242]).

  * Use of the syntax `x::T` to declare the type of a local variable is deprecated.
    In the future this will always mean type assertion, and declarations should use
    `local x::T` instead ([#16071]).
    When `x` is global, `x::T = ...` and `global x::T` used to mean type assertion,
    but this syntax is now reserved for type declaration ([#964]).

  * Dictionary comprehension syntax `[ a=>b for x in y ]` is deprecated.
    Use `Dict(a=>b for x in y)` instead ([#16510]).

  * Parentheses are no longer allowed around iteration specifications, e.g.
    `for (i = 1:n)` ([#17668]).

Breaking changes
----------------

This section lists changes that do not have deprecation warnings.

  * All dimensions indexed by scalars are now dropped, whereas previously only
    trailing scalar dimensions would be omitted from the result ([#13612]). This
    is a very major behavioral change, but should cause obvious failures. To retain
    a dimension sliced with a scalar `i` slice with `i:i` instead.

  * The assignment operations `.+=`, `.*=` and so on now generate calls
    to `broadcast!` on the left-hand side (or call to `view(a, ...)` on the left-hand side
    if the latter is an indexing expression, e.g. `a[...]`). This means that they will fail
    if the left-hand side is immutable (or does not support `view`), and will otherwise
    change the left-hand side in-place ([#17510], [#17546]).

  * Method ambiguities no longer generate warnings when files are loaded,
    nor do they dispatch to an arbitrarily-chosen method; instead, a call that
    cannot be resolved to a single method results in a `MethodError` at run time,
    rather than the previous definition-time warning ([#6190]).

  * Array comprehensions preserve the dimensions of the input ranges. For example,
    `[2x for x in A]` will have the same dimensions as `A` ([#16622]).

  * The result type of an array comprehension depends only on the types of elements
    computed, instead of using type inference ([#7258]). If the result is empty, then
    type inference is still used to determine the element type.

  * `reshape` is now defined to always share data with the original array.
    If a reshaped copy is needed, use `copy(reshape(a))` or `copy!` to a new array of
    the desired shape ([#4211]).

  * `mapslices` now re-uses temporary storage. Recipient functions that expect
    input slices to be persistent should copy data to other storage ([#17266]).
    All usages of `mapslices` should be carefully audited since this change can cause
    silent, incorrect behavior, rather than failing noisily.

  * Local variables and arguments are represented in lowered code as numbered `Slot`
    objects instead of as symbols ([#15609]).

  * The information that used to be in the `ast` field of the `LambdaStaticData` type
    is now divided among the fields `code`, `slotnames`, `slottypes`, `slotflags`,
    `gensymtypes`, `rettype`, `nargs`, and `isva` in the `LambdaInfo` type ([#15609]).

  * `A <: B` is parsed as `Expr(:(<:), :A, :B)` in all cases ([#9503]).
    This also applies to the `>:` operator.

  * Simple 2-argument comparisons like `A < B` are parsed as calls instead of using the
    `:comparison` expression type ([#15524]). The `:comparison` expression type is still
    produced in ASTs when comparisons are chained (e.g. `A < B ≤ C`).

  * `map` on a dictionary now expects a function that expects and returns a `Pair`.
    The result is now another dictionary instead of an array ([#16622]).

  * Bit shift operations (i.e. `<<`, `>>`, and `>>>`) now handle
    negative shift counts differently: Negative counts are interpreted
    as shifts in the opposite direction. For example, `4 >> -1 == 4 <<
    +1 == 8`. Previously, negative counts would implicitly overflow to
    large positive counts, always yielding either `0` or `-1`.

Library improvements
--------------------

  * Strings ([#16107]):

    * The `UTF8String` and `ASCIIString` types have been merged into a single
      `String` type ([#16058]).  Use `isascii(s)` to check whether
      a string contains only ASCII characters. The `ascii(s)` function now
      converts `s` to `String`, raising an `ArgumentError` exception if `s` is
      not pure ASCII.

    * The `UTF16String` and `UTF32String` types and corresponding `utf16` and
      `utf32` converter functions have been removed from the standard library.
      If you need these types, they have been moved to the
      [LegacyStrings.jl package](https://github.com/JuliaArchive/LegacyStrings.jl).
      In the future, more robust Unicode string support will be provided by the
      [StringEncodings.jl package](https://github.com/nalimilan/StringEncodings.jl).
      If you only need these types to call wide string APIs (UTF-16 on Windows,
      UTF-32 on UNIX), consider using the new `transcode` function (see below)
      or the `Cwstring` type as a `ccall` argument type, which also ensures
      correct NUL termination of string data.

    * A `transcode(T, src)` function is now exported for converting data
      between UTF-xx Unicode encodings ([#17323]).

    * The basic string construction routines are now `string(args...)`,
      `String(s)`, `unsafe_string(ptr)` (formerly `bytestring(ptr)`), and
      `unsafe_wrap(String, ptr)` (formerly `pointer_to_string`) ([#16731]).

    * Comparisons between `Char`s and `Integer`s are now deprecated ([#16024]):
      `'x' == 120` now produces a warning but still evaluates to `true`. In the
      future it may evaluate to `false` or the comparison may be an error. To
      compare characters with integers you should either convert the integer to
      a character value or convert the character to the corresponding code point
      first: e.g. `'x' == Char(120)` or `Int('x') == 120`. The former is usually
      preferable.

    * Support for Unicode 9 ([#17402]).

  * Arrays and linear algebra:

    * Dimensions indexed by multidimensional arrays add dimensions. More generally, the
      dimensionality of the result is the sum of the dimensionalities of the indices ([#15431]).

    * New `normalize` and `normalize!` convenience functions for normalizing
      vectors ([#13681]).

    * QR matrix factorization:

      * New method for generic QR with column pivoting ([#13480]).

      * New method for polar decompositions of `AbstractVector`s ([#13681]).

    * A new `SparseVector` type allows for one-dimensional sparse arrays.
      Slicing and reshaping sparse matrices now return vectors when
      appropriate. The `sparsevec` function returns a one-dimensional sparse
      vector instead of a one-column sparse matrix. The `SparseMatrix` module
      has been renamed to `SparseArrays` ([#13440]).

    * Rank one update and downdate functions, `lowrankupdate`, `lowrankupdate!`, `lowrankdowndate`,
      and `lowrankdowndate!`, have been introduced for dense Cholesky factorizations ([#14243], [#14424]).

    * All `sparse` methods now retain provided numerical zeros as structural nonzeros; to
      drop numerical zeros, use `dropzeros!` ([#14798], [#15242]).

    * `setindex!` methods for sparse matrices and vectors no longer purge allocated entries
      on zero assignment. To drop stored entries from sparse matrices and vectors, use
      `Base.SparseArrays.dropstored!` ([#17404]).

    * Concatenating dense and sparse matrices now returns a sparse matrix ([#15172]).

  * Files and I/O:

    * The `open` function now respects `umask` on UNIX when creating files ([#16466], [#16502]).

    * A new function `walkdir()` returns an iterator that walks the tree of a directory ([#8814], [#13707]).

       ```
       for (root, dirs, files) in walkdir(expanduser("~/.julia/v0.5/Plots/src"))
           println("$(length(files)) \t files in $root")
       end
       19    files in /Users/me/.julia/v0.5/Plots/src
       15    files in /Users/me/.julia/v0.5/Plots/src/backends
       4     files in /Users/me/.julia/v0.5/Plots/src/deprecated
      ```

    * A new function `chown()` changes the ownership of files ([#15007]).

    * Display properties can now be passed among output functions (e.g. `show`)
      using an `IOContext` object ([#13825]).

    * `Cmd(cmd; ...)` now accepts new Windows-specific options `windows_verbatim`
      (to alter Windows command-line generation) and `windows_hide` (to
      suppress creation of new console windows) ([#13780]).

    * There is now a default no-op `flush(io)` function for all `IO` types ([#16403]).

  * Parallel computing:

    * `pmap` keyword arguments `err_retry=true` and `err_stop=false` are deprecated.
      Action to be taken on errors can be specified via the `on_error` keyword argument.
      Retry is specified via `retry_n`, `retry_on` and `retry_max_delay` ([#15409], [#15975], [#16663]).

    * The functions `remotecall`, `remotecall_fetch`, and `remotecall_wait` now have the
      function argument as the first argument to allow for do-block syntax ([#13338]).

  * Statistics:

    * Improve performance of `quantile` ([#14413]).

    * `extrema` can now operate over a region ([#15550]).

    * `cov` and `cor` don't use keyword arguments anymore and are therefore now type stable ([#13465]).

    * Histogram functionality has been deprecated in `Base`. Use the
      [StatsBase.jl package](https://github.com/JuliaStats/StatsBase.jl)
      instead ([#6842], [#16450]).

  * Testing:

    * The `Base.Test` module now has a `@testset` feature to bundle
      tests together and delay throwing an error until the end ([#13062]).

    * The new features are mirrored in the
      [BaseTestNext.jl package](https://github.com/IainNZ/BaseTestNext.jl)
      for users who would like to use the new functionality on Julia v0.4.

    * The [BaseTestDeprecated.jl package](https://github.com/IainNZ/BaseTestDeprecated.jl)
      provides the old-style `handler` functionality, for compatibility
      with code that needs to support both Julia v0.4 and v0.5.

  * Package management:

    * The package system (`Pkg`) is now based on the `libgit2` library, rather
      than running the `git` program, increasing performance (especially on
      Windows) ([#11196]).

    * Package-development functions like `Pkg.tag` and `Pkg.publish`
      have been moved to an external [PkgDev] package ([#13387]).

    * Updating only a subset of the packages is now supported,
      e.g. `Pkg.update("Example")` ([#17132]).

  * Miscellaneous:

    * Prime number related functions have been moved from `Base` to the
      [Primes.jl package](https://github.com/JuliaMath/Primes.jl) ([#16481]).

    * Most of the combinatorics functions have been moved from `Base`
      to the [Combinatorics.jl package](https://github.com/JuliaLang/Combinatorics.jl) ([#13897]).

    * New `foreach` function for calling a function on every element of a collection when
      the results are not needed ([#13774]). Compared to `map(f, v)`, which allocates and
      returns a result array, `foreach(f, v)` calls `f` on each element of `v`, returning
      nothing.

    * The new `Base.StackTraces` module makes stack traces easier to use programmatically ([#14469]).

    * The `libjulia` library is now properly versioned and installed to the public `<prefix>/lib`
      directory, instead of the private `<prefix>/lib/julia` directory ([#16362]).

    * System reflection is now more consistently exposed from `Sys` and not `Base`
      (e.g. constants such as `WORD_SIZE` and `CPU_CORES`). `OS_NAME` has been
      replaced by `Sys.KERNEL` and always reports the name of the kernel (as
      reported by `uname`). The `@windows_only` and `@osx` family of macros
      have been replaced with functions such as `is_windows()` and `is_apple()`.
      There is now also a `@static` macro that will evaluate the condition of an
      if-statement at compile time, for when a static branch is required ([#16219]).

    * `Date` and `DateTime` values can now be rounded to a specified resolution (e.g., 1 month or
      15 minutes) with `floor`, `ceil`, and `round` ([#17037]).

[PkgDev]: https://github.com/JuliaLang/PkgDev.jl

Compiler/Runtime improvements
-----------------------------

  * Machine SIMD types can be represented in Julia as a homogeneous tuple of `VecElement` ([#15244]).

  * The performance of higher-order and anonymous functions has been greatly improved.
    For example, `map(x->2x, A)` performs as well as `2.*A`([#13412]).

  * On windows, a DLL of standard library code is now precompiled and used by default,
    improving startup time ([#16953]).

  * LLVM has been upgraded to version 3.7.1, improving the quality of generated
    code and debug info. However compile times may be slightly longer ([#14623]).

New architectures
-----------------

  This release greatly improves support for ARM, and introduces support for Power.

  * [ARM](https://github.com/JuliaLang/julia/issues?utf8=%E2%9C%93&q=label%3Aarm):
    [#14194], [#14519], [#16645], [#16621]

  * [Power](https://github.com/JuliaLang/julia/issues?utf8=%E2%9C%93&q=label%3Apower):
    [#16455], [#16404]

Deprecated or removed
---------------------

  * The following function names have been simplified and unified ([#13232]):

    * `get_bigfloat_precision`  -> `precision(BigFloat)`
    * `set_bigfloat_precision`  -> `setprecision`
    * `with_bigfloat_precision` -> `setprecision`

    * `get_rounding`            -> `rounding`
    * `set_rounding`            -> `setrounding`
    * `with_rounding`           -> `setrounding`

  * The method `A_ldiv_B!(SparseMatrixCSC, StridedVecOrMat)` has been deprecated
    in favor of versions that require the matrix to be in factored form
    ([#13496]).

  * `chol(A,Val{:U/:L})` has been deprecated in favor of `chol(A)` ([#13680]).

  * `rem1(x,y)` is discontinued due to inconsistency for `x==0`. Use `mod1` instead ([#14140]).

  * The `FS` module has been renamed to `Filesystem`. Calling the functions `isreadable`,
   `iswritable`, and `isexecutable` on filesystem paths has been deprecated ([#12819]).

  * `RemoteRef` has been deprecated in favor of `RemoteChannel` ([#14458]).

  * `super` has been renamed to `supertype` ([#14335]).

  * `parseip(str)` has been deprecated in favor of `parse(IPAddr, str)` ([#14676]).

  * `readall` has been renamed to `readstring`, and `readbytes` has been renamed to `read` ([#14608], [#14660]).

  * `fieldoffsets(x)` has been deprecated in favor of calling `fieldoffset(x, i)` on each field ([#14777]).

  * `issym` is deprecated in favor of `issymmetric` to match similar functions
    (`ishermitian`, ...) ([#15192]).

  * `scale` is deprecated in favor of either `α*A`, `Diagonal(x)*A`, or `A*Diagonal(x)` ([#15258]).

  * `istext` has been renamed to `istextmime` ([#12872], [#15708]).

  * "Functor" types are no longer necessary and have been deprecated ([#15804]). To maintain
    performance on older versions of Julia the [Compat.jl package](https://github.com/JuliaLang/Compat.jl/pull/184)
    provides a `@functorize` macro.

  * `bitunpack(B)` and `bitpack(A)` have been deprecated in favor of
    `Array(B)` and `BitArray(A)`, respectively ([#16010]).

  * `xdump` is removed, and `dump` now simply shows the full representation of a value.
    `dump` should not be overloaded, since it is for examining concrete structure ([#4163]).

  * `sprandbool` has been deprecated in favor of `sprand(Bool, ...)` or
    `sprand(rng, Bool, ...)` ([#11688], [#16098]).

  * The lowercase `symbol` function has been deprecated in favor of the `Symbol`
    constructor ([#16154]).

  * `writemime` is deprecated, and output methods specifying a MIME type are now
    methods of `show` ([#14052]).

  * BLAS utility functions `blas_set_num_threads`, `blas_vendor`, and `check_blas`
    have been moved to the BLAS module as `BLAS.set_num_threads`, `BLAS.vendor`,
    and `BLAS.check` ([#10548], [#16600]).

  * `print_escaped` has been renamed to `escape_string`, `print_unescaped` has been
    renamed to `unescape_string`, and `print_joined` has been renamed to `join` ([#16603]).

  * `pointer_to_string` has been renamed to `unsafe_wrap(String, ...)`, and
    `pointer_to_array` has been renamed to `unsafe_wrap(Array, ...)` ([#16731]).

  * `sub` and `slice` have been deprecated in favor of `view` ([#16972]).

  * Sparse matrix functions `etree`, `ereach`, `csc_permute`, and `symperm` have been moved
    to the [SuiteSparse.jl package](https://github.com/JuliaSparse/SuiteSparse.jl) ([#12231], [#17033]).

  * The no-op `transpose` fallback for non-numeric arrays has been deprecated. Consider introducing suitable
    `transpose` methods or calling `permutedims(x, (2, 1))` for matrices and `reshape(x, 1, length(x))` for
    vectors.  ([#13171], [#17075], [#17374]).

  * The following macros have been deprecated ([#16219]):
    * `@windows` is deprecated in favor of `is_windows()`
    * `@unix` is deprecated in favor of `is_unix()`
    * `@osx` is deprecated in favor of `is_apple()`
    * `@linux` is deprecated in favor of `is_linux()`
    * `@windows_only` is deprecated in favor of `if is_windows()`
    * `@unix_only` is deprecated in favor of `if is_unix()`
    * `@osx_only` is deprecated in favor of `if is_apple()`
    * `@linux_only` is deprecated in favor of `if is_linux()`
    * NOTE: Using `@static` could be useful/necessary when used in a function's local scope. See details at the section entitled [Handling Operating System Variation](https://docs.julialang.org/en/latest/manual/handling-operating-system-variation/#man-handling-operating-system-variation) in the manual.

Command-line option changes
---------------------------

  * The `-F` flag to load `~/.juliarc` has been deprecated in favor of
    `--startup-file=yes` ([#9482]).

  * The `-f` and `--no-startup` flags to disable loading of `~/.juliarc` have
    been deprecated in favor of `--startup-file=no` ([#9482]).

  * The `-P` and `--post-boot` flags for evaluating an expression in "interactive mode"
    have been deprecated in favor of `-i -e` ([#16854]).

  * The `--no-history-file` flag to disable loading of `~/.julia_history` has been
    deprecated in favor of `--history-file=no` ([#9482]).

Language tooling improvements
-----------------------------

   * The [Julia debugger](https://github.com/Keno/Gallium.jl) makes its debut
     with this release. Install it with `Pkg.add("Gallium")`, and the
     [documentation](https://github.com/Keno/Gallium.jl#gallium) should
     get you going. The [JuliaCon
     talk](https://www.youtube.com/watch?v=e6-hcOHO0tc&list=PLP8iPy9hna6SQPwZUDtAM59-wPzCPyD_S&index=5)
     on Gallium shows off various features of the debugger.

   * The [Juno IDE](http://junolab.org) has matured significantly, and now
     also includes support for plotting and debugging.

   * [Cxx.jl](https://github.com/Keno/Cxx.jl) provides a convenient FFI for
     calling C++ code from Julia.

Julia v0.4.0 Release Notes
==========================

New language features
---------------------

  * Function call overloading: for arbitrary objects `x` (not of type
    `Function`), `x(...)` is transformed into `call(x, ...)`, and `call`
    can be overloaded as desired.  Constructors are now a special case of
    this mechanism, which allows e.g. constructors for abstract types.
    `T(...)` falls back to `convert(T, x)`, so all `convert` methods implicitly
    define a constructor ([#8712], [#2403]).

  * Unicode version 8 is now supported for identifiers etcetera ([#7917], [#12031]).

  * Type parameters now permit any `isbits` type, not just `Int` and `Bool` ([#6081]).

  * Keyword argument names can be computed, using syntax such as `f(; symbol => val)` ([#7704]).

  * The syntax `@generated function` enables generation of specialized methods based on
    argument types. At compile time, the function is called with its arguments bound to their
    types instead of to their values. The function then returns an expression forming the
    body of the function to be called at run time ([#7311]).

  * [Documentation system](https://docs.julialang.org/en/latest/manual/documentation/)
    for functions, methods, types and macros in packages and user code ([#8791]).

  * The syntax `function foo end` can be used to introduce a generic function without
    yet adding any methods ([#8283]).

  * Incremental precompilation of modules: call `VERSION >= v"0.4.0-dev+6521" && __precompile__()` at the top of a
    module file to automatically precompile it when it is imported ([#12491]), or manually
    run `Base.compilecache(modulename)`. The resulting precompiled `.ji` file is saved in
    `~/.julia/lib/v0.4` ([#8745]).

      * See manual section on `Module initialization and precompilation` (under `Modules`) for
        details and errata.  In particular, to be safely precompilable a module may need an
        `__init__` function to separate code that must be executed at runtime rather than precompile
        time.  Modules that are *not* precompilable should call `__precompile__(false)`.

      * The precompiled `.ji` file includes a list of dependencies (modules and files that
        were imported/included at precompile-time), and the module is automatically recompiled
        upon `import` when any of its dependencies have changed.  Explicit dependencies
        on other files can be declared with `include_dependency(path)` ([#12458]).

      * New option `--output-incremental={yes|no}` added to invoke the equivalent of `Base.compilecache`
        from the command line.

  * The syntax `new{parameters...}(...)` can be used in constructors to specify parameters for
    the type to be constructed ([#8135]).

  * `++` is now parsed as an infix operator, but does not yet have a default definition ([#11030], [#11686]).

  * Support for inter-task communication using `Channels` ([#12264]).
    See https://docs.julialang.org/en/latest/manual/parallel-computing/#channels for details.

  * `RemoteRef`s now point to remote channels. The remote channels can be of length greater than 1.
    Default continues to be of length 1 ([#12385]).
    See https://docs.julialang.org/en/latest/manual/parallel-computing/#remoterefs-and-abstractchannels for details.

  * `@__LINE__` special macro now available to reflect invocation source line number ([#12727]).

Language changes
----------------

  * Tuple types are now written as `Tuple{A, B}` instead of as `(A, B)`.
    Tuples of bits types are inlined into structs and arrays, like other
    immutable types.
    `...` now does splatting inside parentheses, instead of constructing a
    variadic tuple type ([#10380]).
    Variadic tuple types are written as `Tuple{Vararg{T}}`.

  * Using `[x,y]` to concatenate arrays is deprecated, and in the future will
    construct a vector of `x` and `y` instead ([#3737], [#2488], [#8599]).

  * Significant improvements to `ccall` and `cfunction`

    * As a safer alternative to creating pointers (`Ptr`), the managed reference type
      `Ref` has been added. A `Ref` points to the data contained by a value in an
      abstract sense, and in a way that is GC-safe. For example, `Ref(2)` points to
      a storage location that contains the integer `2`, and `Ref(array,3)` points
      to the third element of an array. A `Ref` can be automatically converted to a
      native pointer when passed to a `ccall`.

    * When passing a by-reference argument to `ccall`, you can declare
      the argument type to be `Ref{T}` instead of `Ptr{T}`, and just
      pass `x` instead of `&x`.

    * `ccall` is now lowered to call `unsafe_convert(T, cconvert(T, x))` on each
      argument. `cconvert` falls back to `convert`, but can be used to convert an
      argument to an arbitrarily-different representation more suitable for passing
      to C. `unsafe_convert` then handles conversions to `Ptr`.

    * `ccall` and `cfunction` now support correctly passing and returning structs,
      following the platform ABI (assuming the C types are mirrored accurately in Julia).

    * `cfunction` arguments of struct-like Julia types are now passed by value.
      If `Ref{T}` is used as a `cfunction` argument type, it will look up the
      method applicable to `T`, but pass the argument by reference (as Julia functions
      usually do). However, this should only be used for objects allocated by Julia
      and for `isbits` types.

  * `convert(Ptr,x)` is deprecated for most types, replaced by
    `unsafe_convert`. You can still `convert` between pointer types,
    and between pointers and `Int` or `UInt`.

  * Module `__init__` methods no longer swallow thrown exceptions; they now
    throw an `InitError` wrapping the thrown exception ([#12576]).

  * Unsigned `BigInt` literal syntax has been removed ([#11105]).
    Unsigned literals larger than `UInt128` now throw a syntax error.

  * `error(::Exception)` and `error(::Type{Exception})` have been deprecated
     in favor of using an explicit `throw` ([#9690]).

  * `Uint` etcetera are renamed to `UInt` ([#8905]).

  * `String` is renamed to `AbstractString` ([#8872]).

  * `FloatingPoint` is renamed to `AbstractFloat` ([#12162]).

  * `None` is deprecated; use `Union{}` instead ([#8423]).

  * `Nothing` (the type of `nothing`) is renamed to `Void` ([#8423]).

  * Arrays can be constructed with the syntax `Array{T}(m,n)` ([#3214], [#10075]).

  * `Dict` literal syntax `[a=>b,c=>d]` is replaced by `Dict(a=>b,c=>d)`,
    `{a=>b}` is replaced by `Dict{Any,Any}(a=>b)`, and
    `(K=>V)[...]` is replaced by `Dict{K,V}(...)`.
    The new syntax has many advantages: all of its components are first-class,
    it generalizes to other types of containers, it is easier to guess how to
    specify key and value types, and the syntaxes for empty and pre-populated
    dicts are synchronized. As part of this change, `=>` is parsed as a normal
    operator, and `Base` defines it to construct `Pair` objects ([#6739]).

  * `Char` is no longer a subtype of `Integer` ([#8816]).
    Char now supports a more limited set of operations with `Integer` types:

      * comparison / equality
      * `Char` + `Int` = `Char`
      * `Char` - `Char` = `Int`

  * `round` rounds to the nearest integer using the default rounding mode,
    which is ties-to-even by default ([#8750]).

  * A custom triple-quoted string like `x"""..."""` no longer invokes an `x_mstr`
    macro. Instead, the string is first unindented and then `x_str` is invoked,
    as if the string had been single-quoted ([#10228]).

  * Colons (`:`) within indexing expressions are no longer lowered to the range
    `1:end`. Instead, the `:` identifier is passed directly. Custom array types
    that implement `getindex` or `setindex!` methods must also extend those
    methods to support arguments of type `Colon` ([#10331]).

  * Unions of types should now be written with curly braces instead of parentheses, i.e.
    `Union{Type1, Type2}` instead of `Union(Type1, Type2)` ([#11432]).

  * The keyword `local` is no longer allowed in global scope. Use `let` instead of
    `begin` to create a new scope from the top level ([#7234], [#10472]).

  * Triple-quoted strings no longer treat tabs as 8 spaces. Instead, the
    longest common prefix of spaces and tabs is removed.

  * `global x` in a nested scope is now a syntax error if `x` is local
    to the enclosing scope ([#7264]/[#11985]).

  * The default `importall Base.Operators` is deprecated, and relying on it
    will give a warning ([#8113]).

  * `remotecall_fetch` and `fetch` now rethrow any uncaught remote exception locally as a
    `RemoteException`. Previously they would return the remote exception object.
    The worker pid, remote exception and remote backtrace are available in the
    thrown `RemoteException`.

  * If any of the enclosed async operations in a `@sync` block throw exceptions, they
    are now collected in a `CompositeException` and the `CompositeException` thrown.


Command line option changes
---------------------------

  * The `-i` option now forces the REPL to run after loading the specified script (if any) ([#11347]).

  * New option `--handle-signals={yes|no}` to disable Julia's signal handlers.

  * The `--depwarn={yes|no|error}` option enables/disables syntax and method deprecation warnings,
    or turns them into errors ([#9294]).

  * Some command line options are slated for deprecation / removal
    - `-f, --no-startup` Don't load ~/.juliarc (deprecated, use --startup-file=no)
    - `-F` Load ~/.juliarc (deprecated, use --startup-file=yes)`
    - `-P, --post-boot <expr>`  Evaluate <expr>, but don't disable interactive mode (deprecated, use -i -e instead)
    - `--no-history-file`  Don't load history file (deprecated, use --history-file=no)

Compiler/Runtime improvements
-----------------------------

  * Functions may be annotated with metadata (`:meta` expressions) to be used by the compiler ([#8297]).

  * `@inline` before a function definition forces the compiler to inline the function ([#8297]).

  * Loads from heap-allocated immutables are hoisted out of loops in more cases ([#8867]).

  * Accessing fields that are always initialized no longer produces undefined checks ([#8827]).

  * New generational garbage collector which greatly reduces GC overhead for many common workloads ([#5227]).

Library improvements
--------------------

  * Build with USE_GPL_LIBS=0 to exclude all GPL libraries and code ([#10870]).

  * Linear algebra

    * The `LinAlg` module is now exported.

    * `sparse(A)` now takes any `AbstractMatrix` A as an argument ([#10031]).

    * Factorization API is now type-stable; functions dispatch on `Val{false}` or `Val{true}` instead of a boolean value ([#9575]).

    * Added generic Cholesky factorization, and the Cholesky factorization is now parametrized by the matrix type ([#7236]).

    * Sparse `cholfact` and `ldltfact` functions now accept a `perm` keyword
      for user-provided permutations and a `shift` keyword to factorize
      a shifted matrix ([#10844]).

    * New `svds` function for the sparse truncated SVD ([#9425]).

    * `Symmetric` and `Hermitian` immutables are now parametrized by the matrix type ([#7992]).

    * New `ordschur` and `ordschur!` functions for sorting a Schur factorization by the eigenvalues ([#8467],[#9701]).

    * `Givens` type doesn't have a size anymore and is no longer a subtype of `AbstractMatrix` ([#8660]).

    * Large speedup in sparse `\` and splitting of Cholesky and LDLᵀ factorizations into `cholfact` and `ldltfact` ([#10117]).

    * Add sparse least squares to `\` by adding `qrfact` for sparse matrices based on the SPQR library ([#10180]).

    * Split `Triangular` type into `UpperTriangular`, `LowerTriangular`, `UnitUpperTriagular` and `UnitLowerTriangular` ([#9779])

    * OpenBLAS 64-bit (ILP64) interface is now compiled with a `64_` suffix ([#8734]) to avoid conflicts with external libraries using a 32-bit BLAS ([#4923]).

    * New `vecdot` function, analogous to `vecnorm`, for Euclidean inner products over any iterable container ([#11067]).

    * `p = plan_fft(x)` and similar functions now return a `Base.DFT.Plan` object, rather
    than an anonymous function.  Calling it via `p(x)` is deprecated in favor of
    `p * x` or `p \ x` (for the inverse), and it can also be used with `A_mul_B!`
    to employ pre-allocated output arrays ([#12087]).

    * `LU{T,Tridiagonal{T}}` now supports extraction of `L`, `U`, `p`, and `P` factors ([#12137]).

    * Allocations in sparse matrix factorizations are now tracked by Julia's garbage collector ([#12034]).

  * Strings

    * NUL-terminated strings should now be passed to C via the new `Cstring` type, not `Ptr{UInt8}` or `Ptr{Cchar}`,
      in order to check whether the string is free of NUL characters (which would cause silent truncation in C).
      The analogous type `Cwstring` should be used for NUL-terminated `wchar_t*` strings ([#10994]).

    * `graphemes(s)` returns an iterator over grapheme substrings of `s` ([#9261]).

    * Character predicates such as `islower()`, `isspace()`, etc. use
      utf8proc to provide uniform cross-platform behavior and
      up-to-date, locale-independent support for Unicode standards
      ([#5939]).

    * `reverseind` function to convert indices in reversed strings (e.g. from
      reversed regex searches) to indices in the original string ([#9249]).

    * `charwidth(c)` and `strwidth(s)` now return up-to-date cross-platform
      results (via utf8proc) ([#10659]): Julia now likes pizza ([#3721]), but some terminals still don't.

    * `is_valid_char(c)`, (now `isvalid(Char,c)` ([#11241])), now correctly handles Unicode "non-characters", which are valid Unicode codepoints ([#11171]).

    * Backreferences in replacement strings in calls to `replace` with a `Regex` pattern are now supported ([#11849]).
      Use the `s` string prefix to indicate a replacement string contains a backreference. For example, `replace("ab", r"(.)(.)", s"\2\1")` yields "ba".

    * Capture groups in regular expressions can now be named using PCRE syntax, `(?P<group_name>...)`. Capture group matches can be accessed by name by indexing a `Match` object with the name of the group ([#11566]).

    * `countlines()` now counts all lines, not just non-empty ([#11947]).

  * Array and AbstractArray improvements

    * New multidimensional iterators and index types for efficient iteration over `AbstractArray`s. Array iteration should generally be written as `for i in eachindex(A) ... end` rather than `for i = 1:length(A) ... end` ([#8432]).

    * New implementation of SubArrays with substantial performance and functionality improvements ([#8501]).

    * AbstractArray subtypes only need to implement `size` and `getindex`
      for scalar indices to support indexing; all other indexing behaviors
      (including logical indexing, ranges of indices, vectors, colons, etc.) are
      implemented in default fallbacks. Similarly, they only need to implement
      scalar `setindex!` to support all forms of indexed assingment ([#10525]).

    * AbstractArrays that do not extend `similar` now return an `Array` by
      default ([#10525]).

  * Data structures

    * New `sortperm!` function for pre-allocated index arrays ([#8792]).

    * Switch from `O(N)` to `O(log N)` algorithm for `dequeue!(pq, key)`
    with `PriorityQueue`. This provides major speedups for large
    queues ([#8011]).

    * `PriorityQueue` now includes the order type among its
      parameters, `PriorityQueue{KeyType,ValueType,OrderType}`. An
      empty queue can be constructed as `pq =
      PriorityQueue(KeyType,ValueType)`, if you intend to use the
      default `Forward` order, or `pq = PriorityQueue(KeyType,
      ValueType, OrderType)` otherwise ([#8011]).

    * Efficient `mean` and `median` for ranges ([#8089]).

    * `deepcopy` recurses through immutable types and makes copies of their mutable fields ([#8560]).

    * `copy(a::DArray)` will now make a copy of a `DArray` ([#9745]).

  * New types

    * Enums are now supported through the `@enum EnumName EnumValue1
      EnumValue2` syntax. Enum member values also support abitrary
      value assignment by the `@enum EnumName EnumValue1=1
      EnumValue2=10 EnumValue3=20` syntax ([#10168]).

    * New `Dates` module for calendar dates and other time-interval calculations ([#7654]).

    * New `Nullable` type for missing data ([#8152]).

    * A new `Val{T}` type allows one to dispatch on bits-type values ([#9452]).

    * `linspace` now returns a `LinSpace` object which lazily computes linear interpolation of values between the start and stop values. It "lifts" endpoints which are approximately rational in the same manner as the `colon` operator.

  * Arithmetic

    * `convert` now checks for overflow when truncating integers or converting between
    signed and unsigned ([#5413]).

    * Arithmetic is type-preserving for more types; e.g. `(x::Int8) + (y::Int8)` now
    yields an `Int8` ([#3759]).

    * Reductions (e.g. `reduce`, `sum`) widen small types (integers smaller than `Int`, and `Float16`).

    * Added optional rounding argument to floating-point constructors ([#8845]).

    * Equality (`==`) and inequality (`<`/`<=`) comparisons are now correct
      across all numeric types ([#9133], [#9198]).

    * Rational arithmetic throws errors on overflow ([#8672]).

    * Optional `log` and `log1p` functions implemented in pure Julia (experimental) ([#10008]).

    * The `MathConst` type has been renamed `Irrational` ([#11922]).

    * `isapprox` now has simpler and more sensible default tolerances ([#12393]), supports arrays, and has synonyms `≈` ([U+2248](https://www.fileformat.info/info/unicode/char/2248/index.htm), LaTeX `\approx`) and `≉` ([U+2249](https://www.fileformat.info/info/unicode/char/2249/index.htm), LaTeX `\napprox`) for `isapprox` and `!isapprox`, respectively ([#12472]).

  * Numbers

    * `primes` is now faster and has been extended to generate the primes in a user defined closed interval ([#12025]).

    * The function `primesmask` which generates a prime sieve for a user defined closed interval is now exported ([#12025]).

  * Random numbers

    * Streamlined random number generation APIs [#8246].
    The default `rand` no longer uses global state in the underlying C library,
    dSFMT, making it closer to being thread-safe ([#8399], [#8832]).
    All APIs can now take an `AbstractRNG` argument ([#8854], [#9065]). The seed argument to `srand` is now optional ([#8320], [#8854]).
    The APIs accepting a range argument are extended to accept an arbitrary
    `AbstractArray` ([#9049]).
    Passing a range of `BigInt` to `rand` or `rand!` is now supported ([#9122]).
    There are speed improvements across the board ([#8808], [#8941], [#8958], [#9083]).

    * Significantly faster `randn` ([#9126], [#9132]).

    * The `randexp` and `randexp!` functions are exported ([#9144]).

  * File

    * Added function `readlink` which returns the value of a symbolic link "path" ([#10714]).

    * Added function `ismount` which checks if a directory is a mount point ([#11279]).

    * The `cp` function now accepts keyword arguments `remove_destination` and `follow_symlinks` ([#10888]).

    * The `mv` function now accepts keyword argument `remove_destination` ([#11145]).

  * `Pipe()` creates a bidirectional I/O object that can be passed to `spawn` or `pipeline`
    for redirecting process streams ([#12739]).

  * Other improvements

    * You can now tab-complete emoji via their [short names](http://www.emoji-cheat-sheet.com/), using `\:name:<tab>` ([#10709]).

    * `gc_enable` subsumes `gc_disable`, and also returns the previous GC state.

    * `assert`, `@assert` now throws an `AssertionError` exception type ([#9734]).

    * `@simd` now rejects invalid control flow (`@goto` / break / continue) in the inner loop body at compile time ([#8624]).

    * The `machinefile` now supports a host count ([#7616]).

    * `code_native` now outputs branch labels ([#8897]).

    * Added `recvfrom` to get source address of UDP packets ([#9418]).

    * `ClusterManager` performance improvements ([#9309]) and support for changing transports([#9434]).

    * Added `Base.get_process_title` / `Base.set_process_title` ([#9957]).

    * `readavailable` now returns a byte vector instead of a string.

    * New `lock` and `unlock` functions, operating on `ReentrantLock`, to lock a stream during
      concurrent writes from multiple tasks ([#10679]).

    * `code_llvm` now outputs stripped IR without debug info or other attached metadata.
      Use `code_llvm_raw` for the unstripped output ([#10747]).

    * New `withenv(var=>val, ...) do ... end` function to temporarily
      modify environment variables ([#10914]).

    * New function `relpath` returns a relative filepath to path either from the current
      directory or from an optional start directory ([#10893]).

    * `mktemp` and `mktempdir` now take an optional argument to set which
      directory the temporary file or directory is created in.

    * New garbage collector tracked memory allocator functions: `jl_malloc`, `jl_calloc`,
    `jl_realloc`, and `jl_free` with libc API ([[#12034]]).

    * `mktempdir` and `mktemp` now have variants that take a function as its
      first argument for automated clean-up ([[#9017]]).

Deprecated or removed
---------------------

  * several syntax whitespace insensitivities have been deprecated ([#11891]).
    ```julia
     # function call
     f (x)

     # getindex
     x [17]
     rand(2) [1]

     # function definition
     f (x) = x^2
     function foo (x)
	x^2
     end
    ```

  * indexing with `Real`s that are not subtypes of `Integer` (`Rational`, `AbstractFloat`, etc.) has been deprecated ([#10458]).

  * `push!(A)` has been deprecated, use `append!` instead of splatting arguments to `push!` ([#10400]).

  * `names` for composite datatypes has been deprecated and
    renamed to `fieldnames` ([#10332]).

  * `DArray` functionality has been removed from `Base` and is now a
    standalone package under the JuliaParallel umbrella organization ([#10333]).

  * The `Graphics` module has been removed from `Base` and is now a
    standalone package ([#10150], [#9862]).

  * The `Woodbury` special matrix type has been removed from `LinAlg` ([#10024]).

  * `median` and `median!` no longer accept a `checknan` keyword argument ([#8605]).

  * `inf` and `nan` are now deprecated in favor of `T(Inf)` and `T(NaN)`, respectively ([#8776]).

  * `oftype(T::Type, x)` is deprecated in favor of `convert(T,x)` (or `T(x)`).

  * `{...}` syntax is deprecated in favor of `Any[...]` ([#8578]).

  * `itrunc`, `ifloor`, `iceil` and `iround` are deprecated in favour of
    `trunc{T<:Integer}(T,x)`, `floor{T<:Integer}(T,x)`, etc.. `trunc` is now
    always bound-checked;`Base.unsafe_trunc` provides the old unchecked `itrunc`
    behaviour ([#9133]).

  * `squeeze` now requires that passed dimension(s) are an `Int` or tuple of `Int`s;
    calling `squeeze` with an arbitrary iterator is deprecated ([#9271]).
    Additionally, passed dimensions must be unique and correspond to extant
    dimensions of the input array.

  * `randbool` is deprecated. Use `rand(Bool)` to produce a random boolean value, and
    `bitrand` to produce a random BitArray ([#9105], [#9569]).

  * `beginswith` is renamed to `startswith` ([#9578]).

  * `null` is renamed to `nullspace` ([#9714]).

  * The operators `|>`, `.>`, `>>`, and `.>>` as used for process I/O redirection
    are replaced with the `pipeline` function ([#5349], [#12739]).

  * `flipud(A)` and `fliplr(A)` have been deprecated in favor of `flipdim(A, 1)` and
    `flipdim(A, 2)`, respectively ([#10446]).

  * Numeric conversion functions whose names are lower-case versions of type
    names have been removed. To convert a scalar, use the type name, e.g.
    `Int32(x)`. To convert an array to a different element type, use
    `Array{T}(x)`, `map(T,x)`, or `round(T,x)`. To parse a string as an integer
    or floating-point number, use `parse` ([#1470], [#6211]).

  * Low-level functions from the C library and dynamic linker have been moved to
    modules `Libc` and `Libdl`, respectively ([#10328]).

  * The functions `parseint`, `parsefloat`, `float32_isvalid`,
  `float64_isvalid`, and the string-argument `BigInt` and `BigFloat` have
  been replaced by `parse` and `tryparse` with a type argument. The string
  macro `big"xx"` can be used to construct `BigInt` and `BigFloat` literals
  ([#3631], [#5704], [#9487], [#10543], [#10955]).

  * the `--int-literals` compiler option is no longer accepted ([#9597]).

  * Instead of `linrange`, use `linspace` ([#9666]).

  * The functions `is_valid_char`, `is_valid_ascii`, `is_valid_utf8`, `is_valid_utf16`, and
    `is_valid_utf32` have been replaced by generic `isvalid` methods.
    The single argument form `isvalid(value)` can now be used for values of type `Char`, `ASCIIString`,
    `UTF8String`, `UTF16String` and `UTF32String`.
    The two argument form `isvalid(type, value)` can be used with the above types, with values
    of type `Vector{UInt8}`, `Vector{UInt16}`, `Vector{UInt32}`, and `Vector{Char}` ([#11241]).

  * Instead of `utf32(64,123,...)` use `utf32(UInt32[64,123,...])` ([#11379]).

  * `start_timer` and `stop_timer` are replaced by `Timer` and `close`.

  * The following internal julia C functions have been renamed, in order to prevent
    potential naming conflicts with C libraries: ([#11741])

    * `gc_wb*` -> `jl_gc_wb*`

    * `gc_queue_root` -> `jl_gc_queue_root`

    * `allocobj` -> `jl_gc_allocobj`

    * `alloc_[0-3]w` -> `jl_gc_alloc_*w`

    * `diff_gc_total_bytes` -> `jl_gc_diff_total_bytes`

    * `sync_gc_total_bytes` -> `jl_gc_sync_total_bytes`

  * `require(::AbstractString)` and `reload` (see news about addition of `compile`).

  * `cartesianmap` is deprecated in favor of iterating over a `CartesianRange`

Julia v0.3.0 Release Notes
==========================

New language features
---------------------

  * Greatly enhanced performance for passing and returning `Tuple`s ([#4042]).

  * `Tuple`s (of `Integer`s, `Symbol`s, or `Bool`s) can now be used as type
    parameters ([#5164]).

  * An additional default "inner" constructor accepting any arguments is now
    generated. Constructors that look like `MyType(a, b) = new(a, b)` do not
    need to be added manually ([#4026], [#7071]).

  * Expanded array type hierarchy to include an abstract `DenseArray` for
    in-memory arrays with standard strided storage ([#987], [#2345],
    [#6212]).

  * When reloading code, types whose definitions have not changed can be
    ignored in some cases.

  * Binary `~` now parses as a vararg macro call to `@~`.
    For example `x~y~z` => `@~ x y z` ([#4882]).

  * Structure fields can now be accessed by index ([#4806]).

  * If a module contains a function `__init__()`, it will be called when
    the module is first loaded, and on process startup if a pre-compiled
    version of the module is present ([#1268]).

  * Multi-line comments ([#69], [#6128]): `#= .... =#`

  * `--check-bounds=yes|no` compiler option

  * Unicode identifiers are normalized (NFC) so that different encodings
    of equivalent strings are treated as the same identifier ([#5462]).

  * The set of characters permitted in identifiers has been restricted based
    on Unicode categories. Generally, punctuation, formatting and control
    characters, and operator symbols are not allowed in identifiers.
    Number-like characters cannot begin identifiers ([#5936]).

  * Define a limited number of infix Unicode operators ([#552], [#6582]):

    | Precedence class | Operators (with synonyms, if any)
    | ---------------- | ---------------------------------
    |   ==             |  ≥ (>=) ≤ (<=) ≡ (===) ≠ (!=) ≢ (!==) .≥ (.>=) .≤ (.<=) .!= (.≠) ∈ (`in`) ∉ (`(x,y)->!in(x, y)`) ∋ (`(x,y)->in(y, x)`) ∌ (`(x,y)->!in(y, x)`) ⊆ (`issubset`) ⊈ (`(x,y)->!issubset(x, y)`) ⊊ (`(x,y)->x⊆y && x!=y`) |
    |   +              | ∪ (`union`) |
    |   *              | ÷ (`div`) ⋅ (`dot`) × (`cross`) ∩ (`intersect`) |
    |   unary          | √ ∛ |

    In addition to these, many of the Unicode operator symbols are parsed
    as infix operators and are available for user-defined methods ([#6929]).

  * Improved reporting of syntax errors ([#6179])

  * `break` inside a `for` loop with multiple ranges now exits the entire loop nest ([#5154])

  * Local goto statements using the `@goto` and `@label` macros ([#101]).

REPL improvements
-----------------

  * New native-Julia REPL implementation, eliminating many problems
    stemming from the old GNU Readline-based REPL ([#6270]).

  * Tab-substitution of LaTeX math symbols (e.g. `\alpha` by `α`) ([#6911]).
    This also works in IJulia and in Emacs ([#6920]).

  * `workspace()` function for obtaining a fresh workspace ([#1195]).

Library improvements
--------------------

  * `isequal` now compares all numbers by value, ignoring type ([#6624]).

  * Implement limited shared-memory parallelism with `SharedArray`s ([#5380]).

  * Well-behaved floating-point ranges ([#2333], [#5636]).
    Introduced the `FloatRange` type for floating-point ranges with a step,
    which will give intuitive/correct results for classically problematic
    ranges like `0.1:0.1:0.3`, `0.0:0.7:2.1` or `1.0:1/49:27.0`.

  * `mod2pi` function ([#4799], [#4862]).

  * New functions `minmax` and `extrema` ([#5275]).

  * New macros `@edit`, `@less`, `@code_typed`, `@code_lowered`, `@code_llvm` and `@code_native` that all function like `@which` ([#5832]).

  * `consume(p)` extended to `consume(p, args...)`, allowing it
    to optionally pass `args...` back to the producer ([#4775]).

  * `.juliarc.jl` is now loaded for both script and REPL execution ([#5076]).

  * The `Sys` module now includes convenient functions for working with
    dynamic library handles; `Sys.dllist` will list out all paths currently
    loaded via `dlopen`, and `Sys.dlpath` will lookup a path from a handle

  * `readdlm` treats multiple whitespace characters as a single delimiter
    by default (when no delimiter is specified). This is useful for reading
    fixed-width or messy whitespace-delimited data ([#5403]).

  * The Airy, Bessel, Hankel, and related functions (`airy*`,
    `bessel*`, `hankel*`) now detect errors returned by the underlying
    AMOS library, throwing an `AmosException` in that case ([#4967]).

  * `methodswith` now returns an array of `Method`s ([#5464]) rather
    than just printing its results.

  * `errno([code])` function to get or set the C library's `errno`.

  * `GitHub` module for interacting with the GitHub API.

  * Package improvements

    * Packages are now installed into `.julia/v0.3` by default (or
      whatever the current Julia version is), so that different
      versions of Julia can co-exist with incompatible packages.
      Existing `.julia` installations are unaffected unless `Pkg.init()`
      is run to re-create the package directories ([#3344], [#5737]).

    * `Pkg.submit(pkg[,commit])` function to automatically submit
      a GitHub pull request to the package author.

  * Collections improvements

    * `Array` assignment (e.g. `x[:] = y`) ignores singleton dimensions
      and allows the last dimension of one side to match all trailing dimensions
      of the other ([#4048], [#4383]).

    * `Dict(kv)` constructor for any iterator on `(key,value)` pairs.

    * Multi-key `Dict`s: `D[x,y...]` is now a synonym for `D[(x,y...)]`
      for associations `D` ([#4870]).

    * `push!` and `unshift!` can push multiple arguments ([#4782]).

    * `writedlm` and `writecsv` now accept any iterable collection of
      iterable rows, in addition to `AbstractArray` arguments, and the
      `writedlm` delimiter can be any printable object (e.g. a
      `String`) instead of just a `Char`.

    * `isempty` now works for any iterable collection ([#5827]).

    * `unique` now accepts an optional `dim` argument for finding
      unique rows or columns of a matrix or regions of a
      multidimensional array ([#5811]).

  * `Number` improvements

    * The `ImaginaryUnit` type no longer exists. Instead, `im` is of type
      `Complex{Bool}`. Making this work required changing the semantics of
      boolean multiplication to approximately, `true * x = x` and
      `false * x = zero(x)`, which can itself be considered useful ([#5468]).

    * `big` is now vectorized ([#4766])

    * `nextpow` and `prevpow` now return the `a^n` values instead of the
      exponent `n` ([#4819])

    * Overflow detection in `parseint` ([#4874]).

    * `rand` now supports arbitrary `Ranges` arguments ([#5059]).

    * `expm1` and `log1p` now support complex arguments ([#3141]).

    * Broadcasting `.//` is now included ([#7094]).

    * `prevfloat` and `nextfloat` now saturate at -Inf and Inf,
      respectively, and have otherwise been fixed to follow the IEEE-754
      standard functions `nextDown` and `nextUp` ([#5025]).

    * New function `widen` for widening numeric types and values, and `widemul`
      for multiplying to a larger type ([#6169]).

    * `polygamma`, `digamma`, and `trigamma` now accept complex
      arguments, and `zeta(s, z)` now provides the Hurwitz zeta ([#7125]).

    * Narrow integer types (< 32 bits) are promoted to `Float64` rather
      than to `Float32` by `float(x)` ([#7390]).

  * `String` improvements

    * Triple-quoted regex strings, `r"""..."""` ([#4934]).

    * New string type, `UTF16String` ([#4930]), constructed by
      `utf16(s)` from another string, a `Uint16` array or pointer, or
      a byte array (possibly prefixed by a byte-order marker to
      indicate endian-ness).  Its data is internally `NULL`-terminated
      for passing to C ([#7016]).

    * `CharString` is renamed to `UTF32String` ([#4943]), and its data
      is now internally `NULL`-terminated for passing to C ([#7016]).
      `CharString(c::Char...)` is deprecated in favor of `utf32(c...)`,
      and `utf32(s)` otherwise has functionality similar to `utf16(s)`.

    * New `WString` and `wstring` synonyms for either `UTF16String`
      and `utf16` or `UTF32String` and `utf32`, respectively, depending
      on the width of `Cwchar_t` ([#7016]).

    * `normalize_string` function to perform Unicode normalization,
      case-folding, and other transformations ([#5576]).

    * `pointer(s, i=1)` for `ByteString`, `UTF16String`, `UTF32String`,
      and `SubString`s thereof ([#5703]).

    * `bytestring` is automatically called on `String` arguments for
      conversion to `Ptr{Uint8}` in `ccall` ([#5677]).

  * Linear algebra improvements

      * Balancing options for eigenvector calculations for general matrices ([#5428]).

      * Mutating linear algebra functions no longer promote ([#5526]).

      * `condskeel` for Skeel condition numbers ([#5726]).

      * `norm(::Matrix)` no longer calculates a vector norm when the first
        dimension is one ([#5545]); it always uses the operator (induced)
        matrix norm.

      * New `vecnorm(itr, p=2)` function that computes the norm of
        any iterable collection of numbers as if it were a vector of
        the same length.  This generalizes and replaces `normfro` ([#6057]),
        and `norm` is now type-stable ([#6056]).

      * New `UniformScaling` matrix type and identity `I` constant ([#5810]).

      * None of the concrete matrix factorization types are exported from `Base`
        by default anymore.

    * Sparse linear algebra

      * 1-d sparse `getindex` has been implemented ([#7047])

      * Faster sparse `getindex` ([#7131]).

      * Faster sparse `kron` ([#4958]).

      * `sparse(A) \ B` now supports a matrix `B` of right-hand sides ([#5196]).

      * `eigs(A, sigma)` now uses shift-and-invert for nonzero shifts `sigma` and inverse iteration for `which="SM"`. If `sigma==nothing` (the new default), computes ordinary (forward) iterations ([#5776]).

      * `sprand` is faster, and whether any entry is nonzero is now determined independently with the specified probability ([#6726]).

    * Dense linear algebra for special matrix types

      * Interconversions between the special matrix types `Diagonal`, `Bidiagonal`,
        `SymTridiagonal`, `Triangular`, and `Triangular`, and `Matrix` are now allowed
        for matrices which are representable in both source and destination types ([5e3f074b]).

      * Allow for addition and subtraction over mixed matrix types, automatically promoting
        the result to the denser matrix type ([a448e080], [#5927])

      * new algorithms for linear solvers and eigensystems of `Bidiagonal`
        matrices of generic element types ([#5277])

      * new algorithms for linear solvers, eigensystems and singular systems of `Diagonal`
        matrices of generic element types ([#5263])

      * new algorithms for linear solvers and eigensystems of `Triangular`
        matrices of generic element types ([#5255])

      * specialized `inv` and `det` methods for `Tridiagonal` and `SymTridiagonal`
        based on recurrence relations between principal minors ([#5358])

      * specialized `transpose`, `ctranspose`, `istril`, `istriu` methods for
        `Triangular` ([#5255]) and `Bidiagonal` ([#5277])

      * new LAPACK wrappers
        - condition number estimate `cond(A::Triangular)` ([#5255])

      * parametrize `Triangular` on matrix type ([#7064])

      * Lyapunov / Sylvester solver ([#7435])

      * `eigvals` for `Symmetric`, `Tridiagonal` and `Hermitian` matrices now
        support additional method signatures: ([#3688], [#6652], [#6678], [#7647])
        - `eigvals(M, el, eu)` finds all eigenvalues in the interval `(el, eu]`
        - `eigvals(M, il:iu)` finds the `il`th through the `iu`th eigenvalues (in ascending order)

    * Dense linear algebra for generic matrix element types

      * LU factorization ([#5381] and [#5430])

      * QR factorization ([#5526])

  * New function `deleteat!` deletes a specified index or indices and
    returns the updated collection

  * The `setenv` function for external processes now accepts a `dir` keyword
    argument for specifying the directory to start the child process in ([#4888]).

  * Constructors for collections (`Set`, `Dict`, etc.) now generally accept a
    single iterable argument giving the elements of the collection ([#4996], [#4871])

  * Ranges and arrays with the same elements are now unequal. This allows hashing
    and comparing ranges to be faster ([#5778]).

  * Broadcasting now works on arbitrary `AbstractArrays` ([#5387])

  * Reduction functions that accept a pre-allocated output array, including
    `sum!`, `prod!`, `maximum!`, `minimum!`, `all!`, `any!` ([#6197], [#5387])

  * Faster performance on `fill!` and `copy!` for array types not supporting
    efficient linear indexing ([#5671], [#5387])

  * Changes to range types ([#5585])

    * `Range` is now the abstract range type, instead of `Ranges`

    * New function `range` for constructing ranges by length

    * `Range` is now `StepRange`, and `Range1` is now `UnitRange`. Their
      constructors accept end points instead of lengths. Both are subtypes of a
      new abstract type `OrdinalRange`.

    * Ranges now support `BigInt` and general ordinal types.

    * Very large ranges (e.g. `0:typemax(Int)`) can now be constructed, but some
      operations (e.g. `length`) will raise an `OverflowError`.

  * Extended API for `cov` and `cor`, which accept keyword arguments `vardim`,
    `corrected`, and `mean` ([#6273])

  * New functions `randsubseq` and `randsubseq!` to create a random subsequence of an array ([#6726])

  * New macro `@evalpoly` for efficient inline evaluation of polynomials ([#7146]).

  * The signal filtering function `filt` now accepts an optional initial filter state vector. A new in-place function `filt!` is also exported ([#7513]).

  * Significantly faster `cumsum` and `cumprod` ([#7359]).

  * Implement `findmin` and `findmax` over specified array dimensions ([#6716]).

  * Support memory-mapping of files with offsets on Windows ([#7242]).

  * Catch writes to protect memory, such as when trying to modify a mmapped file opened in read-only mode ([#3434]).

Environment improvements
------------------------

  * New `--code-coverage` and `--track-allocation` startup features allow one to measure the number of executions or the amount of memory allocated, respectively, at each line of code ([#5423],[#7464]).

  * `Profile.init` now accepts keyword arguments, and returns the current settings when no arguments are supplied ([#7365]).

Build improvements
------------------

  * Dependencies are now verified against stored MD5/SHA512 hashes, to ensure
    that the correct file has been downloaded and was not modified ([#6773]).


Deprecated or removed
---------------------

  * `convert(Ptr{T1}, x::Array{T2})` is now deprecated unless `T1 == T2`
    or `T1 == Void` ([#6073]).  (You can still explicitly `convert`
    one pointer type into another if needed.)

  * `Sys.shlib_ext` has been renamed to `Sys.dlext`

  * `dense` is deprecated in favor of `full` ([#4759]).

  * The `Stat` type is renamed `StatStruct` ([#4670]).

  * `setrounding`, `rounding` and `setrounding` now take an additional
    argument specifying the floating point type to which they apply. The old
    behaviour and `[get/set/with]_bigfloat_rounding` functions are deprecated ([#5007]).

  * `cholpfact` and `qrpfact` are deprecated in favor of keyword arguments in
    `cholfact(..., pivot=true)` and `qrfact(..., pivot=true)` ([#5330]).

  * `symmetrize!` is deprecated in favor of `Base.LinAlg.copytri!` ([#5427]).

  * `myindexes` has been renamed to `localindexes` ([#5475]).

  * `factorize!` is deprecated in favor of `factorize` ([#5526]).

  * `nnz` counts the number of structural nonzeros in a sparse
    matrix. Use `countnz` for the actual number of nonzeros ([#6769]).

  * `setfield` is renamed `setfield!` ([#5748]).

  * `put` and `take` are renamed `put!` and `take!` ([#5511]).

  * `put!` now returns its first argument, the remote reference ([#5819]).

  * `read` methods that modify a passed array are now called `read!` ([#5970])

  * `infs` and `nans` are deprecated in favor of the more general `fill`.

  * `*` and `div` are no longer supported for `Char`.

  * `Range` is renamed `StepRange` and `Range1` is renamed `UnitRange`.
    `Ranges` is renamed `Range`.

  * `bitmix` is replaced by a 2-argument form of `hash`.

  * `readsfrom` and `writesto` are replaced by `open` ([#6948]).

  * `insert!` now throws a `BoundsError` if
    `index > length(collection)+1` ([#7373]).

  * No longer exported from `Base`:
    * `start_reading`, `stop_reading`, `start_watching` ([#10885]).

Julia v0.2.0 Release Notes
==========================

The 0.2 release brings improvements to many areas of Julia. Among the
most visible changes are support for 64-bit Windows, keyword arguments
to functions, immutable types, a redesigned and polished package
manager, a multimedia interface supporting usage of Julia in IPython,
a built-in profiler, and major improvements to Julia's linear algebra,
I/O, and parallel capabilities. These are accompanied by many other
changes adding new features, enhancing the library's consistency,
improving performance, increasing test coverage, easing installation,
and expanding the documentation. While not part of Julia proper, the
package ecosystem has also grown and matured considerably since the
0.1 release. See below for more information about the long list of
changes that improve Julia's usability and performance.

New language features
---------------------

  * Keyword & optional function arguments ([#485], [#1817]).

  * Immutable types ([#13]).

  * Triple-quoted string literals ([#70]).

  * New infix operator `in` (e.g. `x in S`), and corresponding function
    `in(x,S)`, replacing `contains(S,x)` function ([#2703]).

  * New variable bindings on each for loop and comprehension iteration ([#1571]).
    For example, before this change:

        julia> map(f->f(), { ()->i for i=1:3 })
        3-element Any Array:
         3
         3
         3

    and after:

        julia> map(f->f(), { ()->i for i=1:3 })
        3-element Any Array:
         1
         2
         3

  * Explicit relative importing ([#2375]).

  * Methods can be added to functions in other modules using dot syntax,
    as in `Foo.bar(x) = 0`.

  * `import module: name1, name2, ...` ([#5214]).

  * A semicolon is now allowed after an `import` or `using` statement ([#4130]).

  * In an interactive session (REPL), you can use `;cmd` to run `cmd` via an interactive
    shell. For example:

        julia> ;ls
        CONTRIBUTING.md  Makefile           VERSION      deps/      julia@  ui/
        DISTRIBUTING.md  NEWS.md            Windows.inc  doc/       src/    usr/
        LICENSE.md       README.md          base/        etc/       test/
        Make.inc         README.windows.md  contrib/     examples/  tmp/

New library functions
---------------------

  * Sampling profiler ([#2597]).

  * Functions for examining stages of the compiler's output:
    `code_lowered`, `code_typed`, `code_llvm`, and `code_native`.

  * Multimedia I/O API (display, writemime, etcetera) ([#3932]).

  * MPFR-based `BigFloat` ([#2814]), and many new `BigFloat` operations.

  * New half-precision IEEE floating-point type, `Float16` ([#3467]).

  * Support for setting floating-point rounding modes ([#3149]).

  * `methodswith` shows all methods with an argument of specific type.

  * `mapslices` provides a general way to perform operations on slices of arrays ([#2204]).

  * `repeat` function for constructing Arrays with repeated elements ([#3605]).

  * `Collections.PriorityQueue` type and `Collections.heap` functions ([#2920]).

  * `quadgk` 1d-integration routine ([#3140]).

  * `erfinv` and `erfcinv` functions ([#2987]).

  * `varm`, `stdm` ([#2265]).

  * `digamma`, `invdigamma`, `trigamma` and `polygamma` for calculating derivatives of `gamma` function ([#3233]).

  * `logdet` ([#3070]).

  * Names for C-compatible types: `Cchar`, `Clong`, etc. ([#2370]).

  * `cglobal` to access global variables ([#1815]).

  * `unsafe_pointer_to_objref` ([#2468]) and `pointer_from_objref` ([#2515]).

  * `readandwrite` for external processes.

  * I/O functions `readbytes` and `readbytes!` ([#3878]).

  * `flush_cstdio` function ([#3949]).

  * ClusterManager makes it possible to support different types of compute clusters
    ([#3649], [#4014]).

  * `rmprocs` for removing processors from a parallel computing session.
    The system can also tolerate to some extent processors that die unexpectedly
    ([#3050]).

  * `interrupt` for interrupting worker processes ([#3819]).

  * `timedwait` does a polled wait for an event till a specified timeout.

  * `Condition` type with `wait` and `notify` functions for `Task` synchronization.

  * `versioninfo` provides detailed version information, especially useful when
    reporting and diagnosing bugs.

  * `detach` for running child processes in a separate process group.

  * `setenv` for passing environment variables to child processes.

  * `ifelse` eagerly-evaluated conditional function, especially useful for
    vectorized conditionals.

Library improvements
--------------------

  * `isequal` now returns `false` for numbers of different types.
    This makes it much easier to define hashing for new numeric types.
    Uses of `Dict` with numeric keys might need to change
    to account for this increased strictness.

  * A redesigned and rewritten `Pkg` system is much more robust in case of problems.
    The basic interface to adding and removing package requirements remains the
    same, but great deal of additional functionality for developing packages in-place
    was added. See the new [packages chapter] in the manual for further details.

  * Sorting API updates ([#3665]) – see [sorting functions].

  * The `delete!(d::Dict, key)` function has been split into separate `pop!`
    and `delete!` functions ([#3439]).
    `pop!(d,key)` removes `key` from `d` and returns the value that was associated with it;
    it throws an exception if `d` does not contain `key`.
    `delete!(d,key)` removes `key` from `d` and succeeds regardless of whether `d`
    contained `key` or not, returning `d` itself in either case.

  * Linear-algebra factorization routines (`lu`, `chol`, etc.) now return
    `Factorization` objects (and `lud`, `chold`, etc. are deprecated; [#2212]).

  * A number of improvements to sparse matrix capabilities and sparse linear algebra.

  * More linear algebra fixes and eigensolver hooks
    for `SymTridiagonal`, `Tridiagonal` and `Bidiagonal` matrix types
    ([#2606], [#2608], [#2609], [#2611], [#2678], [#2713], [#2720], [#2725]).

  * Change `integer_valued`, `real_valued`, and so on to `isinteger`, `isreal`,
    and so on, and semantics of the later are now value-based rather than type-based,
    unlike MATLAB/Octave ([#3071]). `isbool` and `iscomplex` are eliminated in favor
    of a general `iseltype` function.

  * Transitive comparison of floats with rationals ([#3102]).

  * Fast prime generation with `primes` and fast primality testing with `isprime`.

  * `sum` and `cumsum` now use [pairwise summation] for better accuracy ([#4039]).

  * Dot operators (`.+`, `.*` etc.) now broadcast singleton dimensions of array arguments.
    This behavior can be applied to any function using `broadcast(f, ...)`.

  * `combinations`, `permutations`, and `partitions` now return iterators instead of a task,
    and `integer_partitions` has been renamed to `partitions` ([#3989], [#4055]).

  * `isreadable`/`iswritable` methods added for more IO types ([#3872]).

  * Much faster and improved `readdlm` and `writedlm` ([#3350], [#3468], [#3483]).

  * Faster `matchall` ([#3719]), and various string and regex improvements.

  * Documentation of advanced linear algebra features ([#2807]).

  * Support optional RTLD flags in `dlopen` ([#2380]).

  * `pmap` now works with any iterable collection.

  * Options in `pmap` for retrying or ignoring failed tasks.

  * New `sinpi(x)` and `cospi(x)` functions to compute sine and cosine of `pi*x`
    more accurately ([#4112]).

  * New implementations of elementary complex functions
    `sqrt`, `log`, `asin`, `acos`, `atan`, `tanh`, `asinh`, `acosh`, `atanh`
    with correct branch cuts ([#2891]).

  * Improved behavior of `SubArray` ([#4412], [#4284], [#4044], [#3697], [#3790],
    [#3148], [#2844], [#2644] and various other fixes).

  * New convenience functions in graphics API.

  * Improved backtraces on Windows and OS X.

  * Implementation of reduction functions (including `reduce`, `mapreduce`, `sum`, `prod`,
    `maximum`, `minimum`, `all`, and `any`) are refactored, with improved type stability,
    efficiency, and consistency ([#6116], [#7035], [#7061], [#7106]).

Deprecated or removed
---------------------

  * Methods of `min` and `max` that do reductions were renamed to
    `minimum` and `maximum`. `min(x)` is now `minimum(x)`, and
    `min(x,(),dim)` is now `minimum(x,dim)` ([#4235]).

  * `ComplexPair` was renamed to `Complex` and made `immutable`,
    and `Complex128` and so on are now aliases to the new `Complex` type.

  * `!` was added to the name of many mutating functions,
    e.g., `push` was renamed `push!` ([#907]).

  * `ref` renamed to `getindex`, and `assign` to `setindex!` ([#1484]).

  * `writeable` renamed to `writable` ([#3874]).

  * `logb` and `ilogb` renamed to `exponent` ([#2516]).

  * `quote_string` became a method of `repr`.

  * `safe_char`, `check_ascii`, and `check_utf8` replaced by
    `is_valid_char`, `is_valid_ascii`, and `is_valid_utf8`, respectively.

  * `each_line`, `each_match`, `begins_with`, `ends_with`, `parse_float`,
    `parse_int`, and `seek_end` replaced by: `eachline`, `eachmatch`, and so on
    (`_` was removed) ([#1539]).

  * `parse_bin(s)` replaced by `parseint(s,2)`;
    `parse_oct(s)` replaced by `parseint(s,8)`;
    `parse_hex(s)` replaced by `parseint(s,16)`.

  * `findn_nzs` replaced by `findnz` ([#1539]).

  * `DivideByZeroError` replaced by `DivideError`.

  * `addprocs_ssh`, `addprocs_ssh_tunnel`, and `addprocs_local`
    replaced by `addprocs` (with keyword options).

  * `remote_call`, `remote_call_fetch`, and `remote_call_wait`
    replaced by `remotecall`, `remotecall_fetch`, and `remotecall_wait`.

  * `has` replaced by `in` for sets and by `haskey` for dictionaries.

  * `diagmm` and `diagmm!` replaced by `scale` and `scale!` ([#2916]).

  * `unsafe_ref` and `unsafe_assign` replaced by `unsafe_load` and `unsafe_store!`.

  * `add_each!` and `del_each!` replaced by `union!` and `setdiff!`.

  * `isdenormal` renamed to `issubnormal` ([#3105]).

  * `expr` replaced by direct call to `Expr` constructor.

  * `|`, `&`, `$`, `-`, and `~` for sets replaced by
    `union`, `intersect`, `symdiff`, `setdiff`, and `complement` ([#3272]).

  * `square` function removed.

  * `pascal` function removed.

  * `add` and `add!` for `Set` replaced by `push!`.

  * `ls` function deprecated in favor of `readdir` or `;ls` in the REPL.

  * `start_timer` now expects arguments in units of seconds, not milliseconds.

  * Shell redirection operators `|`, `>`, and `<` eliminated in favor of a new
    operator `|>` ([#3523]).

  * `amap` is deprecated in favor of new `mapslices` functionality.

  * The `Reverse` iterator was removed since it did not work in many cases.

  * The `gcd` function now returns a non-negative value regardless of
    the argument signs, and various other sign problems with `invmod`,
    `lcm`, `gcdx`, and `powermod` were fixed ([#4811]).

Miscellaneous changes
---------------------

  * `julia-release-*` executables renamed to `julia-*`,
    and `libjulia-release` renamed to `libjulia` ([#4177]).

  * Packages will now be installed in `.julia/vX.Y`, where
    X.Y is the current Julia version.

Bugfixes and performance updates
--------------------------------

Too numerous to mention.

[packages chapter]: https://docs.julialang.org/en/latest/manual/packages/
[sorting functions]: https://docs.julialang.org/en/latest/stdlib/sort/
[pairwise summation]: https://en.wikipedia.org/wiki/Pairwise_summation
[a448e080]: https://github.com/JuliaLang/julia/commit/a448e080dc736c7fb326426dfcb2528be36973d3
[5e3f074b]: https://github.com/JuliaLang/julia/commit/5e3f074b9173044a0a4219f9b285879ff7cec041
<!--- generated by NEWS-update.jl: -->
[#13]: https://github.com/JuliaLang/julia/issues/13
[#69]: https://github.com/JuliaLang/julia/issues/69
[#70]: https://github.com/JuliaLang/julia/issues/70
[#101]: https://github.com/JuliaLang/julia/issues/101
[#265]: https://github.com/JuliaLang/julia/issues/265
[#485]: https://github.com/JuliaLang/julia/issues/485
[#550]: https://github.com/JuliaLang/julia/issues/550
[#552]: https://github.com/JuliaLang/julia/issues/552
[#907]: https://github.com/JuliaLang/julia/issues/907
[#964]: https://github.com/JuliaLang/julia/issues/964
[#987]: https://github.com/JuliaLang/julia/issues/987
[#1090]: https://github.com/JuliaLang/julia/issues/1090
[#1195]: https://github.com/JuliaLang/julia/issues/1195
[#1268]: https://github.com/JuliaLang/julia/issues/1268
[#1470]: https://github.com/JuliaLang/julia/issues/1470
[#1484]: https://github.com/JuliaLang/julia/issues/1484
[#1539]: https://github.com/JuliaLang/julia/issues/1539
[#1571]: https://github.com/JuliaLang/julia/issues/1571
[#1815]: https://github.com/JuliaLang/julia/issues/1815
[#1817]: https://github.com/JuliaLang/julia/issues/1817
[#2204]: https://github.com/JuliaLang/julia/issues/2204
[#2212]: https://github.com/JuliaLang/julia/issues/2212
[#2265]: https://github.com/JuliaLang/julia/issues/2265
[#2333]: https://github.com/JuliaLang/julia/issues/2333
[#2345]: https://github.com/JuliaLang/julia/issues/2345
[#2370]: https://github.com/JuliaLang/julia/issues/2370
[#2375]: https://github.com/JuliaLang/julia/issues/2375
[#2380]: https://github.com/JuliaLang/julia/issues/2380
[#2403]: https://github.com/JuliaLang/julia/issues/2403
[#2468]: https://github.com/JuliaLang/julia/issues/2468
[#2488]: https://github.com/JuliaLang/julia/issues/2488
[#2515]: https://github.com/JuliaLang/julia/issues/2515
[#2516]: https://github.com/JuliaLang/julia/issues/2516
[#2597]: https://github.com/JuliaLang/julia/issues/2597
[#2606]: https://github.com/JuliaLang/julia/issues/2606
[#2608]: https://github.com/JuliaLang/julia/issues/2608
[#2609]: https://github.com/JuliaLang/julia/issues/2609
[#2611]: https://github.com/JuliaLang/julia/issues/2611
[#2644]: https://github.com/JuliaLang/julia/issues/2644
[#2678]: https://github.com/JuliaLang/julia/issues/2678
[#2703]: https://github.com/JuliaLang/julia/issues/2703
[#2713]: https://github.com/JuliaLang/julia/issues/2713
[#2720]: https://github.com/JuliaLang/julia/issues/2720
[#2725]: https://github.com/JuliaLang/julia/issues/2725
[#2807]: https://github.com/JuliaLang/julia/issues/2807
[#2814]: https://github.com/JuliaLang/julia/issues/2814
[#2844]: https://github.com/JuliaLang/julia/issues/2844
[#2891]: https://github.com/JuliaLang/julia/issues/2891
[#2916]: https://github.com/JuliaLang/julia/issues/2916
[#2920]: https://github.com/JuliaLang/julia/issues/2920
[#2987]: https://github.com/JuliaLang/julia/issues/2987
[#3050]: https://github.com/JuliaLang/julia/issues/3050
[#3070]: https://github.com/JuliaLang/julia/issues/3070
[#3071]: https://github.com/JuliaLang/julia/issues/3071
[#3102]: https://github.com/JuliaLang/julia/issues/3102
[#3105]: https://github.com/JuliaLang/julia/issues/3105
[#3140]: https://github.com/JuliaLang/julia/issues/3140
[#3141]: https://github.com/JuliaLang/julia/issues/3141
[#3148]: https://github.com/JuliaLang/julia/issues/3148
[#3149]: https://github.com/JuliaLang/julia/issues/3149
[#3214]: https://github.com/JuliaLang/julia/issues/3214
[#3233]: https://github.com/JuliaLang/julia/issues/3233
[#3272]: https://github.com/JuliaLang/julia/issues/3272
[#3344]: https://github.com/JuliaLang/julia/issues/3344
[#3350]: https://github.com/JuliaLang/julia/issues/3350
[#3434]: https://github.com/JuliaLang/julia/issues/3434
[#3439]: https://github.com/JuliaLang/julia/issues/3439
[#3467]: https://github.com/JuliaLang/julia/issues/3467
[#3468]: https://github.com/JuliaLang/julia/issues/3468
[#3483]: https://github.com/JuliaLang/julia/issues/3483
[#3523]: https://github.com/JuliaLang/julia/issues/3523
[#3605]: https://github.com/JuliaLang/julia/issues/3605
[#3631]: https://github.com/JuliaLang/julia/issues/3631
[#3649]: https://github.com/JuliaLang/julia/issues/3649
[#3665]: https://github.com/JuliaLang/julia/issues/3665
[#3688]: https://github.com/JuliaLang/julia/issues/3688
[#3697]: https://github.com/JuliaLang/julia/issues/3697
[#3719]: https://github.com/JuliaLang/julia/issues/3719
[#3721]: https://github.com/JuliaLang/julia/issues/3721
[#3737]: https://github.com/JuliaLang/julia/issues/3737
[#3759]: https://github.com/JuliaLang/julia/issues/3759
[#3790]: https://github.com/JuliaLang/julia/issues/3790
[#3819]: https://github.com/JuliaLang/julia/issues/3819
[#3872]: https://github.com/JuliaLang/julia/issues/3872
[#3874]: https://github.com/JuliaLang/julia/issues/3874
[#3878]: https://github.com/JuliaLang/julia/issues/3878
[#3932]: https://github.com/JuliaLang/julia/issues/3932
[#3949]: https://github.com/JuliaLang/julia/issues/3949
[#3989]: https://github.com/JuliaLang/julia/issues/3989
[#4014]: https://github.com/JuliaLang/julia/issues/4014
[#4026]: https://github.com/JuliaLang/julia/issues/4026
[#4039]: https://github.com/JuliaLang/julia/issues/4039
[#4042]: https://github.com/JuliaLang/julia/issues/4042
[#4044]: https://github.com/JuliaLang/julia/issues/4044
[#4048]: https://github.com/JuliaLang/julia/issues/4048
[#4055]: https://github.com/JuliaLang/julia/issues/4055
[#4112]: https://github.com/JuliaLang/julia/issues/4112
[#4130]: https://github.com/JuliaLang/julia/issues/4130
[#4163]: https://github.com/JuliaLang/julia/issues/4163
[#4177]: https://github.com/JuliaLang/julia/issues/4177
[#4211]: https://github.com/JuliaLang/julia/issues/4211
[#4235]: https://github.com/JuliaLang/julia/issues/4235
[#4284]: https://github.com/JuliaLang/julia/issues/4284
[#4383]: https://github.com/JuliaLang/julia/issues/4383
[#4412]: https://github.com/JuliaLang/julia/issues/4412
[#4470]: https://github.com/JuliaLang/julia/issues/4470
[#4615]: https://github.com/JuliaLang/julia/issues/4615
[#4670]: https://github.com/JuliaLang/julia/issues/4670
[#4759]: https://github.com/JuliaLang/julia/issues/4759
[#4766]: https://github.com/JuliaLang/julia/issues/4766
[#4775]: https://github.com/JuliaLang/julia/issues/4775
[#4782]: https://github.com/JuliaLang/julia/issues/4782
[#4799]: https://github.com/JuliaLang/julia/issues/4799
[#4806]: https://github.com/JuliaLang/julia/issues/4806
[#4811]: https://github.com/JuliaLang/julia/issues/4811
[#4819]: https://github.com/JuliaLang/julia/issues/4819
[#4862]: https://github.com/JuliaLang/julia/issues/4862
[#4867]: https://github.com/JuliaLang/julia/issues/4867
[#4870]: https://github.com/JuliaLang/julia/issues/4870
[#4871]: https://github.com/JuliaLang/julia/issues/4871
[#4874]: https://github.com/JuliaLang/julia/issues/4874
[#4882]: https://github.com/JuliaLang/julia/issues/4882
[#4888]: https://github.com/JuliaLang/julia/issues/4888
[#4923]: https://github.com/JuliaLang/julia/issues/4923
[#4930]: https://github.com/JuliaLang/julia/issues/4930
[#4934]: https://github.com/JuliaLang/julia/issues/4934
[#4943]: https://github.com/JuliaLang/julia/issues/4943
[#4958]: https://github.com/JuliaLang/julia/issues/4958
[#4967]: https://github.com/JuliaLang/julia/issues/4967
[#4996]: https://github.com/JuliaLang/julia/issues/4996
[#5007]: https://github.com/JuliaLang/julia/issues/5007
[#5025]: https://github.com/JuliaLang/julia/issues/5025
[#5059]: https://github.com/JuliaLang/julia/issues/5059
[#5076]: https://github.com/JuliaLang/julia/issues/5076
[#5154]: https://github.com/JuliaLang/julia/issues/5154
[#5164]: https://github.com/JuliaLang/julia/issues/5164
[#5196]: https://github.com/JuliaLang/julia/issues/5196
[#5214]: https://github.com/JuliaLang/julia/issues/5214
[#5227]: https://github.com/JuliaLang/julia/issues/5227
[#5255]: https://github.com/JuliaLang/julia/issues/5255
[#5263]: https://github.com/JuliaLang/julia/issues/5263
[#5275]: https://github.com/JuliaLang/julia/issues/5275
[#5277]: https://github.com/JuliaLang/julia/issues/5277
[#5330]: https://github.com/JuliaLang/julia/issues/5330
[#5349]: https://github.com/JuliaLang/julia/issues/5349
[#5358]: https://github.com/JuliaLang/julia/issues/5358
[#5380]: https://github.com/JuliaLang/julia/issues/5380
[#5381]: https://github.com/JuliaLang/julia/issues/5381
[#5387]: https://github.com/JuliaLang/julia/issues/5387
[#5403]: https://github.com/JuliaLang/julia/issues/5403
[#5413]: https://github.com/JuliaLang/julia/issues/5413
[#5423]: https://github.com/JuliaLang/julia/issues/5423
[#5427]: https://github.com/JuliaLang/julia/issues/5427
[#5428]: https://github.com/JuliaLang/julia/issues/5428
[#5430]: https://github.com/JuliaLang/julia/issues/5430
[#5462]: https://github.com/JuliaLang/julia/issues/5462
[#5464]: https://github.com/JuliaLang/julia/issues/5464
[#5468]: https://github.com/JuliaLang/julia/issues/5468
[#5475]: https://github.com/JuliaLang/julia/issues/5475
[#5511]: https://github.com/JuliaLang/julia/issues/5511
[#5526]: https://github.com/JuliaLang/julia/issues/5526
[#5545]: https://github.com/JuliaLang/julia/issues/5545
[#5576]: https://github.com/JuliaLang/julia/issues/5576
[#5585]: https://github.com/JuliaLang/julia/issues/5585
[#5636]: https://github.com/JuliaLang/julia/issues/5636
[#5671]: https://github.com/JuliaLang/julia/issues/5671
[#5677]: https://github.com/JuliaLang/julia/issues/5677
[#5703]: https://github.com/JuliaLang/julia/issues/5703
[#5704]: https://github.com/JuliaLang/julia/issues/5704
[#5726]: https://github.com/JuliaLang/julia/issues/5726
[#5737]: https://github.com/JuliaLang/julia/issues/5737
[#5748]: https://github.com/JuliaLang/julia/issues/5748
[#5776]: https://github.com/JuliaLang/julia/issues/5776
[#5778]: https://github.com/JuliaLang/julia/issues/5778
[#5810]: https://github.com/JuliaLang/julia/issues/5810
[#5811]: https://github.com/JuliaLang/julia/issues/5811
[#5819]: https://github.com/JuliaLang/julia/issues/5819
[#5827]: https://github.com/JuliaLang/julia/issues/5827
[#5832]: https://github.com/JuliaLang/julia/issues/5832
[#5927]: https://github.com/JuliaLang/julia/issues/5927
[#5936]: https://github.com/JuliaLang/julia/issues/5936
[#5939]: https://github.com/JuliaLang/julia/issues/5939
[#5970]: https://github.com/JuliaLang/julia/issues/5970
[#6056]: https://github.com/JuliaLang/julia/issues/6056
[#6057]: https://github.com/JuliaLang/julia/issues/6057
[#6073]: https://github.com/JuliaLang/julia/issues/6073
[#6081]: https://github.com/JuliaLang/julia/issues/6081
[#6116]: https://github.com/JuliaLang/julia/issues/6116
[#6128]: https://github.com/JuliaLang/julia/issues/6128
[#6169]: https://github.com/JuliaLang/julia/issues/6169
[#6179]: https://github.com/JuliaLang/julia/issues/6179
[#6190]: https://github.com/JuliaLang/julia/issues/6190
[#6197]: https://github.com/JuliaLang/julia/issues/6197
[#6211]: https://github.com/JuliaLang/julia/issues/6211
[#6212]: https://github.com/JuliaLang/julia/issues/6212
[#6270]: https://github.com/JuliaLang/julia/issues/6270
[#6273]: https://github.com/JuliaLang/julia/issues/6273
[#6582]: https://github.com/JuliaLang/julia/issues/6582
[#6624]: https://github.com/JuliaLang/julia/issues/6624
[#6652]: https://github.com/JuliaLang/julia/issues/6652
[#6678]: https://github.com/JuliaLang/julia/issues/6678
[#6716]: https://github.com/JuliaLang/julia/issues/6716
[#6726]: https://github.com/JuliaLang/julia/issues/6726
[#6739]: https://github.com/JuliaLang/julia/issues/6739
[#6769]: https://github.com/JuliaLang/julia/issues/6769
[#6773]: https://github.com/JuliaLang/julia/issues/6773
[#6842]: https://github.com/JuliaLang/julia/issues/6842
[#6911]: https://github.com/JuliaLang/julia/issues/6911
[#6920]: https://github.com/JuliaLang/julia/issues/6920
[#6929]: https://github.com/JuliaLang/julia/issues/6929
[#6948]: https://github.com/JuliaLang/julia/issues/6948
[#7016]: https://github.com/JuliaLang/julia/issues/7016
[#7035]: https://github.com/JuliaLang/julia/issues/7035
[#7047]: https://github.com/JuliaLang/julia/issues/7047
[#7061]: https://github.com/JuliaLang/julia/issues/7061
[#7064]: https://github.com/JuliaLang/julia/issues/7064
[#7071]: https://github.com/JuliaLang/julia/issues/7071
[#7094]: https://github.com/JuliaLang/julia/issues/7094
[#7106]: https://github.com/JuliaLang/julia/issues/7106
[#7125]: https://github.com/JuliaLang/julia/issues/7125
[#7131]: https://github.com/JuliaLang/julia/issues/7131
[#7146]: https://github.com/JuliaLang/julia/issues/7146
[#7234]: https://github.com/JuliaLang/julia/issues/7234
[#7236]: https://github.com/JuliaLang/julia/issues/7236
[#7242]: https://github.com/JuliaLang/julia/issues/7242
[#7258]: https://github.com/JuliaLang/julia/issues/7258
[#7264]: https://github.com/JuliaLang/julia/issues/7264
[#7311]: https://github.com/JuliaLang/julia/issues/7311
[#7359]: https://github.com/JuliaLang/julia/issues/7359
[#7365]: https://github.com/JuliaLang/julia/issues/7365
[#7373]: https://github.com/JuliaLang/julia/issues/7373
[#7390]: https://github.com/JuliaLang/julia/issues/7390
[#7435]: https://github.com/JuliaLang/julia/issues/7435
[#7464]: https://github.com/JuliaLang/julia/issues/7464
[#7513]: https://github.com/JuliaLang/julia/issues/7513
[#7616]: https://github.com/JuliaLang/julia/issues/7616
[#7647]: https://github.com/JuliaLang/julia/issues/7647
[#7654]: https://github.com/JuliaLang/julia/issues/7654
[#7669]: https://github.com/JuliaLang/julia/issues/7669
[#7704]: https://github.com/JuliaLang/julia/issues/7704
[#7917]: https://github.com/JuliaLang/julia/issues/7917
[#7992]: https://github.com/JuliaLang/julia/issues/7992
[#8011]: https://github.com/JuliaLang/julia/issues/8011
[#8036]: https://github.com/JuliaLang/julia/issues/8036
[#8089]: https://github.com/JuliaLang/julia/issues/8089
[#8113]: https://github.com/JuliaLang/julia/issues/8113
[#8135]: https://github.com/JuliaLang/julia/issues/8135
[#8152]: https://github.com/JuliaLang/julia/issues/8152
[#8246]: https://github.com/JuliaLang/julia/issues/8246
[#8283]: https://github.com/JuliaLang/julia/issues/8283
[#8297]: https://github.com/JuliaLang/julia/issues/8297
[#8320]: https://github.com/JuliaLang/julia/issues/8320
[#8399]: https://github.com/JuliaLang/julia/issues/8399
[#8423]: https://github.com/JuliaLang/julia/issues/8423
[#8432]: https://github.com/JuliaLang/julia/issues/8432
[#8467]: https://github.com/JuliaLang/julia/issues/8467
[#8501]: https://github.com/JuliaLang/julia/issues/8501
[#8560]: https://github.com/JuliaLang/julia/issues/8560
[#8578]: https://github.com/JuliaLang/julia/issues/8578
[#8599]: https://github.com/JuliaLang/julia/issues/8599
[#8605]: https://github.com/JuliaLang/julia/issues/8605
[#8624]: https://github.com/JuliaLang/julia/issues/8624
[#8660]: https://github.com/JuliaLang/julia/issues/8660
[#8672]: https://github.com/JuliaLang/julia/issues/8672
[#8712]: https://github.com/JuliaLang/julia/issues/8712
[#8734]: https://github.com/JuliaLang/julia/issues/8734
[#8745]: https://github.com/JuliaLang/julia/issues/8745
[#8750]: https://github.com/JuliaLang/julia/issues/8750
[#8776]: https://github.com/JuliaLang/julia/issues/8776
[#8791]: https://github.com/JuliaLang/julia/issues/8791
[#8792]: https://github.com/JuliaLang/julia/issues/8792
[#8808]: https://github.com/JuliaLang/julia/issues/8808
[#8814]: https://github.com/JuliaLang/julia/issues/8814
[#8816]: https://github.com/JuliaLang/julia/issues/8816
[#8827]: https://github.com/JuliaLang/julia/issues/8827
[#8832]: https://github.com/JuliaLang/julia/issues/8832
[#8845]: https://github.com/JuliaLang/julia/issues/8845
[#8846]: https://github.com/JuliaLang/julia/issues/8846
[#8854]: https://github.com/JuliaLang/julia/issues/8854
[#8867]: https://github.com/JuliaLang/julia/issues/8867
[#8872]: https://github.com/JuliaLang/julia/issues/8872
[#8897]: https://github.com/JuliaLang/julia/issues/8897
[#8905]: https://github.com/JuliaLang/julia/issues/8905
[#8941]: https://github.com/JuliaLang/julia/issues/8941
[#8958]: https://github.com/JuliaLang/julia/issues/8958
[#8974]: https://github.com/JuliaLang/julia/issues/8974
[#9017]: https://github.com/JuliaLang/julia/issues/9017
[#9049]: https://github.com/JuliaLang/julia/issues/9049
[#9065]: https://github.com/JuliaLang/julia/issues/9065
[#9083]: https://github.com/JuliaLang/julia/issues/9083
[#9105]: https://github.com/JuliaLang/julia/issues/9105
[#9122]: https://github.com/JuliaLang/julia/issues/9122
[#9126]: https://github.com/JuliaLang/julia/issues/9126
[#9132]: https://github.com/JuliaLang/julia/issues/9132
[#9133]: https://github.com/JuliaLang/julia/issues/9133
[#9144]: https://github.com/JuliaLang/julia/issues/9144
[#9198]: https://github.com/JuliaLang/julia/issues/9198
[#9249]: https://github.com/JuliaLang/julia/issues/9249
[#9261]: https://github.com/JuliaLang/julia/issues/9261
[#9271]: https://github.com/JuliaLang/julia/issues/9271
[#9294]: https://github.com/JuliaLang/julia/issues/9294
[#9309]: https://github.com/JuliaLang/julia/issues/9309
[#9343]: https://github.com/JuliaLang/julia/issues/9343
[#9418]: https://github.com/JuliaLang/julia/issues/9418
[#9425]: https://github.com/JuliaLang/julia/issues/9425
[#9434]: https://github.com/JuliaLang/julia/issues/9434
[#9452]: https://github.com/JuliaLang/julia/issues/9452
[#9482]: https://github.com/JuliaLang/julia/issues/9482
[#9487]: https://github.com/JuliaLang/julia/issues/9487
[#9503]: https://github.com/JuliaLang/julia/issues/9503
[#9569]: https://github.com/JuliaLang/julia/issues/9569
[#9575]: https://github.com/JuliaLang/julia/issues/9575
[#9578]: https://github.com/JuliaLang/julia/issues/9578
[#9597]: https://github.com/JuliaLang/julia/issues/9597
[#9627]: https://github.com/JuliaLang/julia/issues/9627
[#9666]: https://github.com/JuliaLang/julia/issues/9666
[#9690]: https://github.com/JuliaLang/julia/issues/9690
[#9701]: https://github.com/JuliaLang/julia/issues/9701
[#9714]: https://github.com/JuliaLang/julia/issues/9714
[#9734]: https://github.com/JuliaLang/julia/issues/9734
[#9745]: https://github.com/JuliaLang/julia/issues/9745
[#9779]: https://github.com/JuliaLang/julia/issues/9779
[#9862]: https://github.com/JuliaLang/julia/issues/9862
[#9957]: https://github.com/JuliaLang/julia/issues/9957
[#10008]: https://github.com/JuliaLang/julia/issues/10008
[#10024]: https://github.com/JuliaLang/julia/issues/10024
[#10031]: https://github.com/JuliaLang/julia/issues/10031
[#10075]: https://github.com/JuliaLang/julia/issues/10075
[#10117]: https://github.com/JuliaLang/julia/issues/10117
[#10150]: https://github.com/JuliaLang/julia/issues/10150
[#10168]: https://github.com/JuliaLang/julia/issues/10168
[#10180]: https://github.com/JuliaLang/julia/issues/10180
[#10228]: https://github.com/JuliaLang/julia/issues/10228
[#10328]: https://github.com/JuliaLang/julia/issues/10328
[#10331]: https://github.com/JuliaLang/julia/issues/10331
[#10332]: https://github.com/JuliaLang/julia/issues/10332
[#10333]: https://github.com/JuliaLang/julia/issues/10333
[#10380]: https://github.com/JuliaLang/julia/issues/10380
[#10400]: https://github.com/JuliaLang/julia/issues/10400
[#10446]: https://github.com/JuliaLang/julia/issues/10446
[#10458]: https://github.com/JuliaLang/julia/issues/10458
[#10472]: https://github.com/JuliaLang/julia/issues/10472
[#10525]: https://github.com/JuliaLang/julia/issues/10525
[#10543]: https://github.com/JuliaLang/julia/issues/10543
[#10548]: https://github.com/JuliaLang/julia/issues/10548
[#10659]: https://github.com/JuliaLang/julia/issues/10659
[#10679]: https://github.com/JuliaLang/julia/issues/10679
[#10709]: https://github.com/JuliaLang/julia/issues/10709
[#10714]: https://github.com/JuliaLang/julia/issues/10714
[#10747]: https://github.com/JuliaLang/julia/issues/10747
[#10844]: https://github.com/JuliaLang/julia/issues/10844
[#10870]: https://github.com/JuliaLang/julia/issues/10870
[#10885]: https://github.com/JuliaLang/julia/issues/10885
[#10888]: https://github.com/JuliaLang/julia/issues/10888
[#10893]: https://github.com/JuliaLang/julia/issues/10893
[#10914]: https://github.com/JuliaLang/julia/issues/10914
[#10946]: https://github.com/JuliaLang/julia/issues/10946
[#10955]: https://github.com/JuliaLang/julia/issues/10955
[#10994]: https://github.com/JuliaLang/julia/issues/10994
[#11030]: https://github.com/JuliaLang/julia/issues/11030
[#11067]: https://github.com/JuliaLang/julia/issues/11067
[#11105]: https://github.com/JuliaLang/julia/issues/11105
[#11145]: https://github.com/JuliaLang/julia/issues/11145
[#11171]: https://github.com/JuliaLang/julia/issues/11171
[#11196]: https://github.com/JuliaLang/julia/issues/11196
[#11241]: https://github.com/JuliaLang/julia/issues/11241
[#11242]: https://github.com/JuliaLang/julia/issues/11242
[#11250]: https://github.com/JuliaLang/julia/issues/11250
[#11279]: https://github.com/JuliaLang/julia/issues/11279
[#11310]: https://github.com/JuliaLang/julia/issues/11310
[#11347]: https://github.com/JuliaLang/julia/issues/11347
[#11379]: https://github.com/JuliaLang/julia/issues/11379
[#11432]: https://github.com/JuliaLang/julia/issues/11432
[#11566]: https://github.com/JuliaLang/julia/issues/11566
[#11686]: https://github.com/JuliaLang/julia/issues/11686
[#11688]: https://github.com/JuliaLang/julia/issues/11688
[#11741]: https://github.com/JuliaLang/julia/issues/11741
[#11849]: https://github.com/JuliaLang/julia/issues/11849
[#11891]: https://github.com/JuliaLang/julia/issues/11891
[#11922]: https://github.com/JuliaLang/julia/issues/11922
[#11947]: https://github.com/JuliaLang/julia/issues/11947
[#11985]: https://github.com/JuliaLang/julia/issues/11985
[#12025]: https://github.com/JuliaLang/julia/issues/12025
[#12031]: https://github.com/JuliaLang/julia/issues/12031
[#12034]: https://github.com/JuliaLang/julia/issues/12034
[#12087]: https://github.com/JuliaLang/julia/issues/12087
[#12137]: https://github.com/JuliaLang/julia/issues/12137
[#12162]: https://github.com/JuliaLang/julia/issues/12162
[#12231]: https://github.com/JuliaLang/julia/issues/12231
[#12264]: https://github.com/JuliaLang/julia/issues/12264
[#12274]: https://github.com/JuliaLang/julia/issues/12274
[#12385]: https://github.com/JuliaLang/julia/issues/12385
[#12393]: https://github.com/JuliaLang/julia/issues/12393
[#12458]: https://github.com/JuliaLang/julia/issues/12458
[#12472]: https://github.com/JuliaLang/julia/issues/12472
[#12491]: https://github.com/JuliaLang/julia/issues/12491
[#12563]: https://github.com/JuliaLang/julia/issues/12563
[#12576]: https://github.com/JuliaLang/julia/issues/12576
[#12727]: https://github.com/JuliaLang/julia/issues/12727
[#12739]: https://github.com/JuliaLang/julia/issues/12739
[#12819]: https://github.com/JuliaLang/julia/issues/12819
[#12872]: https://github.com/JuliaLang/julia/issues/12872
[#13062]: https://github.com/JuliaLang/julia/issues/13062
[#13171]: https://github.com/JuliaLang/julia/issues/13171
[#13232]: https://github.com/JuliaLang/julia/issues/13232
[#13338]: https://github.com/JuliaLang/julia/issues/13338
[#13387]: https://github.com/JuliaLang/julia/issues/13387
[#13412]: https://github.com/JuliaLang/julia/issues/13412
[#13440]: https://github.com/JuliaLang/julia/issues/13440
[#13465]: https://github.com/JuliaLang/julia/issues/13465
[#13480]: https://github.com/JuliaLang/julia/issues/13480
[#13496]: https://github.com/JuliaLang/julia/issues/13496
[#13542]: https://github.com/JuliaLang/julia/issues/13542
[#13612]: https://github.com/JuliaLang/julia/issues/13612
[#13680]: https://github.com/JuliaLang/julia/issues/13680
[#13681]: https://github.com/JuliaLang/julia/issues/13681
[#13707]: https://github.com/JuliaLang/julia/issues/13707
[#13774]: https://github.com/JuliaLang/julia/issues/13774
[#13780]: https://github.com/JuliaLang/julia/issues/13780
[#13824]: https://github.com/JuliaLang/julia/issues/13824
[#13825]: https://github.com/JuliaLang/julia/issues/13825
[#13897]: https://github.com/JuliaLang/julia/issues/13897
[#14052]: https://github.com/JuliaLang/julia/issues/14052
[#14114]: https://github.com/JuliaLang/julia/issues/14114
[#14140]: https://github.com/JuliaLang/julia/issues/14140
[#14194]: https://github.com/JuliaLang/julia/issues/14194
[#14243]: https://github.com/JuliaLang/julia/issues/14243
[#14335]: https://github.com/JuliaLang/julia/issues/14335
[#14413]: https://github.com/JuliaLang/julia/issues/14413
[#14424]: https://github.com/JuliaLang/julia/issues/14424
[#14458]: https://github.com/JuliaLang/julia/issues/14458
[#14469]: https://github.com/JuliaLang/julia/issues/14469
[#14474]: https://github.com/JuliaLang/julia/issues/14474
[#14519]: https://github.com/JuliaLang/julia/issues/14519
[#14608]: https://github.com/JuliaLang/julia/issues/14608
[#14623]: https://github.com/JuliaLang/julia/issues/14623
[#14660]: https://github.com/JuliaLang/julia/issues/14660
[#14676]: https://github.com/JuliaLang/julia/issues/14676
[#14759]: https://github.com/JuliaLang/julia/issues/14759
[#14777]: https://github.com/JuliaLang/julia/issues/14777
[#14798]: https://github.com/JuliaLang/julia/issues/14798
[#15007]: https://github.com/JuliaLang/julia/issues/15007
[#15032]: https://github.com/JuliaLang/julia/issues/15032
[#15172]: https://github.com/JuliaLang/julia/issues/15172
[#15192]: https://github.com/JuliaLang/julia/issues/15192
[#15242]: https://github.com/JuliaLang/julia/issues/15242
[#15244]: https://github.com/JuliaLang/julia/issues/15244
[#15258]: https://github.com/JuliaLang/julia/issues/15258
[#15409]: https://github.com/JuliaLang/julia/issues/15409
[#15431]: https://github.com/JuliaLang/julia/issues/15431
[#15524]: https://github.com/JuliaLang/julia/issues/15524
[#15550]: https://github.com/JuliaLang/julia/issues/15550
[#15609]: https://github.com/JuliaLang/julia/issues/15609
[#15708]: https://github.com/JuliaLang/julia/issues/15708
[#15731]: https://github.com/JuliaLang/julia/issues/15731
[#15763]: https://github.com/JuliaLang/julia/issues/15763
[#15804]: https://github.com/JuliaLang/julia/issues/15804
[#15850]: https://github.com/JuliaLang/julia/issues/15850
[#15975]: https://github.com/JuliaLang/julia/issues/15975
[#16010]: https://github.com/JuliaLang/julia/issues/16010
[#16024]: https://github.com/JuliaLang/julia/issues/16024
[#16058]: https://github.com/JuliaLang/julia/issues/16058
[#16071]: https://github.com/JuliaLang/julia/issues/16071
[#16098]: https://github.com/JuliaLang/julia/issues/16098
[#16107]: https://github.com/JuliaLang/julia/issues/16107
[#16154]: https://github.com/JuliaLang/julia/issues/16154
[#16213]: https://github.com/JuliaLang/julia/issues/16213
[#16219]: https://github.com/JuliaLang/julia/issues/16219
[#16260]: https://github.com/JuliaLang/julia/issues/16260
[#16285]: https://github.com/JuliaLang/julia/issues/16285
[#16362]: https://github.com/JuliaLang/julia/issues/16362
[#16378]: https://github.com/JuliaLang/julia/issues/16378
[#16403]: https://github.com/JuliaLang/julia/issues/16403
[#16404]: https://github.com/JuliaLang/julia/issues/16404
[#16450]: https://github.com/JuliaLang/julia/issues/16450
[#16455]: https://github.com/JuliaLang/julia/issues/16455
[#16466]: https://github.com/JuliaLang/julia/issues/16466
[#16481]: https://github.com/JuliaLang/julia/issues/16481
[#16502]: https://github.com/JuliaLang/julia/issues/16502
[#16510]: https://github.com/JuliaLang/julia/issues/16510
[#16600]: https://github.com/JuliaLang/julia/issues/16600
[#16603]: https://github.com/JuliaLang/julia/issues/16603
[#16621]: https://github.com/JuliaLang/julia/issues/16621
[#16622]: https://github.com/JuliaLang/julia/issues/16622
[#16645]: https://github.com/JuliaLang/julia/issues/16645
[#16663]: https://github.com/JuliaLang/julia/issues/16663
[#16731]: https://github.com/JuliaLang/julia/issues/16731
[#16854]: https://github.com/JuliaLang/julia/issues/16854
[#16953]: https://github.com/JuliaLang/julia/issues/16953
[#16961]: https://github.com/JuliaLang/julia/issues/16961
[#16972]: https://github.com/JuliaLang/julia/issues/16972
[#16984]: https://github.com/JuliaLang/julia/issues/16984
[#16986]: https://github.com/JuliaLang/julia/issues/16986
[#17033]: https://github.com/JuliaLang/julia/issues/17033
[#17037]: https://github.com/JuliaLang/julia/issues/17037
[#17057]: https://github.com/JuliaLang/julia/issues/17057
[#17075]: https://github.com/JuliaLang/julia/issues/17075
[#17132]: https://github.com/JuliaLang/julia/issues/17132
[#17155]: https://github.com/JuliaLang/julia/issues/17155
[#17261]: https://github.com/JuliaLang/julia/issues/17261
[#17265]: https://github.com/JuliaLang/julia/issues/17265
[#17266]: https://github.com/JuliaLang/julia/issues/17266
[#17300]: https://github.com/JuliaLang/julia/issues/17300
[#17302]: https://github.com/JuliaLang/julia/issues/17302
[#17323]: https://github.com/JuliaLang/julia/issues/17323
[#17374]: https://github.com/JuliaLang/julia/issues/17374
[#17393]: https://github.com/JuliaLang/julia/issues/17393
[#17402]: https://github.com/JuliaLang/julia/issues/17402
[#17404]: https://github.com/JuliaLang/julia/issues/17404
[#17510]: https://github.com/JuliaLang/julia/issues/17510
[#17546]: https://github.com/JuliaLang/julia/issues/17546
[#17599]: https://github.com/JuliaLang/julia/issues/17599
[#17607]: https://github.com/JuliaLang/julia/issues/17607
[#17623]: https://github.com/JuliaLang/julia/issues/17623
[#17654]: https://github.com/JuliaLang/julia/issues/17654
[#17668]: https://github.com/JuliaLang/julia/issues/17668
[#17723]: https://github.com/JuliaLang/julia/issues/17723
[#17758]: https://github.com/JuliaLang/julia/issues/17758
[#17785]: https://github.com/JuliaLang/julia/issues/17785
[#18012]: https://github.com/JuliaLang/julia/issues/18012
[#18050]: https://github.com/JuliaLang/julia/issues/18050
[#18159]: https://github.com/JuliaLang/julia/issues/18159
[#18218]: https://github.com/JuliaLang/julia/issues/18218
[#18251]: https://github.com/JuliaLang/julia/issues/18251
[#18330]: https://github.com/JuliaLang/julia/issues/18330
[#18339]: https://github.com/JuliaLang/julia/issues/18339
[#18342]: https://github.com/JuliaLang/julia/issues/18342
[#18346]: https://github.com/JuliaLang/julia/issues/18346
[#18441]: https://github.com/JuliaLang/julia/issues/18441
[#18442]: https://github.com/JuliaLang/julia/issues/18442
[#18444]: https://github.com/JuliaLang/julia/issues/18444
[#18453]: https://github.com/JuliaLang/julia/issues/18453
[#18457]: https://github.com/JuliaLang/julia/issues/18457
[#18473]: https://github.com/JuliaLang/julia/issues/18473
[#18558]: https://github.com/JuliaLang/julia/issues/18558
[#18628]: https://github.com/JuliaLang/julia/issues/18628
[#18642]: https://github.com/JuliaLang/julia/issues/18642
[#18644]: https://github.com/JuliaLang/julia/issues/18644
[#18660]: https://github.com/JuliaLang/julia/issues/18660
[#18690]: https://github.com/JuliaLang/julia/issues/18690
[#18754]: https://github.com/JuliaLang/julia/issues/18754
[#18777]: https://github.com/JuliaLang/julia/issues/18777
[#18832]: https://github.com/JuliaLang/julia/issues/18832
[#18839]: https://github.com/JuliaLang/julia/issues/18839
[#18891]: https://github.com/JuliaLang/julia/issues/18891
[#18931]: https://github.com/JuliaLang/julia/issues/18931
[#18965]: https://github.com/JuliaLang/julia/issues/18965
[#18977]: https://github.com/JuliaLang/julia/issues/18977
[#19018]: https://github.com/JuliaLang/julia/issues/19018
[#19088]: https://github.com/JuliaLang/julia/issues/19088
[#19157]: https://github.com/JuliaLang/julia/issues/19157
[#19233]: https://github.com/JuliaLang/julia/issues/19233
[#19239]: https://github.com/JuliaLang/julia/issues/19239
[#19246]: https://github.com/JuliaLang/julia/issues/19246
[#19259]: https://github.com/JuliaLang/julia/issues/19259
[#19288]: https://github.com/JuliaLang/julia/issues/19288
[#19305]: https://github.com/JuliaLang/julia/issues/19305
[#19331]: https://github.com/JuliaLang/julia/issues/19331
[#19371]: https://github.com/JuliaLang/julia/issues/19371
[#19438]: https://github.com/JuliaLang/julia/issues/19438
[#19449]: https://github.com/JuliaLang/julia/issues/19449
[#19464]: https://github.com/JuliaLang/julia/issues/19464
[#19469]: https://github.com/JuliaLang/julia/issues/19469
[#19518]: https://github.com/JuliaLang/julia/issues/19518
[#19533]: https://github.com/JuliaLang/julia/issues/19533
[#19543]: https://github.com/JuliaLang/julia/issues/19543
[#19594]: https://github.com/JuliaLang/julia/issues/19594
[#19598]: https://github.com/JuliaLang/julia/issues/19598
[#19635]: https://github.com/JuliaLang/julia/issues/19635
[#19636]: https://github.com/JuliaLang/julia/issues/19636
[#19660]: https://github.com/JuliaLang/julia/issues/19660
[#19669]: https://github.com/JuliaLang/julia/issues/19669
[#19670]: https://github.com/JuliaLang/julia/issues/19670
[#19677]: https://github.com/JuliaLang/julia/issues/19677
[#19680]: https://github.com/JuliaLang/julia/issues/19680
[#19690]: https://github.com/JuliaLang/julia/issues/19690
[#19692]: https://github.com/JuliaLang/julia/issues/19692
[#19711]: https://github.com/JuliaLang/julia/issues/19711
[#19712]: https://github.com/JuliaLang/julia/issues/19712
[#19720]: https://github.com/JuliaLang/julia/issues/19720
[#19721]: https://github.com/JuliaLang/julia/issues/19721
[#19722]: https://github.com/JuliaLang/julia/issues/19722
[#19724]: https://github.com/JuliaLang/julia/issues/19724
[#19730]: https://github.com/JuliaLang/julia/issues/19730
[#19737]: https://github.com/JuliaLang/julia/issues/19737
[#19741]: https://github.com/JuliaLang/julia/issues/19741
[#19766]: https://github.com/JuliaLang/julia/issues/19766
[#19771]: https://github.com/JuliaLang/julia/issues/19771
[#19779]: https://github.com/JuliaLang/julia/issues/19779
[#19784]: https://github.com/JuliaLang/julia/issues/19784
[#19786]: https://github.com/JuliaLang/julia/issues/19786
[#19787]: https://github.com/JuliaLang/julia/issues/19787
[#19791]: https://github.com/JuliaLang/julia/issues/19791
[#19800]: https://github.com/JuliaLang/julia/issues/19800
[#19802]: https://github.com/JuliaLang/julia/issues/19802
[#19811]: https://github.com/JuliaLang/julia/issues/19811
[#19814]: https://github.com/JuliaLang/julia/issues/19814
[#19841]: https://github.com/JuliaLang/julia/issues/19841
[#19878]: https://github.com/JuliaLang/julia/issues/19878
[#19900]: https://github.com/JuliaLang/julia/issues/19900
[#19901]: https://github.com/JuliaLang/julia/issues/19901
[#19903]: https://github.com/JuliaLang/julia/issues/19903
[#19919]: https://github.com/JuliaLang/julia/issues/19919
[#19920]: https://github.com/JuliaLang/julia/issues/19920
[#19925]: https://github.com/JuliaLang/julia/issues/19925
[#19926]: https://github.com/JuliaLang/julia/issues/19926
[#19931]: https://github.com/JuliaLang/julia/issues/19931
[#19934]: https://github.com/JuliaLang/julia/issues/19934
[#19935]: https://github.com/JuliaLang/julia/issues/19935
[#19937]: https://github.com/JuliaLang/julia/issues/19937
[#19944]: https://github.com/JuliaLang/julia/issues/19944
[#19949]: https://github.com/JuliaLang/julia/issues/19949
[#19950]: https://github.com/JuliaLang/julia/issues/19950
[#19989]: https://github.com/JuliaLang/julia/issues/19989
[#20009]: https://github.com/JuliaLang/julia/issues/20009
[#20047]: https://github.com/JuliaLang/julia/issues/20047
[#20058]: https://github.com/JuliaLang/julia/issues/20058
[#20079]: https://github.com/JuliaLang/julia/issues/20079
[#20135]: https://github.com/JuliaLang/julia/issues/20135
[#20164]: https://github.com/JuliaLang/julia/issues/20164
[#20213]: https://github.com/JuliaLang/julia/issues/20213
[#20228]: https://github.com/JuliaLang/julia/issues/20228
[#20248]: https://github.com/JuliaLang/julia/issues/20248
[#20249]: https://github.com/JuliaLang/julia/issues/20249
[#20268]: https://github.com/JuliaLang/julia/issues/20268
[#20308]: https://github.com/JuliaLang/julia/issues/20308
[#20321]: https://github.com/JuliaLang/julia/issues/20321
[#20327]: https://github.com/JuliaLang/julia/issues/20327
[#20328]: https://github.com/JuliaLang/julia/issues/20328
[#20330]: https://github.com/JuliaLang/julia/issues/20330
[#20342]: https://github.com/JuliaLang/julia/issues/20342
[#20345]: https://github.com/JuliaLang/julia/issues/20345
[#20403]: https://github.com/JuliaLang/julia/issues/20403
[#20404]: https://github.com/JuliaLang/julia/issues/20404
[#20406]: https://github.com/JuliaLang/julia/issues/20406
[#20414]: https://github.com/JuliaLang/julia/issues/20414
[#20418]: https://github.com/JuliaLang/julia/issues/20418
[#20427]: https://github.com/JuliaLang/julia/issues/20427
[#20435]: https://github.com/JuliaLang/julia/issues/20435
[#20500]: https://github.com/JuliaLang/julia/issues/20500
[#20530]: https://github.com/JuliaLang/julia/issues/20530
[#20543]: https://github.com/JuliaLang/julia/issues/20543
[#20609]: https://github.com/JuliaLang/julia/issues/20609
[#20889]: https://github.com/JuliaLang/julia/issues/20889
[#20952]: https://github.com/JuliaLang/julia/issues/20952
[#21183]: https://github.com/JuliaLang/julia/issues/21183
[#21818]: https://github.com/JuliaLang/julia/issues/21818
