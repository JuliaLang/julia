Julia v1.3 Release Notes
========================

New language features
---------------------

* Support for Unicode 12.1.0 ([#32002]).
* Methods can now be added to an abstract type ([#31916]).
* Added argument `keep` to `unescape_string` ([#27125]).
* Support for unicode bold-digits and double-struck digits 0 through 9 as valid identifiers ([#32838]).

Language changes
----------------


Multi-threading changes
-----------------------

* All system-level I/O operations (e.g. files and sockets) are now thread-safe.
  This does not include subtypes of `IO` that are entirely in-memory, such as `IOBuffer`,
  although it specifically does include `BufferStream`.
  ([#32309], [#32174], [#31981], [#32421]).
* The global random number generator (`GLOBAL_RNG`) is now thread-safe (and thread-local) ([#32407]).
* New experimental `Threads.@spawn` macro that runs a task on any available thread ([#32600]).
* New `Channel(f::Function)` constructor param (`spawn=true`) to schedule the created Task on
  any available thread, matching the behavior of `Threads.@spawn` ([#32872]).
* Simplified the `Channel` constructor, which is now easier to read and more idiomatic julia.
  The old constructor (which used keyword arguments) is still available, but use is discouraged.
  ([#30855], [#32818]).

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
* `Sockets.recvfrom` now returns both host and port as an InetAddr ([#32729]).
* `nothing` can now be `print`ed, and interpolated into strings etc. as the string `"nothing"`. It is still not permitted to be interpolated into Cmds (i.e. ``echo `$(nothing)` `` will still error without running anything.) ([#32148])
* When `open` is called with a function, command, and keyword argument (e.g. ```open(`ls`, read=true) do f ...```)
  it now correctly throws a `ProcessFailedException` like other similar calls ([#32193]).
* `mktemp` and `mktempdir` now try, by default, to remove temporary paths they create before the process exits ([#32851]).

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
[#24353]: https://github.com/JuliaLang/julia/issues/24353
[#31066]: https://github.com/JuliaLang/julia/issues/31066
[#31459]: https://github.com/JuliaLang/julia/issues/31459
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
[#32174]: https://github.com/JuliaLang/julia/issues/32174
[#32300]: https://github.com/JuliaLang/julia/issues/32300
[#32308]: https://github.com/JuliaLang/julia/issues/32308
[#32309]: https://github.com/JuliaLang/julia/issues/32309
[#32407]: https://github.com/JuliaLang/julia/issues/32407
[#32421]: https://github.com/JuliaLang/julia/issues/32421
[#32534]: https://github.com/JuliaLang/julia/issues/32534
[#32600]: https://github.com/JuliaLang/julia/issues/32600
