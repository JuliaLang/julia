Julia v1.7 Release Notes
========================

New language features
---------------------

* `(; a, b) = x` can now be used to destructure properties `a` and `b` of `x`.
  This syntax is equivalent to `a = getproperty(x, :a); b = getproperty(x, :b)` ([#39285]).
* Implicit multiplication by juxtaposition is now allowed for radical symbols (e.g. `x√y` and `x∛y`) ([#40173]).
* The short-circuiting operators `&&` and `||` can now be dotted to participate in broadcast fusion
  as `.&&` and `.||` ([#39594]).
* `⫪` (U+2AEA, `\Top`, `\downvDash`) and `⫫` (U+2AEB, `\Bot`, `\upvDash`, `\indep`)
  may now be used as binary operators with comparison precedence ([#39403]).
* Repeated semicolons can now be used inside array concatenation expressions to separate dimensions
  of an array, with the number of semicolons specifying the dimension. Just as a single semicolon
  in `[A; B]` has always described concatenating in the first dimension (vertically), now two
  semicolons `[A;; B]` do so in the second dimension (horizontally), three semicolons `;;;` in the
  third, and so on ([#33697]).
* A backslash (`\`) before a newline inside a string literal now removes the newline while also
  respecting indentation. This can be used to split up long strings without newlines into multiple
  lines of code ([#40753]).
* A backslash before a newline in command literals now always removes the newline, similar to standard string
  literals, whereas the result was not well-defined before ([#40753]).
* The default behavior of observing `@inbounds` declarations is now an option via `auto` in `--check-bounds=yes|no|auto` ([#41551])

Language changes
----------------

* `macroexpand`, `@macroexpand`, and `@macroexpand1` no longer wrap errors in a `LoadError`.
  To reduce breakage, `@test_throws` has been modified so that many affected tests will still pass ([#38379]).
* The middle dot `·` (`\cdotp` U+00b7) and the Greek interpunct `·` (U+0387) are now treated as equivalent to
  the dot operator `⋅` (`\cdot` U+22c5) (#25157).
* The minus sign `−` (`\minus` U+2212) is now treated as equivalent to the hyphen-minus sign `-` (U+002d) ([#40948]).
* Destructuring will no longer mutate values on the left-hand side while iterating through values on
  the right-hand side. In the example of an array `x`, `x[2], x[1] = x` will now swap the first and
  second elements of `x`, whereas it used to fill both entries with `x[1]` because `x[2]` was mutated during
  the iteration of `x` ([#40737]).
* The default random number generator has changed, so all random numbers will be different (even with the
  same seed) unless an explicit RNG object is used.
  See the section on the `Random` standard library below ([#40546]).
* `Iterators.peel(itr)` now returns `nothing` when `itr` is empty instead of throwing a `BoundsError` ([#39607]).
* Multiple successive semicolons in an array expresion were previously ignored (e.g., `[1 ;; 2] == [1 ; 2]`).
  This syntax is now used to separate dimensions (see **New language features**).

Compiler/Runtime improvements
-----------------------------


Command-line option changes
---------------------------

* The Julia `--project` option and the `JULIA_PROJECT` environment variable now support selecting shared
  environments like `.julia/environments/myenv` the same way the package management console does:
  use `julia --project=@myenv` resp. `export JULIA_PROJECT="@myenv"` ([#40025]).

Multi-threading changes
-----------------------

* Intrinsics for atomic pointer operations are now defined for certain byte sizes ([#37847]).
* Support for declaring and using individual fields of a mutable struct as atomic has been
  added; see the new `@atomic` macro ([#37847]).
* If the `JULIA_NUM_THREADS` environment variable is set to `auto`, then the
  number of threads will be set to the number of CPU threads ([#38952]).
* Every `Task` object has a local random number generator state, providing
  reproducible (schedule-independent) execution of parallel simulation code by
  default. The default generator is also significantly faster in parallel than
  in previous versions ([#40546]).
* Tasks can now migrate among threads when they are re-scheduled. Previously, a Task
  would always run on whichever thread executed it first ([#40715]).

Build system changes
--------------------


New library functions
---------------------

* Two argument methods `findmax(f, domain)`, `argmax(f, domain)` and the corresponding
  `min` versions ([#35316]).
* `isunordered(x)` returns true if `x` is a value that is normally unordered, such as
  `NaN` or `missing` ([#35316]).
* New `keepat!(vector, inds)` function which is the inplace equivalent of `vector[inds]`
  for a list `inds` of integers ([#36229]).
* Two arguments method `lock(f, lck)` now accepts a `Channel` as the second argument ([#39312]).
* New functor `Returns(value)`, which returns `value` for any arguments ([#39794]).
* New macros `@something` and `@coalesce` which are short-circuiting versions of `something` and
  `coalesce`, respectively ([#40729]).
* New function `redirect_stdio` for redirecting `stdin`, `stdout` and `stderr` ([#37978]).
* New macro `Base.@invoke f(arg1::T1, arg2::T2; kwargs...)` provides an easier syntax to call
  `invoke(f, Tuple{T1,T2}, arg1, arg2; kwargs...)` ([#38438]).
* New macro `Base.@invokelatest f(args...; kwargs...)` providing a convenient way to call
  `Base.invokelatest(f, args...; kwargs...)` ([#37971]).

New library features
--------------------

* The optional keyword argument `context` of `sprint` can now be set to a tuple of `:key => value`
  pairs to specify multiple attributes ([#39381]).
* `bytes2hex` and `hex2bytes` are no longer limited to arguments of type `Union{String,AbstractVector{UInt8}}`
  and now only require that they're iterable and have a length ([#39710]).
* `stat(file)` now has a more detailed and user-friendly `show` method ([#39463]).

Standard library changes
------------------------

* `count` and `findall` now accept an `AbstractChar` argument to search for a character in
  a string ([#38675]).
* New methods `range(start, stop)` and `range(start, stop, length)` ([#39228]).
* `range` now supports `start` as an optional keyword argument ([#38041]).
* Some operations on ranges will return a `StepRangeLen` instead of a `StepRange`, to allow
  the resulting step to be zero. Previously, `λ .* (1:9)` gave an error when `λ = 0` ([#40320]).
* `islowercase` and `isuppercase` are now compliant with the Unicode lower/uppercase categories ([#38574]).
* `iseven` and `isodd` functions now support non-`Integer` numeric types ([#38976]).
* `escape_string` now accepts a collection of characters via the keyword
  `keep` that are to be kept as they are ([#38597]).
* `getindex` for `NamedTuple`s now accepts a tuple of symbols in order to index multiple values ([#38878]).
* Subtypes of `AbstractRange` now correctly follow the general array indexing behavior when indexed by
  `Bool`s, erroring for scalar `Bool`s and treating arrays (including ranges) of `Bool` as
  logical indices ([#31829]).
* `keys(::RegexMatch)` is now defined to return the capture's keys, by name if named, or by index if not ([#37299]).
* `keys(::Generator)` is now defined to return the iterator's keys ([#34678]).
* `RegexMatch` is now iterable, giving the captured substrings ([#34355]).
* `lpad/rpad` are now defined in terms of `textwidth` ([#39044]).
* `Test.@test` now accepts `broken` and `skip` boolean keyword arguments, which
  mimic `Test.@test_broken` and `Test.@test_skip` behavior, but allows skipping
  tests failing only under certain conditions.  For example
  ```julia
  if T == Float64
      @test_broken isequal(complex(one(T)) / complex(T(Inf), T(-Inf)), complex(zero(T), zero(T)))
  else
      @test isequal(complex(one(T)) / complex(T(Inf), T(-Inf)), complex(zero(T), zero(T)))
  end
  ```
  can be replaced by
  ```julia
  @test isequal(complex(one(T)) / complex(T(Inf), T(-Inf)), complex(zero(T), zero(T))) broken=(T == Float64)
  ```
  ([#39322]).
* `@lock` is now exported from Base ([#39588]).
* The experimental function `Base.catch_stack()` has been renamed to `current_exceptions()`, exported
  from Base and given a more specific return type ([#29901]).
* Some degree trigonometric functions, `sind`, `cosd`, `tand`, `asind`, `acosd`, `asecd`, `acscd`,
  `acotd`, `atand` now accept a square matrix ([#39758]).
* `replace(::String)` now accepts multiple patterns, which will be applied left-to-right simultaneously,
  so only one pattern will be applied to any character, and the patterns will only be applied to the input
  text, not the replacements ([#40484]).
* The `length` function on certain ranges of certain specific element types no longer checks for integer
  overflow in most cases. The new function `checked_length` is now available, which will try to use checked
  arithmetic to error if the result may be wrapping. Or use a package such as SaferIntegers.jl when
  constructing the range. ([#40382])
* New `replace` methods to replace elements of a `Tuple` ([#38216]).

#### Package Manager

* If a package is `using` or `import`ed from the `julia>` prompt that isn't found but is available
  from a registry, a `pkg> add` prompt now offers to install the package into the current environment,
  precompile it, and continue to load it ([#39026]).
* A new `Manifest.toml` format is now used that captures extensible metadata fields, including the
  julia version that generated the manifest. Old format manifests are still supported and will be
  maintained in their original format, unless the user runs `Pkg.upgrade_manifest()` to upgrade the
  format of the current environment's manifest without re-resolving ([#40765]).
* `pkg> precompile` will now precompile new versions of packages that are already loaded, rather than
  postponing to the next session (the `?`-marked dependencies) ([#40345]).
* `pkg> rm`, `pin`, and `free` now accept the `--all` argument to call the action on all packages.
* Registries downloaded from the Pkg Server (not git) are no longer uncompressed into files but instead
  read directly from the compressed tarball into memory. This improves performance on
  filesystems which do not handle a large number of files well. To turn this feature off, set the
  environment variable `JULIA_PKG_UNPACK_REGISTRY=true`.
* It is now possible to use an external `git` executable instead of the default libgit2 library
  for the downloads that happen via the Git protocol by setting the environment variable
  `JULIA_PKG_USE_CLI_GIT=true`.
* Registries downloaded from the Pkg Server (not git) is now assumed to be immutable. Manual changes
  to their files might not be picked up by a running Pkg session.
* Adding packages by directory name in the REPL mode now requires prepending `./` to the name if the
  package is in the current directory; e.g. `add ./Package` is required instead of `add Package`.
  This is to avoid confusion between the package name `Package` and the local directory `Package`.
* The `mode` keyword for `PackageSpec` has been removed.

#### LinearAlgebra

* Use [Libblastrampoline](https://github.com/staticfloat/libblastrampoline/) to pick a BLAS
  and LAPACK at runtime. By default it forwards to OpenBLAS in the Julia distribution.
  The forwarding mechanism can be used by packages to replace the BLAS and LAPACK with
  user preferences ([#39455]).
* On aarch64, OpenBLAS now uses an ILP64 BLAS like all other 64-bit platforms ([#39436]).
* OpenBLAS is updated to 0.3.13 ([#39216]).
* SuiteSparse is updated to 5.8.1 ([#39455]).
* The shape of an `UpperHessenberg` matrix is preserved under certain arithmetic operations,
  e.g. when multiplying or dividing by an `UpperTriangular` matrix ([#40039]).
* Real quasitriangular Schur factorizations `S` can now be efficiently converted to complex
  upper-triangular form with `Schur{Complex}(S)` ([#40573]).
* `cis(A)` now supports matrix arguments ([#40194]).
* `dot` now supports `UniformScaling` with `AbstractMatrix` ([#40250]).
* `qr[!]` and `lu[!]` now support `LinearAlgebra.PivotingStrategy` (singleton type) values
  as their optional `pivot` argument: defaults are `qr(A, NoPivot())` (vs. `qr(A, ColumnNorm())`
  for pivoting) and `lu(A, RowMaximum())` (vs. `lu(A, NoPivot())` without pivoting); the former
  `Val{true/false}`-based calls are deprecated ([#40623]).
* `det(M::AbstractMatrix{BigInt})` now calls `det_bareiss(M)`, which uses the
  [Bareiss](https://en.wikipedia.org/wiki/Bareiss_algorithm) algorithm to calculate precise
  values ([#40868]).

#### Markdown


#### Printf


#### Random

* The default random number generator has been changed from Mersenne Twister to
  [Xoshiro256++](https://prng.di.unimi.it/).
  The new generator has smaller state, better performance, and superior statistical properties.
  This generator is the one used for reproducible Task-local randomness ([#40546]).

#### REPL

* Long strings are now elided using the syntax `"head" ⋯ 12345 bytes ⋯ "tail"` when displayed
  in the REPL ([#40736]).
* Pasting repl examples into the repl (prompt pasting) now supports all repl modes (`julia`, `pkg`,
  `shell`, `help?`) and switches mode automatically ([#40604]).
* `help?>` for modules without docstrings now returns a list of exported names and prints
  the contents of an associated `README.md` if found ([#39093]).

#### SparseArrays

* new `sizehint!(::SparseMatrixCSC, ::Integer)` method ([#30676]).
* `cholesky()` now fully preserves the user-specified permutation ([#40560]).
* `issparse` now applies consistently to all wrapper arrays, including nested, by checking
  `issparse` on the wrapped parent array ([#37644]).

#### Dates

* The `Dates.periods` function can be used to get the `Vector` of `Period`s that comprise a
  `CompoundPeriod` ([#39169]).

#### Downloads

* If a cookie header is set in a redirected request, the cookie will now be sent in following
  requests (<https://github.com/JuliaLang/Downloads.jl/pull/98>).
* If a `~/.netrc` file exists, it is used to get passwords for authenticated websites
  (<https://github.com/JuliaLang/Downloads.jl/pull/98>).
* [Server Name Indication](https://en.wikipedia.org/wiki/Server_Name_Indication) is now sent with
  all TLS connections, even when the server's identity is not verified (see [NetworkOptions](https://github.com/JuliaLang/NetworkOptions.jl); <https://github.com/JuliaLang/Downloads.jl/pull/114>).
* When verifying TLS connections on Windows, if the certificate revocation server cannot be
  reached, the connection is allowed; this matches what other applications do and how revocation
  is performed on macOS (<https://github.com/JuliaLang/Downloads.jl/pull/115>).
* There is now a 30-second connection timeout and a 20-second timeout if no data is sent; in
  combination, this guarantees that connections must make some progress or they will timeout in
  under a minute (<https://github.com/JuliaLang/Downloads.jl/pull/126>).

#### Statistics


#### Sockets


#### Tar

* `Tar.extract` now ignores the exact permission mode in a tarball and normalizes modes in the
  same way that `Tar.create` does, which is, in turn the same way that `git` normalizes them
  (<https://github.com/JuliaIO/Tar.jl/pull/99>).
* Functions that consume tarballs now handle hard links: the link target must be a previously seen
  file; `Tar.list` lists the entry with `:hardlink` type and `.link` field giving the path to the
  target; other functions — `Tar.extract`, `Tar.rewrite`, `Tar.tree_hash` — treat a hard link as a
  copy of the target file (<https://github.com/JuliaIO/Tar.jl/pull/102>).
* The standard format generated by `Tar.create` and `Tar.rewrite` now includes entries for non-empty
  directories; this shouldn't be neccessary, but some tools that consume tarballs (including docker)
  are confused by the absence of these directory entries (<https://github.com/JuliaIO/Tar.jl/pull/106>).
* `Tar` now accepts tarballs with leading spaces in octal integer header fields: this is technically
  not a valid format according to the POSIX spec, but old Solaris `tar` commands produced tarballs like
  this so this format does occur in the wild, and it seems harmless to accept it
  (<https://github.com/JuliaIO/Tar.jl/pull/116>).
* `Tar.extract` now takes a `set_permissions` keyword argument, which defaults to `true`; if `false` is
  passed instead, the permissions of extracted files are not modified on extraction
  (<https://github.com/JuliaIO/Tar.jl/pull/113>).

#### Distributed


#### UUIDs


#### Mmap

* `mmap` is now exported ([#39816]).

#### DelimitedFiles

* `readdlm` now defaults to `use_mmap=false` on all OSes for consistent reliability in abnormal
  filesystem situations ([#40415]).

Deprecated or removed
---------------------


External dependencies
---------------------


Tooling Improvements
---------------------


<!--- generated by NEWS-update.jl: -->
[#29901]: https://github.com/JuliaLang/julia/issues/29901
[#30676]: https://github.com/JuliaLang/julia/issues/30676
[#31829]: https://github.com/JuliaLang/julia/issues/31829
[#33697]: https://github.com/JuliaLang/julia/issues/33697
[#34355]: https://github.com/JuliaLang/julia/issues/34355
[#34678]: https://github.com/JuliaLang/julia/issues/34678
[#35316]: https://github.com/JuliaLang/julia/issues/35316
[#36229]: https://github.com/JuliaLang/julia/issues/36229
[#37299]: https://github.com/JuliaLang/julia/issues/37299
[#37644]: https://github.com/JuliaLang/julia/issues/37644
[#37847]: https://github.com/JuliaLang/julia/issues/37847
[#37971]: https://github.com/JuliaLang/julia/issues/37971
[#37978]: https://github.com/JuliaLang/julia/issues/37978
[#38041]: https://github.com/JuliaLang/julia/issues/38041
[#38216]: https://github.com/JuliaLang/julia/issues/38216
[#38379]: https://github.com/JuliaLang/julia/issues/38379
[#38438]: https://github.com/JuliaLang/julia/issues/38438
[#38574]: https://github.com/JuliaLang/julia/issues/38574
[#38597]: https://github.com/JuliaLang/julia/issues/38597
[#38675]: https://github.com/JuliaLang/julia/issues/38675
[#38878]: https://github.com/JuliaLang/julia/issues/38878
[#38952]: https://github.com/JuliaLang/julia/issues/38952
[#38976]: https://github.com/JuliaLang/julia/issues/38976
[#39026]: https://github.com/JuliaLang/julia/issues/39026
[#39044]: https://github.com/JuliaLang/julia/issues/39044
[#39093]: https://github.com/JuliaLang/julia/issues/39093
[#39169]: https://github.com/JuliaLang/julia/issues/39169
[#39216]: https://github.com/JuliaLang/julia/issues/39216
[#39228]: https://github.com/JuliaLang/julia/issues/39228
[#39285]: https://github.com/JuliaLang/julia/issues/39285
[#39312]: https://github.com/JuliaLang/julia/issues/39312
[#39322]: https://github.com/JuliaLang/julia/issues/39322
[#39381]: https://github.com/JuliaLang/julia/issues/39381
[#39403]: https://github.com/JuliaLang/julia/issues/39403
[#39436]: https://github.com/JuliaLang/julia/issues/39436
[#39455]: https://github.com/JuliaLang/julia/issues/39455
[#39463]: https://github.com/JuliaLang/julia/issues/39463
[#39588]: https://github.com/JuliaLang/julia/issues/39588
[#39594]: https://github.com/JuliaLang/julia/issues/39594
[#39607]: https://github.com/JuliaLang/julia/issues/39607
[#39710]: https://github.com/JuliaLang/julia/issues/39710
[#39758]: https://github.com/JuliaLang/julia/issues/39758
[#39794]: https://github.com/JuliaLang/julia/issues/39794
[#39816]: https://github.com/JuliaLang/julia/issues/39816
[#40025]: https://github.com/JuliaLang/julia/issues/40025
[#40039]: https://github.com/JuliaLang/julia/issues/40039
[#40173]: https://github.com/JuliaLang/julia/issues/40173
[#40194]: https://github.com/JuliaLang/julia/issues/40194
[#40250]: https://github.com/JuliaLang/julia/issues/40250
[#40320]: https://github.com/JuliaLang/julia/issues/40320
[#40345]: https://github.com/JuliaLang/julia/issues/40345
[#40415]: https://github.com/JuliaLang/julia/issues/40415
[#40484]: https://github.com/JuliaLang/julia/issues/40484
[#40546]: https://github.com/JuliaLang/julia/issues/40546
[#40560]: https://github.com/JuliaLang/julia/issues/40560
[#40573]: https://github.com/JuliaLang/julia/issues/40573
[#40604]: https://github.com/JuliaLang/julia/issues/40604
[#40623]: https://github.com/JuliaLang/julia/issues/40623
[#40715]: https://github.com/JuliaLang/julia/issues/40715
[#40729]: https://github.com/JuliaLang/julia/issues/40729
[#40736]: https://github.com/JuliaLang/julia/issues/40736
[#40737]: https://github.com/JuliaLang/julia/issues/40737
[#40753]: https://github.com/JuliaLang/julia/issues/40753
[#40765]: https://github.com/JuliaLang/julia/issues/40765
[#40868]: https://github.com/JuliaLang/julia/issues/40868
[#40948]: https://github.com/JuliaLang/julia/issues/40948
