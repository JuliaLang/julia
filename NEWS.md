Julia v1.7 Release Notes
========================

New language features
---------------------

* `(; a, b) = x` can now be used to destructure properties `a` and `b` of `x`. This syntax is equivalent to `a = getproperty(x, :a)`
  and similarly for `b`. ([#39285])
* Implicit multiplication by juxtaposition is now allowed for radical symbols (e.g., `x√y` and `x∛y`). ([#40173])
* The short-circuiting operators `&&` and `||` can now be dotted to participate in broadcast fusion
  as `.&&` and `.||`. ([#39594])
* `⫪` (U+2AEA, `\Top`, `\downvDash`) and `⫫` (U+2AEB, `\Bot`, `\upvDash`, `\indep`)
  may now be used as binary operators with comparison precedence. ([#39403])
* Repeated semicolons may now be used inside array literals to separate dimensions of an array,
  with the number of semicolons specifying the particular dimension. Just as the single semicolon
  in `[A; B]` has always described concatenating along the first dimension (vertically), now two
  semicolons `[A;; B]` do so in the second dimension (horizontally), three semicolons `;;;` in the
  third, and so on. ([#33697])
* A backslash (`\`) before a newline inside a string literal now removes the newline while also
  respecting indentation. This can be used to split up long strings without newlines into multiple
  lines of code. ([#40753])

Language changes
----------------

* `macroexpand`, `@macroexpand`, and `@macroexpand1` no longer wrap errors in a `LoadError`. To reduce breakage, `@test_throws` has been modified so that many affected tests will still pass ([#38379]].
* The middle dot `·` (`\cdotp` U+00b7) and the Greek interpunct `·` (U+0387) are now treated as equivalent to the dot operator `⋅` (`\cdot` U+22c5) (#25157).
* The minus sign `−` (`\minus` U+2212) is now treated as equivalent to the hyphen-minus sign `-` (U+002d).
* Destructuring will no longer mutate values on the left hand side while iterating through values on the right hand side. In the example
  of an array `x`, `x[2], x[1] = x` will now swap the first and second entry of `x`, whereas it used to fill both entries with `x[1]`
  because `x[2]` was mutated during the iteration of `x`. ([#40737])
* The default random number generator has changed, so all random numbers will be different (even with the
  same seed) unless an explicit RNG object is used. See the section on the `Random` standard library below.
* `Iterators.peel(itr)` now returns `nothing` when `itr` is empty rather than throwing a `BoundsError`. ([#39569])

Compiler/Runtime improvements
-----------------------------


Command-line option changes
---------------------------

* The Julia `--project` option and the `JULIA_PROJECT` environment variable now support selecting shared environments like `.julia/environments/myenv` the same way the package management console does: use `julia --project=@myenv` resp. `export JULIA_PROJECT="@myenv"` ([#40025]).


Multi-threading changes
-----------------------

* Intrinsics for atomic pointer operations are now defined for certain byte sizes. ([#37847])
* Support for declaring and using individual fields of a mutable struct as
  atomic now available. ([#37847])
* If the `JULIA_NUM_THREADS` environment variable is set to `auto`, then the
  number of threads will be set to the number of CPU threads ([#38952])
* Every `Task` object has a local random number generator state, providing
  reproducible (schedule-independent) execution of parallel simulation code by
  default. The default generator is also significantly faster in parallel than
  in previous versions.


Build system changes
--------------------


New library functions
---------------------

* Two argument methods `findmax(f, domain)`, `argmax(f, domain)` and the corresponding `min` versions ([#27613]).
* `isunordered(x)` returns true if `x` is value that is normally unordered, such as `NaN` or `missing`.
* New `keepat!(vector, inds)` function which is the inplace equivalent of `vector[inds]`
  for a list `inds` of integers ([#36229]).
* New macro `Base.@invokelatest f(args...; kwargs...)` provides a convenient way to call `Base.invokelatest(f, args...; kwargs...)` ([#37971])
* Two arguments method `lock(f, lck)` now accepts a `Channel` as the second argument. ([#39312])
* New functor `Returns(value)`, which returns `value` for any arguments ([#39794])
* New macro `Base.@invoke f(arg1::T1, arg2::T2; kwargs...)` provides an easier syntax to call `invoke(f, Tuple{T1,T2}, arg1, arg2; kwargs...)` ([#38438])
* New macros `@something` and `@coalesce` which are short-circuiting versions of `something` and `coalesce`, respectively ([#40729])
* New function `redirect_stdio` for redirecting `stdin`, `stdout` and `stderr` ([#37978]).

New library features
--------------------

* The optional keyword argument `context` of `sprint` can now be set to a tuple of `:key => value` pairs to specify multiple attributes. ([#39381])
* `bytes2hex` and `hex2bytes` are no longer limited to arguments of type `Union{String,AbstractVector{UInt8}}` and now only require that they're iterable and have a length. ([#39710])

Standard library changes
------------------------

* Long strings are now elided using the syntax `"head" ⋯ 12345 bytes ⋯ "tail"` when displayed in the REPL ([#40736]).
* `count` and `findall` now accept an `AbstractChar` argument to search for a character in a string ([#38675]).
* `range` now supports the `range(start, stop)` and `range(start, stop, length)` methods ([#39228]).
* `range` now supports `start` as an optional keyword argument ([#38041]).
* Some operations on ranges will return a `StepRangeLen` instead of a `StepRange`, to allow the resulting step to be zero. Previously, `λ .* (1:9)` gave an error when `λ = 0`. ([#40320])
* `islowercase` and `isuppercase` are now compliant with the Unicode lower/uppercase categories ([#38574]).
* `iseven` and `isodd` functions now support non-`Integer` numeric types ([#38976]).
* `escape_string` can now receive a collection of characters in the keyword
  `keep` that are to be kept as they are. ([#38597]).
* `getindex` can now be used on `NamedTuple`s with multiple values ([#38878])
* Subtypes of `AbstractRange` now correctly follow the general array indexing
  behavior when indexed by `Bool`s, erroring for scalar `Bool`s and treating
  arrays (including ranges) of `Bool` as an logical index ([#31829])
* `keys(::RegexMatch)` is now defined to return the capture's keys, by name if named, or by index if not ([#37299]).
* `keys(::Generator)` is now defined to return the iterator's keys ([#34678])
* `RegexMatch` now iterate to give their captures. ([#34355]).
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
  ([#39322])
* `@lock` is now exported from Base ([#39588]).
* The experimental function `Base.catch_stack()` has been renamed to `current_exceptions()`, exported from Base and given a more specific return type ([#29901])
* Some degree trigonometric functions, `sind`, `cosd`, `tand`, `asind`, `acosd`, `asecd`, `acscd`, `acotd`, `atand` now accept an square matrix ([#39758]).
* A backslash before a newline in command literals now always removes the newline, similar to standard string
  literals, whereas the result was not well-defined before. ([#40753])

#### Package Manager


#### LinearAlgebra

* Use [Libblastrampoline](https://github.com/staticfloat/libblastrampoline/) to pick a BLAS and LAPACK at runtime. By default it forwards to OpenBLAS in the Julia distribution. The forwarding mechanism can be used by packages to replace the BLAS and LAPACK with user preferences. ([#39455])
* On aarch64, OpenBLAS now uses an ILP64 BLAS like all other 64-bit platforms. ([#39436])
* OpenBLAS is updated to 0.3.13. ([#39216])
* SuiteSparse is updated to 5.8.1. ([#39455])
* The shape of an `UpperHessenberg` matrix is preserved under certain arithmetic operations, e.g. when multiplying or dividing by an `UpperTriangular` matrix. ([#40039])
* Real quasitriangular Schur factorizations `S` can now be efficiently converted to complex upper-triangular form with `Schur{Complex}(S)` ([#40573]).
* `cis(A)` now supports matrix arguments ([#40194]).
* `dot` now supports `UniformScaling` with `AbstractMatrix` ([#40250]).
* `qr[!]` and `lu[!]` now support `LinearAlgebra.PivotingStrategy` (singleton type) values
  as their optional `pivot` argument: defaults are `qr(A, NoPivot())` (vs.
  `qr(A, ColumnNorm())` for pivoting) and `lu(A, RowMaximum())` (vs. `lu(A, NoPivot())`
  without pivoting); the former `Val{true/false}`-based calls are deprecated. ([#40623])
* `det(M::AbstractMatrix{BigInt})` now calls `det_bareiss(M)`, which uses the [Bareiss](https://en.wikipedia.org/wiki/Bareiss_algorithm) algorithm to calculate precise values.([#40868]).

#### Markdown


#### Printf


#### Random

* The default random number generator has been changed from Mersenne Twister to [Xoshiro256++](https://prng.di.unimi.it/).
  The new generator has smaller state, better performance, and superior statistical properties.
  This generator is the one used for reproducible Task-local randomness.

#### REPL


#### SparseArrays

* new `sizehint!(::SparseMatrixCSC, ::Integer)` method ([#30676]).
* `cholesky()` now fully preserves the user-specified permutation. ([#40560])
* `issparse` now applies consistently to all wrapper arrays, including nested, by checking `issparse` on the wrapped parent array ([#37644]).

#### Dates

* The `Dates.periods` function can be used to get the `Vector` of `Period`s that comprise a `CompoundPeriod` ([#39169]).

#### Statistics


#### Sockets


#### Distributed


#### UUIDs


#### Mmap

* `mmap` is now exported ([#39816]).

#### DelimitedFiles

* `readdlm` now defaults to `use_mmap=false` on all OSes for consistent reliability in abnormal filesystem situations ([#40415]).

Deprecated or removed
---------------------
- Multiple successive semicolons in an array expresion were previously ignored (e.g., `[1 ;; 2] == [1 ; 2]`). This is now being used to separate dimensions for array literals. (see **New language features**)


External dependencies
---------------------


Tooling Improvements
---------------------


<!--- generated by NEWS-update.jl: -->
