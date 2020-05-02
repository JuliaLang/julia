Julia v1.6 Release Notes
========================

New language features
---------------------

* Types written with `where` syntax can now be used to define constructors, e.g.
  `(Foo{T} where T)(x) = ...`.
* `<--` and `<-->` are now available as infix operators, with the same precedence
  and associativity as other arrow-like operators ([#36666]).
* Compilation and type inference can now be enabled or disabled at the module level
  using the experimental macro `Base.Experimental.@compiler_options` ([#37041]).
* The library name passed to `ccall` or `@ccall` can now be an expression involving
  global variables and function calls. The expression will be evaluated the first
  time the `ccall` executes ([#36458]).

* Standalone "dotted" operators now get lowered to `Base.BroadcastFunction(op)` and can be passed to
  higher-order functions, i.e. `.op` is functionally equivalent to `(x...) -> (op).(x...)`.
  ([#35706])

Language changes
----------------

* The `-->` operator now lowers to a `:call` expression, so it can be defined as
  a function like other operators. The dotted version `.-->` is now parsed as well.
  For backwards compatibility, `-->` still parses using its own expression head
  instead of `:call`.

Compiler/Runtime improvements
-----------------------------

* All platforms can now use `@executable_path` within `jl_load_dynamic_library()`.
  This allows executable-relative paths to be embedded within executables on all
  platforms, not just MacOS, which the syntax is borrowed from. ([#35627])
* Constant propogation now occurs through keyword arguments ([#35976])
* The precompilation cache is now created atomically ([#36416]). Invoking _n_
  Julia processes simultaneously may create _n_ temporary caches.

Command-line option changes
---------------------------

* There is no longer a concept of "home project": starting `julia --project=dir`
  is now exactly equivalent to starting `julia` and then doing `pkg> activate
  $dir` and `julia --project` is exactly equivalent to doing that where
  `dir = Base.current_project()`. In particular, this means that if you do
  `pkg> activate` after starting `julia` with the `--project` option (or with
  `JULIA_PROJECT` set) it will take you to the default active project, which is
  `@v1.5` unless you have modified `LOAD_PATH`. ([#36434])

Multi-threading changes
-----------------------


Build system changes
--------------------

* Windows Installer now has the option to 'Add Julia to Path'. To unselect this option
  from the commandline simply remove the tasks you do not want to be installed: e.g.
  `./julia-installer.exe /TASKS="desktopicon,startmenu,addtopath"`, adds a desktop
  icon, a startmenu group icon, and adds Julia to system PATH.


Library functions
-----------------
* The `Base.Grisu` code has been officially removed (float printing was switched to the ryu algorithm code in 1.4)


New library functions
---------------------

* New function `Base.kron!` and corresponding overloads for various matrix types for performing Kronecker product in-place. ([#31069]).
* New function `Base.Threads.foreach(f, channel::Channel)` for multithreaded `Channel` consumption. ([#34543]).
* New function `Base.readeach(io, T)` for iteratively performing `read(io, T)`. ([#36150])
* `Iterators.map` is added. It provides another syntax `Iterators.map(f, iterators...)`
  for writing `(f(args...) for args in zip(iterators...))`, i.e. a lazy `map` ([#34352]).
* New function `sincospi` for simultaneously computing `sinpi(x)` and `cospi(x)` more
  efficiently ([#35816]).
* New function `addenv` for adding environment mappings into a `Cmd` object, returning the new `Cmd` object.

New library features
--------------------

* The `redirect_*` functions can now be called on `IOContext` objects.
* New constructor `NamedTuple(iterator)` that constructs a named tuple from a key-value pair iterator.

Standard library changes
------------------------
* The `nextprod` function now accepts tuples and other array types for its first argument ([#35791]).
* The `reverse(A; dims)` function for multidimensional `A` can now reverse multiple dimensions at once
  by passing a tuple for `dims`, and defaults to reversing all dimensions; there is also a multidimensional
  in-place `reverse!(A; dims)` ([#37367]).
* The function `isapprox(x,y)` now accepts the `norm` keyword argument also for numeric (i.e., non-array) arguments `x` and `y` ([#35883]).
* `view`, `@view`, and `@views` now work on `AbstractString`s, returning a `SubString` when appropriate ([#35879]).
* All `AbstractUnitRange{<:Integer}`s now work with `SubString`, `view`, `@view` and `@views` on strings ([#35879]).
* `sum`, `prod`, `maximum`, and `minimum` now support `init` keyword argument ([#36188], [#35839]).
* `unique(f, itr; seen=Set{T}())` now allows you to declare the container type used for
  keeping track of values returned by `f` on elements of `itr` ([#36280]).
* `Libdl` has been moved to `Base.Libc.Libdl`, however it is still accessible as an stdlib ([#35628]).
* `first` and `last` functions now accept an integer as second argument to get that many
  leading or trailing elements of any iterable ([#34868]).
* `intersect` on `CartesianIndices` now returns `CartesianIndices` instead of `Vector{<:CartesianIndex}` ([#36643]).
* `push!(c::Channel, v)` now returns channel `c`. Previously, it returned the pushed value `v` ([#34202]).
* `RegexMatch` objects can now be probed for whether a named capture group exists within it through `haskey()` ([#36717]).
* For consistency `haskey(r::RegexMatch, i::Integer)` has also been added and returns if the capture group for `i` exists ([#37300]).
* A new standard library `TOML` has been added for parsing and printing [TOML files](https://toml.io) ([#37034]).
* The `Pkg.BinaryPlatforms` module has been moved into `Base` as `Base.BinaryPlatforms` and heavily reworked.
  Applications that want to be compatible with the old API should continue to import `Pkg.BinaryPlatforms`,
  however new users should use `Base.BinaryPlatforms` directly. ([#37320])
* The `Pkg.Artifacts` module has been imported as a separate standard library.  It is still available as
  `Pkg.Artifacts`, however starting from Julia v1.6+, packages may import simply `Artifacts` without importing
  all of `Pkg` alongside. ([#37320])

#### LinearAlgebra
* New method `LinearAlgebra.issuccess(::CholeskyPivoted)` for checking whether pivoted Cholesky factorization was successful ([#36002]).
* `UniformScaling` can now be indexed into using ranges to return dense matrices and vectors ([#24359]).
* New function `LinearAlgebra.BLAS.get_num_threads()` for getting the number of BLAS threads. ([#36360])
* `(+)(::UniformScaling)` is now defined, making `+I` a valid unary operation. ([#36784])

#### Markdown

#### Printf

* Complete overhaul of internal code to use the ryu float printing algorithms (from Julia 1.4); leads to consistent 2-5x performance improvements
* New `Printf.tofloat` function allowing custom float types to more easily integrate with Printf formatting by converting their type to `Float16`, `Float32`, `Float64`, or `BigFloat`
* New `Printf.format"..."` and `Printf.Format(...)` functions that allow creating `Printf.Format` objects that can be passed to `Printf.format` for easier dynamic printf formatting
* `Printf.format(f::Printf.Format, args...)` as a non-macro function that applies a printf format `f` to provided `args`


#### Random


#### REPL

* The `AbstractMenu` extension interface of `REPL.TerminalMenus` has been extensively
  overhauled. The new interface does not rely on global configuration variables, is more
  consistent in delegating printing of the navigation/selection markers, and provides
  improved support for dynamic menus.  These changes are compatible with the previous
  (deprecated) interface, so are non-breaking.

  The new API offers several enhancements:

  + Menus are configured in their constructors via keyword arguments
  + For custom menu types, the new `Config` and `MultiSelectConfig` replace the global `CONFIG` Dict
  + `request(menu; cursor=1)` allows you to control the initial cursor position in the menu (defaults to first item)
  + `MultiSelectMenu` allows you to pass a list of initially-selected items with the `selected` keyword argument
  + `writeLine` was deprecated to `writeline`, and `writeline` methods are not expected to print the cursor indicator.
    The old `writeLine` continues to work, and any of its method extensions should print the cursor indicator as before.
  + `printMenu` has been deprecated to `printmenu`, and it both accepts a state input and returns a state output
    that controls the number of terminal lines erased when the menu is next refreshed. This plus related changes
    makes `printmenu` work properly when the number of menu items might change depending on user choices.
  + `numoptions`, returning the number of items in the menu, has been added as an alternative to implementing `options`
  + `suppress_output` (primarily a testing option) has been added as a keyword argument to `request`,
    rather than a configuration option

* Windows REPL now supports 24-bit colors, by correctly interpreting virtual terminal escapes.


#### SparseArrays

* Display large sparse matrices with a Unicode "spy" plot of their nonzero patterns, and display small sparse matrices by an `Matrix`-like 2d layout of their contents.

#### Dates
* `Quarter` period is defined ([#35519]).

#### Statistics


#### Sockets


#### Distributed


#### UUIDs
* Change `uuid1` and `uuid4` to use `Random.RandomDevice()` as default random number generator ([#35872]).
* Added `parse(::Type{UUID}, ::AbstractString)` method

Deprecated or removed
---------------------

External dependencies
---------------------


Tooling Improvements
---------------------


<!--- generated by NEWS-update.jl: -->
