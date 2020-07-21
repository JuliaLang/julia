Julia v1.6 Release Notes
========================

New language features
---------------------

* Types written with `where` syntax can now be used to define constructors, e.g.
  `(Foo{T} where T)(x) = ...`.

Language changes
----------------


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

New library functions
---------------------

* New function `Base.kron!` and corresponding overloads for various matrix types for performing Kronecker product in-place. ([#31069]).
* New function `Base.Threads.foreach(f, channel::Channel)` for multithreaded `Channel` consumption. ([#34543]).
* `Iterators.map` is added. It provides another syntax `Iterators.map(f, iterators...)`
  for writing `(f(args...) for args in zip(iterators...))`, i.e. a lazy `map` ([#34352]).
* New function `sincospi` for simultaneously computing `sinpi(x)` and `cospi(x)` more
  efficiently ([#35816]).

New library features
--------------------

* The `redirect_*` functions can now be called on `IOContext` objects.

Standard library changes
------------------------

* The `nextprod` function now accepts tuples and other array types for its first argument ([#35791]).
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

#### LinearAlgebra
* New method `LinearAlgebra.issuccess(::CholeskyPivoted)` for checking whether pivoted Cholesky factorization was successful ([#36002]).
* `UniformScaling` can now be indexed into using ranges to return dense matrices and vectors ([#24359]).
* New function `LinearAlgebra.BLAS.get_num_threads()` for getting the number of BLAS threads. ([#36360])

#### Markdown


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
