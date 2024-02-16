# [Creating Packages](@id creating-packages-tutorial)

## Generating files for a package

!!! note
    The [PkgTemplates](https://github.com/invenia/PkgTemplates.jl) package offers an easy, repeatable, and
    customizable way to generate the files for a new package. It can also generate files needed for Documentation, CI, etc.
    We recommend that you use PkgTemplates for creating
    new packages instead of using the minimal `pkg> generate` functionality described below.

To generate the bare minimum files for a new package, use `pkg> generate`.

```julia-repl
(@v1.8) pkg> generate HelloWorld
```

This creates a new project `HelloWorld` in a subdirectory by the same name, with the following files (visualized with the external [`tree` command](https://linux.die.net/man/1/tree)):

```julia-repl
shell> tree HelloWorld/
HelloWorld/
├── Project.toml
└── src
    └── HelloWorld.jl

2 directories, 2 files
```

The `Project.toml` file contains the name of the package, its unique UUID, its version, the authors and potential dependencies:

```toml
name = "HelloWorld"
uuid = "b4cd1eb8-1e24-11e8-3319-93036a3eb9f3"
version = "0.1.0"
authors = ["Some One <someone@email.com>"]

[deps]
```

The content of `src/HelloWorld.jl` is:

```julia
module HelloWorld

greet() = print("Hello World!")

end # module
```

We can now activate the project by using the path to the directory where it is installed, and load the package:

```julia-repl
pkg> activate ./HelloWorld

julia> import HelloWorld

julia> HelloWorld.greet()
Hello World!
```

For the rest of the tutorial we enter inside the directory of the project, for convenience:

```julia-repl
julia> cd("HelloWorld")
```

## Adding dependencies to the project

Let’s say we want to use the standard library package `Random` and the registered package `JSON` in our project.
We simply `add` these packages (note how the prompt now shows the name of the newly generated project,
since we `activate`d it):

```julia-repl
(HelloWorld) pkg> add Random JSON
   Resolving package versions...
    Updating `~/HelloWorld/Project.toml`
  [682c06a0] + JSON v0.21.3
  [9a3f8284] + Random
    Updating `~/HelloWorld/Manifest.toml`
  [682c06a0] + JSON v0.21.3
  [69de0a69] + Parsers v2.4.0
  [ade2ca70] + Dates
 ...
```

Both `Random` and `JSON` got added to the project’s `Project.toml` file, and the resulting dependencies got added to the `Manifest.toml` file.
The resolver has installed each package with the highest possible version, while still respecting the compatibility that each package enforces on its dependencies.

We can now use both `Random` and `JSON` in our project. Changing `src/HelloWorld.jl` to

```julia
module HelloWorld

import Random
import JSON

greet() = print("Hello World!")
greet_alien() = print("Hello ", Random.randstring(8))

end # module
```

and reloading the package, the new `greet_alien` function that uses `Random` can be called:

```julia-repl
julia> HelloWorld.greet_alien()
Hello aT157rHV
```

## Defining a public API

If you want your package to be useful to other packages and you want folks to be able to
easily update to newer version of your package when they come out, it is important to
document what behavior will stay consistent across updates.

Unless you note otherwise, the public API of your package is defined as all the behavior you
describe about public symbols. A public symbol is a symbol that is exported from your
package with the `export` keyword or marked as public with the `public` keyword. When you
change the behavior of something that was previously public so that the new
version no longer conforms to the specifications provided in the old version, you should
adjust your package version number according to [Julia's variant on SemVer](#Version-specifier-format).
If you would like to include a symbol in your public API without exporting it into the
global namespace of folks who call `using YourPackage`, you should mark that symbol as
public with `public that_symbol`. Symbols marked as public with the `public` keyword are
just as public as those marked as public with the `export` keyword, but when folks call
`using YourPackage`, they will still have to qualify access to those symbols with
`YourPackage.that_symbol`.

Let's say we would like our `greet` function to be part of the public API, but not the
`greet_alien` function. We could the write the following and release it as version `1.0.0`.

```julia
module HelloWorld

export greet

import Random
import JSON

"Writes a friendly message."
greet() = print("Hello World!")

"Greet an alien by a randomly generated name."
greet_alien() = print("Hello ", Random.randstring(8))

end # module
```

Then, if we change `greet` to

```julia
"Writes a friendly message that is exactly three words long."
greet() = print("Hello Lovely World!")
```

We would release the new version as `1.1.0`. This is not breaking
because the new implementation conforms to the old documentation, but
it does add a new feature, that the message must be three words long.

Later, we may wish to change `greet_alien` to

```julia
"Greet an alien by the name of \"Zork\"."
greet_alien() = print("Hello Zork")
```

And also export it by changing

```julia
export greet
```

to

```julia
export greet, greet_alien
```

We should release this new version as `1.2.0` because it adds a new feature
`greet_alien` to the public API. Even though `greet_alien` was documented before
and the new version does not conform to the old documentation, this is not breaking
because the old documentation was not attached to a symbol that was exported
at the time so that documentation does not apply across released versions.

However, if we now wish to change `greet` to

```julia
"Writes a friendly message that is exactly four words long."
greet() = print("Hello very lovely world")
```

we would need to release the new version as `2.0.0`. In version `1.1.0`, we specified that
the greeting would be three words long, and because `greet` was exported, that description
also applies to all future versions until the next breaking release. Because this new
version does not conform to the old specification, it must be tagged as a breaking change.

Please note that version numbers are free and unlimited. It is okay to use lots of them
(e.g. version `6.62.8`).

## Adding a build step to the package

The build step is executed the first time a package is installed or when explicitly invoked with `build`.
A package is built by executing the file `deps/build.jl`.

```julia-repl
julia> mkpath("deps");

julia> write("deps/build.jl",
             """
             println("I am being built...")
             """);

(HelloWorld) pkg> build
  Building HelloWorld → `deps/build.log`
 Resolving package versions...

julia> print(readchomp("deps/build.log"))
I am being built...
```

If the build step fails, the output of the build step is printed to the console

```julia-repl
julia> write("deps/build.jl",
             """
             error("Ooops")
             """);

(HelloWorld) pkg> build
    Building HelloWorld → `~/HelloWorld/deps/build.log`
ERROR: Error building `HelloWorld`:
ERROR: LoadError: Ooops
Stacktrace:
 [1] error(s::String)
   @ Base ./error.jl:35
 [2] top-level scope
   @ ~/HelloWorld/deps/build.jl:1
 [3] include(fname::String)
   @ Base.MainInclude ./client.jl:476
 [4] top-level scope
   @ none:5
in expression starting at /home/kc/HelloWorld/deps/build.jl:1
```

!!! warning
    A build step should generally not create or modify any files in the package directory. If you need to store some files
    from the build step, use the [Scratch.jl](https://github.com/JuliaPackaging/Scratch.jl) package.

## [Adding tests to the package](@id adding-tests-to-packages)

When a package is tested the file `test/runtests.jl` is executed:

```julia-repl
julia> mkpath("test");

julia> write("test/runtests.jl",
             """
             println("Testing...")
             """);

(HelloWorld) pkg> test
   Testing HelloWorld
 Resolving package versions...
Testing...
   Testing HelloWorld tests passed
```

Tests are run in a new Julia process, where the package itself, and any
test-specific dependencies, are available, see below.


!!! warning
    Tests should generally not create or modify any files in the package directory. If you need to store some files
    from the build step, use the [Scratch.jl](https://github.com/JuliaPackaging/Scratch.jl) package.

### Test-specific dependencies

There are two ways of adding test-specific dependencies (dependencies that are not dependencies of the package but will still be available to
load when the package is tested).

#### `target` based test specific dependencies

Using this method of adding test-specific dependencies, the packages are added under an `[extras]` section and to a test target,
e.g. to add `Markdown` and `Test` as test dependencies, add the following to the `Project.toml` file:

```toml
[extras]
Markdown = "d6f4376e-aef5-505a-96c1-9c027394607a"
Test = "8dfed614-e22c-5e08-85e1-65c5234f0b40"

[targets]
test = ["Markdown", "Test"]
```

Note that the only supported targets are `test` and `build`, the latter of which (not recommended) can be used
for any `deps/build.jl` scripts.

#### Alternative approach: `test/Project.toml` file test specific dependencies

!!! note
    The exact interaction between `Project.toml`, `test/Project.toml` and their corresponding
    `Manifest.toml`s are not fully worked out and may be subject to change in future versions.
    The older method of adding test-specific dependencies, described in the previous section,
    will therefore be supported throughout all Julia 1.X releases.

In Julia 1.2 and later test dependencies can be declared in `test/Project.toml`. When running
tests, Pkg will automatically merge this and the package Projects to create the test environment.

!!! note
    If no `test/Project.toml` exists Pkg will use the `target` based test specific dependencies.

To add a test-specific dependency, i.e. a dependency that is available only when testing,
it is thus enough to add this dependency to the `test/Project.toml` project. This can be
done from the Pkg REPL by activating this environment, and then use `add` as one normally
does. Let's add the `Test` standard library as a test dependency:

```julia-repl
(HelloWorld) pkg> activate ./test
[ Info: activating environment at `~/HelloWorld/test/Project.toml`.

(test) pkg> add Test
 Resolving package versions...
  Updating `~/HelloWorld/test/Project.toml`
  [8dfed614] + Test
  Updating `~/HelloWorld/test/Manifest.toml`
  [...]
```

We can now use `Test` in the test script and we can see that it gets installed when testing:

```julia-repl
julia> write("test/runtests.jl",
             """
             using Test
             @test 1 == 1
             """);

(test) pkg> activate .

(HelloWorld) pkg> test
   Testing HelloWorld
 Resolving package versions...
  Updating `/var/folders/64/76tk_g152sg6c6t0b4nkn1vw0000gn/T/tmpPzUPPw/Project.toml`
  [d8327f2a] + HelloWorld v0.1.0 [`~/.julia/dev/Pkg/HelloWorld`]
  [8dfed614] + Test
  Updating `/var/folders/64/76tk_g152sg6c6t0b4nkn1vw0000gn/T/tmpPzUPPw/Manifest.toml`
  [d8327f2a] + HelloWorld v0.1.0 [`~/.julia/dev/Pkg/HelloWorld`]
   Testing HelloWorld tests passed```
```

## Compatibility on dependencies

Every dependency should in general have a compatibility constraint on it.
This is an important topic so there is a chapter in the package docs about it:
[Compatibility](https://pkgdocs.julialang.org/v1/compatibility).

## Weak dependencies

!!! note
    This is a somewhat advanced usage of Pkg which can be skipped for people new to Julia and Julia packages.

!!! compat
    The described feature requires Julia 1.9+.

A weak dependency is a dependency that will not automatically install when the package is installed but
you can still control what versions of that package are allowed to be installed by setting compatibility on it.
These are listed in the project file under the `[weakdeps]` section:

```toml
[weakdeps]
SomePackage = "b3785f31-9d33-4cdf-bc73-f646780f1739"

[compat]
SomePackage = "1.2"
```

The current usage of this is almost solely limited to "extensions" which is described in the next section.

## Conditional loading of code in packages (Extensions)

!!! note
    This is a somewhat advanced usage of Pkg which can be skipped for people new to Julia and Julia packages.

!!! compat
    The described feature requires Julia 1.9+.

Sometimes one wants to make two or more packages work well together, but may be reluctant (perhaps due to increased load times) to make one an unconditional dependency of the other.
A package *extension* is a module in a file (similar to a package) that is automatically loaded when *some other set of packages* are
loaded into the Julia session. This is very similar to functionality that the external package
[Requires.jl](https://github.com/JuliaPackaging/Requires.jl) provides, but which is now available directly through Julia,
and provides added benefits such as being able to precompile the extension.

### Code structure

A useful application of extensions could be for a plotting package that should be able to plot
objects from a wide variety of different Julia packages.
Adding all those different Julia packages as dependencies of the plotting package
could be expensive since they would end up getting loaded even if they were never used.
Instead, the code required to plot objects for specific packages can be put into separate files
(extensions) and these are loaded only when the packages that define the type(s) we want to plot
are loaded.

Below is an example of how the code can be structured for a use case in which a
`Plotting` package wants to be able to display objects defined in the external package `Contour`.
The file and folder structure shown below is found in the `Plotting` package.

 `Project.toml`:
 ```toml
name = "Plotting"
version = "0.1.0"
uuid = "..."

[weakdeps]
Contour = "d38c429a-6771-53c6-b99e-75d170b6e991"

[extensions]
# name of extension to the left
# extension dependencies required to load the extension to the right
# use a list for multiple extension dependencies
PlottingContourExt = "Contour"

[compat]
Contour = "0.6.2"
```

`src/Plotting.jl`:
```julia
module Plotting

function plot(x::Vector)
    # Some functionality for plotting a vector here
end

end # module
```

`ext/PlottingContourExt.jl` (can also be in `ext/PlottingContourExt/PlottingContourExt.jl`):
```julia
module PlottingContourExt # Should be same name as the file (just like a normal package)

using Plotting, Contour

function Plotting.plot(c::Contour.ContourCollection)
    # Some functionality for plotting a contour here
end

end # module
```

Extensions can have any arbitrary name (here `PlottingContourExt`), but using something similar to the format of
this example that makes the extended functionality and dependency of the extension clear is likely a good idea.

!!! compat
    Often you will put the extension dependencies into the `test` target so they are loaded when running e.g. `Pkg.test()`. On earlier Julia versions
    this requires you to also put the package in the `[extras]` section. This is unfortunate but the project verifier on older Julia versions will
    complain if this is not done.

!!! note
    If you use a manifest generated by a Julia version that does not know about extensions with a Julia version that does
    know about them, the extensions will not load. This is because the manifest lacks some information that tells Julia
    when it should load these packages. So make sure you use a manifest generated at least the Julia version you are using.

### Behavior of extensions

A user that depends only on `Plotting` will not pay the cost of the "extension" inside the `PlottingContourExt` module.
It is only when the `Contour` package actually gets loaded that the `PlottingContourExt` extension is loaded too
and provides the new functionality.

In our example, the new functionality is an additional _method_, which we add to an existing _function_ from the parent package `Plotting`.
Implementing such methods is among the most standard use cases of package extensions.
Within the parent package, the function to extend can even be defined with zero methods, as follows:

```julia
function plot end
```

!!! note
    If one considers `PlottingContourExt` as a completely separate package, it could be argued that defining `Plotting.plot(c::Contour.ContourCollection)` is
    [type piracy](@ref avoid-type-piracy) since `PlottingContourExt` _owns_ neither the function `Plotting.plot` nor the type `Contour.ContourCollection`.
    However, for extensions, it is ok to assume that the extension owns the functions in its parent package.

In other situations, one may need to define new symbols in the extension (types, structs, functions, etc.) instead of reusing those from the parent package.
Such symbols are created in a separate module corresponding to the extension, namely `PlottingContourExt`, and thus not in `Plotting` itself.
If extension symbols are needed in the parent package, one must call `Base.get_extension` to retrieve them.
Here is an example showing how a custom type defined in `PlottingContourExt` can be accessed in `Plotting`:

```julia
ext = Base.get_extension(@__MODULE__, :PlottingContourExt)
if !isnothing(ext)
    ContourPlotType = ext.ContourPlotType
end
```

On the other hand, accessing extension symbols from a third-party package (i.e. not the parent) is not a recommended practice at the moment.

### Backwards compatibility

This section discusses various methods for using extensions on Julia versions that support them,
while simultaneously providing similar functionality on older Julia versions.

#### Requires.jl

This section is relevant if you are currently using Requires.jl but want to transition to using extensions (while still having Requires be used on Julia versions that do not support extensions).
This is done by making the following changes (using the example above):

- Add the following to the package file. This makes it so that Requires.jl loads and inserts the
  callback only when extensions are not supported
  ```julia
  # This symbol is only defined on Julia versions that support extensions
  if !isdefined(Base, :get_extension)
  using Requires
  end

  @static if !isdefined(Base, :get_extension)
  function __init__()
      @require Contour = "d38c429a-6771-53c6-b99e-75d170b6e991" include("../ext/PlottingContourExt.jl")
  end
  end
  ```
  or if you have other things in your `__init__()` function:
  ```julia
  if !isdefined(Base, :get_extension)
  using Requires
  end

  function __init__()
      # Other init functionality here

      @static if !isdefined(Base, :get_extension)
          @require Contour = "d38c429a-6771-53c6-b99e-75d170b6e991" include("../ext/PlottingContourExt.jl")
      end
  end
  ```
- Make the following change in the conditionally-loaded code:
  ```julia
  isdefined(Base, :get_extension) ? (using Contour) : (using ..Contour)
  ```
- Add `Requires` to `[weakdeps]` in your `Project.toml` file, so that it is listed in both `[deps]` and `[weakdeps]`.
  Julia 1.9+ knows to not install it as a regular dependency, whereas earlier versions will consider it a dependency.

The package should now work with Requires.jl on Julia versions before extensions were introduced
and with extensions on more recent Julia versions.

####  Transition from normal dependency to extension

This section is relevant if you have a normal dependency that you want to transition be an extension (while still having the dependency be a normal dependency on Julia versions that do not support extensions).
This is done by making the following changes (using the example above):

- Make sure that the package is **both** in the `[deps]` and `[weakdeps]` section. Newer Julia versions will ignore dependencies in `[deps]` that are also in `[weakdeps]`.
- Add the following to your main package file (typically at the bottom):
  ```julia
  if !isdefined(Base, :get_extension)
    include("../ext/PlottingContourExt.jl")
  end
  ```

#### Using an extension while supporting older Julia versions

In the case where one wants to use an extension (without worrying about the
feature of the extension being available on older Julia versions) while still
supporting older Julia versions the packages under `[weakdeps]` should be
duplicated into `[extras]`. This is an unfortunate duplication, but without
doing this the project verifier under older Julia versions will throw an error
if it finds packages under `[compat]` that is not listed in `[extras]`.

## Package naming guidelines

Package names should be sensible to most Julia users, *even to those who are not domain experts*.
The following guidelines apply to the `General` registry but may be useful for other package
registries as well.

Since the `General` registry belongs to the entire community, people may have opinions about
your package name when you publish it, especially if it's ambiguous or can be confused with
something other than what it is. Usually, you will then get suggestions for a new name that
may fit your package better.

1. Avoid jargon. In particular, avoid acronyms unless there is minimal possibility of confusion.

     * It's ok to say `USA` if you're talking about the USA.
     * It's not ok to say `PMA`, even if you're talking about positive mental attitude.
2. Avoid using `Julia` in your package name or prefixing it with `Ju`.

     * It is usually clear from context and to your users that the package is a Julia package.
     * Package names already have a `.jl` extension, which communicates to users that `Package.jl` is a Julia package.
     * Having Julia in the name can imply that the package is connected to, or endorsed by, contributors
       to the Julia language itself.
3. Packages that provide most of their functionality in association with a new type should have pluralized
   names.

     * `DataFrames` provides the `DataFrame` type.
     * `BloomFilters` provides the `BloomFilter` type.
     * In contrast, `JuliaParser` provides no new type, but instead new functionality in the `JuliaParser.parse()`
       function.
4. Err on the side of clarity, even if clarity seems long-winded to you.

     * `RandomMatrices` is a less ambiguous name than `RndMat` or `RMT`, even though the latter are shorter.
5. A less systematic name may suit a package that implements one of several possible approaches to
   its domain.

     * Julia does not have a single comprehensive plotting package. Instead, `Gadfly`, `PyPlot`, `Winston`
       and other packages each implement a unique approach based on a particular design philosophy.
     * In contrast, `SortingAlgorithms` provides a consistent interface to use many well-established
       sorting algorithms.
6. Packages that wrap external libraries or programs should be named after those libraries or programs.

     * `CPLEX.jl` wraps the `CPLEX` library, which can be identified easily in a web search.
     * `MATLAB.jl` provides an interface to call the MATLAB engine from within Julia.
7. Avoid naming a package closely to an existing package
     * `Websocket` is too close to `WebSockets` and can be confusing to users. Rather use a new name such as `SimpleWebsockets`.

## Registering packages

Once a package is ready it can be registered with the [General Registry](https://github.com/JuliaRegistries/General#registering-a-package-in-general) (see also the [FAQ](https://github.com/JuliaRegistries/General#faq)).
Currently, packages are submitted via [`Registrator`](https://juliaregistrator.github.io/).
In addition to `Registrator`, [`TagBot`](https://github.com/marketplace/actions/julia-tagbot) helps manage the process of tagging releases.

## Best Practices

Packages should avoid mutating their own state (writing to files within their package directory).
Packages should, in general, not assume that they are located in a writable location (e.g. if installed as part of a system-wide depot) or even a stable one (e.g. if they are bundled into a system image by [PackageCompiler.jl](https://github.com/JuliaLang/PackageCompiler.jl)).
To support the various use cases in the Julia package ecosystem, the Pkg developers have created a number of auxiliary packages and techniques to help package authors create self-contained, immutable, and relocatable packages:

* [`Artifacts`](https://pkgdocs.julialang.org/v1/artifacts/) can be used to bundle chunks of data alongside your package, or even allow them to be downloaded on-demand.
  Prefer artifacts over attempting to open a file via a path such as `joinpath(@__DIR__, "data", "my_dataset.csv")` as this is non-relocatable.
  Once your package has been precompiled, the result of `@__DIR__` will have been baked into your precompiled package data, and if you attempt to distribute this package, it will attempt to load files at the wrong location.
  Artifacts can be bundled and accessed easily using the `artifact"name"` string macro.

* [`Scratch.jl`](https://github.com/JuliaPackaging/Scratch.jl) provides the notion of "scratch spaces", mutable containers of data for packages.
  Scratch spaces are designed for data caches that are completely managed by a package and should be removed when the package itself is uninstalled.
  For important user-generated data, packages should continue to write out to a user-specified path that is not managed by Julia or Pkg.

* [`Preferences.jl`](https://github.com/JuliaPackaging/Preferences.jl) allows packages to read and write preferences to the top-level `Project.toml`.
  These preferences can be read at runtime or compile-time, to enable or disable different aspects of package behavior.
  Packages previously would write out files to their own package directories to record options set by the user or environment, but this is highly discouraged now that `Preferences` is available.
