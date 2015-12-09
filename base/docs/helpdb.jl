# This file is a part of Julia. License is MIT: http://julialang.org/license

import .Docs: keywords

include("helpdb/BLAS.jl")
include("helpdb/Libdl.jl")
include("helpdb/Libc.jl")
include("helpdb/Collections.jl")
include("helpdb/Profile.jl")
include("helpdb/Cartesian.jl")
include("helpdb/Base.jl")
include("helpdb/Dates.jl")

# Base.Pkg

doc"""
    build()

Run the build scripts for all installed packages in depth-first recursive order.
"""
Pkg.build()

doc"""
    build(pkgs...)

Run the build script in `deps/build.jl` for each package in `pkgs` and all of their dependencies in depth-first recursive order. This is called automatically by `Pkg.resolve()` on all installed or updated packages.
"""
Pkg.build(pkgs...)

doc"""
    init(meta::AbstractString=DEFAULT_META, branch::AbstractString=META_BRANCH)

Initialize `Pkg.dir()` as a package directory. This will be done automatically when the `JULIA_PKGDIR` is not set and `Pkg.dir()` uses its default value. As part of this process, clones a local METADATA git repository from the site and branch specified by its arguments, which are typically not provided. Explicit (non-default) arguments can be used to support a custom METADATA setup.
"""
Pkg.init()

doc"""
    pin(pkg)

Pin `pkg` at the current version. To go back to using the newest compatible released version, use `Pkg.free(pkg)`
"""
Pkg.pin(pkg)

doc"""
    pin(pkg, version)

Pin `pkg` at registered version `version`.
"""
Pkg.pin(pkg, version)

doc"""
    resolve()

Determines an optimal, consistent set of package versions to install or upgrade to. The optimal set of package versions is based on the contents of `Pkg.dir("REQUIRE")` and the state of installed packages in `Pkg.dir()`, Packages that are no longer required are moved into `Pkg.dir(".trash")`.
"""
Pkg.resolve()

doc"""
    available() -> Vector{ASCIIString}

Returns the names of available packages.
"""
Pkg.available()

doc"""
    available(pkg) -> Vector{VersionNumber}

Returns the version numbers available for package `pkg`.
"""
Pkg.available(pkg)

doc"""
    rm(pkg)

Remove all requirement entries for `pkg` from `Pkg.dir("REQUIRE")` and call `Pkg.resolve()`.
"""
Pkg.rm(pkg)

doc"""
    free(pkg)

Free the package `pkg` to be managed by the package manager again. It calls `Pkg.resolve()` to determine optimal package versions after. This is an inverse for both `Pkg.checkout` and `Pkg.pin`.

You can also supply an iterable collection of package names, e.g., `Pkg.free(("Pkg1", "Pkg2"))` to free multiple packages at once.
"""
Pkg.free()

doc"""
    status()

Prints out a summary of what packages are installed and what version and state they're in.
"""
Pkg.status

doc"""
    edit()

Opens `Pkg.dir("REQUIRE")` in the editor specified by the `VISUAL` or `EDITOR` environment variables; when the editor command returns, it runs `Pkg.resolve()` to determine and install a new optimal set of installed package versions.
"""
Pkg.edit()

doc"""
    clone(url, [pkg])

Clone a package directly from the git URL `url`. The package does not need to be a registered in `Pkg.dir("METADATA")`. The package repo is cloned by the name `pkg` if provided; if not provided, `pkg` is determined automatically from `url`.
"""
Pkg.clone(url,?)

doc"""
    clone(pkg)

If `pkg` has a URL registered in `Pkg.dir("METADATA")`, clone it from that URL on the default branch. The package does not need to have any registered versions.
"""
Pkg.clone(pkg)

doc"""
    checkout(pkg, [branch="master"]; merge=true, pull=true)

Checkout the `Pkg.dir(pkg)` repo to the branch `branch`. Defaults to checking out the "master" branch. To go back to using the newest compatible released version, use `Pkg.free(pkg)`. Changes are merged (fast-forward only) if the keyword argument `merge == true`, and the latest version is pulled from the upsream repo if `pull == true`.
"""
Pkg.checkout(pkg)

doc"""
    update()

Update package the metadata repo – kept in `Pkg.dir("METADATA")` – then update any fixed packages that can safely be pulled from their origin; then call `Pkg.resolve()` to determine a new optimal set of packages versions.
"""
Pkg.update

doc"""
    add(pkg, vers...)

Add a requirement entry for `pkg` to `Pkg.dir("REQUIRE")` and call `Pkg.resolve()`. If `vers` are given, they must be `VersionNumber` objects and they specify acceptable version intervals for `pkg`.
"""
Pkg.add(pkg, vers...)

doc"""
    test()

Run the tests for all installed packages ensuring that each package's test dependencies are installed for the duration of the test. A package is tested by running its `test/runtests.jl` file and test dependencies are specified in `test/REQUIRE`.
"""
Pkg.test()

doc"""
    test(pkgs...)

Run the tests for each package in `pkgs` ensuring that each package's test dependencies are installed for the duration of the test. A package is tested by running its `test/runtests.jl` file and test dependencies are specified in `test/REQUIRE`.
"""
Pkg.test(pkgs...)

doc"""
    dir() -> AbstractString

Returns the absolute path of the package directory. This defaults to `joinpath(homedir(),".julia","v$(VERSION.major).$(VERSION.minor)")` on all platforms (i.e. `~/.julia/v0.4` in UNIX shell syntax). If the `JULIA_PKGDIR` environment variable is set, then that path is used in the returned value as `joinpath(ENV["JULIA_PKGDIR"],"v$(VERSION.major).$(VERSION.minor)")`. If `JULIA_PKGDIR` is a relative path, it is interpreted relative to whatever the current working directory is.
"""
Pkg.dir()

doc"""
    dir(names...) -> AbstractString

Equivalent to `normpath(Pkg.dir(),names...)` – i.e. it appends path components to the package directory and normalizes the resulting path. In particular, `Pkg.dir(pkg)` returns the path to the package `pkg`.
"""
Pkg.dir(names...)

doc"""
    installed() -> Dict{ASCIIString,VersionNumber}

Returns a dictionary mapping installed package names to the installed version number of each package.
"""
Pkg.installed()

doc"""
    installed(pkg) -> Void | VersionNumber

If `pkg` is installed, return the installed version number, otherwise return `nothing`.
"""
Pkg.installed(pkg)

doc"""
    randjump(r::MersenneTwister, jumps, [jumppoly]) -> Vector{MersenneTwister}

Create an array of the size `jumps` of initialized `MersenneTwister` RNG objects where the first RNG object given as a parameter and following `MersenneTwister` RNGs in the array initialized such that a state of the RNG object in the array would be moved forward (without generating numbers) from a previous RNG object array element on a particular number of steps encoded by the jump polynomial `jumppoly`.

Default jump polynomial moves forward `MersenneTwister` RNG state by 10^20 steps.
"""
randjump

doc"""
```rst
..  \:(start, [step], stop)

Range operator. ``a:b`` constructs a range from ``a`` to ``b`` with a step size of 1, and ``a:s:b`` is similar but uses a step size of ``s``. These syntaxes call the function ``colon``.
The colon is also used in indexing to select whole dimensions.
```
"""
colon(start, step, stop)

doc"""
```rst
..  $(x, y)

Bitwise exclusive or
```
"""
Base.(:$)(x, y)

doc"""
    getsockname(sock::Union{TCPServer, TCPSocket}) -> (IPAddr,UInt16)

Get the IP address and the port that the given TCP socket is connected to (or bound to, in the case of TCPServer).
"""
getsockname

doc"""
    Base.remoteref_id(r::AbstractRemoteRef) -> (whence, id)

A low-level API which returns the unique identifying tuple for a remote reference. A reference id is a tuple of two
elements - pid where the reference was created from and a one-up number from that node.
"""
Base.remoteref_id

doc"""
    Base.channel_from_id(refid) -> c

A low-level API which returns the backing AbstractChannel for an id returned by `remoteref_id`. The call is valid only on the node where the backing channel exists.
"""
Base.channel_from_id

doc"""
    Base.worker_id_from_socket(s::IO) -> pid

A low-level API which given a `IO` connection, returns the pid of the worker it is connected to. This is useful when writing custom `serialize` methods for a type, which
optimizes the data written out depending on the receiving process id.
"""
Base.worker_id_from_socket



