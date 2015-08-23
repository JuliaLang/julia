# This file is a part of Julia. License is MIT: http://julialang.org/license

module Pkg

export Git, Dir, GitHub, Types, Reqs, Cache, Read, Query, Resolve, Write, Generate, Entry
export dir, init, rm, add, available, installed, status, clone, checkout,
       update, resolve, register, tag, publish, generate, test,
       build, free, pin

const DEFAULT_META = "git://github.com/JuliaLang/METADATA.jl"
const META_BRANCH = "metadata-v2"

for file in split("git dir github types reqs cache read query resolve write generate entry")
    include("pkg/$file.jl")
end
const cd = Dir.cd

doc"""
    dir() -> AbstractString

Returns the absolute path of the package directory. This defaults to
`joinpath(homedir(),".julia","v$(VERSION.major).$(VERSION.minor)")` on all platforms (i.e.
`~/.julia/v0.4` in UNIX shell syntax). If the `JULIA_PKGDIR` environment variable is set,
then that path is used in the returned value as
`joinpath(ENV["JULIA_PKGDIR"],"v$(VERSION.major).$(VERSION.minor)")`. If `JULIA_PKGDIR` is
a relative path, it is interpreted relative to whatever the current working directory is.

    dir(names...) -> AbstractString

Equivalent to `normpath(Pkg.dir(),names...)` – i.e. it appends path components to the
package directory and normalizes the resulting path. In particular, `Pkg.dir(pkg)` returns
the path to the package `pkg`.
"""
dir(path...) = Dir.path(path...)

"""
    init(meta = DEFAULT_META, branch = META_BRANCH)

Initialize `Pkg.dir()` as a package directory. This will be done automatically when the
`JULIA_PKGDIR` is not set and `Pkg.dir()` uses its default value. As part of this process,
clones a local METADATA git repository from the site and branch specified by its arguments,
which are typically not provided. Explicit (non-default) arguments can be used to support a
custom METADATA setup.
"""
init(meta::AbstractString=DEFAULT_META, branch::AbstractString=META_BRANCH) = Dir.init(meta,branch)

"""
    edit()

Opens `Pkg.dir("REQUIRE")` in the editor specified by the `VISUAL` or `EDITOR` environment
variables; when the editor command returns, it runs `Pkg.resolve()` to determine and install
a new optimal set of installed package versions.
"""
edit() = cd(Entry.edit)

"""
    rm(pkg)

Remove all requirement entries for `pkg` from `Pkg.dir("REQUIRE")` and call `Pkg.resolve()`.
"""
rm(pkg::AbstractString) = cd(Entry.rm,pkg)

"""
    add(pkg, vers...)

Add a requirement entry for `pkg` to `Pkg.dir("REQUIRE")` and call `Pkg.resolve()`. If
`vers` are given, they must be `VersionNumber` objects and they specify acceptable version
intervals for `pkg`.
"""
add(pkg::AbstractString, vers::VersionNumber...) = cd(Entry.add,pkg,vers...)

"""
    available() -> Vector{ASCIIString}

Returns the names of available packages.
"""
available() = cd(Entry.available)

"""
    available(pkg) -> Vector{VersionNumber}

Returns the version numbers available for package `pkg`.
"""
available(pkg::AbstractString) = cd(Entry.available,pkg)

"""
    installed() -> Dict{ASCIIString,VersionNumber}

Returns a dictionary mapping installed package names to the installed version number of each
package.
"""
installed() = cd(Entry.installed)

"""
    installed(pkg) -> Void | VersionNumber

If `pkg` is installed, return the installed version number, otherwise return `nothing`.
"""
installed(pkg::AbstractString) = cd(Entry.installed,pkg)

"""
    status()

Prints out a summary of what packages are installed and what version and state they're in.
"""
status(io::IO=STDOUT) = cd(Entry.status,io)
status(pkg::AbstractString = "", io::IO=STDOUT) = cd(Entry.status,io,pkg)

"""
    clone(url, [pkg])

Clone a package directly from the git URL `url`. The package does not need to be a
registered in `Pkg.dir("METADATA")`. The package repo is cloned by the name `pkg` if
provided; if not provided, `pkg` is determined automatically from `url`.

    clone(pkg)

If `pkg` has a URL registered in `Pkg.dir("METADATA")`, clone it from that URL on the
default branch. The package does not need to have any registered versions.
"""
clone(url_or_pkg::AbstractString) = cd(Entry.clone,url_or_pkg)
clone(url::AbstractString, pkg::AbstractString) = cd(Entry.clone,url,pkg)

"""
    checkout(pkg, [branch="master"])

Checkout the `Pkg.dir(pkg)` repo to the branch `branch`. Defaults to checking out the
"master" branch. To go back to using the newest compatible released version, use
`Pkg.free(pkg)`.
"""
checkout(pkg::AbstractString, branch::AbstractString="master"; merge::Bool=true, pull::Bool=true) =
    cd(Entry.checkout,pkg,branch,merge,pull)

"""
    free(pkg)

Free the package `pkg` to be managed by the package manager again. It calls `Pkg.resolve()`
to determine optimal package versions after. This is an inverse for both `Pkg.checkout` and
`Pkg.pin`.

You can also supply an iterable collection of package names, e.g., `Pkg.free(("Pkg1",
"Pkg2"))` to free multiple packages at once.
"""
free(pkg) = cd(Entry.free,pkg)

"""
    pin(pkg)

Pin `pkg` at the current version. To go back to using the newest compatible released
version, use `Pkg.free(pkg)`
"""
pin(pkg::AbstractString) = cd(Entry.pin,pkg)

"""
    pin(pkg, version)

Pin `pkg` at registered version `version`.
"""
pin(pkg::AbstractString, ver::VersionNumber) = cd(Entry.pin,pkg,ver)

"""
    update()

Update package the metadata repo – kept in `Pkg.dir("METADATA")` – then update any fixed
packages that can safely be pulled from their origin; then call `Pkg.resolve()` to determine
a new optimal set of packages versions.
"""
update() = cd(Entry.update,Dir.getmetabranch())

"""
    resolve()

Determines an optimal, consistent set of package versions to install or upgrade to. The
optimal set of package versions is based on the contents of `Pkg.dir("REQUIRE")` and the
state of installed packages in `Pkg.dir()`, Packages that are no longer required are moved
into `Pkg.dir(".trash")`.
"""
resolve() = cd(Entry.resolve)

"""
    register(pkg, [url])

Register `pkg` at the git URL `url`, defaulting to the configured origin URL of the git repo
`Pkg.dir(pkg)`.
"""
register(pkg::AbstractString) = cd(Entry.register,pkg)
register(pkg::AbstractString, url::AbstractString) = cd(Entry.register,pkg,url)

"""
    tag(pkg, [ver, [commit]])

Tag `commit` as version `ver` of package `pkg` and create a version entry in `METADATA`. If
not provided, `commit` defaults to the current commit of the `pkg` repo. If `ver` is one of
the symbols `:patch`, `:minor`, `:major` the next patch, minor or major version is used. If
`ver` is not provided, it defaults to `:patch`.
"""
tag(pkg::AbstractString, sym::Symbol=:patch) = cd(Entry.tag,pkg,sym)
tag(pkg::AbstractString, sym::Symbol, commit::AbstractString) = cd(Entry.tag,pkg,sym,false,commit)

tag(pkg::AbstractString, ver::VersionNumber; force::Bool=false) = cd(Entry.tag,pkg,ver,force)
tag(pkg::AbstractString, ver::VersionNumber, commit::AbstractString; force::Bool=false) =
    cd(Entry.tag,pkg,ver,force,commit)

submit(pkg::AbstractString) = cd(Entry.submit,pkg)
submit(pkg::AbstractString, commit::AbstractString) = cd(Entry.submit,pkg,commit)

"""
    publish()

For each new package version tagged in `METADATA` not already published, make sure that the
tagged package commits have been pushed to the repo at the registered URL for the package
and if they all have, open a pull request to `METADATA`.
"""
publish() = cd(Entry.publish,Dir.getmetabranch())

"""
    build()

Run the build scripts for all installed packages in depth-first recursive order.
"""
build() = cd(Entry.build)

"""
    build(pkgs...)

Run the build script in "deps/build.jl" for each package in `pkgs` and all of their
dependencies in depth-first recursive order. This is called automatically by `Pkg.resolve()`
on all installed or updated packages.
"""
build(pkgs::AbstractString...) = cd(Entry.build,[pkgs...])

doc"""
    generate(pkg,license)

Generate a new package named `pkg` with one of these license keys: `"MIT"`, `"BSD"` or
`"ASL"`. If you want to make a package with a different license, you can edit it afterwards.
Generate creates a git repo at `Pkg.dir(pkg)` for the package and inside it `LICENSE.md`,
`README.md`, `REQUIRE`, the julia entrypoint `$pkg/src/$pkg.jl`, and Travis and AppVeyor CI
configuration files `.travis.yml` and `appveyor.yml`.
"""
generate(pkg::AbstractString, license::AbstractString; force::Bool=false, authors::Union{AbstractString,Array} = [], config::Dict=Dict()) =
    cd(Generate.package,pkg,license,force=force,authors=authors,config=config)

"""
    test()

Run the tests for all installed packages ensuring that each package's test dependencies are
installed for the duration of the test. A package is tested by running its
`test/runtests.jl` file and test dependencies are specified in `test/REQUIRE`.
"""
test(;coverage::Bool=false) = cd(Entry.test; coverage=coverage)

"""
    test(pkgs...)

Run the tests for each package in `pkgs` ensuring that each package's test dependencies are
installed for the duration of the test. A package is tested by running its
`test/runtests.jl` file and test dependencies are specified in `test/REQUIRE`.
"""
test(pkgs::AbstractString...; coverage::Bool=false) = cd(Entry.test,AbstractString[pkgs...]; coverage=coverage)

"""
    Pkg.dependents("PackageName")

Find the direct dependents of the given package, i.e. all packages which include `MyPackage`
in their `REQUIRE` file.
"""
dependents(packagename::AbstractString) = Reqs.dependents(packagename)

end # module
