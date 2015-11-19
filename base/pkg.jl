# This file is a part of Julia. License is MIT: http://julialang.org/license

module Pkg

export Dir, Types, Reqs, Cache, Read, Query, Resolve, Write, Entry, Git
export dir, init, rm, add, available, installed, status, clone, checkout,
       update, resolve, test, build, free, pin, PkgError, setprotocol!

const DEFAULT_META = "https://github.com/JuliaLang/METADATA.jl"
const META_BRANCH = "metadata-v2"

type PkgError <: Exception
    msg::AbstractString
end

for file in split("dir types reqs cache read query resolve write entry git")
    include("pkg/$file.jl")
end
const cd = Dir.cd

dir(path...) = Dir.path(path...)
init(meta::AbstractString=DEFAULT_META, branch::AbstractString=META_BRANCH) = Dir.init(meta,branch)

edit() = cd(Entry.edit)
rm(pkg::AbstractString) = cd(Entry.rm,pkg)
add(pkg::AbstractString, vers::VersionNumber...) = cd(Entry.add,pkg,vers...)

available() = cd(Entry.available)
available(pkg::AbstractString) = cd(Entry.available,pkg)

installed() = cd(Entry.installed)
installed(pkg::AbstractString) = cd(Entry.installed,pkg)

status(io::IO=STDOUT) = cd(Entry.status,io)
status(pkg::AbstractString = "", io::IO=STDOUT) = cd(Entry.status,io,pkg)

clone(url_or_pkg::AbstractString) = cd(Entry.clone,url_or_pkg)
clone(url::AbstractString, pkg::AbstractString) = cd(Entry.clone,url,pkg)

checkout(pkg::AbstractString, branch::AbstractString="master"; merge::Bool=true, pull::Bool=true) =
    cd(Entry.checkout,pkg,branch,merge,pull)

free(pkg) = cd(Entry.free,pkg)

pin(pkg::AbstractString) = cd(Entry.pin,pkg)
pin(pkg::AbstractString, ver::VersionNumber) = cd(Entry.pin,pkg,ver)

update() = cd(Entry.update,Dir.getmetabranch())
resolve() = cd(Entry.resolve)

build() = cd(Entry.build)
build(pkgs::AbstractString...) = cd(Entry.build,[pkgs...])

test(;coverage::Bool=false) = cd(Entry.test; coverage=coverage)
test(pkgs::AbstractString...; coverage::Bool=false) = cd(Entry.test,AbstractString[pkgs...]; coverage=coverage)

dependents(packagename::AbstractString) = Reqs.dependents(packagename)

doc"""
    setprotocol!(proto)

Set the protocol used to access GitHub-hosted packages.  Defaults to 'https', with a blank `proto` delegating the choice to the package developer.
"""
setprotocol!(proto::AbstractString) = Cache.setprotocol!(proto)


# point users to PkgDev
register(args...) =
    error("Pkg.register(pkg,[url]) has been moved to the package PkgDev.jl.\n",
          "Run Pkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-")

tag(pkg, ver=nothing, commit=nothing) =
    error("Pkg.tag(pkg, [ver, [commit]]) has been moved to the package PkgDev.jl.\n",
          "Run Pkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-")

publish() =
    error("Pkg.publish() has been moved to the package PkgDev.jl.\n",
          "Run Pkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-")

generate(pkg, license) =
    error("Pkg.generate(pkg, license) has been moved to the package PkgDev.jl.\n",
          "Run Pkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-")

license(lic=nothing) =
    error("Pkg.license([lic]) has been moved to the package PkgDev.jl.\n",
          "Run Pkg.add(\"PkgDev\") to install PkgDev on Julia v0.5-")

end # module
