module Pkg

export Git, Dir, GitHub, Types, Reqs, Cache, Read, Query, Resolve, Write, Generate, Entry
export dir, init, rm, add, available, installed, status, clone, checkout,
       release, fix, update, resolve, register, tag, publish, generate, test

const DEFAULT_META = "git://github.com/JuliaLang/METADATA.jl"
const META_BRANCH = "metadata-v2"

for file in split("git dir github types reqs cache read query resolve write generate entry")
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

free(pkg::AbstractString) = cd(Entry.free,pkg)

pin(pkg::AbstractString) = cd(Entry.pin,pkg)
pin(pkg::AbstractString, ver::VersionNumber) = cd(Entry.pin,pkg,ver)

update() = cd(Entry.update,Dir.getmetabranch())
resolve() = cd(Entry.resolve)

register(pkg::AbstractString) = cd(Entry.register,pkg)
register(pkg::AbstractString, url::AbstractString) = cd(Entry.register,pkg,url)

tag(pkg::AbstractString, sym::Symbol=:patch) = cd(Entry.tag,pkg,sym)
tag(pkg::AbstractString, sym::Symbol, commit::AbstractString) = cd(Entry.tag,pkg,sym,false,commit)

tag(pkg::AbstractString, ver::VersionNumber; force::Bool=false) = cd(Entry.tag,pkg,ver,force)
tag(pkg::AbstractString, ver::VersionNumber, commit::AbstractString; force::Bool=false) =
    cd(Entry.tag,pkg,ver,force,commit)

submit(pkg::AbstractString) = cd(Entry.submit,pkg)
submit(pkg::AbstractString, commit::AbstractString) = cd(Entry.submit,pkg,commit)

publish() = cd(Entry.publish,Dir.getmetabranch())

build() = cd(Entry.build)
build(pkgs::AbstractString...) = cd(Entry.build,[pkgs...])

generate(pkg::AbstractString, license::AbstractString; force::Bool=false, authors::Union(AbstractString,Array) = [], config::Dict=Dict()) =
    cd(Generate.package,pkg,license,force=force,authors=authors,config=config)


test(;coverage::Bool=false) = cd(Entry.test; coverage=coverage)
test(pkgs::AbstractString...; coverage::Bool=false) = cd(Entry.test,AbstractString[pkgs...]; coverage=coverage)

dependents(packagename::AbstractString) = Reqs.dependents(packagename)

end # module
