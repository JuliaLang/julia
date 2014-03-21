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
init(meta::String=DEFAULT_META, branch::String=META_BRANCH) = Dir.init(meta,branch)

edit() = cd(Entry.edit)
rm(pkg::String) = cd(Entry.rm,pkg)
add(pkg::String, vers::VersionNumber...) = cd(Entry.add,pkg,vers...)

available() = cd(Entry.available)
available(pkg::String) = cd(Entry.available,pkg)

installed() = cd(Entry.installed)
installed(pkg::String) = cd(Entry.installed,pkg)

status(io::IO=STDOUT) = cd(Entry.status,io)

clone(url_or_pkg::String) = cd(Entry.clone,url_or_pkg)
clone(url::String, pkg::String) = cd(Entry.clone,url,pkg)

checkout(pkg::String, branch::String="master"; merge::Bool=true, pull::Bool=true) =
    cd(Entry.checkout,pkg,branch,merge,pull)

free(pkg::String) = cd(Entry.free,pkg)

pin(pkg::String) = cd(Entry.pin,pkg)
pin(pkg::String, ver::VersionNumber) = cd(Entry.pin,pkg,ver)

update() = cd(Entry.update,META_BRANCH)
resolve() = cd(Entry.resolve)

register(pkg::String) = cd(Entry.register,pkg)
register(pkg::String, url::String) = cd(Entry.register,pkg,url)

tag(pkg::String, sym::Symbol=:patch) = cd(Entry.tag,pkg,sym)
tag(pkg::String, sym::Symbol, commit::String) = cd(Entry.tag,pkg,sym,false,commit)

tag(pkg::String, ver::VersionNumber; force::Bool=false) = cd(Entry.tag,pkg,ver,force)
tag(pkg::String, ver::VersionNumber, commit::String; force::Bool=false) =
	cd(Entry.tag,pkg,ver,force,commit)

submit(pkg::String) = cd(Entry.submit,pkg)
submit(pkg::String, commit::String) = cd(Entry.submit,pkg,commit)

publish() = cd(Entry.publish,META_BRANCH)

build() = cd(Entry.build)
build(pkgs::String...) = cd(Entry.build,[pkgs...])

generate(pkg::String, license::String; force::Bool=false) =
	cd(Generate.package,pkg,license,force=force)


test() = cd(Entry.test)
test(pkgs::String...) = cd(Entry.test,String[pkgs...])

@deprecate release free
@deprecate fixup build
@deprecate fix pin

end # module
