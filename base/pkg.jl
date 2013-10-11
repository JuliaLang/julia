module Pkg

export dir, init, rm, add, available, installed, status, clone, checkout,
       release, fix, update, resolve, register, tag, publish, generate

const DEFAULT_META = "git://github.com/JuliaLang/METADATA.jl"
const META_BRANCH = "metadata-v2"

for file in split("dir types reqs cache read query resolve write generate entry")
    include("pkg/$file.jl")
end
const cd = Dir.cd
const dir = Dir.path

init(meta::String=DEFAULT_META, branch::String=META_BRANCH) = Dir.init(meta,branch)

rm(pkg::String) = cd(Entry.rm,pkg)
add(pkg::String, vers::VersionNumber...) = cd(Entry.add,pkg,vers...)

available() = cd(Entry.available)
available(pkg::String) = cd(Entry.available,pkg)

installed() = cd(Entry.installed)
installed(pkg::String) = cd(Entry.installed,pkg)

status(io::IO=STDOUT) = cd(Entry.status,io)

clone(url::String, pkg::String=Entry.url2pkg(url); opts::Cmd=``) =
    cd(Entry.clone,url,pkg,opts)

checkout(pkg::String, branch::String="master"; merge::Bool=true, pull::Bool=false) =
    cd(Entry.checkout,pkg,branch,merge,pull)

release(pkg::String) = cd(Entry.release,pkg)

fix(pkg::String) = cd(Entry.fix,pkg)
fix(pkg::String, ver::VersionNumber) = cd(Entry.fix,pkg,ver)

update() = cd(Entry.update,META_BRANCH)
resolve() = cd(Entry._resolve)

register(pkg::String) = cd(Entry.register,pkg)
register(pkg::String, url::String) = cd(Entry.register,pkg,url)

tag(pkg::String, sym::Symbol=:bump; commit::String="", msg::String="") =
    cd(Entry.tag,pkg,sym,commit,msg)
tag(pkg::String, ver::VersionNumber; commit::String="", msg::String="") =
    cd(Entry.tag,pkg,ver,commit,msg)

publish() = cd(Entry.publish,META_BRANCH)

fixup() = cd(Entry._fixup)
fixup(pkg::String) = cd(Entry._fixup,pkg)

generate(pkg::String, license::String) = cd(Generate.package,pkg,license)

end # module
