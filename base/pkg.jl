module Pkg
import Base.Git
for file in split("dir types reqs cache read query resolve write generate entry")
    include("pkg/$file.jl")
end
using .Types
const cd = Dir.cd
const dir = Dir.path

init(meta::String=Dir.DEFAULT_META) = Dir.init(meta)

rm(pkg::String) = cd(Entry.rm,pkg)
add(pkg::String, vers::VersionSet) = cd(Entry.add,pkg,vers)
add(pkg::String, vers::VersionNumber...) = add(pkg,VersionSet(vers...))

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

update() = cd(Entry.update)
resolve() = cd(Entry._resolve)

register(pkg::String) = cd(Entry.register,pkg)
register(pkg::String, url::String) = cd(Entry.register,pkg,url)

tag(pkg::String, sym::Symbol=:bump; commit::String="", msg::String="") =
    cd(Entry.tag,pkg,sym,commit,msg)
tag(pkg::String, ver::VersionNumber; commit::String="", msg::String="") =
    cd(Entry.tag,pkg,ver,commit,msg)

fixup() = cd(Entry.fixup)
fixup(pkg::String) = cd(Entry.fixup,pkg)

generate(pkg::String, license::String) = cd(Generate.package,pkg,license)

end # module
