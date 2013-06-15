module Pkg2

include("pkg2/dir.jl")
include("pkg2/types.jl")
include("pkg2/reqs.jl")
include("pkg2/read.jl")
include("pkg2/query.jl")
include("pkg2/resolve.jl")
include("pkg2/write.jl")

using .Types

resolve() = Dir.cd() do
    reqs  = Reqs.parse("REQUIRE")
    avail = Read.available()
    fixed = Read.fixed(avail)
    reqs  = Query.requirements(reqs,fixed)
    deps  = Query.dependencies(avail,fixed)
    want  = Resolve.resolve(reqs,deps)
    have  = Read.free(avail)
    diff  = Query.diff(have,want)
end

add(pkg::ByteString, vers::VersionSet) = Dir.cd() do
    Write.update_file(Reqs.add, "REQUIRE", pkg, vers)
end
add(pkg::ByteString, vers::VersionNumber...) = add(pkg, VersionSet(vers...))

rm(pkg::ByteString) = Dir.cd() do
    Write.update_file(Reqs.rm, "REQUIRE", pkg)
end

end # module
