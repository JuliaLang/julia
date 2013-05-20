module Pkg2

include("pkg/dir.jl")
include("pkg/types.jl")
include("pkg/read.jl")
include("pkg/query.jl")
include("pkg/write.jl")

requirements() = Dir.cd() do
    avail = Read.available()
    reqs  = Read.parse_requires()
    fix   = Read.fixed(avail)
    Query.requirements(reqs,fix)
end

dependencies() = Dir.cd() do
    avail = Read.available()
    fix   = Read.fixed(avail)
    Query.dependencies(avail,fix)
end

end # module
