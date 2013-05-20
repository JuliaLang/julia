module Pkg2

include("pkg/types.jl")
include("pkg/read.jl")
include("pkg/query.jl")
include("pkg/write.jl")

function requirements()
    avail = Read.available()
    reqs  = Read.parse_requires()
    fix   = Read.fixed(avail)
    Query.requirements(reqs,fix)
end

function dependencies()
    avail = Read.available()
    fix   = Read.fixed(avail)
    Query.dependencies(avail,fix)
end

end # module
