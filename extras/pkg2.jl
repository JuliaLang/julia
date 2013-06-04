module Pkg2

include("pkg/dir.jl")
include("pkg/types.jl")
include("pkg/reqs.jl")
include("pkg/read.jl")
include("pkg/query.jl")
include("pkg/write.jl")

using .Types
import Base.Pkg.Resolve

function versions(vset::VersionSet)
    vers = VersionNumber[]
    vset == VersionSet() && return vers
    for ival in vset.intervals
        push!(vers, ival.lower)
        ival.upper < typemax(VersionNumber) &&
        push!(vers, ival.upper)
    end
    return vers
end

resolve() = Dir.cd() do
    reqs  = Reqs.parse("REQUIRE")           :: Dict{ByteString,VersionSet}
    avail = Read.available()                :: Dict{ByteString,Dict{VersionNumber,Available}}
    fix   = Read.fixed(avail)               :: Dict{ByteString,Fixed}
    reqs  = Query.requirements(reqs,fix)    :: Dict{ByteString,VersionSet}
    deps  = Query.dependencies(avail,fix)   :: Dict{ByteString,Dict{VersionNumber,Available}}

    # adapt for old resolve code
    vx = Resolve.Version[]
    rx = Resolve.VersionSet[]
    dx = Array((Resolve.Version,Resolve.VersionSet),0)

    for (pkg,vset) in reqs
        push!(rx, Resolve.VersionSet(pkg,versions(vset)))
    end
    for (pkg,vers) in deps, (v,a) in vers
        ver = Resolve.Version(pkg,v)
        push!(vx, ver)
        for (p,vset) in a.requires
            push!(dx, (ver,Resolve.VersionSet(p,versions(vset))))
        end
    end

    Resolve.resolve(sort!(rx), sort!(vx), sort!(dx))
end

add(pkg::ByteString, vers::VersionSet) = Dir.cd() do
    Write.update_file(Reqs.add, "REQUIRE", pkg, vers)
end
add(pkg::ByteString, vers::VersionNumber...) = add(pkg, VersionSet(vers...))

rm(pkg::ByteString) = Dir.cd() do
    Write.update_file(Reqs.rm, "REQUIRE", pkg)
end

end # module
