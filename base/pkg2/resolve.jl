module Resolve

using ..Types
const old = Base.Pkg.Resolve

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

# adapt requirements and dependencies for old resolve code

function resolve(reqs::Dict{ByteString,VersionSet},
	             deps::Dict{ByteString,Dict{VersionNumber,Available}})

    vx = old.Version[]
    rx = old.VersionSet[]
    dx = Array((old.Version,old.VersionSet),0)

    for (pkg,vset) in reqs
        push!(rx, old.VersionSet(pkg,versions(vset)))
    end
    for (pkg,vers) in deps, (v,a) in vers
        ver = old.Version(pkg,v)
        push!(vx, ver)
        for (p,vset) in a.requires
            push!(dx, (ver,old.VersionSet(p,versions(vset))))
        end
    end

    old.resolve(sort!(rx), sort!(vx), sort!(dx))
end

end # module
