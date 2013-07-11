module Resolve

include("resolve/interface.jl")
include("resolve/maxsum.jl")

using ..Types, ..Query, .PkgToMaxSumInterface, .MaxSum

export resolve, sanity_check

# Use the max-sum algorithm to resolve packages dependencies
function resolve(reqs::Requires, deps::Dict{ByteString,Dict{VersionNumber,Available}})

    # init structures
    interface = Interface(reqs, deps)

    graph = Graph(interface)
    msgs = Messages(interface, graph)

    # find solution
    local sol::Vector{Int}
    try
        sol = maxsum(graph, msgs)
    catch err
        if isa(err, UnsatError)
            p = interface.pkgs[err.info]
            msg = "Unsatisfiable package requirements detected: " *
                  "no feasible version could be found for package: $p"
            if msgs.num_nondecimated != graph.np
                msg *= "\n  (you may try increasing the value of the" *
                       "\n   JULIA_PKGRESOLVE_ACCURACY environment variable)"
            end
            error(msg)
        end
        rethrow(err)
    end

    # verify solution (debug code) and enforce its optimality
    verify_solution(sol, interface)
    enforce_optimality!(sol, interface)

    # return the solution as a Dict mapping package_name => sha1
    return compute_output_dict(sol, interface)
end

# Scan dependencies for (explicit or implicit) contradictions
function sanity_check(deps::Dict{ByteString,Dict{VersionNumber,Available}})

    deps, eq_classes_map = Query.prune_versions(deps)

    ndeps = (ByteString=>Dict{VersionNumber,Int})[]

    for (p,depsp) in deps
        ndeps[p] = ndepsp = (VersionNumber=>Int)[]
        for (vn,a) in depsp
            ndepsp[vn] = length(a.requires)
        end
    end

    vers = Array((ByteString,VersionNumber), 0)
    for (p,d) in deps, (vn,_) in d
        push!(vers, (p,vn))
    end
    sort!(vers, by=pvn->(-ndeps[pvn[1]][pvn[2]]))

    nv = length(vers)

    svdict = ((ByteString,VersionNumber)=>Int)[ vers[i]=>i for i = 1:nv ]

    checked = falses(nv)

    problematic = Array((ByteString,VersionNumber,ByteString),0)
    i = 1
    psl = 0
    for (p,vn) in vers
        if ndeps[p][vn] == 0
            break
        end
        if checked[i]
            i += 1
            continue
        end

        nvn = VersionNumber(vn.major, vn.minor, vn.patch, vn.prerelease, tuple(vn.build..., 0))
        sub_reqs = (ByteString=>VersionSet)[p=>VersionSet([vn, nvn])]
        interface = Interface(sub_reqs, deps)

        graph = Graph(interface)
        msgs = Messages(interface, graph)

        red_pkgs = interface.pkgs
        red_np = interface.np
        red_spp = interface.spp
        red_pvers = interface.pvers

        local sol::Vector{Int}
        try
            sol = maxsum(graph, msgs)
            verify_solution(sol, interface)

            for p0 = 1:red_np
                s0 = sol[p0]
                if s0 != red_spp[p0]
                    j = svdict[(red_pkgs[p0], red_pvers[p0][s0])]
                    checked[j] = true
                end
            end
            checked[i] = true
        catch err
            if isa(err, UnsatError)
                pp = red_pkgs[err.info]
                for vneq in eq_classes_map[p][vn]
                    push!(problematic, (p, vneq, pp))
                end
            else
                rethrow(err)
            end
        end
        i += 1
    end

    return sort!(problematic)
end

end # module
