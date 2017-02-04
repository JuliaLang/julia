# This file is a part of Julia. License is MIT: http://julialang.org/license

module Resolve

include("resolve/versionweight.jl")
include("resolve/interface.jl")
include("resolve/maxsum.jl")

using ..Types, ..Query, .PkgToMaxSumInterface, .MaxSum
import ...Pkg.PkgError

export resolve, sanity_check

# Use the max-sum algorithm to resolve packages dependencies
function resolve(reqs::Requires, deps::Dict{String,Dict{VersionNumber,Available}})
    # init interface structures
    interface = Interface(reqs, deps)

    # attempt trivial solution first
    ok, sol = greedysolver(interface)
    if !ok
        # trivial solution failed, use maxsum solver
        graph = Graph(interface)
        msgs = Messages(interface, graph)

        try
            sol = maxsum(graph, msgs)
        catch err
            if isa(err, UnsatError)
                p = interface.pkgs[err.info]
                msg = "unsatisfiable package requirements detected: " *
                      "no feasible version could be found for package: $p"
                if msgs.num_nondecimated != graph.np
                    msg *= "\n  (you may try increasing the value of the" *
                           "\n   JULIA_PKGRESOLVE_ACCURACY environment variable)"
                end
                throw(PkgError(msg))
            end
            rethrow(err)
        end

        # verify solution (debug code) and enforce its optimality
        @assert verify_solution(sol, interface)
        enforce_optimality!(sol, interface)
        @assert verify_solution(sol, interface)
    end

    # return the solution as a Dict mapping package_name => sha1
    return compute_output_dict(sol, interface)
end

# Scan dependencies for (explicit or implicit) contradictions
function sanity_check(deps::Dict{String,Dict{VersionNumber,Available}},
                      pkgs::Set{String} = Set{String}())
    isempty(pkgs) || (deps = Query.undirected_dependencies_subset(deps, pkgs))

    deps, eq_classes = Query.prune_versions(deps)

    ndeps = Dict{String,Dict{VersionNumber,Int}}()

    for (p,depsp) in deps
        ndeps[p] = ndepsp = Dict{VersionNumber,Int}()
        for (vn,a) in depsp
            ndepsp[vn] = length(a.requires)
        end
    end

    vers = Array{Tuple{String,VersionNumber,VersionNumber}}(0)
    for (p,d) in deps, vn in keys(d)
        lvns = VersionNumber[Iterators.filter(vn2->(vn2>vn), keys(d))...]
        nvn = isempty(lvns) ? typemax(VersionNumber) : minimum(lvns)
        push!(vers, (p,vn,nvn))
    end
    sort!(vers, by=pvn->(-ndeps[pvn[1]][pvn[2]]))

    nv = length(vers)

    svdict = Dict{Tuple{String,VersionNumber},Int}(vers[i][1:2]=>i for i = 1:nv)

    checked = falses(nv)

    problematic = Array{Tuple{String,VersionNumber,String}}(0)
    i = 1
    psl = 0
    for (p,vn,nvn) in vers
        if ndeps[p][vn] == 0
            break
        end
        if checked[i]
            i += 1
            continue
        end

        sub_reqs = Dict{String,VersionSet}(p=>VersionSet([vn, nvn]))
        sub_deps = Query.prune_dependencies(sub_reqs, deps)
        interface = Interface(sub_reqs, sub_deps)

        red_pkgs = interface.pkgs
        red_np = interface.np
        red_spp = interface.spp
        red_pvers = interface.pvers

        ok, sol = greedysolver(interface)

        if !ok
            try
                graph = Graph(interface)
                msgs = Messages(interface, graph)
                sol = maxsum(graph, msgs)
                ok = verify_solution(sol, interface)
                @assert ok
            catch err
                isa(err, UnsatError) || rethrow(err)
                pp = red_pkgs[err.info]
                for vneq in eq_classes[p][vn]
                    push!(problematic, (p, vneq, pp))
                end
            end
        end
        if ok
            let p0 = interface.pdict[p]
                svn = red_pvers[p0][sol[p0]]
                @assert svn == vn
            end

            for p0 = 1:red_np
                s0 = sol[p0]
                if s0 != red_spp[p0]
                    j = svdict[(red_pkgs[p0], red_pvers[p0][s0])]
                    checked[j] = true
                end
            end
            checked[i] = true
        end
        i += 1
    end

    return sort!(problematic)
end

end # module
