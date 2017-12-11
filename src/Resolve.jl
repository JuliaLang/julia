# This file is a part of Julia. License is MIT: https://julialang.org/license

module Resolve

include(joinpath("resolve", "VersionWeights.jl"))
include(joinpath("resolve", "PkgToMaxSumInterface.jl"))
include(joinpath("resolve", "MaxSum.jl"))

using ..Types
using ..Query, .PkgToMaxSumInterface, .MaxSum
import ..Types: uuid_julia

export resolve, sanity_check

# Use the max-sum algorithm to resolve packages dependencies
function resolve(reqs::Requires, deps::DepsGraph, uuid_to_name::Dict{UUID,String})
    id(p) = pkgID(p, uuid_to_name)

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
            isa(err, UnsatError) || rethrow(err)
            p = interface.pkgs[err.info]
            # TODO: build tools to analyze the problem, and suggest to use them here.
            msg =
                """
                resolve is unable to satisfy package requirements.
                  The problem was detected when trying to find a feasible version
                  for package $(id(p)).
                  However, this only means that package $(id(p)) is involved in an
                  unsatisfiable or difficult dependency relation, and the root of
                  the problem may be elsewhere.
                """
            if msgs.num_nondecimated != graph.np
                msg *= """
                         (you may try increasing the value of the JULIA_PKGRESOLVE_ACCURACY
                          environment variable)
                       """
            end
            ## info("ERROR MESSAGE:\n" * msg)
            throw(PkgError(msg))
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
function sanity_check(deps::DepsGraph, uuid_to_name::Dict{UUID,String},
                      pkgs::Set{UUID} = Set{UUID}())
    id(p) = pkgID(p, uuid_to_name)

    isempty(pkgs) || (deps = Query.undirected_dependencies_subset(deps, pkgs))

    deps, eq_classes = Query.prune_versions(deps, uuid_to_name)

    ndeps = Dict{UUID,Dict{VersionNumber,Int}}()

    for (p,depsp) in deps
        ndeps[p] = ndepsp = Dict{VersionNumber,Int}()
        for (vn,vdep) in depsp
            ndepsp[vn] = length(vdep)
        end
    end

    vers = [(p,vn) for (p,depsp) in deps for vn in keys(depsp)]
    sort!(vers, by=pvn->(-ndeps[pvn[1]][pvn[2]]))

    nv = length(vers)

    svdict = Dict{Tuple{UUID,VersionNumber},Int}(vers[i][1:2]=>i for i = 1:nv)

    checked = falses(nv)

    problematic = Tuple{String,VersionNumber,String}[]
    i = 1
    for (p,vn) in vers
        ndeps[p][vn] == 0 && break
        checked[i] && (i += 1; continue)

        fixed = Dict{UUID,Fixed}(p=>Fixed(vn, deps[p][vn]), uuid_julia=>Fixed(VERSION))
        sub_reqs = Requires()
        bktrc = Query.init_resolve_backtrace(uuid_to_name, sub_reqs, fixed)
        Query.propagate_fixed!(sub_reqs, bktrc, fixed)
        sub_deps = Query.dependencies_subset(deps, Set{UUID}([p]))
        sub_deps, conflicts = Query.dependencies(sub_deps, fixed)

        try
            for rp in keys(sub_reqs)
                haskey(sub_deps, rp) && continue
                if uuid_julia in conflicts[rp]
                    throw(PkgError("$(id(rp)) can't be installed because it has no versions that support $VERSION " *
                       "of julia. You may need to update METADATA by running `Pkg.update()`"))
                else
                    sconflicts = join(map(id, conflicts[rp]), ", ", " and ")
                    throw(PkgError("$(id(rp)) requirements can't be satisfied because " *
                        "of the following fixed packages: $sconflicts"))
                end
            end
            Query.check_requirements(sub_reqs, sub_deps, fixed, uuid_to_name)
            sub_deps = Query.prune_dependencies(sub_reqs, sub_deps, uuid_to_name, bktrc)
        catch err
            isa(err, PkgError) || rethrow(err)
            ## info("ERROR MESSAGE:\n" * err.msg)
            for vneq in eq_classes[p][vn]
                push!(problematic, (id(p), vneq, ""))
            end
            i += 1
            continue
        end
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
                    push!(problematic, (id(p), vneq, pp))
                end
            end
        end
        if ok
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
