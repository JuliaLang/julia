# This file is a part of Julia. License is MIT: https://julialang.org/license

module Resolve

import Pkg

include(joinpath("resolve", "versionweight.jl"))
include(joinpath("resolve", "interface.jl"))
include(joinpath("resolve", "maxsum.jl"))

using ..Types, ..Query, .PkgToMaxSumInterface, .MaxSum
import ..PkgError

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
            isa(err, UnsatError) || rethrow(err)
            p = interface.pkgs[err.info]
            # TODO: build tools to analyze the problem, and suggest to use them here.
            msg =
                """
                resolve is unable to satisfy package requirements.
                  The problem was detected when trying to find a feasible version
                  for package $p.
                  However, this only means that package $p is involved in an
                  unsatisfiable or difficult dependency relation, and the root of
                  the problem may be elsewhere.
                """
            if msgs.num_nondecimated != graph.np
                msg *= """
                         (you may try increasing the value of the JULIA_PKGRESOLVE_ACCURACY
                          environment variable)
                       """
            end
            ## @info "ERROR MESSAGE:\n" * msg
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

    vers = [(p,vn) for (p,d) in deps for vn in keys(d)]
    sort!(vers, by=pvn->(-ndeps[pvn[1]][pvn[2]]))

    nv = length(vers)

    svdict = Dict{Tuple{String,VersionNumber},Int}(vers[i][1:2]=>i for i = 1:nv)

    checked = falses(nv)

    problematic = Vector{Tuple{String,VersionNumber,String}}()

    i = 1
    for (p,vn) in vers
        ndeps[p][vn] == 0 && break
        checked[i] && (i += 1; continue)

        fixed = Dict{String,Fixed}(p=>Fixed(vn, deps[p][vn].requires), "julia"=>Fixed(VERSION))
        sub_reqs = Dict{String,VersionSet}()
        bktrc = Query.init_resolve_backtrace(sub_reqs, fixed)
        Query.propagate_fixed!(sub_reqs, bktrc, fixed)
        sub_deps = Query.dependencies_subset(deps, Set{String}([p]))
        sub_deps, conflicts = Query.dependencies(sub_deps, fixed)

        try
            for pkg in keys(sub_reqs)
                if !haskey(sub_deps, pkg)
                    if "julia" in conflicts[pkg]
                        throw(PkgError("$pkg can't be installed because it has no versions that support $VERSION " *
                           "of julia. You may need to update METADATA by running `Pkg.update()`"))
                    else
                        sconflicts = join(conflicts[pkg], ", ", " and ")
                        throw(PkgError("$pkg's requirements can't be satisfied because " *
                            "of the following fixed packages: $sconflicts"))
                    end
                end
            end
            Query.check_requirements(sub_reqs, sub_deps, fixed)
            sub_deps = Query.prune_dependencies(sub_reqs, sub_deps, bktrc)
        catch err
            isa(err, PkgError) || rethrow(err)
            ## @info "ERROR MESSAGE:\n" * err.msg
            for vneq in eq_classes[p][vn]
                push!(problematic, (p, vneq, ""))
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
                    push!(problematic, (p, vneq, pp))
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
