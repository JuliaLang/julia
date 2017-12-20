# This file is a part of Julia. License is MIT: https://julialang.org/license

module Resolve

include(joinpath("resolve", "VersionWeights.jl"))
include(joinpath("resolve", "MaxSum.jl"))

using ..Types
using ..GraphType
using .MaxSum
import ..Types: uuid_julia
import ..GraphType: is_julia, log_event_greedysolved!, log_event_maxsumsolved!

export resolve, sanity_check

"Resolve package dependencies."
function resolve(graph::Graph; verbose::Bool = false)
    id(p) = pkgID(p, graph)

    # attempt trivial solution first
    ok, sol = greedysolver(graph)

    ok && @goto solved

    verbose && info("resolve: greedy failed")

    # trivial solution failed, use maxsum solver
    msgs = Messages(graph)

    try
        sol = maxsum(graph, msgs)
    catch err
        isa(err, UnsatError) || rethrow(err)
        verbose && info("resolve: maxsum failed")
        # p = graph.data.pkgs[err.info]
        # TODO: build tools to analyze the problem, and suggest to use them here.
        msg =
            """
            resolve is unable to satisfy package requirements.
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
    @assert verify_solution(sol, graph)
    enforce_optimality!(sol, graph)

    @label solved

    verbose && info("resolve: succeeded")

    # return the solution as a Dict mapping UUID => VersionNumber
    return compute_output_dict(sol, graph)
end

"""
Scan the graph for (explicit or implicit) contradictions. Returns a list of problematic
(package,version) combinations.
"""
function sanity_check(graph::Graph, sources::Set{UUID} = Set{UUID}(); verbose::Bool = false)
    req_inds = graph.req_inds
    fix_inds = graph.fix_inds

    id(p) = pkgID(p, graph)

    isempty(req_inds) || warn("sanity check called on a graph with non-empty requirements")
    if !any(is_julia(graph, fp0) for fp0 in fix_inds)
        warn("sanity check called on a graph without julia requirement, adding it")
        add_fixed!(graph, Dict(uuid_julia=>Fixed(VERSION)))
    end
    if length(fix_inds) ≠ 1
        warn("sanity check called on a graph with extra fixed requirements (besides julia)")
    end

    isources = isempty(sources) ?
        Set{Int}(1:graph.np) :
        Set{Int}(graph.data.pdict[p] for p in sources)

    simplify_graph!(graph, isources, verbose = verbose)

    np = graph.np
    spp = graph.spp
    gadj = graph.gadj
    data = graph.data
    pkgs = data.pkgs
    pdict = data.pdict
    pvers = data.pvers
    eq_classes = data.eq_classes

    problematic = Tuple{String,VersionNumber}[]

    np == 0 && return problematic

    vers = [(pkgs[p0],pvers[p0][v0]) for p0 = 1:np for v0 = 1:(spp[p0]-1)]
    sort!(vers, by=pv->(-length(gadj[pdict[pv[1]]])))

    nv = length(vers)

    svdict = Dict{Tuple{UUID,VersionNumber},Int}(vers[i] => i for i = 1:nv)

    checked = falses(nv)

    i = 1
    for (p,vn) in vers
        length(gadj[pdict[p]]) == 0 && break
        checked[i] && (i += 1; continue)

        req = Requires(p => vn)
        sub_graph = copy(graph)
        add_reqs!(sub_graph, req)

        try
            simplify_graph!(sub_graph, verbose = verbose)
        catch err
            isa(err, PkgError) || rethrow(err)
            ## info("ERROR MESSAGE:\n" * err.msg)
            for vneq in eq_classes[p][vn]
                push!(problematic, (id(p), vneq))
            end
            i += 1
            continue
        end

        ok, sol = greedysolver(sub_graph)

        ok && @goto solved

        msgs = Messages(sub_graph)

        try
            sol = maxsum(sub_graph, msgs)
            @assert verify_solution(sol, sub_graph)
        catch err
            isa(err, UnsatError) || rethrow(err)
            for vneq in eq_classes[p][vn]
                push!(problematic, (id(p), vneq))
            end
            i += 1
            continue
        end

        @label solved

        sol_dict = compute_output_dict(sol, sub_graph)
        for (sp, svn) in sol_dict
            j = svdict[sp,svn]
            checked[j] = true
        end

        i += 1
    end

    return sort!(problematic)
end

"""
Translate the solver output (a Vector{Int} of package states) into a Dict which
associates a VersionNumber to each installed package UUID.
"""
function compute_output_dict(sol::Vector{Int}, graph::Graph)
    np = graph.np
    spp = graph.spp
    fix_inds = graph.fix_inds
    pkgs = graph.data.pkgs
    pvers = graph.data.pvers
    pruned = graph.data.pruned

    want = Dict{UUID,VersionNumber}()
    for p0 = 1:np
        p0 ∈ fix_inds && continue
        p = pkgs[p0]
        s0 = sol[p0]
        s0 == spp[p0] && continue
        vn = pvers[p0][s0]
        want[p] = vn
    end
    for (p,vn) in pruned
        @assert !haskey(want, p)
        want[p] = vn
    end

    return want
end

"""
Preliminary solver attempt: tries to maximize each version; bails out as soon as
some non-trivial requirement is detected.
"""
function greedysolver(graph::Graph)
    spp = graph.spp
    gadj = graph.gadj
    gmsk = graph.gmsk
    gconstr = graph.gconstr
    np = graph.np

    # initialize solution: all uninstalled
    sol = [spp[p0] for p0 = 1:np]

    # packages which are not allowed to be uninstalled
    # (NOTE: this is potentially a superset of graph.req_inds,
    #        since it may include implicit requirements)
    req_inds = Set{Int}(p0 for p0 = 1:np if !gconstr[p0][end])

    # set up required packages to their highest allowed versions
    for rp0 in req_inds
        # look for the highest version which satisfies the requirements
        rv0 = findlast(gconstr[rp0])
        @assert rv0 ≠ 0 && rv0 ≠ spp[rp0]
        sol[rp0] = rv0
    end

    # we start from required packages and explore the graph
    # following dependencies
    staged = req_inds
    seen = copy(staged)

    while !isempty(staged)
        staged_next = Set{Int}()
        for p0 in staged
            s0 = sol[p0]
            @assert s0 < spp[p0]

            # scan dependencies
            for (j1,p1) in enumerate(gadj[p0])
                msk = gmsk[p0][j1]
                # look for the highest version which satisfies the requirements
                v1 = findlast(msk[:,s0] .& gconstr[p1])
                v1 == spp[p1] && continue # p1 is not required by p0's current version
                # if we found a version, and the package was uninstalled
                # or the same version was already selected, we're ok;
                # otherwise we can't be sure what the optimal configuration is
                # and we bail out
                if v1 > 0 && (sol[p1] == spp[p1] || sol[p1] == v1)
                    sol[p1] = v1
                else
                    return (false, Int[])
                end

                p1 ∈ seen || push!(staged_next, p1)
            end
        end
        union!(seen, staged_next)
        staged = staged_next
    end

    @assert verify_solution(sol, graph)

    for p0 = 1:np
        log_event_greedysolved!(graph, p0, sol[p0])
    end

    return true, sol
end

"""
Verifies that the solver solution fulfills all hard constraints
(requirements and dependencies). This is intended as debug code.
"""
function verify_solution(sol::Vector{Int}, graph::Graph)
    np = graph.np
    spp = graph.spp
    gadj = graph.gadj
    gmsk = graph.gmsk
    gconstr = graph.gconstr

    # verify constraints and dependencies
    for p0 = 1:np
        s0 = sol[p0]
        gconstr[p0][s0] || return false
        for (j1,p1) in enumerate(gadj[p0])
            msk = gmsk[p0][j1]
            s1 = sol[p1]
            msk[s1,s0] || return false # TODO: print debug info
        end
    end
    return true
end

"""
Push the given solution to a local optimium if needed: keeps increasing
the states of the given solution as long as no constraints are violated.
It also removes unnecessary parts of the solution which are unconnected
to the required packages.
"""
function enforce_optimality!(sol::Vector{Int}, graph::Graph)
    np = graph.np
    spp = graph.spp
    gadj = graph.gadj
    gmsk = graph.gmsk
    gdir = graph.gdir
    gconstr = graph.gconstr
    pkgs = graph.data.pkgs

    # keep a track for the log
    why = Union{Symbol,Int}[0 for p0 = 1:np]

    restart = true
    while restart
        restart = false
        for p0 = 1:np
            s0 = sol[p0]
            s0 == spp[p0] && (why[p0] = :uninst; continue) # the package is not installed

            # check if bumping to the higher version would violate a constraint
            gconstr[p0][s0+1] || (why[p0] = :constr; continue)

            # check if bumping to the higher version would violate a constraint
            viol = false
            for (j1,p1) in enumerate(gadj[p0])
                s1 = sol[p1]
                msk = gmsk[p0][j1]
                if !msk[s1, s0+1]
                    viol = true
                    why[p0] = p1
                    break
                end
            end
            viol && continue

            # So the solution is non-optimal: we bump it manually
            sol[p0] += 1
            restart = true
        end
    end

    # Finally uninstall unneeded packages:
    # start from the required ones and keep only
    # the packages reachable from them along the graph.
    # (These should have been removed in the previous step, but in principle
    # an unconnected yet self-sustaining cycle may have survived.)
    uninst = trues(np)
    staged = Set{Int}(p0 for p0 = 1:np if !gconstr[p0][end])
    seen = copy(staged)

    while !isempty(staged)
        staged_next = Set{Int}()
        for p0 in staged
            s0 = sol[p0]
            @assert s0 < spp[p0]
            uninst[p0] = false
            for (j1,p1) in enumerate(gadj[p0])
                gmsk[p0][j1][end,s0] && continue # the package is not required by p0 at version s0
                p1 ∈ seen || push!(staged_next, p1)
            end
        end
        union!(seen, staged_next)
        staged = staged_next
    end

    for p0 in find(uninst)
        sol[p0] = spp[p0]
        why[p0] = :uninst
    end

    @assert verify_solution(sol, graph)

    for p0 = 1:np
        log_event_maxsumsolved!(graph, p0, sol[p0], why[p0])
    end
end

end # module
