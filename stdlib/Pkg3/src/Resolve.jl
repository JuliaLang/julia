# This file is a part of Julia. License is MIT: https://julialang.org/license

module Resolve

include(joinpath("resolve", "VersionWeights.jl"))
include(joinpath("resolve", "MaxSum.jl"))

using Pkg3.Types
using Pkg3.GraphType
using Pkg3.Resolve.MaxSum
import Pkg3.Types: uuid_julia
import Pkg3.GraphType: is_julia, check_constraints, log_event_global!, log_event_greedysolved!, log_event_maxsumsolved!, log_event_maxsumtrace!

using Printf

export resolve, sanity_check

"Resolve package dependencies."
function resolve(graph::Graph)
    # attempt trivial solution first
    greedy_ok, sol = greedysolver(graph)

    greedy_ok && @goto solved

    log_event_global!(graph, "greedy solver failed")

    # trivial solution failed, use maxsum solver
    maxsum_ok, sol, staged = maxsum(graph)

    maxsum_ok && @goto solved

    log_event_global!(graph, "maxsum solver failed")

    # the problem is unsat, force-trigger a failure
    # in order to produce a log - this will contain
    # information about the best that the solver could
    # achieve
    trigger_failure!(graph, sol, staged)

    @label solved

    # verify solution (debug code) and enforce its optimality
    @assert verify_solution(sol, graph)
    greedy_ok || enforce_optimality!(sol, graph)

    log_event_global!(graph, "the solver found $(greedy_ok ? "an optimal" : "a feasible") configuration")

    # return the solution as a Dict mapping UUID => VersionNumber
    return compute_output_dict(sol, graph)
end

"""
Scan the graph for (explicit or implicit) contradictions. Returns a list of problematic
(package,version) combinations.
"""
function sanity_check(graph::Graph, sources::Set{UUID} = Set{UUID}(); verbose = true)
    req_inds = graph.req_inds
    fix_inds = graph.fix_inds

    id(p) = pkgID(p, graph)

    isempty(req_inds) || @warn("sanity check called on a graph with non-empty requirements")
    if !any(is_julia(graph, fp0) for fp0 in fix_inds)
        @warn("sanity check called on a graph without julia requirement, adding it")
        add_fixed!(graph, Dict(uuid_julia=>Fixed(VERSION)))
    end
    if length(fix_inds) ≠ 1
        @warn("sanity check called on a graph with extra fixed requirements (besides julia)")
    end

    isources = isempty(sources) ?
        Set{Int}(1:graph.np) :
        Set{Int}(graph.data.pdict[p] for p in sources)

    simplify_graph!(graph, isources)

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

    last_str_len = 0

    for (i,(p,vn)) in enumerate(vers)
        if verbose
            frac_compl = i / nv
            print("\r", " "^last_str_len, "\r")
            progr_msg = @sprintf("%.3i/%.3i (%i%%) — problematic so far: %i", i, nv, round(Int, 100 * frac_compl), length(problematic))
            print(progr_msg)
            last_str_len = length(progr_msg)
        end

        length(gadj[pdict[p]]) == 0 && break
        checked[i] && continue

        push_snapshot!(graph)

        # enforce package version
        # TODO: use add_reqs! instead...
        p0 = graph.data.pdict[p]
        v0 = graph.data.vdict[p0][vn]
        fill!(graph.gconstr[p0], false)
        graph.gconstr[p0][v0] = true
        push!(graph.req_inds, p0)

        ok = false
        try
            simplify_graph_soft!(graph, Set{Int}([p0]), log_events = false)
        catch err
            isa(err, ResolverError) || rethrow(err)
            @goto done
        end

        ok, sol = greedysolver(graph)
        ok && @goto done
        ok, sol = maxsum(graph)

        @label done

        if !ok
            for vneq in eq_classes[p][vn]
                push!(problematic, (id(p), vneq))
            end
        else
            @assert verify_solution(sol, graph)
            sol_dict = compute_output_dict(sol, graph)
            for (sp,svn) in sol_dict
                j = svdict[sp,svn]
                checked[j] = true
            end
        end

        # state reset
        empty!(graph.req_inds)
        pop_snapshot!(graph)
    end
    if verbose
        print("\r", " "^last_str_len, "\r")
        println("found $(length(problematic)) problematic versions")
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
    np = graph.np

    push_snapshot!(graph)
    gconstr = graph.gconstr

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
        fill!(gconstr[rp0], false)
        gconstr[rp0][rv0] = true
    end

    # propagate the requirements
    try
        simplify_graph_soft!(graph, req_inds, log_events = false)
    catch err
        err isa ResolverError || rethrow(err)
        pop_snapshot!(graph)
        return (false, Int[])
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
                    pop_snapshot!(graph)
                    return (false, Int[])
                end

                p1 ∈ seen || push!(staged_next, p1)
            end
        end
        union!(seen, staged_next)
        staged = staged_next
    end

    pop_snapshot!(graph)

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

    @assert length(sol) == np
    @assert all(sol .> 0)

    # verify constraints and dependencies
    for p0 = 1:np
        s0 = sol[p0]
        gconstr[p0][s0] || (@warn("gconstr[$p0][$s0] fail"); return false)
        for (j1,p1) in enumerate(gadj[p0])
            msk = gmsk[p0][j1]
            s1 = sol[p1]
            msk[s1,s0] || (@warn("gmsk[$p0][$p1][$s1,$s0] fail"); return false)
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

    for p0 in findall(uninst)
        sol[p0] = spp[p0]
        why[p0] = :uninst
    end

    @assert verify_solution(sol, graph)

    for p0 = 1:np
        log_event_maxsumsolved!(graph, p0, sol[p0], why[p0])
    end
end

function apply_maxsum_trace!(graph::Graph, sol::Vector{Int})
    gconstr = graph.gconstr

    for (p0,s0) in enumerate(sol)
        s0 == 0 && continue
        gconstr0 = gconstr[p0]
        old_constr = copy(gconstr0)
        @assert old_constr[s0]
        fill!(gconstr0, false)
        gconstr0[s0] = true
        gconstr0 ≠ old_constr && log_event_maxsumtrace!(graph, p0, s0)
    end
end

function trigger_failure!(graph::Graph, sol::Vector{Int}, staged::Tuple{Int,Int})
    apply_maxsum_trace!(graph, sol)
    simplify_graph_soft!(graph, Set(findall(sol .> 0)), log_events = true) # this may throw an error...

    np = graph.np
    gconstr = graph.gconstr
    p0, v0 = staged

    @assert gconstr[p0][v0]
    fill!(gconstr[p0], false)
    gconstr[p0][v0] = true
    log_event_maxsumtrace!(graph, p0, v0)
    simplify_graph!(graph) # this may throw an error...
    outdict = resolve(graph) # ...otherwise, this MUST throw an error
    open(io->showlog(io, graph, view=:chronological), "logchrono.errresolve.txt", "w")
    error("this is not supposed to happen... $(Dict(pkgID(p, graph) => vn for (p,vn) in outdict))")
end

end # module
