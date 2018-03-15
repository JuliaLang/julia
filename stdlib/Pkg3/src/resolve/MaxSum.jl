# This file is a part of Julia. License is MIT: https://julialang.org/license

module MaxSum

using Random

include("FieldValues.jl")

using .FieldValues, ..VersionWeights, ...Types, ...GraphType

export UnsatError, Messages, maxsum

# An exception type used internally to signal that an unsatisfiable
# constraint was detected
struct UnsatError <: Exception
    p0::Int
end

# Some parameters to drive the decimation process
mutable struct MaxSumParams
    dec_interval # number of iterations between decimations
    dec_fraction # fraction of nodes to decimate at every decimation
                 # step

    function MaxSumParams()
        accuracy = parse(Int, get(ENV, "JULIA_PKGRESOLVE_ACCURACY", "1"))
        accuracy > 0 || error("JULIA_PKGRESOLVE_ACCURACY must be > 0")
        dec_interval = accuracy * 5
        dec_fraction = 0.05 / accuracy
        return new(dec_interval, dec_fraction)
    end
end

# Messages has the cavity messages and the total fields, and
# gets updated iteratively (and occasionally decimated) until
# convergence
mutable struct Messages
    # cavity incoming messages: for each package p0,
    #                           for each neighbor p1 of p0,
    #                           msg[p0][p1] is a vector of length spp[p0]
    #                           messages are normalized (i.e. the max is always 0)
    msg::Vector{Vector{Field}}

    # overall fields: for each package p0,
    #                 fld[p0] is a vector of length spp[p0]
    #                 fields are not normalized
    fld::Vector{Field}

    # backup of the initial value of fld, to be used when resetting
    initial_fld::Vector{Field}

    function Messages(graph::Graph)
        np = graph.np
        spp = graph.spp
        gadj = graph.gadj
        gconstr = graph.gconstr
        req_inds = graph.req_inds
        ignored = graph.ignored
        pvers = graph.data.pvers
        pdict = graph.data.pdict

        ## generate wveights (v0 == spp[p0] is the "uninstalled" state)
        vweight = [[VersionWeight(v0 < spp[p0] ? pvers[p0][v0] : v"0") for v0 = 1:spp[p0]] for p0 = 1:np]

        # external fields: favor newest versions over older, and no-version over all;
        #                  explicit requirements use level l1 instead of l2
        fv(p0, v0) = p0 ∈ req_inds ?
            FieldValue(0, vweight[p0][v0],     zero(VersionWeight), (v0==spp[p0])) :
            FieldValue(0, zero(VersionWeight), vweight[p0][v0],     (v0==spp[p0]))
        fld = [[fv(p0, v0) for v0 = 1:spp[p0]] for p0 = 1:np]

        initial_fld = [copy(f0) for f0 in fld]

        # allocate cavity messages
        msg = [[Field(undef, spp[p0]) for j1 = 1:length(gadj[p0])] for p0 = 1:np]

        msgs = new(msg, fld, initial_fld)

        reset_messages!(msgs, graph)

        return msgs
    end
end

# enforce constraints and normalize fields;
# zero out all cavity messages
function reset_messages!(msgs::Messages, graph::Graph)
    msg = msgs.msg
    fld = msgs.fld
    initial_fld = msgs.initial_fld
    np = graph.np
    spp = graph.spp
    gconstr = graph.gconstr
    ignored = graph.ignored
    for p0 = 1:np
        ignored[p0] && continue
        map(m->fill!(m, zero(FieldValue)), msg[p0])
        copyto!(fld[p0], initial_fld[p0])
        gconstr0 = gconstr[p0]
        for v0 = 1:spp[p0]
            gconstr0[v0] || (fld[p0][v0] = FieldValue(-1))
        end
        fld[p0] .-= maximum(fld[p0])
    end
    return msgs
end

mutable struct SolutionTrace
    # the solution is built progressively by a decimation process
    solution::Vector{Int}
    num_nondecimated::Int

    best::Vector{Int}
    staged::Union{Tuple{Int,Int},Nothing}

    function SolutionTrace(graph::Graph)
        np = graph.np
        solution = zeros(Int, np)
        num_nondecimated = np

        best = zeros(Int, np)
        staged = nothing

        return new(solution, num_nondecimated, best, staged)
    end
end

function update_solution!(strace::SolutionTrace, graph::Graph)
    np = graph.np
    ignored = graph.ignored
    gconstr = graph.gconstr
    solution = strace.solution
    best = strace.best
    nnd = np
    fill!(solution, 0)
    for p0 in findall(ignored)
        s0 = findfirst(gconstr[p0])
        solution[p0] = s0
        nnd -= 1
    end
    strace.num_nondecimated = nnd
    if nnd ≤ sum(best .== 0)
        copyto!(best, solution)
        strace.staged = nothing
        return true
    else
        return false
    end
end

# This is the core of the max-sum solver:
# for a given node p0 (i.e. a package) updates all
# input cavity messages and fields of its neighbors
function update!(p0::Int, graph::Graph, msgs::Messages)
    gadj = graph.gadj
    gmsk = graph.gmsk
    adjdict = graph.adjdict
    ignored = graph.ignored
    spp = graph.spp
    np = graph.np
    msg = msgs.msg
    fld = msgs.fld

    maxdiff = zero(FieldValue)

    gadj0 = gadj[p0]
    msg0 = msg[p0]
    fld0 = fld[p0]
    spp0 = spp[p0]
    adjdict0 = adjdict[p0]

    # iterate over all neighbors of p0
    for j0 in 1:length(gadj0)

        p1 = gadj0[j0]
        ignored[p1] && continue
        j1 = adjdict0[p1]
        #@assert j0 == adjdict[p1][p0]
        bm1 = gmsk[p1][j1]
        spp1 = spp[p1]
        msg1 = msg[p1]

        # compute the output cavity field p0->p1
        cavfld = fld0 - msg0[j0]

        # keep the old input cavity message p0->p1
        oldmsg = msg1[j1]

        # init the new message to minus infinity
        newmsg = [FieldValue(-1) for v1 = 1:spp1]

        # compute the new message by passing cavfld
        # through the constraint encoded in the bitmask
        # (equivalent to:
        #    newmsg = [maximum(cavfld[bm1[:,v1]]) for v1 = 1:spp1]
        # )
        for v1 = 1:spp1, v0 = 1:spp0
            bm1[v0, v1] || continue
            newmsg[v1] = max(newmsg[v1], cavfld[v0])
        end
        m = maximum(newmsg)
        validmax(m) || throw(UnsatError(p0)) # No state available without violating some
                                             # hard constraint

        # normalize the new message
        newmsg .-= m

        diff = newmsg - oldmsg
        maxdiff = max(maxdiff, maximum(abs.(diff)))

        # update the field of p1
        fld[p1] .+= diff

        # put the newly computed message in place
        msg1[j1] = newmsg
    end
    return maxdiff
end

# A simple shuffling machinery for the update order in iterate!()
# (wouldn't pass any random quality test but it's arguably enough)
mutable struct NodePerm
    p::Vector{Int}
    step::Int64
    NodePerm(np::Integer) = new(collect(1:np), 1)
end

function Random.shuffle!(perm::NodePerm)
    p = perm.p
    for j = length(p):-1:2
        k = perm.step % j + 1
        p[j], p[k] = p[k], p[j]
        perm.step += isodd(j) ? 1 : k
    end
    #@assert isperm(p)
end

Base.start(perm::NodePerm) = start(perm.p)
Base.next(perm::NodePerm, x) = next(perm.p, x)
Base.done(perm::NodePerm, x) = done(perm.p, x)

# Call update for all nodes (i.e. packages) in
# random order
function iterate!(graph::Graph, msgs::Messages, perm::NodePerm)
    maxdiff = zero(FieldValue)
    shuffle!(perm)
    for p0 in perm
        graph.ignored[p0] && continue
        maxdiff0 = update!(p0, graph, msgs)
        maxdiff = max(maxdiff, maxdiff0)
    end
    return maxdiff
end

function decimate1!(p0::Int, graph::Graph, strace::SolutionTrace, msgs::Messages)
    solution = strace.solution
    fld = msgs.fld
    adjdict = graph.adjdict
    gmsk = graph.gmsk
    gconstr = graph.gconstr

    @assert solution[p0] == 0
    @assert !graph.ignored[p0]
    fld0 = fld[p0]
    s0 = indmax(fld0)
    # only do the decimation if it is consistent with
    # the constraints...
    gconstr[p0][s0] || return 0
    # ...and with the previously decimated nodes
    for p1 in findall(solution .> 0)
        haskey(adjdict[p0], p1) || continue
        s1 = solution[p1]
        j1 = adjdict[p0][p1]
        gmsk[p1][j1][s0,s1] || return 0
    end
    solution[p0] = s0
    strace.num_nondecimated -= 1
    return s0
end

function decimate!(graph::Graph, strace::SolutionTrace, msgs::Messages, n::Integer)
    np = graph.np
    gconstr = graph.gconstr
    ignored = graph.ignored
    fld = msgs.fld

    @assert n ≥ 1
    dtrace = Tuple{Int,Int}[]
    dec = 0

    fldorder = sort(findall(.!(ignored)), by=p0->secondmax(fld[p0], gconstr[p0]))
    for p0 in fldorder
        s0 = decimate1!(p0, graph, strace, msgs)
        s0 == 0 && continue
        push!(dtrace, (p0,s0))
        dec += 1
        dec == n && break
    end

    return dtrace
end

function clean_forbidden!(graph::Graph, msgs::Messages)
    np = graph.np
    gconstr = graph.gconstr
    ignored = graph.ignored
    fld = msgs.fld
    affected = Tuple{Int,Int}[]
    removed = 0

    for p0 = 1:np
        ignored[p0] && continue
        fld0 = fld[p0]
        gconstr0 = gconstr[p0]
        for v0 in findall(gconstr0)
            validmax(fld0[v0]) && continue
            push!(affected, (p0,v0))
            removed += 1
        end
    end
    return affected
end

# Iterative solver: run iterate!() until convergence
# (occasionally calling decimate())
function maxsum(graph::Graph)
    params = MaxSumParams()

    perm = NodePerm(graph.np)
    strace = SolutionTrace(graph)
    msgs = Messages(graph)

    push_snapshot!(graph)
    # gconstr_sav = graph.gconstr
    # ignored_sav = graph.ignored
    ok = converge!(graph, msgs, strace, perm, params)
    # @assert graph.gconstr ≡ gconstr_sav
    # @assert graph.ignored ≡ ignored_sav
    pop_snapshot!(graph)
    if ok
        @assert strace.best == strace.solution
        @assert strace.num_nondecimated == 0
        @assert all(strace.solution .> 0)
        @assert strace.staged ≡ nothing
    else
        @assert strace.staged ≢ nothing
    end
    return ok, strace.best, strace.staged
end

function try_simplify_graph_soft!(graph, sources)
    try
        simplify_graph_soft!(graph, sources, log_events = false)
    catch err
        err isa ResolverError || rethrow(err)
        return false
    end
    return true
end

function converge!(graph::Graph, msgs::Messages, strace::SolutionTrace, perm::NodePerm, params::MaxSumParams)
    is_best_sofar = update_solution!(strace, graph)

    # this is the base of the recursion: the case when
    # the solver has succeeded in decimating everything
    strace.num_nondecimated == 0 && return true

    reset_messages!(msgs, graph)

    # perform some maxsum iterations, then decimate one node.
    # If failure happens during this process, we bail (return false)
    it = 0
    for it = 1:params.dec_interval
        local maxdiff::FieldValue
        try
            maxdiff = iterate!(graph, msgs, perm)
        catch err
            err isa UnsatError || rethrow(err)
            if is_best_sofar
                p0 = err.p0
                s0 = findlast(graph.gconstr[p0])
                strace.staged = (p0, s0)
            end
            return false
        end
        maxdiff == zero(FieldValue) && break
    end

    # if maxsum has found some forbidden states, remove
    # them and propagate the effect
    affected = clean_forbidden!(graph, msgs)

    isempty(affected) && @goto decimate

    sources = Set{Int}()
    for (p0,v0) in affected
        graph.gconstr[p0][v0] = false
        push!(sources, p0)
    end

    if !try_simplify_graph_soft!(graph, sources)
        # found an implicit contradiction
        is_best_sofar && (strace.staged = first(affected))
        return false
    end
    return converge!(graph, msgs, strace, perm, params)

    @label decimate

    ndec = max(1, round(Int, params.dec_fraction * strace.num_nondecimated))
    dtrace = decimate!(graph, strace, msgs, ndec)
    if isempty(dtrace)
        # decimation has failed, all candidate states are forbidden
        # (which shouldn't really happen, this is a failsafe)
        if is_best_sofar
            # pick the first decimation candidate
            smx(p1) = secondmax(msgs.fld[p1], graph.gconstr[p1])
            p0 = reduce((p1,p2)->(smx(p1)≤smx(p2) ? p1 : p2), findall(.!(graph.ignored)))
            s0 = indmax(fld[p0])
            strace.staged = dec_firstcandidate(graph, msgs)
        end
        return false
    end

    while true
        # decimation has succeeded, at least nominally.
        # We need to propagate its effects and check for contradictions

        lentr = length(dtrace)
        if lentr == 1 && is_best_sofar
            strace.staged = dtrace[1]
        end

        push_snapshot!(graph)
        # info("setting dtrace=$dtrace")
        for (p0,s0) in dtrace
            @assert !graph.ignored[p0]
            @assert graph.gconstr[p0][s0]
            fill!(graph.gconstr[p0], false)
            graph.gconstr[p0][s0] = true
            graph.ignored[p0] = true
        end

        # if decimation has produced an implicit contradiction, backtrack
        try_simplify_graph_soft!(graph, Set{Int}(first.(dtrace))) || @goto backtrack

        # otherwise, keep going...
        converge!(graph, msgs, strace, perm, params) && (pop_snapshot!(graph); return true)

        @label backtrack

        # warn("reverting dtrace=$dtrace")

        # if we're here, the last decimation step has been proven to lead
        # to an unsat situation at some point, we need to roll it back

        # revert the state of the constraints
        pop_snapshot!(graph)

        lentr == 1 && break
        # halve the dtrace
        deleteat!(dtrace, ((lentr÷2)+1):lentr)
    end

    @assert length(dtrace) == 1
    p0, s0 = dtrace[1]

    is_best_sofar && @assert strace.staged ≢ nothing

    # forbid the state used in the last attempt
    # (note that we're working on the "entry" snapshot here!)
    graph.gconstr[p0][s0] = false
    # check if we have finished all available possibilities
    any(graph.gconstr[p0]) || return false
    # if neither the s0 state nor its negation are valid, give up
    try_simplify_graph_soft!(graph, Set{Int}([p0])) || return false

    # keep going, with one possible state less...
    return converge!(graph, msgs, strace, perm, params)
end

end
