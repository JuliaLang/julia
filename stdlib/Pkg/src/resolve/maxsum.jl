# This file is a part of Julia. License is MIT: https://julialang.org/license

module MaxSum

include("fieldvalue.jl")

using .FieldValues, ..VersionWeights, ..PkgToMaxSumInterface

export UnsatError, Graph, Messages, maxsum

# An exception type used internally to signal that an unsatisfiable
# constraint was detected
struct UnsatError <: Exception
    info
end

# Some parameters to drive the decimation process
mutable struct MaxSumParams
    nondec_iterations # number of initial iterations before starting
                      # decimation
    dec_interval # number of iterations between decimations
    dec_fraction # fraction of nodes to decimate at every decimation
                 # step

    function MaxSumParams()
        accuracy = parse(Int, get(ENV, "JULIA_PKGRESOLVE_ACCURACY", "1"))
        if accuracy <= 0
            error("JULIA_PKGRESOLVE_ACCURACY must be > 0")
        end
        nondec_iterations = accuracy * 20
        dec_interval = accuracy * 10
        dec_fraction = 0.05 / accuracy
        return new(nondec_iterations, dec_interval, dec_fraction)
    end
end

# Graph holds the graph structure onto which max-sum is run, in
# sparse format
mutable struct Graph
    # adjacency matrix:
    #   for each package, has the list of neighbors
    #   indices (both dependencies and dependants)
    gadj::Vector{Vector{Int}}

    # compatibility mask:
    #   for each package p0 has a list of bool masks.
    #   Each entry in the list gmsk[p0] is relative to the
    #   package p1 as read from gadj[p0].
    #   Each mask has dimension spp1 x spp0, where
    #   spp0 is the number of states of p0, and
    #   spp1 is the number of states of p1.
    gmsk::Vector{Vector{BitMatrix}}

    # dependency direction:
    #   keeps track of which direction the dependency goes
    #   takes 3 values:
    #     1  = dependant
    #     -1 = dependency
    #     0  = both
    #   Used to break symmetry between dependants and
    #   dependencies (introduces a FieldValue at level l3).
    #   The "both" case is for when there are dependency
    #   relations which go both ways, in which case the
    #   noise is left to discriminate in case of ties
    gdir::Vector{Vector{Int}}

    # adjacency dict:
    #   allows one to retrieve the indices in gadj, so that
    #   gadj[p0][adjdict[p1][p0]] = p1
    #   ("At which index does package p1 appear in gadj[p0]?")
    adjdict::Vector{Dict{Int,Int}}

    # states per package: same as in Interface
    spp::Vector{Int}

    # update order: shuffled at each iteration
    perm::Vector{Int}

    # number of packages (all Vectors above have this length)
    np::Int

    function Graph(interface::Interface)
        deps = interface.deps
        np = interface.np

        spp = interface.spp
        pdict = interface.pdict
        pvers = interface.pvers
        vdict = interface.vdict

        gadj = [Int[] for i = 1:np]
        gmsk = [BitMatrix[] for i = 1:np]
        gdir = [Int[] for i = 1:np]
        adjdict = [Dict{Int,Int}() for i = 1:np]

        for (p,d) in deps
            p0 = pdict[p]
            vdict0 = vdict[p0]
            for (vn,a) in d
                v0 = vdict0[vn]
                for (rp, rvs) in a.requires
                    p1 = pdict[rp]

                    j0 = 1
                    while j0 <= length(gadj[p0]) && gadj[p0][j0] != p1
                        j0 += 1
                    end
                    j1 = 1
                    while j1 <= length(gadj[p1]) && gadj[p1][j1] != p0
                        j1 += 1
                    end
                    @assert (j0 > length(gadj[p0]) && j1 > length(gadj[p1])) ||
                            (j0 <= length(gadj[p0]) && j1 <= length(gadj[p1]))

                    if j0 > length(gadj[p0])
                        push!(gadj[p0], p1)
                        push!(gadj[p1], p0)
                        j0 = length(gadj[p0])
                        j1 = length(gadj[p1])

                        adjdict[p1][p0] = j0
                        adjdict[p0][p1] = j1

                        bm = trues(spp[p1], spp[p0])
                        bmt = trues(spp[p0], spp[p1])

                        push!(gmsk[p0], bm)
                        push!(gmsk[p1], bmt)

                        push!(gdir[p0], 1)
                        push!(gdir[p1], -1)
                    else
                        bm = gmsk[p0][j0]
                        bmt = gmsk[p1][j1]
                        if gdir[p0][j0] == -1
                            gdir[p0][j0] = 0
                            gdir[p1][j1] = 0
                        end
                    end

                    for v1 = 1:length(pvers[p1])
                        if pvers[p1][v1] ∉ rvs
                            bm[v1, v0] = false
                            bmt[v0, v1] = false
                        end
                    end
                    bm[end,v0] = false
                    bmt[v0,end] = false
                end
            end
        end

        perm = [1:np;]

        return new(gadj, gmsk, gdir, adjdict, spp, perm, np)
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

    # keep track of which variables have been decimated
    decimated::BitVector
    num_nondecimated::Int

    function Messages(interface::Interface, graph::Graph)
        reqs = interface.reqs
        pkgs = interface.pkgs
        np = interface.np
        spp = interface.spp
        pvers = interface.pvers
        pdict = interface.pdict
        vweight = interface.vweight

        # a "deterministic noise" function based on hashes
        function noise(p0::Int, v0::Int)
            s = pkgs[p0] * string(v0 == spp[p0] ? "UNINST" : pvers[p0][v0])
            Int128(hash(s))
        end

        # external fields: there are 2 terms, a noise to break potential symmetries
        #                  and one to favor newest versions over older, and no-version over all
        fld = [[FieldValue(0, zero(VersionWeight), vweight[p0][v0], (v0==spp[p0]), 0, noise(p0,v0)) for v0 = 1:spp[p0]] for p0 = 1:np]

        # enforce requirements
        for (rp, rvs) in reqs
            p0 = pdict[rp]
            pvers0 = pvers[p0]
            fld0 = fld[p0]
            for v0 = 1:spp[p0]-1
                vn = pvers0[v0]
                if !in(vn, rvs)
                    # the state is forbidden by requirements
                    fld0[v0] = FieldValue(-1)
                else
                    # the state is one of those explicitly requested:
                    # favor it at a higer level than normal (upgrade
                    # FieldValue from l2 to l1)
                    fld0[v0] += FieldValue(0, vweight[p0][v0], -vweight[p0][v0])
                end
            end
            # the uninstalled state is forbidden by requirements
            fld0[spp[p0]] = FieldValue(-1)
        end
        # normalize fields
        for p0 = 1:np
            m = maximum(fld[p0])
            for v0 = 1:spp[p0]
                fld[p0][v0] -= m
            end
        end

        initial_fld = deepcopy(fld)

        # initialize cavity messages to 0
        gadj = graph.gadj
        msg = [[zeros(FieldValue, spp[p0]) for p1 = 1:length(gadj[p0])] for p0 = 1:np]

        return new(msg, fld, initial_fld, falses(np), np)
    end
end

function getsolution(msgs::Messages)
    # the solution is just the location of the maximum in
    # each field

    fld = msgs.fld
    np = length(fld)
    sol = Vector{Int}(undef, np)
    for p0 = 1:np
        fld0 = fld[p0]
        s0 = argmax(fld0)
        if !validmax(fld0[s0])
            throw(UnsatError(p0))
        end
        sol[p0] = s0
    end
    return sol
end

# This is the core of the max-sum solver:
# for a given node p0 (i.e. a package) updates all
# input cavity messages and fields of its neighbors
function update(p0::Int, graph::Graph, msgs::Messages)
    gadj = graph.gadj
    gmsk = graph.gmsk
    gdir = graph.gdir
    adjdict = graph.adjdict
    spp = graph.spp
    np = graph.np
    msg = msgs.msg
    fld = msgs.fld
    decimated = msgs.decimated

    maxdiff = zero(FieldValue)

    gadj0 = gadj[p0]
    msg0 = msg[p0]
    fld0 = fld[p0]
    spp0 = spp[p0]
    adjdict0 = adjdict[p0]

    # iterate over all neighbors of p0
    for j0 in 1:length(gadj0)

        p1 = gadj0[j0]
        decimated[p1] && continue
        j1 = adjdict0[p1]
        #@assert j0 == adjdict[p1][p0]
        bm1 = gmsk[p1][j1]
        dir1 = gdir[p1][j1]
        spp1 = spp[p1]
        msg1 = msg[p1]

        # compute the output cavity message p0->p1
        cavmsg = fld0 - msg0[j0]

        if dir1 == -1
            # p0 depends on p1
            for v0 = 1:spp0-1
                cavmsg[v0] += FieldValue(0, VersionWeight(0), VersionWeight(0), 0, v0)
            end
        end

        # keep the old input cavity message p0->p1
        oldmsg = msg1[j1]

        # init the new message to minus infinity
        newmsg = [FieldValue(-1) for v1 = 1:spp1]

        # compute the new message by passing cavmsg
        # through the constraint encoded in the bitmask
        # (nearly equivalent to:
        #    newmsg = [maximum(cavmsg[bm1[:,v1]]) for v1 = 1:spp1]
        #  except for the gnrg term)
        m = FieldValue(-1)
        for v1 = 1:spp1
            for v0 = 1:spp0
                if bm1[v0, v1]
                    newmsg[v1] = max(newmsg[v1], cavmsg[v0])
                end
            end
            if dir1 == 1 && v1 != spp1
                # p1 depends on p0
                newmsg[v1] += FieldValue(0, VersionWeight(0), VersionWeight(0), 0, v1)
            end
            m = max(m, newmsg[v1])
        end
        if !validmax(m)
            # No state available without violating some
            # hard constraint
            throw(UnsatError(p1))
        end

        # normalize the new message
        for v1 = 1:spp1
            newmsg[v1] -= m
        end

        diff = newmsg - oldmsg
        maxdiff = max(maxdiff, maximum(abs.(diff)))

        # update the field of p1
        fld1 = fld[p1]
        for v1 = 1:spp1
            fld1[v1] += diff[v1]
        end

        # put the newly computed message in place
        msg1[j1] = newmsg
    end
    return maxdiff
end

# A simple shuffling machinery for the update order in iterate()
# (woulnd't pass any random quality test but it's arguably enough)
let step=1
global shuffleperm, shuffleperminit
shuffleperminit() = (step = 1)
function shuffleperm(graph::Graph)
    perm = graph.perm
    np = graph.np
    for j = np:-1:2
        k = mod(step,j)+1
        perm[j], perm[k] = perm[k], perm[j]
        step += isodd(j) ? 1 : k
    end
    #@assert isperm(perm)
end
end

# Call update for all nodes (i.e. packages) in
# random order
function iterate(graph::Graph, msgs::Messages)
    np = graph.np

    maxdiff = zero(FieldValue)
    shuffleperm(graph)
    perm = graph.perm
    for p0 in perm
        maxdiff0 = update(p0, graph, msgs)
        maxdiff = max(maxdiff, maxdiff0)
    end
    return maxdiff
end

function decimate1(p0::Int, graph::Graph, msgs::Messages)
    decimated = msgs.decimated
    fld = msgs.fld
    adjdict = graph.adjdict
    gmsk = graph.gmsk

    @assert !decimated[p0]
    fld0 = fld[p0]
    s0 = argmax(fld0)
    # only do the decimation if it is consistent with
    # the previously decimated nodes
    for p1 in findall(decimated)
        haskey(adjdict[p0], p1) || continue
        s1 = argmax(fld[p1])
        j1 = adjdict[p0][p1]
        gmsk[p1][j1][s0,s1] || return false
    end
    #println("DECIMATING $p0 (s0=$s0 fld=$fld0)")
    for v0 = 1:length(fld0)
        v0 == s0 && continue
        fld0[v0] = FieldValue(-1)
    end
    msgs.decimated[p0] = true
    msgs.num_nondecimated -= 1
    return true
end

function reset_messages!(msgs::Messages)
    msg = msgs.msg
    fld = msgs.fld
    initial_fld = msgs.initial_fld
    decimated = msgs.decimated
    np = length(fld)
    for p0 = 1:np
        map(m->fill!(m, zero(FieldValue)), msg[p0])
        decimated[p0] && continue
        fld[p0] = copy(initial_fld[p0])
    end
    return msgs
end

# If normal convergence fails (or is too slow) fix the most
# polarized packages by adding extra infinite fields on every state
# but the maximum
function decimate(n::Int, graph::Graph, msgs::Messages)
    #println("DECIMATING $n NODES")
    adjdict = graph.adjdict
    fld = msgs.fld
    decimated = msgs.decimated
    fldorder = sortperm(fld, by=secondmax)
    did_dec = false
    for p0 in fldorder
        decimated[p0] && continue
        did_dec |= decimate1(p0, graph, msgs)
        n -= 1
        n == 0 && break
    end
    @assert n == 0
    if !did_dec
        # did not succeed in decimating anything;
        # try to decimate at least one node
        for p0 in fldorder
            decimated[p0] && continue
            if decimate1(p0, graph, msgs)
                did_dec = true
                break
            end
        end
    end
    if !did_dec
        # still didn't succeed, give up
        p0 = first(fldorder[.~(decimated)])
        throw(UnsatError(p0))
    end

    reset_messages!(msgs)
    return
end

# In case ties still exist at convergence, break them and
# keep converging
function break_ties(msgs::Messages)
    fld = msgs.fld
    unbroken_ties = Int[]
    for p0 = 1:length(fld)
        fld0 = fld[p0]
        z = 0
        m = typemin(FieldValue)
        for v0 = 1:length(fld0)
            if fld0[v0] > m
                m = fld0[v0]
                z = 1
            elseif fld0[v0] == m
                z += 1
            end
        end
        if z > 1
            #println("TIE! p0=$p0")
            decimate1(p0, msgs) && return false
            push!(unbroken_ties, p0)
        end
    end
    # If there were ties, but none were broken, bail out
    isempty(unbroken_ties) || throw(PkgError(first(unbroken_ties)))
    return true
end

# Iterative solver: run iterate() until convergence
# (occasionally calling decimate())
function maxsum(graph::Graph, msgs::Messages)
    params = MaxSumParams()

    it = 0
    shuffleperminit()
    while true
        it += 1
        maxdiff = iterate(graph, msgs)
        #println("it = $it maxdiff = $maxdiff")

        if maxdiff == zero(FieldValue)
            break_ties(msgs) && break
            continue
        end
        if it >= params.nondec_iterations &&
           (it - params.nondec_iterations) % params.dec_interval == 0
            numdec = clamp(floor(Int, params.dec_fraction * graph.np), 1, msgs.num_nondecimated)
            decimate(numdec, graph, msgs)
            msgs.num_nondecimated == 0 && break
        end
    end

    # Finally, decimate everything just to
    # check against inconsistencies
    # (old_numnondec is saved just to prevent
    # wrong messages about accuracy)
    old_numnondec = msgs.num_nondecimated
    decimate(msgs.num_nondecimated, graph, msgs)
    msgs.num_nondecimated = old_numnondec

    return getsolution(msgs)
end

end
