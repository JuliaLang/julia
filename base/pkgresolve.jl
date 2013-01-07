require("pkgmetadata")

module PkgResolve
#
# Use max-sum algorithm to resolve packages dependencies
#

# TODO:
#     contradictory requirements/dependecies should be handled
#     more nicely, e.g. triggering an analysis of what went wrong


import Metadata.Version, Metadata.VersionSet,
       Metadata.packages, Metadata.versions, Metadata.dependencies

import Base.<, Base.<=, Base.==, Base.-, Base.+,
       Base.zero, Base.isless, Base.abs, Base.typemin, Base.typemax,
       Base.indmax

export resolve

# Some parameters to drive the decimation process
const nondec_iterations = 6 # number of initial iterations before starting
                            # decimation
const dec_interval = 3      # number of iterations between decimations
const dec_fraction = 0.05   # fraction of nodes to decimate at every decimation
                            # step

# Fetch all data and keep it in a single structure
type ReqsStruct
    reqs::Vector{VersionSet}
    pkgs::Vector{String}
    vers::Vector{Version}
    deps::Vector{(Version,VersionSet)}
    np::Int

    function ReqsStruct(reqs::Vector{VersionSet})
        pkgs = packages()
        vers = versions(pkgs)
        deps = dependencies(pkgs,vers)

        #sort!(deps)
        #println("DEPS:")
        #for d in deps
            #println("$(d[1].package) v$(d[1].version) <- $(d[2].package) $(d[2].versions)")
        #end
        #println()

        np = length(pkgs)

        return new(reqs, pkgs, vers, deps, np)
    end
end

# Auxiliary structure to map data from ReqsStruct into
# internal representation and vice versa
type PkgStruct
    # states per package: one per version + uninstalled
    spp::Vector{Int}

    # pakage dict: associates an index to each package name
    pdict::Dict{String,Int}

    # package versions: for each package, keep the list of the
    #                   possible version numbers; this defines a
    #                   mapping from version numbers of a package
    #                   to indices
    pvers::Vector{Vector{VersionNumber}}

    # versions dict: associates a package number and a version index
    #                to each Version: suppose we have v::Version such that
    #                  v.package == p
    #                  v.version == vn
    #                and that
    #                  vdict[v] = (p0, v0)
    #                then
    #                  pdict[p] = p0
    #                  pvers[p0][v0] = vn
    vdict::Dict{ASCIIString,(Int,Int)}

    function PkgStruct(reqsstruct::ReqsStruct)

        pkgs = reqsstruct.pkgs
        vers = reqsstruct.vers
        np = reqsstruct.np

        spp = ones(Int, np)
        pdict = [ pkgs[i] => i for i = 1:np ]
        pvers = [ VersionNumber[] for i = 1:np ]

        for v in vers
            vp = v.package
            vv = v.version
            j = pdict[vp]
            spp[j] += 1
            push!(pvers[j], vv)
        end
        for j = 1:np
            sort!(pvers[j])
        end

        vdict = (ASCIIString=>(Int,Int))[]
        for v in vers
            vp = v.package
            vv = v.version
            j = pdict[vp]
            for i in 1:length(pvers[j])
                if pvers[j][i] == vv
                    vdict[string(v)] = (j, i)
                    break
                end
            end
        end

        return new(spp, pdict, pvers, vdict)
    end
end

# FieldValue is a numeric type which helps dealing with
# infinities. It holds 5 numbers l0,l1,l2,l3,l4. It can
# be interpreted as a polynomial
#  x = a^4 * l0 + a^3 * l1 + a^2 + l2 + a^1 * l3 + l4
# where a -> Inf
# The levels are used as such:
#  l0 : for hard constraints (dependencies and requirements)
#  l1 : for favoring higher versions of the explicitly required
#       packages
#  l2 : for favoring higher versions of all other packages (and
#       favoring uninstallation of non-needed packages)
#  l3 : for favoring dependants over dependencies
#  l4 : for symmetry-breaking random noise
#
type FieldValue
    v::Vector{Int}
    function FieldValue(v::Vector{Int})
        if length(v) != 5
            error("FieldValue only accepts Vectors of length 5")
        end
        new(v)
    end
end
FieldValue(l0::Int,l1::Int,l2::Int,l3::Int,l4::Int) = FieldValue([l0, l1, l2, l3, l4])
FieldValue(l0::Int,l1::Int,l2::Int,l3::Int) = FieldValue([l0, l1, l2, l3, 0])
FieldValue(l0::Int,l1::Int,l2::Int) = FieldValue([l0, l1, l2, 0, 0])
FieldValue(l0::Int,l1::Int) = FieldValue([l0, l1, 0, 0, 0])
FieldValue(l0::Int) = FieldValue([l0, 0, 0, 0, 0])
FieldValue() = FieldValue([0,0,0,0,0])

zero(::Type{FieldValue}) = FieldValue()

typemin(::Type{FieldValue}) = FieldValue([typemin(Int) for i = 1:5])
typemax(::Type{FieldValue}) = FieldValue([typemax(Int) for i = 1:5])

(-)(a::FieldValue, b::FieldValue) = FieldValue(a.v - b.v)
(+)(a::FieldValue, b::FieldValue) = FieldValue(a.v + b.v)

(==)(a::FieldValue, b::FieldValue) = (a.v == b.v)

function (<)(a::FieldValue, b::FieldValue)
    va = a.v
    vb = b.v
    return va[1] < vb[1] || (va[1] == vb[1] &&
           (va[2] < vb[2] || (va[2] == vb[2] &&
           (va[3] < vb[3] || (va[3] == vb[3] &&
           (va[4] < vb[4] || (va[4] == vb[4] &&
           va[5] < vb[5])))))))
end

isless(a::FieldValue, b::FieldValue) = a < b

abs(a::FieldValue) = FieldValue(abs(a.v))
abs(v::Vector{FieldValue}) = FieldValue[abs(a) for a in v]

# if the maximum field has l0 < 0, it means that
# some hard constraint is being violated
validmax(a::FieldValue) = a.v[1] >= 0

# like usual indmax, but favors the highest indices
# in case of a tie
function indmax(v::Vector{FieldValue})
    m = typemin(FieldValue)
    mi = 0
    for j = length(v):-1:1
        if v[j] > m
            m = v[j]
            mi = j
        end
    end
    @assert mi != 0
    return mi
end

# secondmax returns the value of the second maximum in a vector
# of FieldValues. It's used to determine the most polarized field.
function secondmax(v::Vector{FieldValue})
    m = typemin(FieldValue)
    m2 = typemin(FieldValue)
    for i = 1:length(v)
        a = v[i]
        if a > m
            m2 = m
            m = a
        elseif a > m2
            m2 = a
        end
    end
    return m2
end

# Graph holds the graph structure onto which max-sum is run, in
# sparse format
type Graph
    # adjacency matrix: for each package, has the list of neighbors
    #                   indices (both dependencies and dependants)
    gadj::Vector{Vector{Int}}

    # compatibility mask: for each package p0 has a list of bool masks.
    #                     Each entry in the list gmsk[p0] is relative to the
    #                     package p1 as read from gadj[p0].
    #                     Each mask has dimension spp1 x spp0, where
    #                     spp0 is the number of states of p0, and
    #                     spp1 is the number of states of p1.
    gmsk::Vector{Vector{BitMatrix}}

    # energy mask: like gmsk, but it's used to favor dependants over
    # dependencies in case of a tie (works at FieldValue level l3)
    gnrg::Vector{Vector{Matrix{Int}}}

    # adjacency dict: allows to retrieve the indices in gadj, so that
    #                 gadj[p0][adjdict[p0][p1]] = p1
    adjdict::Vector{Dict{Int,Int}}

    # states per package: same as in PkgStruct
    spp::Vector{Int}

    # update order: shuffled at each iteration
    perm::Vector{Int}

    # number of packages (all Vectors above have this length)
    np::Int

    function Graph(reqsstruct::ReqsStruct,
                   pkgstruct::PkgStruct)

        deps = reqsstruct.deps
        np = reqsstruct.np

        spp = pkgstruct.spp
        pdict = pkgstruct.pdict
        pvers = pkgstruct.pvers
        vdict = pkgstruct.vdict

        gadj = [ Int[] for i = 1:np ]
        gmsk = [ BitMatrix[] for i = 1:np ]
        gnrg = [ Matrix{Int}[] for i = 1:np ]
        adjdict = [ (Int=>Int)[] for i = 1:np ]

        for d in deps
            p0, v0 = vdict[string(d[1])]
            vs = d[2]
            p1 = pdict[vs.package]

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

                adjdict[p0][p1] = j0
                adjdict[p1][p0] = j1

                bm = trues(spp[p1], spp[p0])
                bmt = bm'

                push!(gmsk[p0], bm)
                push!(gmsk[p1], bmt)

                nrgm = zeros(Int, spp[p1], spp[p0])
                for vv0 = 1:spp[p0]-2, vv1 = 1:spp[p1]-1
                    nrgm[vv1,vv0] = v0 - spp[p0] + 1
                end
                nrgmt = nrgm'
                push!(gnrg[p0], nrgm)
                push!(gnrg[p1], nrgmt)
            else
                bm = gmsk[p0][j0]
                bmt = gmsk[p1][j1]
                nrgm = gnrg[p0][j0]
                nrgmt = gnrg[p1][j1]
            end

            nrgi = 0
            for v1 = 1:length(pvers[p1])
                if !contains(vs, Version(vs.package, pvers[p1][v1]))
                    bm[v1, v0] = false
                    bmt[v0, v1] = false
                    nrgm[v1, v0] = 0
                    nrgmt[v0, v1] = 0
                end
            end
            bm[end,v0] = false
            bmt[v0,end] = false
        end

        perm = [1:np]

        return new(gadj, gmsk, gnrg, adjdict, spp, perm, np)
    end
end

# Messages has the cavity messages and the total fields, and
# gets updated iteratively (and occasionally decimated) until
# convergence
type Messages
    # cavity incoming messages: for each package p0,
    #                           for each neighbor p1 of p0,
    #                           msg[p0][p1] is a vector of length spp[p0]
    #                           messages are normalized (i.e. the max is always 0)
    msg::Vector{Vector{Vector{FieldValue}}}

    # overall fields: for each package p0,
    #                 fld[p0] is a vector of length spp[p0]
    #                 fields are normalized (i.e. the max is always 0)
    fld::Vector{Vector{FieldValue}}

    # keep track of which variables have been decimated
    decimated::BitVector
    num_nondecimated::Int

    function Messages(reqsstruct::ReqsStruct,
                      pkgstruct::PkgStruct,
                      graph::Graph)

        reqs = reqsstruct.reqs
        pkgs = reqsstruct.pkgs
        vers = reqsstruct.vers
        np = reqsstruct.np
        spp = pkgstruct.spp
        pvers = pkgstruct.pvers
        vdict = pkgstruct.vdict

        function noise(p0::Int, v0::Int)
            s = pkgs[p0] * string(v0 == spp[p0] ? "UNINST" : pvers[p0][v0])
            int(hash(s)) >>> 30
        end

        # external fields: there are 2 terms, a noise to break potential symmetries
        #                  and one to favor newest versions over older, and no-version over all
        fld = [ [ FieldValue(0,0,v0-1,0,noise(p0,v0)) for v0 = 1:spp[p0] ] for p0 = 1:np]

        # enforce requirements as infinite external fields over the desired
        # version ranges
        reqps = falses(np)
        reqmsk = [ falses(spp[p0]) for p0 = 1:np ]

        for r in reqs, v in vers
            if contains(r, v)
                p0, v0 = vdict[string(v)]
                reqps[p0] = true
                reqmsk[p0][v0] = true
            end
        end
        for p0 = 1:np
            if reqps[p0]
                for v0 = 1:spp[p0]
                    if !reqmsk[p0][v0]
                        # the state is forbidden by requirements
                        fld[p0][v0] = FieldValue(-1)
                    else
                        # the state is one of those explicitly requested:
                        # favor it at a higer level than normal (upgrade
                        # FieldValue from l2 to l1)
                        fld[p0][v0] += FieldValue(0,v0-1,-v0+1)
                    end
                end
            end
        end
        # normalize fields
        for p0 = 1:np
            m = max(fld[p0])
            for v0 = 1:spp[p0]
                fld[p0][v0] -= m
            end
        end

        # initialize cavity messages to 0
        gadj = graph.gadj
        msg = [ [ zeros(FieldValue,spp[p0]) for p1 = 1:length(gadj[p0])] for p0 = 1:np]

        return new(msg, fld, falses(np), np)
    end
end

function getsolution(msgs::Messages)
    # the solution is just the location of the maximum in
    # each field
    return map(indmax, msgs.fld)
end

# This is the core of the max-sum solver:
# for a given node p0 (i.e. a package) updates all
# input cavity messages and fields of its neighbors
function update(p0::Int, graph::Graph, msgs::Messages)

    gadj = graph.gadj
    gmsk = graph.gmsk
    gnrg = graph.gnrg
    adjdict = graph.adjdict
    spp = graph.spp
    np = graph.np
    msg = msgs.msg
    fld = msgs.fld

    maxdiff = zero(FieldValue)

    gadj0 = gadj[p0]
    msg0 = msg[p0]
    fld0 = fld[p0]
    spp0 = spp[p0]

    # iterate over all neighbors of p0
    for j0 in 1:length(gadj0)

        p1 = gadj0[j0]
        j1 = adjdict[p1][p0]
        #@assert j0 == adjdict[p0][p1]
        bm1 = gmsk[p1][j1]
        nrg1 = gnrg[p1][j1]
        spp1 = spp[p1]
        msg1 = msg[p1]

        # compute the output cavity message p0->p1
        cavmsg = fld0 - msg0[j0]

        # keep the old input cavity message p0->p1
        oldmsg = msg1[j1]

        # init the new message to minus infinity
        newmsg = [ FieldValue(-1) for v1 = 1:spp1 ]

        # compute the new message by passing cavmsg
        # through the constraint encoded in the bitmask
        # (roughly equivalent to:
        #    newmsg = [ max(cavmsg[bm1[:,v1]]) for v1 = 1:spp1 ]
        #  except for the gnrg term)
        m = FieldValue(-1)
        for v1 = 1:spp1
            for v0 = 1:spp0
                if bm1[v0, v1]
                    newmsg[v1] = max(newmsg[v1], cavmsg[v0] + FieldValue(0,0,0,nrg1[v0,v1]))
                end
            end
            if newmsg[v1] > m
                m = newmsg[v1]
            end
        end
        @assert validmax(m) # TODO: failure here is the result of contradictory requirements,
                            #       we should throw an exception

        # normalize the new message
        for v1 = 1:spp1
            newmsg[v1] -= m
        end

        absdiff = max(abs(newmsg - oldmsg))
        maxdiff = max(maxdiff, absdiff)


        # update the field of p1
        fld1 = fld[p1]
        m = FieldValue(-1)
        for v1 = 1:spp1
            fld1[v1] += newmsg[v1] - oldmsg[v1]
            if fld1[v1] > m
                m = fld1[v1]
            end
        end
        @assert validmax(m) # TODO: failure here is the result of contradictory requirements,
                            #       we should throw an exception

        # normalize the field
        for v1 = 1:spp1
            fld1[v1] -= m
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
    @assert isperm(perm)
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

function decimate1(p0::Int, msgs::Messages)
    @assert !msgs.decimated[p0]
    fld0 = msgs.fld[p0]
    s0 = indmax(fld0)
    #println("DECIMATING $p0 ($(packages()[p0]) s0=$s0)")
    for v0 = 1:length(fld0)
        if v0 != s0
            fld0[v0] -= FieldValue(1)
        end
    end
    msgs.decimated[p0] = true
    msgs.num_nondecimated -= 1
end

# If normal convergence fails (or is too slow) fix the most
# polarized packages by adding extra infinite fields on every state
# but the maximum
function decimate(n::Int, graph::Graph, msgs::Messages)
    fld = msgs.fld
    decimated = msgs.decimated
    fldorder = Sort.sortperm_by(secondmax, fld)[2]
    for p0 in fldorder
        if decimated[p0]
            continue
        end
        decimate1(p0, msgs)
        n -= 1
        if n == 0
            break
        end
    end
    @assert n == 0
    return
end

# In case ties still exist at convergence, break them and
# keep converging
function break_ties(msgs::Messages)
    fld = msgs.fld
    for p0 = 1:length(fld)
        fld0 = fld[p0]
        z = 0
        for v0 = 1:length(fld0)
            if fld0[v0] == zero(FieldValue)
                z += 1
                if z > 1
                    break
                end
            end
        end
        if z > 1
            #println("TIE! p0=$p0")
            decimate1(p0, msgs)
            return false
        end
    end
    return true
end

# Iterative solver: run iterate() until convergence
# (occasionally calling decimate())
function converge(graph::Graph, msgs::Messages)

    it = 0
    shuffleperminit()
    while true
        it += 1
        maxdiff = iterate(graph, msgs)
        #println("it = $it maxdiff = $maxdiff")

        if maxdiff == zero(FieldValue)
            if break_ties(msgs)
                break
            else
                continue
            end
        end
        if it >= nondec_iterations && (it - nondec_iterations) % dec_interval == 0
            numdec = clamp(ifloor(dec_fraction * graph.np),  1, msgs.num_nondecimated)
            decimate(numdec, graph, msgs)
            if msgs.num_nondecimated == 0
                break
            end
        end
    end

    return getsolution(msgs)
end

# The output format is a dict which associates sha1's to each installed package name
function compute_output_dict(reqsstruct::ReqsStruct, pkgstruct::PkgStruct, sol::Vector{Int})

    pkgs = reqsstruct.pkgs
    np = reqsstruct.np
    pvers = pkgstruct.pvers
    spp = pkgstruct.spp

    want = (String=>ASCIIString)[]
    for p0 = 1:np
        p = pkgs[p0]
        s = sol[p0]
        if s != spp[p0]
            v = pvers[p0][s]
            want[p] = readchomp("METADATA/$p/versions/$v/sha1")
            #want[p] = "$v"
        end
    end

    return want
end

# verifies that the solution fulfills all hard constraints
# (requirements and dependencies)
function verify_sol(reqsstruct::ReqsStruct, pkgstruct::PkgStruct, sol::Vector{Int})

    reqs = reqsstruct.reqs
    deps = reqsstruct.deps
    spp = pkgstruct.spp
    pdict = pkgstruct.pdict
    pvers = pkgstruct.pvers
    vdict = pkgstruct.vdict

    # verify requirements
    for r in reqs
        p = r.package
        p0 = pdict[p]
        @assert sol[p0] != spp[p0]
        v = pvers[p0][sol[p0]]
        @assert contains(r, Version(p, v))
    end

    # verify dependencies
    for d in deps
        p0, v0 = vdict[string(d[1])]
        if sol[p0] == v0
            vs = d[2]
            p = vs.package
            p1 = pdict[p]
            @assert sol[p1] != spp[p1]
            v = pvers[p1][sol[p1]]
            @assert contains(vs, Version(p, v))
        end
    end

end

# Verifies that the given solution is a local optimium, i.e. that for each
# installed package, bumping its version would violate some hard constraint
function verify_optimality(reqsstruct::ReqsStruct, pkgstruct::PkgStruct, sol::Vector{Int})
    np = reqsstruct.np

    reqs = reqsstruct.reqs
    deps = reqsstruct.deps
    spp = pkgstruct.spp
    pdict = pkgstruct.pdict
    pvers = pkgstruct.pvers
    vdict = pkgstruct.vdict

    for p0 = 1:np
        s0 = sol[p0]
        if s0 >= spp[p0] - 1
            # either the package is not installed,
            # or it's already at the maximum version
            continue
        end
        # check if bumping would violate a requirement
        viol = false
        for r in reqs
            p = r.package
            if p0 != pdict[p]
                continue
            end
            v = pvers[p0][s0+1]
            if !contains(r, Version(p, v))
                viol = true
                break
            end
        end
        if viol
            continue
        end
        # check if the higher version has a depencency which
        # would be violated by the state of the remaining packages
        for d in deps
            p0b, v0 = vdict[string(d[1])]
            if p0 != p0b || v0 != s0+1
                # we're looking for the depencencies of the
                # higher version
                continue
            end
            vs = d[2]
            p = vs.package
            p1 = pdict[p]
            if sol[p1] == spp[p1]
                # the dependency is violated because
                # the other package is not being installed
                viol = true
                break
            end
            v = pvers[p1][sol[p1]]
            if !contains(vs, Version(p, v))
                # the dependency is violated because
                # the other package version is invalid
                viol = true
                break
            end
        end
        if viol
            continue
        end
        # check if bumping the version would violate some
        # dependency of another package
        for d in deps
            vs = d[2]
            p = vs.package
            if p0 != pdict[p]
                # we're looking for packages which
                # depend on this one
                continue
            end
            p1, v1 = vdict[string(d[1])]
            if sol[p1] != v1
                # we're looking for the dependencies
                # of the (other) installed packages)
                continue
            end
            v = pvers[p0][s0+1]
            if !contains(vs, Version(p, v))
                # bumping the version would violate
                # the dependency
                viol = true
                break
            end
        end
        if viol
            continue
        end
        # So the solution is non-optimal
        # TODO: we should probably update manually and iterate when this happens
        # (never seen this happen)
        println(stderr_stream, "Warning: nonoptimal solution for package $(reqsstruct.pkgs[p0]): sol=$s0")
        return false
    end
    return true
end

# The external-facing function
function resolve(reqs)
    # fetch data
    reqsstruct = ReqsStruct(reqs)

    # init structures
    pkgstruct = PkgStruct(reqsstruct)
    graph = Graph(reqsstruct, pkgstruct)
    msgs = Messages(reqsstruct, pkgstruct, graph)

    # find solution
    sol = converge(graph, msgs)

    # verify solution (debug code)
    verify_sol(reqsstruct, pkgstruct, sol)
    verify_optimality(reqsstruct, pkgstruct, sol)

    # return the solution as a Dict mapping package_name => sha1
    return compute_output_dict(reqsstruct, pkgstruct, sol)
end

end # module PkgResolve
