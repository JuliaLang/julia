module Resolve
#
# Use max-sum algorithm to resolve packages dependencies
#

import Metadata.Version, Metadata.VersionSet,
       Metadata.packages, Metadata.versions, Metadata.dependencies

import Base.<, Base.<=, Base.==, Base.-, Base.+,
       Base.zero, Base.isless, Base.abs, Base.typemin, Base.typemax,
       Base.indmax

export resolve, sanity_check, MetadataError

# An exception type used internally to signal that an unsatisfiable
# constraint was detected
type UnsatError <: Exception
    info
end

type MetadataError <: Exception
    info
end

# Some parameters to drive the decimation process
type ResolveParams
    nondec_iterations # number of initial iterations before starting
                      # decimation
    dec_interval # number of iterations between decimations
    dec_fraction # fraction of nodes to decimate at every decimation
                 # step

    function ResolveParams()
        accuracy = int(get(ENV, "JULIA_PKGRESOLVE_ACCURACY", 1))
        if accuracy <= 0
            error("JULIA_PKGRESOLVE_ACCURACY must be >= 1")
        end
        nondec_iterations = accuracy * 20
        dec_interval = accuracy * 10
        dec_fraction = 0.05 / accuracy
        return new(nondec_iterations, dec_interval, dec_fraction)
    end
end

# Fetch all data and keep it in a single structure
type ReqsStruct
    reqs::Vector{VersionSet}
    pkgs::Vector{String}
    vers::Vector{Version}
    deps::Vector{(Version,VersionSet)}
    np::Int

    function ReqsStruct(
        reqs::Vector{VersionSet},
        pkgs::Vector{String},
        vers::Vector{Version},
        deps::Vector{(Version,VersionSet)})
        new(reqs, pkgs, vers, deps, length(pkgs))
    end
end

function ReqsStruct(reqs::Vector{VersionSet}, fixed::Dict)
    pkgs = packages()
    vers = versions(pkgs)
    deps = dependencies(union(pkgs,keys(fixed)))

    filter!(reqs) do r
        if has(fixed, r.package)
            if !contains(r, Version(r.package,fixed[r.package]))
                warn("$(r.package) is fixed at $(repr(fixed[r.package])) which doesn't satisfy $(r.versions).")
            end
            false
        else
            true
        end
    end
    filter!(pkgs) do p
        !has(fixed, p)
    end
    filter!(vers) do v
        !has(fixed, v.package)
    end
    unsatisfiable = Set{Version}()
    filter!(deps) do d
        p = d[2].package
        if has(fixed, p)
            if !contains(d[2], Version(p, fixed[p]))
                add!(unsatisfiable, d[1])
            end
            false # drop
        else
            true # keep
        end
    end
    filter!(vers) do v
        !contains(unsatisfiable, v)
    end
    filter!(deps) do d
        !contains(unsatisfiable, d[1])
    end
    version_packages_set = Set{String}(String[v.package for v in vers]...)
    filter!(pkgs) do p
        contains(version_packages_set, p)
    end
    for r in reqs
        contains(pkgs, r.package) && continue
        error("$(r.package) has no versions compatible with your fixed requirements (e.g. julia version).")
    end

    ReqsStruct(reqs, pkgs, vers, deps)
end

# The numeric type used to determine how the different
# versions of a package should be weighed
typealias VersionWeight Int

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
    vdict::Dict{Version,(Int,Int)}

    # version weights: the weight for each version of each package
    #                  (versions include the uninstalled state; the
    #                   higher the weight, the more favored the version)
    vweight::Vector{Vector{VersionWeight}}

    # has version pruning been performed?
    # (used for debug purposes only)
    waspruned::Bool

    PkgStruct(spp::Vector{Int}, pdict::Dict{String,Int},
              pvers::Vector{Vector{VersionNumber}},
              vdict::Dict{Version,(Int,Int)},
              vweight::Vector{Vector{VersionWeight}}) =
        new(spp, pdict, pvers, vdict, vweight, false)
end

# The initial constructor function (pre variable pruning)
function PkgStruct(reqsstruct::ReqsStruct)

    pkgs = reqsstruct.pkgs
    vers = reqsstruct.vers
    np = reqsstruct.np

    pdict = [ pkgs[i] => i for i = 1:np ]

    spp, pvers = gen_pvers(np, pdict, vers)
    vdict = gen_vdict(pdict, pvers, vers)

    # the version weights are just progressive integer numbers,
    # there is no difference between major, minor, patch etc.
    # TODO: change this to weigh differently major, minor etc. ?
    vweight = [ [ v0-1 for v0 = 1:spp[p0] ] for p0 = 1:np ]

    return PkgStruct(spp, pdict, pvers, vdict, vweight)
end

# Generate the pvers field in PkgStruct; used by the
# constructor and within `prune_versions!`
function gen_pvers(np::Int, pdict::Dict{String,Int}, vers::Vector{Version})
    spp = ones(Int, np)

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

    return spp, pvers
end

# Generate the vdict field in PkgStruct; used by the
# constructor and within `prune_versions!`
function gen_vdict(pdict::Dict{String,Int}, pvers::Vector{Vector{VersionNumber}},
                   vers::Vector{Version})

    vdict = (Version=>(Int,Int))[]
    for v in vers
        vp = v.package
        vv = v.version
        j = pdict[vp]
        for i in 1:length(pvers[j])
            if pvers[j][i] == vv
                vdict[v] = (j, i)
                break
            end
        end
    end
    return vdict
end

# Reduce the number of versions by creating equivalence classes, and retaining
# only the highest version for each equivalence class.
# Two versions are equivalent if:
#   1) They appear together as dependecies of another package (i.e. for each
#      dependency relation, they are both required or both not required)
#   2) They have the same dependencies
# Also, for each package explicitly required, dicards all versions outside
# the allowed range (checking for impossible ranges while at it).
# This function mutates both input structs.
prune_versions!(reqsstruct::ReqsStruct, pkgstruct::PkgStruct) = prune_versions!(reqsstruct, pkgstruct, true)
function prune_versions!(reqsstruct::ReqsStruct, pkgstruct::PkgStruct, prune_reqs::Bool)

    np = reqsstruct.np
    reqs = reqsstruct.reqs
    pkgs = reqsstruct.pkgs
    vers = reqsstruct.vers
    deps = reqsstruct.deps
    spp = pkgstruct.spp
    pdict = pkgstruct.pdict
    pvers = pkgstruct.pvers
    vdict = pkgstruct.vdict
    vweight = pkgstruct.vweight

    # To each version in each package, we associate a BitVector.
    # It is going to hold a pattern such that all versions with
    # the same pattern are equivalent.
    vmask = [ [ BitVector() for v0 = 1:spp[p0]-1 ] for p0 = 1:np ]

    # From the point of view of resolve(), VectorSet(pkg,[]) and
    # VectorSet(pkg, [v0]) are equivelent if v0 is the first
    # available version of pkg
    function contains_any(vs::VersionSet, first::VersionNumber)
        vvs = vs.versions
        return isempty(vvs) || (length(vvs) == 1 && vvs[1] == first)
    end

    # Parse requirements and store allowed versions.
    allowed = [ trues(spp[p0]-1) for p0 = 1:np ]
    if prune_reqs
        for r in reqs
            p = r.package
            p0 = pdict[p]
            pvers0 = pvers[p0]
            allowed0 = allowed[p0]
            for v0 = 1:spp[p0]-1
                v = pvers0[v0]
                allowed0[v0] = contains(r, Version(p, v))
            end
        end
        for p0 = 1:np
            allowed0 = allowed[p0]
            if !any(allowed0)
                error("Invalid requirements: no version allowed for package $(pkgs[p0])")
            end
        end
    end

    # Parse the dependency list, segregate them according to the
    # dependant package and version
    pdeps = [ [ VersionSet[] for v0 = 1:spp[p0]-1 ] for p0 = 1:np ]
    for d in deps
        p0, v0 = vdict[d[1]]
        if !allowed[p0][v0]
            continue
        end
        p1 = pdict[d[2].package]
        if contains_any(d[2], pvers[p1][1])
            dd = VersionSet(d[2].package)
        else
            dd = d[2]
        end
        push!(pdeps[p0][v0], dd)
    end

    # For each package, we examine the dependencies of its versions
    # and put together those which are equal.
    # While we're at it, we also collect all dependencies into alldeps
    alldeps = VersionSet[]
    for p0 = 1:np
        pdeps0 = pdeps[p0]

        # Extract unique dependencies lists (aka classes), thereby
        # assigning an index to each class.
        uniqdepssets = unique(pdeps0)

        # Store all dependencies seen so far for later use
        for dd in uniqdepssets, v in dd
            push!(alldeps, v)
        end

        # If the package has just one version, it's uninteresting
        if spp[p0] == 2
            continue
        end

        # Grow the pattern by the number of classes
        ff = falses(length(uniqdepssets))
        vmask0 = vmask[p0]
        vmind_base = length(vmask0[1])
        for vm in vmask0
            append!(vm, ff)
        end

        # For each version, determine to which class it belongs and
        # store that info in the patterns
        for v0 = 1:spp[p0]-1
            if !allowed[p0][v0]
                continue
            end
            vmind = findfirst(uniqdepssets, pdeps0[v0])
            @assert vmind >= 0
            vmind += vmind_base
            vm = vmask0[v0]
            vm[vmind] = true
        end
    end

    # Produce dependency patterns. Ideally, one would use unique(alldeps)
    # here (or better still, use a Set from the beginning), but it takes
    # more time than it saves - better waste a little extra memory.
    for r in alldeps
        p = r.package
        p0 = pdict[p]

        # packages with just one version, or dependencies
        # which do not distiguish between versions, are not
        # interesting
        if spp[p0] == 2 || contains_any(r, pvers[p0][1])
            continue
        end

        pvers0 = pvers[p0]

        # Grow the patterns by one bit
        vmask0 = vmask[p0]
        for vm in vmask0
            resize!(vm, length(vm)+1)
        end

        # Store the dependency info in the patterns
        for v0 = 1:spp[p0]-1
            v = pvers0[v0]
            vm = vmask0[v0]
            vm[end] = contains(r, Version(p, v))
        end
    end

    # At this point, the vmask patterns are computed. We divide them into
    # classes so that we can keep just one version for each class.
    pruned_vers_id = [ Int[] for p0 = 1:np ]
    eq_classes_map = [ (VersionNumber=>VersionNumber)[] for p0 = 1:np ]
    for p0 = 1:np
        vmask0 = vmask[p0]
        vmask0_uniq = unique(vmask0)
        nc = length(vmask0_uniq)
        classes = [ Int[] for c0 = 1:nc ]
        for v0 = 1:spp[p0]-1
            if !allowed[p0][v0]
                continue
            end
            vm = vmask0[v0]
            c0 = findfirst(vmask0_uniq, vm)
            push!(classes[c0], v0)
        end

        # For each nonempty class, we store only the last entry (i.e. the
        # highest version)
        pruned0 = pruned_vers_id[p0]
        eqclass0 = eq_classes_map[p0]
        pvers0 = pvers[p0]
        for cl in classes
            if !isempty(cl)
                vtop0 = cl[end]
                push!(pruned0, vtop0)
                if !prune_reqs
                    vtop = pvers0[vtop0]
                    for v0 in cl
                        eqclass0[pvers0[v0]] = vtop
                    end
                end
            end
        end
        sort!(pruned0)
    end

    # All that follows is just recomputing the structures' fields
    # by throwing away unnecessary versions

    # Recompute pvers
    new_pvers = [ pvers[p0][pruned_vers_id[p0]] for p0 = 1:np ]

    # Recompute vers
    new_vers = Version[]
    for p0 = 1:np
        p = pkgs[p0]
        for v in new_pvers[p0]
            push!(new_vers, Version(p, v))
        end
    end

    # Reompute deps. We could simplify them, but it's not worth it
    new_deps = Array((Version,VersionSet), 0)

    for d0 = 1:length(deps)
        d = deps[d0]
        p0, v0 = vdict[d[1]]
        if !contains(pruned_vers_id[p0], v0)
            continue
        end
        push!(new_deps, d)
    end

    reqsstruct.vers = new_vers
    reqsstruct.deps = new_deps

    # Finally, mutate pkgstruct fields by regenerating pvers, vdict
    # and vweights

    new_spp, new_pvers = gen_pvers(np, pdict, new_vers)
    new_vdict = gen_vdict(pdict, new_pvers, new_vers)

    new_vweight = [ Array(VersionWeight,new_spp[p0]) for p0 = 1:np ]
    for p0 = 1:np
        vweight0 = vweight[p0]
        new_vweight0 = new_vweight[p0]
        pversid0 = pruned_vers_id[p0]
        for v0 = 1:length(pversid0)
            new_vweight0[v0] = vweight0[pversid0[v0]]
        end
        new_vweight0[end] = vweight0[end]
    end

    pkgstruct.spp = new_spp
    pkgstruct.pvers = new_pvers
    pkgstruct.vdict = new_vdict
    pkgstruct.vweight = new_vweight
    if prune_reqs
        pkgstruct.waspruned = true
    end

    #println("pruning stats:")
    #println("  before: vers=$(length(vers)) deps=$(length(deps))")
    #println("  after: vers=$(length(new_vers)) deps=$(length(new_deps))")

    return eq_classes_map
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
    va[1] < vb[1] && return true
    va[1] > vb[1] && return false
    va[2] < vb[2] && return true
    va[2] > vb[2] && return false
    va[3] < vb[3] && return true
    va[3] > vb[3] && return false
    va[4] < vb[4] && return true
    va[4] > vb[4] && return false
    va[5] < vb[5] && return true
    return false
end

isless(a::FieldValue, b::FieldValue) = a < b

abs(a::FieldValue) = FieldValue(abs(a.v))
abs(v::Vector{FieldValue}) = FieldValue[abs(a) for a in v]

# A faster, in-place version of
#   a += FieldValue(0,0,0,x,0)
# where the position of the x is set by n
inplaceadd!(a::FieldValue, x::Int, n::Int) = (a.v[n] = x)

# A faster, in-place version of
#   a += b
function inplaceadd!(a::FieldValue, b::FieldValue)
    av = a.v
    bv = b.v
    for i = 1:5
        av[i] += bv[i]
    end
end

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

# secondmax returns the (normalized) value of the second maximum in a
# field (i.e. a Vector of FieldValues. It's used to determine the most
# polarized field.
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
    return m2 - m
end

# Graph holds the graph structure onto which max-sum is run, in
# sparse format
type Graph
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
    gmsk::Vector{Vector{Matrix{Bool}}}

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
    #   allows to retrieve the indices in gadj, so that
    #   gadj[p0][adjdict[p1][p0]] = p1
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
        gmsk = [ Matrix{Bool}[] for i = 1:np ]
        gdir = [ Int[] for i = 1:np ]
        adjdict = [ (Int=>Int)[] for i = 1:np ]

        for d in deps
            p0, v0 = vdict[d[1]]
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

                adjdict[p1][p0] = j0
                adjdict[p0][p1] = j1

                bm = ones(Bool, spp[p1], spp[p0])
                bmt = bm'

                push!(gmsk[p0], bm)
                push!(gmsk[p1], bmt)

                push!(gdir[p0], 1)
                push!(gdir[p1], -1)
            else
                bm = gmsk[p0][j0]
                bmt = gmsk[p1][j1]
                if gdir[p0][j0] == -1
                    gdir[p0][j0] = 0
                    gdir[p1][j0] = 0
                end
            end

            for v1 = 1:length(pvers[p1])
                if !contains(vs, Version(vs.package, pvers[p1][v1]))
                    bm[v1, v0] = false
                    bmt[v0, v1] = false
                end
            end
            bm[end,v0] = false
            bmt[v0,end] = false
        end

        perm = [1:np]

        return new(gadj, gmsk, gdir, adjdict, spp, perm, np)
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
    #                 fields are not normalized
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
        vweight = pkgstruct.vweight

        # a "deterministic noise" function based on hashes
        function noise(p0::Int, v0::Int)
            s = pkgs[p0] * string(v0 == spp[p0] ? "UNINST" : pvers[p0][v0])
            int(hash(s)) >>> 30
        end

        # external fields: there are 2 terms, a noise to break potential symmetries
        #                  and one to favor newest versions over older, and no-version over all
        fld = [ [ FieldValue(0,0,vweight[p0][v0],0,noise(p0,v0)) for v0 = 1:spp[p0] ] for p0 = 1:np]

        # enforce requirements as infinite external fields over the desired
        # version ranges
        reqps = falses(np)
        reqmsk = [ falses(spp[p0]) for p0 = 1:np ]

        for r in reqs, v in vers
            if contains(r, v)
                p0, v0 = vdict[v]
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
                        fld[p0][v0] += FieldValue(0,vweight[p0][v0],-vweight[p0][v0])
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

    fld = msgs.fld
    np = length(fld)
    sol = Array(Int, np)
    for p0 = 1:np
        fld0 = fld[p0]
        s0 = indmax(fld0)
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
        if decimated[p1]
            continue
        end
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
                inplaceadd!(cavmsg[v0], v0, 4)
            end
        end

        # keep the old input cavity message p0->p1
        oldmsg = msg1[j1]

        # init the new message to minus infinity
        newmsg = [ FieldValue(-1) for v1 = 1:spp1 ]

        # compute the new message by passing cavmsg
        # through the constraint encoded in the bitmask
        # (nearly equivalent to:
        #    newmsg = [ max(cavmsg[bm1[:,v1]]) for v1 = 1:spp1 ]
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
                # Note: cannot use inplaceadd! here since it may
                #       mutate cavmsg
                newmsg[v1] += FieldValue(0,0,0,v1)
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
        maxabsdiff = max(abs(diff))
        maxdiff = max(maxdiff, maxabsdiff)

        # update the field of p1
        fld1 = fld[p1]
        for v1 = 1:spp1
            inplaceadd!(fld1[v1], diff[v1])
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
    @assert !msgs.decimated[p0]
    fld0 = msgs.fld[p0]
    s0 = indmax(fld0)
    #println("DECIMATING $p0 ($(packages()[p0]) s0=$s0)")
    for v0 = 1:length(fld0)
        if v0 != s0
            fld0[v0] -= FieldValue(1)
        end
    end
    update(p0, graph, msgs)
    msgs.decimated[p0] = true
    msgs.num_nondecimated -= 1
end

# If normal convergence fails (or is too slow) fix the most
# polarized packages by adding extra infinite fields on every state
# but the maximum
function decimate(n::Int, graph::Graph, msgs::Messages)
    #println("DECIMATING $n NODES")
    fld = msgs.fld
    decimated = msgs.decimated
    fldorder = sortperm(fld, Sort.By(secondmax))
    for p0 in fldorder
        if decimated[p0]
            continue
        end
        decimate1(p0, graph, msgs)
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
            decimate1(p0, msgs)
            return false
        end
    end
    return true
end

# Iterative solver: run iterate() until convergence
# (occasionally calling decimate())
function converge(graph::Graph, msgs::Messages)

    params = ResolveParams()

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
        if it >= params.nondec_iterations &&
           (it - params.nondec_iterations) % params.dec_interval == 0
            numdec = clamp(ifloor(params.dec_fraction * graph.np),  1, msgs.num_nondecimated)
            decimate(numdec, graph, msgs)
            if msgs.num_nondecimated == 0
                break
            end
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
        p0, v0 = vdict[d[1]]
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

# Push the given solution to a local optimium if needed
function enforce_optimality(reqsstruct::ReqsStruct, pkgstruct::PkgStruct, sol::Vector{Int})
    np = reqsstruct.np

    reqs = reqsstruct.reqs
    deps = reqsstruct.deps
    spp = pkgstruct.spp
    pdict = pkgstruct.pdict
    pvers = pkgstruct.pvers
    vdict = pkgstruct.vdict
    waspruned = pkgstruct.waspruned

    # prepare some useful structures
    # pdeps[p0][v0] has all dependencies of package p0 version v0
    pdeps = [ [ VersionSet[] for v0 = 1:spp[p0]-1 ] for p0 = 1:np ]
    # prevdeps[p1][p0][v0] is the VersionSet of package p1 which package p0 version v0
    # depends upon
    prevdeps = [ (Int=>Dict{Int,VersionSet})[] for p0 = 1:np ]

    for d in deps
        p0, v0 = vdict[d[1]]
        vs = d[2]
        push!(pdeps[p0][v0], vs)
        p1 = pdict[vs.package]
        if !has(prevdeps[p1], p0)
            prevdeps[p1][p0] = (Int=>VersionSet)[]
        end
        prevdeps[p1][p0][v0] = vs
    end

    restart = true
    while restart
        restart = false
        for p0 = 1:np
            s0 = sol[p0]
            if s0 >= spp[p0] - 1
                # either the package is not installed,
                # or it's already at the maximum version
                continue
            end
            viol = false
            if !waspruned
                # check if bumping would violate a requirement
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
            end
            # check if the higher version has a depencency which
            # would be violated by the state of the remaining packages
            for vs in pdeps[p0][s0+1]
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
            for (p1,d) in prevdeps[p0]
                vs = get(d, sol[p1], nothing)
                if vs == nothing
                    continue
                end
                p = vs.package
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
            # So the solution is non-optimal: we bump it manually
            #println(STDERR, "Warning: nonoptimal solution for package $(reqsstruct.pkgs[p0]): sol=$s0")
            sol[p0] += 1
            restart = true
        end
    end
    return
end

# The external-facing function
function resolve(reqs, fixed)
    # fetch data
    reqsstruct = ReqsStruct(reqs, fixed)

    # init structures
    pkgstruct = PkgStruct(reqsstruct)

    prune_versions!(reqsstruct, pkgstruct)

    graph = Graph(reqsstruct, pkgstruct)
    msgs = Messages(reqsstruct, pkgstruct, graph)

    # find solution
    local sol::Vector{Int}
    try
        sol = converge(graph, msgs)
    catch err
        if isa(err, UnsatError)
            p = reqsstruct.pkgs[err.info]
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
    verify_sol(reqsstruct, pkgstruct, sol)
    enforce_optimality(reqsstruct, pkgstruct, sol)

    # return the solution as a Dict mapping package_name => sha1
    return compute_output_dict(reqsstruct, pkgstruct, sol)
end

# Build a subgraph incuding only the (direct and indirect) dependencies
# of a given package (used in check_sanity)
function substructs(reqsstruct0::ReqsStruct, pkgstruct0::PkgStruct, pdeps::Vector, v::Version)

    pkgs = reqsstruct0.pkgs
    vers = reqsstruct0.vers
    deps = reqsstruct0.deps
    np = reqsstruct0.np
    spp = pkgstruct0.spp
    pdict = pkgstruct0.pdict
    pvers = pkgstruct0.pvers
    vdict = pkgstruct0.vdict

    p = v.package
    vn = v.version
    nvn = deepcopy(vn)
    nvn.patch += 1
    reqs = [VersionSet(p, [vn, nvn])]

    p0 = pdict[p]
    staged = IntSet(p0)
    pset = IntSet(p0)
    while !isempty(staged)
        staged_next = IntSet()
        for p0 in staged
            for w in pdeps[p0], vs in w
                p1 = pdict[vs.package]
                add!(staged_next, p1)
            end
        end
        pset = union(pset, staged_next)
        staged = staged_next
    end

    red_pkgs = [ pkgs[p0] for p0 in pset ]
    red_vers = Version[]
    for p0 in pset
        pvers0 = pvers[p0]
        for vn in pvers0
            push!(red_vers, Version(pkgs[p0], vn))
        end
    end
    red_deps = Array((Version,VersionSet),0)
    for p0 in pset
        pdeps0 = pdeps[p0]
        pvers0 = pvers[p0]
        p = pkgs[p0]
        for v0 = 1:spp[p0]-1
            vn = pvers0[v0]
            for vs in pdeps0[v0]
                push!(red_deps, (Version(p, vn), vs))
            end
        end
    end

    reqsstruct = ReqsStruct(reqs, red_pkgs, red_vers, red_deps)
    pkgstruct = PkgStruct(reqsstruct)
    return reqsstruct, pkgstruct
end

# Scan dependencies for (explicit or implicit) contradictions
function sanity_check()
    reqsstruct0 = ReqsStruct(VersionSet[])
    pkgstruct0 = PkgStruct(reqsstruct0)

    eq_classes_map = prune_versions!(reqsstruct0, pkgstruct0, false)

    pkgs = reqsstruct0.pkgs
    vers = reqsstruct0.vers
    deps = reqsstruct0.deps
    np = reqsstruct0.np
    spp = pkgstruct0.spp
    pdict = pkgstruct0.pdict
    pvers = pkgstruct0.pvers
    vdict = pkgstruct0.vdict

    pdeps = [ [ VersionSet[] for v0 = 1:spp[p0]-1 ] for p0 = 1:np ]
    pndeps = [ zeros(Int,spp[p0]-1) for p0 = 1:np ]
    for d in deps
        p0, v0 = vdict[d[1]]
        vs = d[2]
        push!(pdeps[p0][v0], vs)
        pndeps[p0][v0] += 1
    end

    rev_eq_classes_map = [ (VersionNumber=>Vector{VersionNumber})[] for p0 = 1:np ]
    for p0 = 1:np
        eqclass0 = eq_classes_map[p0]
        reveqclass0 = rev_eq_classes_map[p0]
        for (v,vtop) in eqclass0
            if !has(reveqclass0, vtop)
                reveqclass0[vtop] = VersionNumber[v]
            else
                push!(reveqclass0[vtop], v)
            end
        end
    end

    function vrank(v::Version)
        p0, v0 = vdict[v]
        return -pndeps[p0][v0]
    end
    svers = sort(vers, Sort.By(vrank))

    nv = length(svers)
    nnzv = findfirst(v->vrank(v)==0, svers) - 1

    svdict = (Version=>Int)[]
    i = 1
    for v in svers
        svdict[v] = i
        i += 1
    end
    checked = falses(nv)

    insane_ids = Array((Int,Int),0)
    problematic_pkgs = String[]

    i = 1
    psl = 0
    for v in svers
        vr = -vrank(v)
        if vr == 0
            break
        end
        if checked[i]
            i += 1
            continue
        elseif vr == 1
            p0, v0 = vdict[v]
            vs = pdeps[p0][v0][1]
            p1 = pdict[vs.package]
            found = false
            for vn in pvers[p1]
                if contains(vs, Version(vs.package, vn))
                    found = true
                    break
                end
            end
            if !found
                pp = vs.package
                push!(insane_ids, (p0,v0))
                push!(problematic_pkgs, pp)
            else
                checked[i] = true
            end
        else
            reqsstruct, pkgstruct = substructs(reqsstruct0, pkgstruct0, pdeps, v)

            graph = Graph(reqsstruct, pkgstruct)
            msgs = Messages(reqsstruct, pkgstruct, graph)

            red_pkgs = reqsstruct.pkgs
            red_np = reqsstruct.np
            red_spp = pkgstruct.spp
            red_pvers = pkgstruct.pvers

            local sol::Vector{Int}
            try
                sol = converge(graph, msgs)
                verify_sol(reqsstruct, pkgstruct, sol)

                for p0 = 1:red_np
                    s0 = sol[p0]
                    if s0 != red_spp[p0]
                        j = svdict[Version(red_pkgs[p0], red_pvers[p0][s0])]
                        checked[j] = true
                    end
                end
                checked[i] = true
            catch err
                if isa(err, UnsatError)
                    pp = red_pkgs[err.info]
                    push!(insane_ids, vdict[v])
                    push!(problematic_pkgs, pp)
                else
                    rethrow(err)
                end
            end
        end
        i += 1
    end

    insane = Array((Version,String), 0)
    if !isempty(insane_ids)
        i = 1
        for (p0,v0) in insane_ids
            p = pkgs[p0]
            vn = pvers[p0][v0]
            pp = problematic_pkgs[i]
            for vneq in rev_eq_classes_map[p0][vn]
                push!(insane, (Version(p, vneq), pp))
            end
            i += 1
        end
        sort!(insane, Sort.By(x->x[1]))
        throw(MetadataError(insane))
    end

    return
end

end # module
