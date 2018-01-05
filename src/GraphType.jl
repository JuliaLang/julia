# This file is a part of Julia. License is MIT: https://julialang.org/license

module GraphType

using ..Types
import ..Types.uuid_julia
import Pkg3.equalto

export Graph, add_reqs!, add_fixed!, simplify_graph!

# This is used to keep track of dependency relations when propagating
# requirements, so as to emit useful information in case of unsatisfiable
# conditions.
# The `why` field is a Vector which keeps track of the requirements. Each
# entry is a Tuple of two elements:
# 1) the first element is the reason, and it can be either :fixed (for
#    fixed packages), :explicit_requirement (for explicitly required packages),
#    or a Tuple `(:constr_prop, p, backtrace_item)` (for requirements induced
#    indirectly), where `p` is the package index and `backtrace_item` is
#    another ResolveBacktraceItem.
# 2) the second element is a BitVector representing the requirement as a mask
#    over the possible states of the package
mutable struct ResolveBacktraceItem
    why::Vector{Any}
    ResolveBacktraceItem() = new(Any[])
end

function Base.push!(ritem::ResolveBacktraceItem, reason, versionmask)
    push!(ritem.why, (reason, versionmask))
end

# Installation state: either a version, or uninstalled
const InstState = Union{VersionNumber,Void}

mutable struct GraphData
    # packages list
    pkgs::Vector{UUID}

    # number of packages
    np::Int

    # states per package: one per version + uninstalled
    spp::Vector{Int}

    # pakage dict: associates an index to each package id
    pdict::Dict{UUID,Int}

    # package versions: for each package, keep the list of the
    #                   possible version numbers; this defines a
    #                   mapping from version numbers of a package
    #                   to indices
    pvers::Vector{Vector{VersionNumber}}

    # versions dict: associates a version index to each package
    #                version; such that
    #                  pvers[p0][vdict[p0][vn]] = vn
    vdict::Vector{Dict{VersionNumber,Int}}

    uuid_to_name::Dict{UUID,String}

    reqs::Requires
    fixed::Dict{UUID,Fixed}

    # pruned packages: during graph simplification, packages that
    #                  only have one allowed version are pruned.
    #                  This keeps track of them, so that they may
    #                  be returned in the solution (unless they
    #                  were explicitly fixed)
    pruned::Dict{UUID,VersionNumber}

    # equivalence classes: for each package and each of its possible
    #                      states, keep track of other equivalent states
    eq_classes::Dict{UUID,Dict{InstState,Set{InstState}}}

    function GraphData(
            versions::Dict{UUID,Set{VersionNumber}},
            deps::Dict{UUID,Dict{VersionRange,Dict{String,UUID}}},
            compat::Dict{UUID,Dict{VersionRange,Dict{String,VersionSpec}}},
            uuid_to_name::Dict{UUID,String},
            reqs::Requires,
            fixed::Dict{UUID,Fixed}
        )
        # generate pkgs
        pkgs = sort!(collect(keys(versions)))
        np = length(pkgs)

        # generate pdict
        pdict = Dict{UUID,Int}(pkgs[p0] => p0 for p0 = 1:np)

        # generate spp and pvers
        pvers = [sort!(collect(versions[pkgs[p0]])) for p0 = 1:np]
        spp = length.(pvers) .+ 1

        # generate vdict
        vdict = [Dict{VersionNumber,Int}(vn => i for (i,vn) in enumerate(pvers[p0])) for p0 = 1:np]

        pruned = Dict{UUID,VersionNumber}()

        # equivalence classes (at the beginning each state represents just itself)
        eq_vn(v0, p0) = (v0 == spp[p0] ? nothing : pvers[p0][v0])
        eq_classes = Dict(pkgs[p0] => Dict(eq_vn(v0,p0) => Set([eq_vn(v0,p0)]) for v0 = 1:spp[p0]) for p0 = 1:np)

        return new(pkgs, np, spp, pdict, pvers, vdict, uuid_to_name, reqs, fixed, pruned, eq_classes)
    end
end

@enum DepDir FORWARD BACKWARDS BIDIR NONE

function update_depdir(dd0::DepDir, dd1::DepDir)
    dd0 == dd1 && return dd0
    dd0 == NONE && return dd1
    dd1 == NONE && return dd0
    return BIDIR
end

mutable struct Graph
    # data:
    #   stores all the structures required to map between
    #   parsed items (names, UUIDS, version numbers...) and
    #   the numeric representation used in the main Graph data
    #   structure.
    data::GraphData

    # adjacency matrix:
    #   for each package, has the list of neighbors
    #   indices
    gadj::Vector{Vector{Int}}

    # compatibility mask:
    #   for each package p0 has a list of bool masks.
    #   Each entry in the list gmsk[p0] is relative to the
    #   package p1 as read from gadj[p0].
    #   Each mask has dimension spp1 × spp0, where
    #   spp0 is the number of states of p0, and
    #   spp1 is the number of states of p1.
    gmsk::Vector{Vector{BitMatrix}}

    # dependency direction:
    #   keeps track of which direction the dependency goes.
    gdir::Vector{Vector{DepDir}}

    # constraints:
    #   a mask of allowed states for each package (e.g. to express
    #   requirements)
    gconstr::Vector{BitVector}

    # adjacency dict:
    #   allows one to retrieve the indices in gadj, so that
    #   gadj[p0][adjdict[p1][p0]] = p1
    #   ("At which index does package p1 appear in gadj[p0]?")
    adjdict::Vector{Dict{Int,Int}}

    # indices of the packages that were *explicitly* required
    #   used to favor their versions at resolution
    req_inds::Set{Int}

    # indices of the packages that were *explicitly* fixed
    #   used to avoid returning them in the solution
    fix_inds::Set{Int}

    # states per package: same as in GraphData
    spp::Vector{Int}

    # backtrace: keep track of the resolution process
    bktrc::Vector{ResolveBacktraceItem}

    # number of packages (all Vectors above have this length)
    np::Int

    function Graph(
            versions::Dict{UUID,Set{VersionNumber}},
            deps::Dict{UUID,Dict{VersionRange,Dict{String,UUID}}},
            compat::Dict{UUID,Dict{VersionRange,Dict{String,VersionSpec}}},
            uuid_to_name::Dict{UUID,String},
            reqs::Requires = Requires(),
            fixed::Dict{UUID,Fixed} = Dict{UUID,Fixed}(uuid_julia=>Fixed(VERSION))
        )

        extra_uuids = union(keys(reqs), keys(fixed), map(fx->keys(fx.requires), values(fixed))...)
        extra_uuids ⊆ keys(versions) || error("unknown UUID found in reqs/fixed") # TODO?

        data = GraphData(versions, deps, compat, uuid_to_name, reqs, fixed)
        pkgs, np, spp, pdict, pvers, vdict = data.pkgs, data.np, data.spp, data.pdict, data.pvers, data.vdict

        extended_deps = [[Dict{Int,BitVector}() for v0 = 1:(spp[p0]-1)] for p0 = 1:np]
        for p0 = 1:np, v0 = 1:(spp[p0]-1)
            n2u = Dict{String,UUID}()
            vn = pvers[p0][v0]
            for (vr,vrmap) in deps[pkgs[p0]]
                vn ∈ vr || continue
                for (name,uuid) in vrmap
                    # check conflicts ??
                    n2u[name] = uuid
                end
            end
            req = Dict{Int,VersionSpec}()
            for (vr,vrmap) in compat[pkgs[p0]]
                vn ∈ vr || continue
                for (name,vs) in vrmap
                    haskey(n2u, name) || error("Unknown package $name found in the compatibility requirements of $(pkgID(pkgs[p0], uuid_to_name))")
                    uuid = n2u[name]
                    p1 = pdict[uuid]
                    # check conflicts instead of intersecting?
                    # (intersecting is used by fixed packages though...)
                    req_p1 = get!(req, p1) do; VersionSpec() end
                    req[p1] = req_p1 ∩ vs
                end
            end
            # The remaining dependencies do not have compatibility constraints
            for uuid in values(n2u)
                get!(req, pdict[uuid]) do; VersionSpec() end
            end
            # Translate the requirements into bit masks
            # req_msk = Dict(p1 => BitArray(pvers[p1][v1] ∈ vs for v1 = 0:(spp[p1]-1)) for (p1,vs) in req)
            req_msk = Dict(p1 => (pvers[p1][1:(spp[p1]-1)] .∈ vs) for (p1,vs) in req)
            extended_deps[p0][v0] = req_msk
        end

        gadj = [Int[] for p0 = 1:np]
        gmsk = [BitMatrix[] for p0 = 1:np]
        gdir = [DepDir[] for p0 = 1:np]
        gconstr = [trues(spp[p0]) for p0 = 1:np]
        adjdict = [Dict{Int,Int}() for p0 = 1:np]

        for p0 = 1:np, v0 = 1:(spp[p0]-1), (p1,rmsk1) in extended_deps[p0][v0]
            j0 = get(adjdict[p1], p0, length(gadj[p0]) + 1)
            j1 = get(adjdict[p0], p1, length(gadj[p1]) + 1)

            @assert (j0 > length(gadj[p0]) && j1 > length(gadj[p1])) ||
                    (j0 ≤ length(gadj[p0]) && j1 ≤ length(gadj[p1]))

            if j0 > length(gadj[p0])
                push!(gadj[p0], p1)
                push!(gadj[p1], p0)
                j0 = length(gadj[p0])
                j1 = length(gadj[p1])

                adjdict[p1][p0] = j0
                adjdict[p0][p1] = j1

                bm = trues(spp[p1], spp[p0])
                bmt = bm'

                push!(gmsk[p0], bm)
                push!(gmsk[p1], bmt)

                push!(gdir[p0], FORWARD)
                push!(gdir[p1], BACKWARDS)
            else
                bm = gmsk[p0][j0]
                bmt = gmsk[p1][j1]
                gdir[p0][j0] = update_depdir(gdir[p0][j0], FORWARD)
                gdir[p1][j1] = update_depdir(gdir[p1][j1], BACKWARDS)
            end

            for v1 = 1:(spp[p1]-1)
                rmsk1[v1] && continue
                bm[v1, v0] = false
                bmt[v0, v1] = false
            end
            bm[end,v0] = false
            bmt[v0,end] = false
        end

        req_inds = Set{Int}()
        fix_inds = Set{Int}()

        bktrc = [ResolveBacktraceItem() for p0 = 1:np]

        graph = new(data, gadj, gmsk, gdir, gconstr, adjdict, req_inds, fix_inds, spp, bktrc, np)

        _add_fixed!(graph, fixed)
        _add_reqs!(graph, reqs, :explicit_requirement)

        @assert check_consistency(graph)
        check_constraints(graph)

        return graph
    end
end

"""
Add explicit requirements to the graph.
"""
function add_reqs!(graph::Graph, reqs::Requires)
    _add_reqs!(graph, reqs, :explicit_requirement)
    check_constraints(graph)
    # TODO: add reqs to graph data?
    return graph
end

function _add_reqs!(graph::Graph, reqs::Requires, reason)
    gconstr = graph.gconstr
    spp = graph.spp
    req_inds = graph.req_inds
    bktrc = graph.bktrc
    pdict = graph.data.pdict
    pvers = graph.data.pvers

    for (rp,rvs) in reqs
        haskey(pdict, rp) || error("unknown required package $(pkgID(rp, graph))")
        rp0 = pdict[rp]
        new_constr = trues(spp[rp0])
        for rv0 = 1:(spp[rp0]-1)
            rvn = pvers[rp0][rv0]
            rvn ∈ rvs || (new_constr[rv0] = false)
        end
        new_constr[end] = false
        old_constr = copy(gconstr[rp0])
        gconstr[rp0] .&= new_constr
        reason ≡ :explicit_requirement && push!(req_inds, rp0)
        old_constr ≠ gconstr[rp0] && push!(bktrc[rp0], reason, new_constr)
    end
    return graph
end

"Add fixed packages to the graph, and their requirements."
function add_fixed!(graph::Graph, fixed::Dict{UUID,Fixed})
    _add_fixed!(graph, fixed)
    check_constraints(graph)
    # TODO: add fixed to graph data?
    return graph
end

function _add_fixed!(graph::Graph, fixed::Dict{UUID,Fixed})
    gconstr = graph.gconstr
    spp = graph.spp
    fix_inds = graph.fix_inds
    bktrc = graph.bktrc
    pdict = graph.data.pdict
    vdict = graph.data.vdict

    for (fp,fx) in fixed
        haskey(pdict, fp) || error("unknown fixed package $(pkgID(fp, graph))")
        fp0 = pdict[fp]
        fv0 = vdict[fp0][fx.version]
        new_constr = falses(spp[fp0])
        new_constr[fv0] = true
        gconstr[fp0] .&= new_constr
        push!(fix_inds, fp0)
        push!(bktrc[fp0], :fixed, new_constr)
        _add_reqs!(graph, fx.requires, (:constr_prop, fp0, bktrc[fp0]))
    end
    return graph
end

Types.pkgID(p::UUID, graph::Graph) = pkgID(p, graph.data.uuid_to_name)
Types.pkgID(p0::Int, graph::Graph) = pkgID(graph.data.pkgs[p0], graph)

function check_consistency(graph::Graph)
    np = graph.np
    spp = graph.spp
    gadj = graph.gadj
    gmsk = graph.gmsk
    gdir = graph.gdir
    gconstr = graph.gconstr
    adjdict = graph.adjdict
    req_inds = graph.req_inds
    fix_inds = graph.fix_inds
    bktrc = graph.bktrc
    data = graph.data
    pkgs = data.pkgs
    pdict = data.pdict
    pvers = data.pvers
    vdict = data.vdict
    pruned = data.pruned
    eq_classes = data.eq_classes

    @assert np ≥ 0
    for x in [spp, gadj, gmsk, gdir, gconstr, adjdict, bktrc, pkgs, pdict, pvers, vdict]
        @assert length(x) == np
    end
    for p0 = 1:np
        @assert pdict[pkgs[p0]] == p0
        spp0 = spp[p0]
        @assert spp0 ≥ 1
        pvers0 = pvers[p0]
        vdict0 = vdict[p0]
        @assert length(pvers0) == spp0 - 1
        for v0 = 1:(spp0-1)
            @assert vdict0[pvers0[v0]] == v0
        end
        for (vn,v0) in vdict0
            @assert 1 ≤ v0 ≤ spp0-1
            @assert pvers0[v0] == vn
        end
        gconstr0 = gconstr[p0]
        @assert length(gconstr0) == spp0

        gadj0 = gadj[p0]
        gmsk0 = gmsk[p0]
        gdir0 = gdir[p0]
        adjdict0 = adjdict[p0]
        @assert length(gmsk0) == length(gadj0)
        @assert length(adjdict0) == length(gadj0)
        @assert length(gdir0) == length(gadj0)
        for (j0,p1) in enumerate(gadj0)
            @assert adjdict[p1][p0] == j0
            spp1 = spp[p1]
            @assert size(gmsk0[j0]) == (spp1,spp0)
            j1 = adjdict0[p1]
            gmsk1 = gmsk[p1]
            @assert gmsk1[j1] == gmsk0[j0]'
        end
    end
    for (p,p0) in pdict
        @assert 1 ≤ p0 ≤ np
        @assert pkgs[p0] == p
        @assert !haskey(pruned, p)
    end
    for p0 in req_inds
        @assert 1 ≤ p0 ≤ np
        @assert !gconstr[p0][end]
    end
    for p0 in fix_inds
        @assert 1 ≤ p0 ≤ np
        @assert !gconstr[p0][end]
        @assert count(gconstr[p0]) == 1
    end

    for (p,eq_cl) in eq_classes, (rvn,rvs) in eq_cl
        @assert rvn ∈ rvs
    end

    return true
end

"Show the resolution backtrace for some package"
function showbacktrace(io::IO, graph::Graph, p0::Int)
    _show(io, graph, p0, graph.bktrc[p0], "", Set{ResolveBacktraceItem}())
end

# Show a recursive tree with requirements applied to a package, either directly or indirectly
function _show(io::IO, graph::Graph, p0::Int, ritem::ResolveBacktraceItem, indent::String, seen::Set{ResolveBacktraceItem})
    id0 = pkgID(p0, graph)
    gconstr = graph.gconstr
    pkgs = graph.data.pkgs
    pvers = graph.data.pvers

    function vs_string(p0::Int, vmask::BitVector)
        vns = Vector{Any}(pvers[p0][vmask[1:(end-1)]])
        vmask[end] && push!(vns, "uninstalled")
        return join(string.(vns), ", ", " or ")
    end

    l = length(ritem.why)
    for (i,(w,vmask)) in enumerate(ritem.why)
        print(io, indent, (i==l ? '└' : '├'), '─')
        if w ≡ :fixed
            @assert count(vmask) == 1
            println(io, "$id0 is fixed to version ", vs_string(p0, vmask))
        elseif w ≡ :explicit_requirement
            @assert !vmask[end]
            if any(vmask)
                println(io, "an explicit requirement sets $id0 to versions: ", vs_string(p0, vmask))
            else
                println(io, "an explicit requirement cannot be matched by any of the available versions of $id0")
            end
        else
            @assert w isa Tuple{Symbol,Int,ResolveBacktraceItem}
            @assert w[1] == :constr_prop
            p1 = w[2]
            if !is_current_julia(graph, p1)
                id1 = pkgID(p1, graph)
                otheritem = w[3]
                if any(vmask)
                    println(io, "the only versions of $id0 compatible with $id1 (whose allowed versions are $(vs_string(p1, gconstr[p1])))\n",
                                indent, (i==l ? "  " : "│ "),"are these: ", vs_string(p0, vmask))
                else
                    println(io, "no versions of $id0 are compatible with $id1 (whose allowed versions are $(vs_string(p1, gconstr[p1])))")
                end
                if otheritem ∈ seen
                    println(io, indent, (i==l ? "  " : "│ "), "└─see above for $id1 backtrace")
                    continue
                end
                push!(seen, otheritem)
                _show(io, graph, p1, otheritem, indent * (i==l ? "  " : "│ "), seen)
            else
                if any(vmask)
                    println(io, "the only versions of $id0 compatible with julia v$VERSION are these: ", vs_string(p0, vmask))
                else
                    println(io, "no versions of $id0 are compatible with julia v$VERSION")
                end
            end
        end
    end
end

function is_current_julia(graph::Graph, p1::Int)
    gconstr = graph.gconstr
    fix_inds = graph.fix_inds
    pkgs = graph.data.pkgs
    pvers = graph.data.pvers

    (pkgs[p1] == uuid_julia && p1 ∈ fix_inds) || return false
    jconstr = gconstr[p1]
    return length(jconstr) == 2 && !jconstr[2] && pvers[p1][1] == VERSION
end

"Check for contradictions in the constraints."
function check_constraints(graph::Graph)
    np = graph.np
    gconstr = graph.gconstr
    pkgs = graph.data.pkgs
    pvers = graph.data.pvers

    id(p0::Int) = pkgID(pkgs[p0], graph)

    for p0 = 1:np
        any(gconstr[p0]) && continue
        err_msg = "Unsatisfiable requirements detected for package $(id(p0)):\n"
        err_msg *= sprint(showbacktrace, graph, p0)
        throw(PkgError(err_msg))
    end
    return true
end

"""
Propagates current constraints, determining new implicit constraints.
Throws an error in case impossible requirements are detected, printing
a backtrace.
"""
function propagate_constraints!(graph::Graph)
    np = graph.np
    spp = graph.spp
    gadj = graph.gadj
    gmsk = graph.gmsk
    bktrc = graph.bktrc
    gconstr = graph.gconstr
    adjdict = graph.adjdict
    pkgs = graph.data.pkgs
    pvers = graph.data.pvers

    id(p0::Int) = pkgID(pkgs[p0], graph)

    # packages which are not allowed to be uninstalled
    staged = Set{Int}(p0 for p0 = 1:np if !gconstr[p0][end])

    while !isempty(staged)
        staged_next = Set{Int}()
        for p0 in staged
            gconstr0 = gconstr[p0]
            for (j1,p1) in enumerate(gadj[p0])
                # we don't propagate to julia (purely to have better error messages)
                is_current_julia(graph, p1) && continue

                msk = gmsk[p0][j1]
                # consider the sub-mask with only allowed versions of p0
                sub_msk = msk[:,gconstr0]
                # if an entire row of the sub-mask is false, that version of p1
                # is effectively forbidden
                # (this is just like calling `any` row-wise)
                added_constr1 = any!(BitVector(spp[p1]), sub_msk)
                # apply the new constraints, checking for contradictions
                # (keep the old ones for comparison)
                gconstr1 = gconstr[p1]
                old_gconstr1 = copy(gconstr1)
                gconstr1 .&= added_constr1
                # if the new constraints are more restrictive than the
                # previous ones, record it and propagate them next
                if gconstr1 ≠ old_gconstr1
                    push!(staged_next, p1)
                    push!(bktrc[p1], (:constr_prop, p0, bktrc[p0]), added_constr1)
                end
                if !any(gconstr1)
                    err_msg = "Unsatisfiable requirements detected for package $(id(p1)):\n"
                    err_msg *= sprint(showbacktrace, graph, p1)
                    throw(PkgError(err_msg))
                end
            end
        end
        staged = staged_next
    end
    return graph
end

"""
Enforce the uninstalled state on all packages that are not reachable from the required ones
or from the packages in the `sources` argument.
"""
function disable_unreachable!(graph::Graph, sources::Set{Int} = Set{Int}())
    np = graph.np
    spp = graph.spp
    gadj = graph.gadj
    gmsk = graph.gmsk
    gconstr = graph.gconstr
    adjdict = graph.adjdict
    pkgs = graph.data.pkgs

    # packages which are not allowed to be uninstalled
    staged = union(sources, Set{Int}(p0 for p0 = 1:np if !gconstr[p0][end]))
    seen = copy(staged)

    while !isempty(staged)
        staged_next = Set{Int}()
        for p0 in staged
            gconstr0idx = find(gconstr[p0][1:(end-1)])
            for (j1,p1) in enumerate(gadj[p0])
                all(gmsk[p0][j1][end,gconstr0idx]) && continue # the package is not required by any of the allowed versions of p0
                p1 ∈ seen || push!(staged_next, p1)
            end
        end
        union!(seen, staged_next)
        staged = staged_next
    end

    # Force uninstalled state for all unseen packages
    for p0 = 1:np
        p0 ∈ seen && continue
        gconstr0 = gconstr[p0]
        @assert gconstr0[end]
        fill!(gconstr0, false)
        gconstr0[end] = true
    end

    return graph
end

"""
Reduce the number of versions in the graph by putting all the versions of
a package that behave identically into equivalence classes, keeping only
the highest version of the class as representative.
"""
function compute_eq_classes!(graph::Graph; verbose::Bool = false)
    np = graph.np
    sumspp = sum(graph.spp)
    for p0 = 1:np
        build_eq_classes1!(graph, p0)
    end

    if verbose
        info("""
             EQ CLASSES STATS:
               before: $(sumspp)
               after:  $(sum(graph.spp))
             """)
    end

    # wipe out backtrace because it doesn't make sense now
    # TODO: save it somehow?
    graph.bktrc = [ResolveBacktraceItem() for p0 = 1:np]

    @assert check_consistency(graph)

    return graph
end

function build_eq_classes1!(graph::Graph, p0::Int)
    np = graph.np
    spp = graph.spp
    gadj = graph.gadj
    gmsk = graph.gmsk
    gconstr = graph.gconstr
    adjdict = graph.adjdict
    data = graph.data
    pkgs = data.pkgs
    pvers = data.pvers
    vdict = data.vdict
    eq_classes = data.eq_classes

    # concatenate all the constraints; the columns of the
    # result encode the behavior of each version
    cmat = vcat(BitMatrix(gconstr[p0]'), gmsk[p0]...)
    cvecs = [cmat[:,v0] for v0 = 1:spp[p0]]

    # find unique behaviors
    repr_vecs = unique(cvecs)

    # number of equivaent classes
    neq = length(repr_vecs)

    neq == spp[p0] && return # nothing to do here

    # group versions into sets that behave identically
    eq_sets = [Set{Int}(v0 for v0 in 1:spp[p0] if cvecs[v0] == rvec) for rvec in repr_vecs]
    sort!(eq_sets, by=maximum)

    # each set is represented by its highest-valued member
    repr_vers = map(maximum, eq_sets)
    # the last representative must always be the uninstalled state
    @assert repr_vers[end] == spp[p0]

    # update equivalence classes
    eq_vn(v0) = (v0 == spp[p0] ? nothing : pvers[p0][v0])
    eq_classes0 = eq_classes[pkgs[p0]]
    for (v0,rvs) in zip(repr_vers, eq_sets)
        @assert v0 ∈ rvs
        vn0 = eq_vn(v0)
        for v1 in rvs
            v1 == v0 && continue
            vn1 = eq_vn(v1)
            @assert vn1 ≢ nothing
            union!(eq_classes0[vn0], eq_classes0[vn1])
            delete!(eq_classes0, vn1)
        end
    end

    # reduce the constraints and the interaction matrices
    spp[p0] = neq
    gconstr[p0] = gconstr[p0][repr_vers]
    for (j1,p1) in enumerate(gadj[p0])
        gmsk[p0][j1] = gmsk[p0][j1][:,repr_vers]

        j0 = adjdict[p0][p1]
        gmsk[p1][j0] = gmsk[p1][j0][repr_vers,:]
    end

    # reduce/rebuild version dictionaries
    pvers[p0] = pvers[p0][repr_vers[1:(end-1)]]
    vdict[p0] = Dict(vn => i for (i,vn) in enumerate(pvers[p0]))

    return
end

"""
Prune away fixed and unnecessary packages, and the
disallowed versions for the remaining packages.
"""
function prune_graph!(graph::Graph; verbose::Bool = false)
    np = graph.np
    spp = graph.spp
    gadj = graph.gadj
    gmsk = graph.gmsk
    gdir = graph.gdir
    gconstr = graph.gconstr
    adjdict = graph.adjdict
    req_inds = graph.req_inds
    fix_inds = graph.fix_inds
    bktrc = graph.bktrc
    data = graph.data
    pkgs = data.pkgs
    pdict = data.pdict
    pvers = data.pvers
    vdict = data.vdict
    pruned = data.pruned

    # We will remove all packages that only have one allowed state
    # (includes fixed packages and forbidden packages)
    pkg_mask = BitArray(count(gconstr[p0]) ≠ 1 for p0 = 1:np)
    new_np = count(pkg_mask)

    # a map that translates the new index ∈ 1:new_np into its
    # corresponding old index ∈ 1:np
    old_idx = find(pkg_mask)
    # the reverse of the above
    new_idx = Dict{Int,Int}()
    for new_p0 = 1:new_np
        new_idx[old_idx[new_p0]] = new_p0
    end

    # Update requirement indices
    new_req_inds = Set{Int}()
    for p0 in req_inds
        pkg_mask[p0] || continue
        push!(new_req_inds, new_idx[p0])
    end

    # Fixed packages will all be pruned
    new_fix_inds = Set{Int}()
    for p0 in fix_inds
        @assert !pkg_mask[p0]
    end

    # Record which packages we are going to prune
    for p0 in find(.~(pkg_mask))
        # Find the version
        s0 = findfirst(gconstr[p0])
        # We don't record fixed packages
        p0 ∈ fix_inds && (@assert s0 ≠ spp[p0]; continue)
        p0 ∈ req_inds && @assert s0 ≠ spp[p0]
        # We don't record packages that are not going to be installed
        s0 == spp[p0] && continue
        @assert !haskey(pruned, pkgs[p0])
        pruned[pkgs[p0]] = pvers[p0][s0]
    end

    # Update packages records
    new_pkgs = pkgs[pkg_mask]
    new_pdict = Dict(new_pkgs[new_p0]=>new_p0 for new_p0 = 1:new_np)

    # For each package (unless it's going to be pruned) we will remove all
    # versions that aren't allowed (but not the "uninstalled" state)
    function keep_vers(new_p0)
        p0 = old_idx[new_p0]
        return BitArray((v0 == spp[p0]) | gconstr[p0][v0] for v0 = 1:spp[p0])
    end
    vers_mask = [keep_vers(new_p0) for new_p0 = 1:new_np]

    # Update number of states per package
    new_spp = Int[count(vers_mask[new_p0]) for new_p0 = 1:new_np]

    # Update versions maps
    function compute_pvers(new_p0)
        p0 = old_idx[new_p0]
        pvers0 = pvers[p0]
        vmsk0 = vers_mask[new_p0]
        return pvers0[vmsk0[1:(end-1)]]
    end
    new_pvers = [compute_pvers(new_p0) for new_p0 = 1:new_np]
    new_vdict = [Dict(vn => v0 for (v0,vn) in enumerate(new_pvers[new_p0])) for new_p0 = 1:new_np]

    # The new constraints are all going to be `true`, except possibly
    # for the "uninstalled" state, which we copy over from the old
    function compute_gconstr(new_p0)
        p0 = old_idx[new_p0]
        new_gconstr0 = trues(new_spp[new_p0])
        new_gconstr0[end] = gconstr[p0][end]
        return new_gconstr0
    end
    new_gconstr = [compute_gconstr(new_p0) for new_p0 = 1:new_np]

    # Recreate the graph adjacency list, skipping some packages
    new_gadj = [Int[] for new_p0 = 1:new_np]
    new_adjdict = [Dict{Int,Int}() for new_p0 = 1:new_np]

    for new_p0 = 1:new_np, (j1,p1) in enumerate(gadj[old_idx[new_p0]])
        pkg_mask[p1] || continue
        new_p1 = new_idx[p1]

        new_j0 = get(new_adjdict[new_p1], new_p0, length(new_gadj[new_p0]) + 1)
        new_j1 = get(new_adjdict[new_p0], new_p1, length(new_gadj[new_p1]) + 1)

        @assert (new_j0 > length(new_gadj[new_p0]) && new_j1 > length(new_gadj[new_p1])) ||
                (new_j0 ≤ length(new_gadj[new_p0]) && new_j1 ≤ length(new_gadj[new_p1]))

        new_j0 > length(new_gadj[new_p0]) || continue
        push!(new_gadj[new_p0], new_p1)
        push!(new_gadj[new_p1], new_p0)
        new_j0 = length(new_gadj[new_p0])
        new_j1 = length(new_gadj[new_p1])

        new_adjdict[new_p1][new_p0] = new_j0
        new_adjdict[new_p0][new_p1] = new_j1
    end

    # Recompute gdir on the new adjacency list
    function compute_gdir(new_p0, new_j0)
        p0 = old_idx[new_p0]
        new_p1 = new_gadj[new_p0][new_j0]
        p1 = old_idx[new_p1]
        j0 = adjdict[p1][p0]
        return gdir[p0][j0]
    end
    new_gdir = [[compute_gdir(new_p0, new_j0) for new_j0 = 1:length(new_gadj[new_p0])] for new_p0 = 1:new_np]

    # Recompute compatibility masks on the new adjacency list, and filtering out some versions
    function compute_gmsk(new_p0, new_j0)
        p0 = old_idx[new_p0]
        new_p1 = new_gadj[new_p0][new_j0]
        p1 = old_idx[new_p1]
        j0 = adjdict[p1][p0]
        return gmsk[p0][j0][vers_mask[new_p1],vers_mask[new_p0]]
    end
    new_gmsk = [[compute_gmsk(new_p0, new_j0) for new_j0 = 1:length(new_gadj[new_p0])] for new_p0 = 1:new_np]

    # Clear out resolution backtrace
    # TODO: save it somehow?
    new_bktrc = [ResolveBacktraceItem() for new_p0 = 1:new_np]

    # Done

    if verbose
        info("""
             GRAPH SIMPLIFY STATS:
               before: np = $np ⟨spp⟩ = $(mean(spp))
               after:  np = $new_np ⟨spp⟩ = $(mean(new_spp))
             """)
    end

    # Replace old data with new
    data.pkgs = new_pkgs
    data.np = new_np
    data.spp = new_spp
    data.pdict = new_pdict
    data.pvers = new_pvers
    data.vdict = new_vdict
    # Notes:
    #   * uuid_to_name, reqs, fixed, eq_classes are unchanged
    #   * pruned was updated in-place

    # Replace old structures with new ones
    graph.gadj = new_gadj
    graph.gmsk = new_gmsk
    graph.gdir = new_gdir
    graph.gconstr = new_gconstr
    graph.adjdict = new_adjdict
    graph.req_inds = new_req_inds
    graph.fix_inds = new_fix_inds
    graph.spp = new_spp
    graph.bktrc = new_bktrc
    graph.np = new_np

    @assert check_consistency(graph)

    return graph
end

"""
Simplifies the graph by propagating constraints, disabling unreachable versions, pruning
and grouping versions into equivalence classes.
"""
function simplify_graph!(graph::Graph, sources::Set{Int} = Set{Int}(); verbose::Bool = false)
    propagate_constraints!(graph)
    disable_unreachable!(graph, sources)
    prune_graph!(graph, verbose = verbose)
    compute_eq_classes!(graph, verbose = verbose)
    return graph
end

end # module
