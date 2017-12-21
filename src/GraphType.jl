# This file is a part of Julia. License is MIT: https://julialang.org/license

module GraphType

using ..Types
import ..Types.uuid_julia
import Pkg3.equalto

export Graph, add_reqs!, add_fixed!, simplify_graph!, showlog

# The ResolveLog is used to keep track of events that take place during the
# resolution process. We use one ResolveLogEntry per package, and record all events
# associated with that package. An event consists of two items: another
# entry representing another package's influence (or `nothing`) and
# a message for the log.
#
# Specialized functions called `log_event_[...]!` are used to store the
# various events. The events are also recorded in orded in a shared
# ResolveJournal, which is used to provide a plain chronological view.
#
# The `showlog` functions are used for display, and called to create messages
# in case of resolution errors.

const ResolveJournal = Vector{Tuple{UUID,String}}

mutable struct ResolveLogEntry
    journal::ResolveJournal # shared with all other entries
    pkg::UUID
    header::String
    events::Vector{Tuple{Any,String}} # here Any should ideally be Union{ResolveLogEntry,Void}
    ResolveLogEntry(journal::ResolveJournal, pkg::UUID, msg::String = "") = new(journal, pkg, msg, [])
end

function Base.push!(entry::ResolveLogEntry, reason::Tuple{Union{ResolveLogEntry,Void},String})
    push!(entry.events, reason)
    entry.pkg ≠ uuid_julia && push!(entry.journal, (entry.pkg, reason[2]))
    return entry
end

# Note: the `init` field is used to keep track of all entries which were there
# at the beginning, since the `pool` can be pruned during the resolution process.
mutable struct ResolveLog
    init::ResolveLogEntry
    pool::Dict{UUID,ResolveLogEntry}
    journal::Vector{Tuple{UUID,String}}
    function ResolveLog()
        journal = ResolveJournal()
        return new(ResolveLogEntry(journal, uuid_julia), Dict(), journal)
    end
end

# Installation state: either a version, or uninstalled
const InstState = Union{VersionNumber,Void}


# GraphData is basically a part of Graph that collects data structures useful
# for interfacing the internal abstract representation of Graph with the
# input/output (e.g. converts between package UUIDs and node numbers, etc.)
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

    # UUID to names
    uuid_to_name::Dict{UUID,String}

    # pruned packages: during graph simplification, packages that
    #                  only have one allowed version are pruned.
    #                  This keeps track of them, so that they may
    #                  be returned in the solution (unless they
    #                  were explicitly fixed)
    pruned::Dict{UUID,VersionNumber}

    # equivalence classes: for each package and each of its possible
    #                      states, keep track of other equivalent states
    eq_classes::Dict{UUID,Dict{InstState,Set{InstState}}}

    # resolve log: keep track of the resolution process
    rlog::ResolveLog

    function GraphData(
            versions::Dict{UUID,Set{VersionNumber}},
            deps::Dict{UUID,Dict{VersionRange,Dict{String,UUID}}},
            compat::Dict{UUID,Dict{VersionRange,Dict{String,VersionSpec}}},
            uuid_to_name::Dict{UUID,String}
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

        # nothing is pruned yet, of course
        pruned = Dict{UUID,VersionNumber}()

        # equivalence classes (at the beginning each state represents just itself)
        eq_vn(v0, p0) = (v0 == spp[p0] ? nothing : pvers[p0][v0])
        eq_classes = Dict(pkgs[p0] => Dict(eq_vn(v0,p0) => Set([eq_vn(v0,p0)]) for v0 = 1:spp[p0]) for p0 = 1:np)

        # the resolution log is actually initialized below
        rlog = ResolveLog()

        data = new(pkgs, np, spp, pdict, pvers, vdict, uuid_to_name, pruned, eq_classes, rlog)

        init_log!(data)

        return data
    end

    function Base.copy(data::GraphData)
        pkgs = copy(data.pkgs)
        np = data.np
        spp = copy(data.spp)
        pdict = copy(data.pdict)
        pvers = [copy(data.pvers[p0]) for p0 = 1:np]
        vdict = [copy(data.vdict[p0]) for p0 = 1:np]
        uuid_to_name = copy(data.uuid_to_name)
        pruned = copy(data.pruned)
        eq_classes = Dict(p => copy(eq) for (p,eq) in data.eq_classes)
        rlog = deepcopy(data.rlog)

        return new(pkgs, np, spp, pdict, pvers, vdict, uuid_to_name, pruned, eq_classes, rlog)
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

        data = GraphData(versions, deps, compat, uuid_to_name)
        pkgs, np, spp, pdict, pvers, vdict, rlog = data.pkgs, data.np, data.spp, data.pdict, data.pvers, data.vdict, data.rlog

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

        graph = new(data, gadj, gmsk, gdir, gconstr, adjdict, req_inds, fix_inds, spp, np)

        _add_fixed!(graph, fixed)
        _add_reqs!(graph, reqs, :explicit_requirement)

        @assert check_consistency(graph)
        check_constraints(graph)

        return graph
    end

    function Base.copy(graph::Graph)
        data = copy(graph.data)
        np = graph.np
        spp = data.spp
        gadj = [copy(graph.gadj[p0]) for p0 = 1:np]
        gmsk = [[copy(graph.gmsk[p0][j0]) for j0 = 1:length(gadj[p0])] for p0 = 1:np]
        gdir = [copy(graph.gdir[p0]) for p0 = 1:np]
        gconstr = [copy(graph.gconstr[p0]) for p0 = 1:np]
        adjdict = [copy(graph.adjdict[p0]) for p0 = 1:np]
        req_inds = copy(graph.req_inds)
        fix_inds = copy(graph.fix_inds)

        return new(data, gadj, gmsk, gdir, gconstr, adjdict, req_inds, fix_inds, spp, np)
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
        old_constr ≠ gconstr[rp0] && log_event_req!(graph, rp, rvs, reason)
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
        bkitem = log_event_fixed!(graph, fp, fx)
        _add_reqs!(graph, fx.requires, (fp, bkitem))
    end
    return graph
end

Types.pkgID(p::UUID, data::GraphData) = pkgID(p, data.uuid_to_name)
Types.pkgID(p0::Int, data::GraphData) = pkgID(data.pkgs[p0], data)
Types.pkgID(p, graph::Graph) = pkgID(p, graph.data)

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
    data = graph.data
    pkgs = data.pkgs
    pdict = data.pdict
    pvers = data.pvers
    vdict = data.vdict
    pruned = data.pruned
    eq_classes = data.eq_classes
    rlog = data.rlog

    @assert np ≥ 0
    for x in [spp, gadj, gmsk, gdir, gconstr, adjdict, rlog.pool, pkgs, pdict, pvers, vdict]
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

function init_log!(data::GraphData)
    np = data.np
    pkgs = data.pkgs
    pvers = data.pvers
    rlog = data.rlog
    for p0 = 1:np
        p = pkgs[p0]
        id = pkgID(p0, data)
        versions = pvers[p0]
        if isempty(versions)
            msg = "$id has no known versions!" # This shouldn't happen?
        else
            msg = "possible versions are: $(VersionSpec(VersionRange.(versions))) or uninstalled"
        end
        first_entry = get!(rlog.pool, p) do; ResolveLogEntry(rlog.journal, p, "$id log:") end

        if p ≠ uuid_julia
            push!(first_entry, (nothing, msg))
            push!(rlog.init, (first_entry, ""))
        end
    end
    return data
end

function log_event_fixed!(graph::Graph, fp::UUID, fx::Fixed)
    rlog = graph.data.rlog
    id = pkgID(fp, graph)
    msg = "$id is fixed to version $(fx.version)"
    entry = rlog.pool[fp]
    push!(entry, (nothing, msg))
    return entry
end

function log_event_req!(graph::Graph, rp::UUID, rvs::VersionSpec, reason)
    rlog = graph.data.rlog
    gconstr = graph.gconstr
    pdict = graph.data.pdict
    pvers = graph.data.pvers
    id = pkgID(rp, graph)
    msg = "restricted to versions $rvs by "
    if reason isa Symbol
        @assert reason == :explicit_requirement
        other_entry = nothing
        msg *= "an explicit requirement"
    else
        other_p, other_entry = reason::Tuple{UUID,ResolveLogEntry}
        if other_p == uuid_julia
            msg *= "julia compatibility requirements"
            other_entry = nothing # don't propagate the log
        else
            other_id = pkgID(other_p, graph)
            msg *= "$other_id"
        end
    end
    rp0 = pdict[rp]
    @assert !gconstr[rp0][end]
    if any(gconstr[rp0])
        msg *= ", leaving only versions $(VersionSpec(VersionRange.(pvers[rp0][gconstr[rp0][1:(end-1)]])))"
    else
        msg *= " — no versions left"
    end
    entry = rlog.pool[rp]
    push!(entry, (other_entry, msg))
    return entry
end

function log_event_implicit_req!(graph::Graph, p1::Int, vmask::BitVector, p0::Int)
    rlog = graph.data.rlog
    gconstr = graph.gconstr
    pkgs = graph.data.pkgs
    pvers = graph.data.pvers

    function vs_string(p0::Int, vmask::BitVector)
        if any(vmask[1:(end-1)])
            vns = string(VersionSpec(VersionRange.(pvers[p0][vmask[1:(end-1)]])))
            vmask[end] && (vns *= " or uninstalled")
        else
            @assert vmask[end]
            vns = "uninstalled"
        end
        return vns
    end

    p = pkgs[p1]
    id = pkgID(p, graph)
    other_p, other_entry = pkgs[p0], rlog.pool[pkgs[p0]]
    other_id = pkgID(other_p, graph)
    if any(vmask)
        msg = "restricted by "
        if other_p == uuid_julia
            msg *= "julia compatibility requirements "
            other_entry = nothing # don't propagate the log
        else
            other_id = pkgID(other_p, graph)
            msg *= "compatibility requirements with $other_id "
        end
        msg *= "to versions: $(vs_string(p1, vmask))"
        if vmask ≠ gconstr[p1]
            if any(gconstr[p1])
                msg *= ", leaving only versions: $(vs_string(p1, gconstr[p1]))"
            else
                msg *= " — no versions left"
            end
        end
    else
        msg = "found to have no compatible versions left with "
        if other_p == uuid_julia
            msg *= "julia"
            other_entry = nothing # don't propagate the log
        else
            other_id = pkgID(other_p, graph)
            msg *= "$other_id "
        end
    end
    entry = rlog.pool[p]
    push!(entry, (other_entry, msg))
    return entry
end

function log_event_pruned!(graph::Graph, p0::Int, s0::Int)
    rlog = graph.data.rlog
    spp = graph.spp
    pkgs = graph.data.pkgs
    pvers = graph.data.pvers

    p = pkgs[p0]
    id = pkgID(p, graph)
    if s0 == spp[p0]
        msg = "determined to be unneeded during graph pruning"
    else
        msg = "fixed during graph pruning to its only remaining available version, $(pvers[p0][s0])"
    end
    entry = rlog.pool[p]
    push!(entry, (nothing, msg))
    return entry
end

function log_event_greedysolved!(graph::Graph, p0::Int, s0::Int)
    rlog = graph.data.rlog
    spp = graph.spp
    pkgs = graph.data.pkgs
    pvers = graph.data.pvers

    p = pkgs[p0]
    id = pkgID(p, graph)
    if s0 == spp[p0]
        msg = "determined to be unneeded by the solver"
    else
        if s0 == spp[p0] - 1
            msg = "set by the solver to its maximum version: $(pvers[p0][s0])"
        else
            msg = "set by the solver to the maximum version compatible with the constraints: $(pvers[p0][s0])"
        end
    end
    entry = rlog.pool[p]
    push!(entry, (nothing, msg))
    return entry
end

function log_event_maxsumsolved!(graph::Graph, p0::Int, s0::Int, why::Symbol)
    rlog = graph.data.rlog
    spp = graph.spp
    pkgs = graph.data.pkgs
    pvers = graph.data.pvers

    p = pkgs[p0]
    id = pkgID(p, graph)
    if s0 == spp[p0]
        @assert why == :uninst
        msg = "determined to be unneeded by the solver"
    else
        @assert why == :constr
        if s0 == spp[p0] - 1
            msg = "set by the solver to its maximum version: $(pvers[p0][s0])"
        else
            msg = "set by the solver version: $(pvers[p0][s0]) (version $(pvers[p0][s0+1]) would violate its constraints)"
        end
    end
    entry = rlog.pool[p]
    push!(entry, (nothing, msg))
    return entry
end

function log_event_maxsumsolved!(graph::Graph, p0::Int, s0::Int, p1::Int)
    rlog = graph.data.rlog
    spp = graph.spp
    pkgs = graph.data.pkgs
    pvers = graph.data.pvers

    p = pkgs[p0]
    id = pkgID(p, graph)
    other_id = pkgID(p1, graph)
    @assert s0 ≠ spp[p0]
    if s0 == spp[p0] - 1
        msg = "set by the solver to its maximum version: $(pvers[p0][s0]) (installation is required by $other_id)"
    else
        msg = "set by the solver version: $(pvers[p0][s0]) (version $(pvers[p0][s0+1]) would violate a dependecy relation with $other_id)"
    end
    other_entry = rlog.pool[pkgs[p1]]
    entry = rlog.pool[p]
    push!(entry, (other_entry, msg))
    return entry
end

function log_event_eq_classes!(graph::Graph, p0::Int)
    rlog = graph.data.rlog
    spp = graph.spp
    gconstr = graph.gconstr
    pkgs = graph.data.pkgs
    pvers = graph.data.pvers

    if any(gconstr[p0][1:(end-1)])
        vns = string(VersionSpec(VersionRange.(pvers[p0][gconstr[p0][1:(end-1)]])))
        gconstr[p0][end] && (vns *= " or uninstalled")
    elseif gconstr[p0][end]
        vns = "uninstalled"
    else
        vns = "no version"
    end

    p = pkgs[p0]
    id = pkgID(p, graph)
    msg = "versions reduced by equivalence to: $vns"
    entry = rlog.pool[p]
    push!(entry, (nothing, msg))
    return entry
end

function log_event_maxsumtrace!(graph::Graph, p0::Int, s0::Int)
    rlog = graph.data.rlog
    p = graph.data.pkgs[p0]
    id = pkgID(p0, graph)
    if s0 < graph.spp[p0]
        msg = "fixed by the MaxSum heuristic to version $(graph.data.pvers[p0][s0])"
    else
        msg = "determined to be unneeded by the MaxSum heuristic"
    end
    entry = rlog.pool[p]
    push!(entry, (nothing, msg))
    return entry
end

const _logindent = " "

showlog(graph::Graph, args...; kw...) = showlog(STDOUT, graph, args...; kw...)

"""
Show the full resolution log. The `view` keyword controls how the events are displayed/grouped:

 * `:plain` for a shallow view, grouped by package, alphabetically (the default)
 * `:tree` for a tree view in which the log of a package is displayed as soon as it appears
           in the process (the top-level is still grouped by package, alphabetically)
 * `:chronological` for a flat view of all events in chronological order
"""
function showlog(io::IO, graph::Graph; view::Symbol = :plain)
    view ∈ [:plain, :tree, :chronological] || throw(ArgumentError("the view argument should be `:plain`, `:tree` or `:chronological`"))
    println(io, "Resolve log:")
    view == :chronological && return showlogjournal(io, graph)
    seen = ObjectIdDict()
    recursive = (view == :tree)
    initentries = [event[1] for event in graph.data.rlog.init.events]
    for entry in sort!(initentries, by=(entry->pkgID(entry.pkg, graph)))
        seen[entry] = true
        _show(io, graph, entry, _logindent, seen, recursive)
    end
end

function showlogjournal(io::IO, graph::Graph)
    journal = graph.data.rlog.journal
    padding = maximum(length(pkgID(p, graph)) for (p,_) in journal)
    for (p, msg) in journal
        println(io, ' ', rpad(pkgID(p, graph), padding), ": ", msg)
    end
end

"""
Show the resolution log for some package, and all the other packages that affected
it during resolution. The `view` option can be either `:plain` or `:tree` (works
the same as for `showlog(io, graph)`); the default is `:tree`.
"""
function showlog(io::IO, graph::Graph, p::UUID; view::Symbol = :tree)
    view ∈ [:plain, :tree] || throw(ArgumentError("the view argument should be `:plain` or `:tree`"))
    rlog = graph.data.rlog
    entry = rlog.pool[p]
    if view == :tree
        _show(io, graph, entry, _logindent, ObjectIdDict(entry=>true), true)
    else
        entries = ResolveLogEntry[entry]
        function getentries(entry)
            for (other_entry,_) in entry.events
                (other_entry ≡ nothing || other_entry ∈ entries) && continue
                push!(entries, other_entry)
                getentries(other_entry)
            end
        end
        getentries(entry)
        for entry in entries
            _show(io, graph, entry, _logindent, ObjectIdDict(), false)
        end
    end
end

# Show a recursive tree with requirements applied to a package, either directly or indirectly
function _show(io::IO, graph::Graph, entry::ResolveLogEntry, indent::String, seen::ObjectIdDict, recursive::Bool)
    toplevel = (indent == _logindent)
    firstglyph = toplevel ? "" : "└─"
    pre = toplevel ? "" : "  "
    println(io, indent, firstglyph, entry.header)
    l = length(entry.events)
    for (i,(otheritem,msg)) in enumerate(entry.events)
        if !isempty(msg)
            print(io, indent * pre, (i==l ? '└' : '├'), '─')
            println(io, msg)
            newindent = indent * pre * (i==l ? "  " : "│ ")
        else
            newindent = indent
        end
        otheritem ≡ nothing && continue
        recursive || continue
        if otheritem ∈ keys(seen)
            println(io, newindent, "└─", otheritem.header, " see above")
            continue
        end
        seen[otheritem] = true
        _show(io, graph, otheritem, newindent, seen, recursive)
    end
end

is_julia(graph::Graph, p0::Int) = graph.data.pkgs[p0] == uuid_julia

"Check for contradictions in the constraints."
function check_constraints(graph::Graph; arewesure::Bool = true)
    np = graph.np
    gconstr = graph.gconstr
    pkgs = graph.data.pkgs
    pvers = graph.data.pvers

    id(p0::Int) = pkgID(p0, graph)

    for p0 = 1:np
        any(gconstr[p0]) && continue
        if arewesure
            err_msg = "Unsatisfiable requirements detected for package $(id(p0)):\n"
        else
            err_msg = "Resolve failed to satisfy requirements for package $(id(p0)):\n"
        end
        err_msg *= sprint(showlog, graph, pkgs[p0])
        throw(PkgError(err_msg))
    end
    return true
end

"""
Propagates current constraints, determining new implicit constraints.
Throws an error in case impossible requirements are detected, printing
a log trace.
"""
function propagate_constraints!(graph::Graph; arewesure::Bool = true)
    np = graph.np
    spp = graph.spp
    gadj = graph.gadj
    gmsk = graph.gmsk
    gconstr = graph.gconstr
    adjdict = graph.adjdict
    pkgs = graph.data.pkgs
    pvers = graph.data.pvers
    rlog = graph.data.rlog

    id(p0::Int) = pkgID(pkgs[p0], graph)

    # packages which are not allowed to be uninstalled
    staged = Set{Int}(p0 for p0 = 1:np if !gconstr[p0][end])

    while !isempty(staged)
        staged_next = Set{Int}()
        for p0 in staged
            gconstr0 = gconstr[p0]
            for (j1,p1) in enumerate(gadj[p0])
                # we don't propagate to julia (purely to have better error messages)
                pkgs[p1] == uuid_julia && continue

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
                    log_event_implicit_req!(graph, p1, added_constr1, p0)
                end
                if !any(gconstr1)
                    if arewesure
                        err_msg = "Unsatisfiable requirements detected for package $(id(p1)):\n"
                    else
                        err_msg = "Resolve failed to satisfy requirements for package $(id(p1)):\n"
                    end
                    err_msg *= sprint(showlog, graph, pkgs[p1])
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
    rlog = data.rlog

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

    # put a record in the log
    log_event_eq_classes!(graph, p0)

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
    data = graph.data
    pkgs = data.pkgs
    pdict = data.pdict
    pvers = data.pvers
    vdict = data.vdict
    pruned = data.pruned
    rlog = data.rlog

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
        log_event_pruned!(graph, p0, s0)
        # We don't record as pruned packages that are not going to be installed
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

    # Reduce log pool (the other items are still reachable through rlog.init)
    rlog.pool = Dict(p=>rlog.pool[p] for p in new_pkgs)

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
    #   * uuid_to_name, eq_classes are unchanged
    #   * pruned and rlog were updated in-place

    # Replace old structures with new ones
    graph.gadj = new_gadj
    graph.gmsk = new_gmsk
    graph.gdir = new_gdir
    graph.gconstr = new_gconstr
    graph.adjdict = new_adjdict
    graph.req_inds = new_req_inds
    graph.fix_inds = new_fix_inds
    graph.spp = new_spp
    graph.np = new_np

    @assert check_consistency(graph)

    return graph
end

"""
Simplifies the graph by propagating constraints, disabling unreachable versions, pruning
and grouping versions into equivalence classes.
"""
function simplify_graph!(graph::Graph, sources::Set{Int} = Set{Int}(); verbose::Bool = false, arewesure::Bool = true)
    propagate_constraints!(graph, arewesure = arewesure)
    disable_unreachable!(graph, sources)
    prune_graph!(graph, verbose = verbose)
    compute_eq_classes!(graph, verbose = verbose)
    return graph
end

end # module
