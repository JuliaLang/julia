# This file is a part of Julia. License is MIT: https://julialang.org/license

module GraphType

import ..Pkg3
using Pkg3.Types
import Pkg3: equalto, Nothing, Types.uuid_julia

export Graph, ResolveLog, add_reqs!, add_fixed!, simplify_graph!, simplify_graph_soft!,
       get_resolve_log, showlog, push_snapshot!, pop_snapshot!, wipe_snapshots!

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

const UUID0 = UUID(UInt128(0))

const ResolveJournal = Vector{Tuple{UUID,String}}

mutable struct ResolveLogEntry
    journal::ResolveJournal # shared with all other entries
    pkg::UUID
    header::String
    events::Vector{Tuple{Any,String}} # here Any should ideally be Union{ResolveLogEntry,Nothing}
    ResolveLogEntry(journal::ResolveJournal, pkg::UUID, header::String = "") = new(journal, pkg, header, [])
end

function Base.push!(entry::ResolveLogEntry, reason::Tuple{Union{ResolveLogEntry,Nothing},String}, to_journal::Bool = true)
    push!(entry.events, reason)
    to_journal && entry.pkg ≠ uuid_julia && push!(entry.journal, (entry.pkg, reason[2]))
    return entry
end

mutable struct ResolveLog
    # init: used to keep track of all package entries which were created during
    #       intialization, since the `pool` can be pruned during the resolution
    #       process.
    init::ResolveLogEntry

    # globals: records global events not associated to any particular package
    globals::ResolveLogEntry

    # pool: records entries associated to each package
    pool::Dict{UUID,ResolveLogEntry}

    # journal: record all messages in order (shared between all entries)
    journal::Vector{Tuple{UUID,String}}

    # exact: keeps track of whether the resolve process is still exact, or
    #        heuristics have been employed
    exact::Bool

    # verbose: print global events messages on screen
    verbose::Bool

    # UUID to names
    uuid_to_name::Dict{UUID,String}

    function ResolveLog(uuid_to_name::Dict{UUID,String}, verbose::Bool = false)
        journal = ResolveJournal()
        init = ResolveLogEntry(journal, UUID0, "")
        globals = ResolveLogEntry(journal, UUID0, "Global events:")
        return new(init, globals, Dict(), journal, true, verbose, uuid_to_name)
    end
end

# Installation state: either a version, or uninstalled
const InstState = Union{VersionNumber,Nothing}


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
            uuid_to_name::Dict{UUID,String};
            verbose::Bool = false
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

        # Hot code, measure performance before changing
        eq_classes = Dict{UUID,Dict{InstState,Set{InstState}}}()
        for p0 = 1:np
            d = Dict{InstState, Set{InstState}}()
            for v0 = 1:spp[p0]
                let p0 = p0 # Due to https://github.com/JuliaLang/julia/issues/15276
                    d[eq_vn(v0,p0)] = Set([eq_vn(v0,p0)])
                end
            end
            eq_classes[pkgs[p0]] = d
        end

        # the resolution log is actually initialized below
        rlog = ResolveLog(uuid_to_name, verbose)

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
        pruned = copy(data.pruned)
        eq_classes = Dict(p => copy(eq) for (p,eq) in data.eq_classes)
        rlog = deepcopy(data.rlog)
        uuid_to_name = rlog.uuid_to_name

        return new(pkgs, np, spp, pdict, pvers, vdict, uuid_to_name, pruned, eq_classes, rlog)
    end
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

    ignored::BitVector

    # stack of constraints/ignored packages:
    #   allows to keep a sort of "versioning" of the constraints
    #   such that the solver can implement tentative solutions
    solve_stack::Vector{Tuple{Vector{BitVector},BitVector}}

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
            fixed::Dict{UUID,Fixed} = Dict{UUID,Fixed}(uuid_julia=>Fixed(VERSION));
            verbose::Bool = false
        )

        extra_uuids = union(keys(reqs), keys(fixed), map(fx->keys(fx.requires), values(fixed))...)
        extra_uuids ⊆ keys(versions) || error("unknown UUID found in reqs/fixed") # TODO?

        # Type assert below due to https://github.com/JuliaLang/julia/issues/25918
        data = GraphData(versions, deps, compat, uuid_to_name, verbose = verbose)::GraphData
        pkgs, np, spp, pdict, pvers, vdict, rlog = data.pkgs, data.np, data.spp, data.pdict, data.pvers, data.vdict, data.rlog

        local extended_deps
        let spp = spp # Due to https://github.com/JuliaLang/julia/issues/15276
            # Type assert below to help inference
            extended_deps = [Vector{Dict{Int,BitVector}}(uninitialized, spp[p0]-1) for p0 = 1:np]::Vector{Vector{Dict{Int,BitVector}}}
        end
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
                    p1 == p0 && error("Package $(pkgID(pkgs[p0], uuid_to_name)) version $vn has a dependency with itself")
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
            # Hot code, measure performance before changing
            req_msk = Dict{Int,BitVector}()
            for (p1, vs) in req
                pv = pvers[p1]
                req_msk_p1 = BitArray(uninitialized, spp[p1] - 1)
                @inbounds for i in 1:spp[p1] - 1
                    req_msk_p1[i] = pv[i] ∈ vs
                end
                req_msk[p1] = req_msk_p1
            end
            extended_deps[p0][v0] = req_msk
        end

        gadj = [Int[] for p0 = 1:np]
        gmsk = [BitMatrix[] for p0 = 1:np]
        local gconstr
        let spp = spp # Due to https://github.com/JuliaLang/julia/issues/15276
            gconstr = [trues(spp[p0]) for p0 = 1:np]
        end
        adjdict = [Dict{Int,Int}() for p0 = 1:np]

        for p0 = 1:np, v0 = 1:(spp[p0]-1), (p1,rmsk1) in extended_deps[p0][v0]
            @assert p0 ≠ p1
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
                bmt = copy(transpose(bm))

                push!(gmsk[p0], bm)
                push!(gmsk[p1], bmt)
            else
                bm = gmsk[p0][j0]
                bmt = gmsk[p1][j1]
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

        ignored = falses(np)
        solve_stack = Tuple{Vector{BitVector},BitVector}[]

        graph = new(data, gadj, gmsk, gconstr, adjdict, req_inds, fix_inds, ignored, solve_stack, spp, np)

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
        gconstr = [copy(graph.gconstr[p0]) for p0 = 1:np]
        adjdict = [copy(graph.adjdict[p0]) for p0 = 1:np]
        req_inds = copy(graph.req_inds)
        fix_inds = copy(graph.fix_inds)
        ignored = copy(graph.ignored)
        solve_stack = [([copy(gc0) for gc0 in sav_gconstr],copy(sav_ignored)) for (sav_gconstr,sav_ignored) in graph.solve_stack]

        return new(data, gadj, gmsk, gconstr, adjdict, req_inds, fix_inds, ignored, solve_stack, spp, np)
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

Types.pkgID(p::UUID, rlog::ResolveLog) = pkgID(p, rlog.uuid_to_name)
Types.pkgID(p::UUID, data::GraphData) = pkgID(p, data.uuid_to_name)
Types.pkgID(p0::Int, data::GraphData) = pkgID(data.pkgs[p0], data)
Types.pkgID(p, graph::Graph) = pkgID(p, graph.data)

function check_consistency(graph::Graph)
    np = graph.np
    spp = graph.spp
    gadj = graph.gadj
    gmsk = graph.gmsk
    gconstr = graph.gconstr
    adjdict = graph.adjdict
    req_inds = graph.req_inds
    fix_inds = graph.fix_inds
    ignored = graph.ignored
    solve_stack = graph.solve_stack
    data = graph.data
    pkgs = data.pkgs
    pdict = data.pdict
    pvers = data.pvers
    vdict = data.vdict
    pruned = data.pruned
    eq_classes = data.eq_classes
    rlog = data.rlog
    # TODO: check ignored and solve_stack

    @assert np ≥ 0
    for x in [spp, gadj, gmsk, gconstr, adjdict, ignored, rlog.pool, pkgs, pdict, pvers, vdict]
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
        adjdict0 = adjdict[p0]
        @assert length(gmsk0) == length(gadj0)
        @assert length(adjdict0) == length(gadj0)
        for (j0,p1) in enumerate(gadj0)
            @assert p1 ≠ p0
            @assert adjdict[p1][p0] == j0
            spp1 = spp[p1]
            @assert size(gmsk0[j0]) == (spp1,spp0)
            j1 = adjdict0[p1]
            gmsk1 = gmsk[p1]
            @assert gmsk1[j1] == copy(transpose(gmsk0[j0]))
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

    for (sav_gconstr,sav_ignored) in solve_stack
        @assert length(sav_ignored) == np
        @assert length(sav_gconstr) == np
        for p0 = 1:np
            @assert length(sav_gconstr[p0]) == spp[p0]
        end
    end

    return true
end

function push_snapshot!(graph::Graph)
    np = graph.np
    gconstr = graph.gconstr
    ignored = graph.ignored
    solve_stack = graph.solve_stack
    sav_gconstr = [copy(gc0) for gc0 in gconstr]
    sav_ignored = copy(ignored)
    push!(solve_stack, (gconstr, ignored))
    graph.gconstr = sav_gconstr
    graph.ignored = sav_ignored
    return graph
end

function pop_snapshot!(graph::Graph)
    np = graph.np
    solve_stack = graph.solve_stack
    @assert length(solve_stack) > 0
    sav_gconstr, sav_ignored = pop!(graph.solve_stack)
    graph.gconstr = sav_gconstr
    graph.ignored = sav_ignored
    return graph
end

function wipe_snapshots!(graph::Graph)
    np = graph.np
    solve_stack = graph.solve_stack
    if length(solve_stack) > 0
        graph.gconstr, graph.ignored = first(solve_stack)
        empty!(solve_stack)
    end
    return graph
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
            push!(rlog.init, (first_entry, ""), false)
        end
    end
    return data
end

function log_event_fixed!(graph::Graph, fp::UUID, fx::Fixed)
    rlog = graph.data.rlog
    id = pkgID(fp, rlog)
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
    id = pkgID(rp, rlog)
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
            other_id = pkgID(other_p, rlog)
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

function log_event_global!(graph::Graph, msg::String)
    rlog = graph.data.rlog
    rlog.verbose && @info(msg)
    push!(rlog.globals, (nothing, msg))
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
    id = pkgID(p, rlog)
    other_p, other_entry = pkgs[p0], rlog.pool[pkgs[p0]]
    other_id = pkgID(other_p, rlog)
    if any(vmask)
        msg = "restricted by "
        if other_p == uuid_julia
            msg *= "julia compatibility requirements "
            other_entry = nothing # don't propagate the log
        else
            other_id = pkgID(other_p, rlog)
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
            other_id = pkgID(other_p, rlog)
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
    id = pkgID(p, rlog)
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
    id = pkgID(p, rlog)
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
    id = pkgID(p, rlog)
    if s0 == spp[p0]
        @assert why == :uninst
        msg = "determined to be unneeded by the solver"
    else
        @assert why == :constr
        if s0 == spp[p0] - 1
            msg = "set by the solver to its maximum version: $(pvers[p0][s0])"
        else
            msg = "set by the solver to version: $(pvers[p0][s0]) (version $(pvers[p0][s0+1]) would violate its constraints)"
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
    id = pkgID(p, rlog)
    other_id = pkgID(pkgs[p1], rlog)
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
    id = pkgID(p, rlog)
    msg = "versions reduced by equivalence to: $vns"
    entry = rlog.pool[p]
    push!(entry, (nothing, msg))
    return entry
end

function log_event_maxsumtrace!(graph::Graph, p0::Int, s0::Int)
    rlog = graph.data.rlog
    rlog.exact = false
    p = graph.data.pkgs[p0]
    id = pkgID(p, rlog)
    if s0 < graph.spp[p0]
        msg = "fixed by the MaxSum heuristic to version $(graph.data.pvers[p0][s0])"
    else
        msg = "determined to be unneeded by the MaxSum heuristic"
    end
    entry = rlog.pool[p]
    push!(entry, (nothing, msg))
    return entry
end

"Get the resolution log, detached"
get_resolve_log(graph::Graph) = deepcopy(graph.data.rlog)

const _logindent = " "

showlog(graph::Graph, args...; kw...) = showlog(STDOUT, graph, args...; kw...)
showlog(io::IO, graph::Graph, args...; kw...) = showlog(io, graph.data.rlog, args...; kw...)
showlog(rlog::ResolveLog, args...; kw...) = showlog(STDOUT, rlog, args...; kw...)

"""
Show the full resolution log. The `view` keyword controls how the events are displayed/grouped:

 * `:plain` for a shallow view, grouped by package, alphabetically (the default)
 * `:tree` for a tree view in which the log of a package is displayed as soon as it appears
           in the process (the top-level is still grouped by package, alphabetically)
 * `:chronological` for a flat view of all events in chronological order
"""
function showlog(io::IO, rlog::ResolveLog; view::Symbol = :plain)
    view ∈ [:plain, :tree, :chronological] || throw(ArgumentError("the view argument should be `:plain`, `:tree` or `:chronological`"))
    println(io, "Resolve log:")
    view == :chronological && return showlogjournal(io, rlog)
    seen = IdDict()
    recursive = (view == :tree)
    _show(io, rlog, rlog.globals, _logindent, seen, false)
    initentries = [event[1] for event in rlog.init.events]
    for entry in sort!(initentries, by=(entry->pkgID(entry.pkg, rlog)))
        seen[entry] = true
        _show(io, rlog, entry, _logindent, seen, recursive)
    end
end

function showlogjournal(io::IO, rlog::ResolveLog)
    journal = rlog.journal
    id(p) = p == UUID0 ? "[global event]" : pkgID(p, rlog)
    padding = maximum(length(id(p)) for (p,_) in journal)
    for (p,msg) in journal
        println(io, ' ', rpad(id(p), padding), ": ", msg)
    end
end

"""
Show the resolution log for some package, and all the other packages that affected
it during resolution. The `view` option can be either `:plain` or `:tree` (works
the same as for `showlog(io, rlog)`); the default is `:tree`.
"""
function showlog(io::IO, rlog::ResolveLog, p::UUID; view::Symbol = :tree)
    view ∈ [:plain, :tree] || throw(ArgumentError("the view argument should be `:plain` or `:tree`"))
    entry = rlog.pool[p]
    if view == :tree
        _show(io, rlog, entry, _logindent, IdDict(entry=>true), true)
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
            _show(io, rlog, entry, _logindent, IdDict(), false)
        end
    end
end

# Show a recursive tree with requirements applied to a package, either directly or indirectly
function _show(io::IO, rlog::ResolveLog, entry::ResolveLogEntry, indent::String, seen::IdDict, recursive::Bool)
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
        _show(io, rlog, otheritem, newindent, seen, recursive)
    end
end

is_julia(graph::Graph, p0::Int) = graph.data.pkgs[p0] == uuid_julia

"Check for contradictions in the constraints."
function check_constraints(graph::Graph)
    np = graph.np
    gconstr = graph.gconstr
    pkgs = graph.data.pkgs
    pvers = graph.data.pvers
    rlog = graph.data.rlog
    exact = graph.data.rlog.exact

    id(p0::Int) = pkgID(p0, graph)

    for p0 = 1:np
        any(gconstr[p0]) && continue
        if exact
            err_msg = "Unsatisfiable requirements detected for package $(id(p0)):\n"
        else
            err_msg = "Resolve failed to satisfy requirements for package $(id(p0)):\n"
        end
        err_msg *= sprint(showlog, rlog, pkgs[p0])
        throw(ResolverError(err_msg))
    end
    return true
end

"""
Propagates current constraints, determining new implicit constraints.
Throws an error in case impossible requirements are detected, printing
a log trace.
"""
function propagate_constraints!(graph::Graph, sources::Set{Int} = Set{Int}(); log_events::Bool = true)
    np = graph.np
    spp = graph.spp
    gadj = graph.gadj
    gmsk = graph.gmsk
    gconstr = graph.gconstr
    adjdict = graph.adjdict
    ignored = graph.ignored
    pkgs = graph.data.pkgs
    pvers = graph.data.pvers
    rlog = graph.data.rlog
    exact = rlog.exact

    id(p0::Int) = pkgID(pkgs[p0], graph)

    log_events && log_event_global!(graph, "propagating constraints")

    # unless otherwise specified, start from packages which
    # are not allowed to be uninstalled
    staged = isempty(sources) ?
        Set{Int}(p0 for p0 = 1:np if !gconstr[p0][end]) :
        sources

    while !isempty(staged)
        staged_next = Set{Int}()
        for p0 in staged
            gconstr0 = gconstr[p0]
            for (j1,p1) in enumerate(gadj[p0])
                # if p1 is ignored, the relation between it and all its neighbors
                # has already been propagated
                ignored[p1] && continue

                # we don't propagate to julia (purely to have better error messages)
                pkgs[p1] == uuid_julia && continue

                msk = gmsk[p0][j1]
                # consider the sub-mask with only allowed versions of p0
                sub_msk = msk[:,gconstr0]
                # if an entire row of the sub-mask is false, that version of p1
                # is effectively forbidden
                # (this is just like calling `any` row-wise)
                added_constr1 = any!(BitVector(uninitialized, spp[p1]), sub_msk)
                # apply the new constraints, checking for contradictions
                # (keep the old ones for comparison)
                gconstr1 = gconstr[p1]
                old_gconstr1 = copy(gconstr1)
                gconstr1 .&= added_constr1
                # if the new constraints are more restrictive than the
                # previous ones, record it and propagate them next
                if gconstr1 ≠ old_gconstr1
                    push!(staged_next, p1)
                    log_events && log_event_implicit_req!(graph, p1, added_constr1, p0)
                end
                if !any(gconstr1)
                    if exact
                        err_msg = "Unsatisfiable requirements detected for package $(id(p1)):\n"
                    else
                        err_msg = "Resolve failed to satisfy requirements for package $(id(p1)):\n"
                    end
                    err_msg *= sprint(showlog, rlog, pkgs[p1])
                    throw(ResolverError(err_msg))
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

    log_event_global!(graph, "disabling unreachable nodes")

    # 2nd argument are packages which are not allowed to be uninstalled
    staged = union(sources, Set{Int}(p0 for p0 = 1:np if !gconstr[p0][end]))
    seen = copy(staged)

    while !isempty(staged)
        staged_next = Set{Int}()
        for p0 in staged
            gconstr0idx = findall(gconstr[p0][1:(end-1)])
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

function deep_clean!(graph::Graph)
    np = graph.np
    spp = graph.spp

    log_event_global!(graph, "cleaning graph")

    sumspp = sum(count(graph.gconstr[p0]) for p0 = 1:np)

    while true
        gconstr_msk = [trues(spp[p0]) for p0 = 1:np]

        str_len = 0

        for p0 = 1:np, v0 in findall(graph.gconstr[p0])
            print("\r" * " "^str_len * "\r")
            msg = "> $p0 / $np"
            print(msg)
            str_len = length(msg)
            push_snapshot!(graph)
            fill!(graph.gconstr[p0], false)
            graph.gconstr[p0][v0] = true
            try
                propagate_constraints!(graph, Set{Int}([p0]), log_events = false)
            catch err
                err isa ResolverError || rethrow(err)
                gconstr_msk[p0][v0] = false
            end
            pop_snapshot!(graph)
        end

        println()

        affected = Int[]
        for p0 = 1:np
            gconstr0 = graph.gconstr[p0]
            old_gconstr0 = copy(gconstr0)
            gconstr0 .&= gconstr_msk[p0]
            if old_gconstr0 ≠ gconstr0
                push!(affected, p0)
                #TODO : log event
            end
            if !any(gconstr0)
                # TODO : what should we do here??
                # throw(ResolverError("aaaaaaaaaaaaaahhhhhhhh")) # XXX
            end
        end
        println("> affected = $(length(affected))")
        isempty(affected) && break
    end

    sumspp_new = sum(count(graph.gconstr[p0]) for p0 = 1:np)

    log_event_global!(graph, "cleaned graph, stats (total n. of states): before = $(sumspp) after = $(sumspp_new) diff = $(sumspp-sumspp_new)")

    return graph
end

"""
Reduce the number of versions in the graph by putting all the versions of
a package that behave identically into equivalence classes, keeping only
the highest version of the class as representative.
"""
function compute_eq_classes!(graph::Graph)
    log_event_global!(graph, "computing version equivalence classes")

    np = graph.np
    sumspp = sum(graph.spp)
    for p0 = 1:np
        build_eq_classes1!(graph, p0)
    end

    log_event_global!(graph, "computed version equivalence classes, stats (total n. of states): before = $(sumspp) after = $(sum(graph.spp))")

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
    cmat = vcat(BitMatrix(copy(transpose(gconstr[p0]))), gmsk[p0]...)
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

function compute_eq_classes_soft!(graph::Graph; log_events::Bool = true)
    log_events && log_event_global!(graph, "computing version equivalence classes")

    np = graph.np

    np == 0 && return graph

    ignored = graph.ignored
    gconstr = graph.gconstr
    sumspp = sum(count(gconstr[p0]) for p0 = 1:np)
    for p0 = 1:np
        ignored[p0] && continue
        build_eq_classes_soft1!(graph, p0)
    end
    sumspp_new = sum(count(gconstr[p0]) for p0 = 1:np)

    log_events && log_event_global!(graph, "computed version equivalence classes, stats (total n. of states): before = $(sumspp) after = $(sumspp_new) diff = $(sumspp_new-sumspp)")

    @assert check_consistency(graph)

    return graph
end

function build_eq_classes_soft1!(graph::Graph, p0::Int)
    np = graph.np
    spp = graph.spp
    gadj = graph.gadj
    gmsk = graph.gmsk
    gconstr = graph.gconstr
    ignored = graph.ignored

    # concatenate all the constraints; the columns of the
    # result encode the behavior of each version
    gadj0 = gadj[p0]
    gmsk0 = gmsk[p0]
    gconstr0 = gconstr[p0]
    eff_spp0 = count(gconstr0)
    cvecs = BitVector[vcat(BitVector(), (gmsk0[j1][gconstr[gadj0[j1]],v0] for j1 = 1:length(gadj0) if !ignored[gadj0[j1]])...) for v0 in findall(gconstr0)]

    @assert length(cvecs) == eff_spp0

    # find unique behaviors
    repr_vecs = unique(cvecs)

    # number of equivaent classes
    neq = length(repr_vecs)

    neq == eff_spp0 && return # nothing to do here

    # group versions into sets that behave identically
    # each set is represented by its highest-valued member
    repr_vers = sort!(Int[findlast(equalto(repr_vecs[w0]), cvecs) for w0 = 1:neq])
    @assert all(repr_vers .> 0)
    @assert repr_vers[end] == eff_spp0

    # convert the version numbers into the original numbering
    repr_vers = findall(gconstr0)[repr_vers]

    @assert all(gconstr0[repr_vers])

    # disable the other versions by introducing additional constraints
    fill!(gconstr0, false)
    gconstr0[repr_vers] = true

    return
end

function update_ignored!(graph::Graph)
    np = graph.np
    gconstr = graph.gconstr
    ignored = graph.ignored

    for p0 = 1:np
        ignored[p0] = (count(gconstr[p0]) == 1)
    end

    return graph
end

"""
Prune away fixed and unnecessary packages, and the
disallowed versions for the remaining packages.
"""
function prune_graph!(graph::Graph)
    np = graph.np
    spp = graph.spp
    gadj = graph.gadj
    gmsk = graph.gmsk
    gconstr = graph.gconstr
    adjdict = graph.adjdict
    req_inds = graph.req_inds
    fix_inds = graph.fix_inds
    ignored = graph.ignored
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
    old_idx = findall(pkg_mask)
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
    for p0 in findall(.~(pkg_mask))
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
    new_ignored = ignored[pkg_mask]
    empty!(graph.solve_stack)

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

    log_event_global!(graph, "pruned graph — stats (n. of packages, mean connectivity): before = ($np,$(mean(spp))) after = ($new_np,$(mean(new_spp)))")

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
    graph.gconstr = new_gconstr
    graph.adjdict = new_adjdict
    graph.req_inds = new_req_inds
    graph.fix_inds = new_fix_inds
    graph.ignored = new_ignored
    graph.spp = new_spp
    graph.np = new_np
    # Note: solve_stack was emptied in-place

    @assert check_consistency(graph)

    return graph
end

"""
Simplifies the graph by propagating constraints, disabling unreachable versions, pruning
and grouping versions into equivalence classes.
"""
function simplify_graph!(graph::Graph, sources::Set{Int} = Set{Int}(); clean_graph::Bool = false)
    propagate_constraints!(graph)
    disable_unreachable!(graph, sources)
    clean_graph && deep_clean!(graph)
    prune_graph!(graph)
    compute_eq_classes!(graph)
    return graph
end

function simplify_graph_soft!(graph::Graph, sources::Set{Int} = Set{Int}(); log_events = true)
    propagate_constraints!(graph, sources, log_events = log_events)
    update_ignored!(graph)
    compute_eq_classes_soft!(graph, log_events = log_events)
    update_ignored!(graph)
    return graph
end

end # module
