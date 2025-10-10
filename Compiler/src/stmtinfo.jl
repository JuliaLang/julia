# This file is a part of Julia. License is MIT: https://julialang.org/license

@nospecialize

"""
    call::CallMeta

A simple struct that captures both the return type (`call.rt`)
and any additional information (`call.info`) for a given generic call.
"""
struct CallMeta
    rt::Any
    exct::Any
    effects::Effects
    info::CallInfo
    refinements # ::Union{Nothing,SlotRefinement,Vector{Any}}
    function CallMeta(rt::Any, exct::Any, effects::Effects, info::CallInfo,
                      refinements=nothing)
        @nospecialize rt exct info
        return new(rt, exct, effects, info, refinements)
    end
end

struct NoCallInfo <: CallInfo end
add_edges_impl(::Vector{Any}, ::NoCallInfo) = nothing

abstract type InferredCallResult end

struct CachedCallResult <: InferredCallResult
    src
    effects::Effects
    edge::CodeInstance
end

struct VolatileInferenceResult <: InferredCallResult
    inf_result::InferenceResult
end

struct LocalInferenceResult <: InferredCallResult
    inf_result::InferenceResult
end

abstract type InferredConstCallResult <: InferredCallResult end

struct ConstPropResult <: InferredConstCallResult
    result::InferenceResult
end

struct ConcreteResult <: InferredConstCallResult
    edge::CodeInstance
    effects::Effects
    result
    ConcreteResult(edge::CodeInstance, effects::Effects) = new(edge, effects)
    ConcreteResult(edge::CodeInstance, effects::Effects, @nospecialize val) = new(edge, effects, val)
end

struct SemiConcreteResult <: InferredConstCallResult
    edge::CodeInstance
    ir::IRCode
    effects::Effects
    spec_info::SpecInfo
end

"""
    info::MethodMatchInfo <: CallInfo

Captures the essential arguments and result of a `:jl_matching_methods` lookup
for the given call (`info.results`). This info may then be used by the
optimizer, without having to re-consult the method table.
This info is illegal on any statement that is not a call to a generic function.
"""
struct MethodMatchInfo <: CallInfo
    results::MethodLookupResult
    mt::MethodTable
    atype
    fullmatch::Bool
    edges::Vector{Union{Nothing,CodeInstance}}
    call_results::Vector{Union{Nothing,InferredCallResult}}
    function MethodMatchInfo(
        results::MethodLookupResult, mt::MethodTable, @nospecialize(atype), fullmatch::Bool)
        edges = fill!(Vector{Union{Nothing,CodeInstance}}(undef, length(results)), nothing)
        call_results = fill!(Vector{Union{Nothing,InferredCallResult}}(undef, length(results)), nothing)
        return new(results, mt, atype, fullmatch, edges, call_results)
    end
end
add_edges_impl(edges::Vector{Any}, info::MethodMatchInfo) = _add_edges_impl(edges, info)
function _add_edges_impl(edges::Vector{Any}, info::MethodMatchInfo, mi_edge::Bool=false)
    if !fully_covering(info)
        exists = false
        for i in 2:length(edges)
            if edges[i] === Core.methodtable && edges[i-1] == info.atype
                exists = true
                break
            end
        end
        if !exists
            push!(edges, info.atype)
            push!(edges, Core.methodtable)
        end
    end
    nmatches = length(info.results)
    if nmatches == length(info.edges) == 1 && fully_covering(info)
        # try the optimized format for the representation, if possible and applicable
        # if this doesn't succeed, the backedge will be less precise,
        # but the forward edge will maintain the precision
        edge = info.edges[1]
        m = info.results[1]
        if edge === nothing
            mi = specialize_method(m) # don't allow `Method`-edge for this optimized format
            edge = mi
        else
            mi = edge.def::MethodInstance
        end
        if mi.specTypes === m.spec_types
            add_one_edge!(edges, edge)
            return nothing
        end
    end
    # add check for whether this lookup already existed in the edges list
    # encode nmatches as negative if fully_covers is false
    encoded_nmatches = fully_covering(info) ? nmatches : -nmatches
    for i in 1:length(edges)
        if edges[i] === encoded_nmatches && edges[i+1] == info.atype
            # TODO: must also verify the CodeInstance match too
            return nothing
        end
    end
    push!(edges, encoded_nmatches, info.atype)
    for i = 1:nmatches
        edge = info.edges[i]
        m = info.results[i]
        if edge === nothing
            edge = mi_edge ? specialize_method(m) : m.method
        else
            @assert edge.def.def === m.method
        end
        push!(edges, edge)
    end
    nothing
end
function add_one_edge!(edges::Vector{Any}, edge::MethodInstance)
    i = 1
    while i <= length(edges)
        edgeᵢ = edges[i]
        edgeᵢ isa Int && (i += 2 + abs(edgeᵢ); continue)
        edgeᵢ isa CodeInstance && (edgeᵢ = get_ci_mi(edgeᵢ))
        edgeᵢ isa MethodInstance || (i += 1; continue)
        if edgeᵢ === edge && !(i > 1 && edges[i-1] isa Type)
            return # found existing covered edge
        end
        i += 1
    end
    push!(edges, edge)
    nothing
end
function add_one_edge!(edges::Vector{Any}, edge::CodeInstance)
    i = 1
    while i <= length(edges)
        edgeᵢ_orig = edgeᵢ = edges[i]
        edgeᵢ isa Int && (i += 2 + abs(edgeᵢ); continue)
        edgeᵢ isa CodeInstance && (edgeᵢ = get_ci_mi(edgeᵢ))
        edgeᵢ isa MethodInstance || (i += 1; continue)
        if edgeᵢ === edge.def && !(i > 1 && edges[i-1] isa Type)
            if edgeᵢ_orig isa MethodInstance
                # found edge we can upgrade
                edges[i] = edge
                return
            elseif true # XXX compare `CodeInstance` identify?
                return
            end
        end
        i += 1
    end
    push!(edges, edge)
    nothing
end
nsplit_impl(::MethodMatchInfo) = 1
getsplit_impl(info::MethodMatchInfo, idx::Int) = (@assert idx == 1; info.results)
getresult_impl(info::MethodMatchInfo, idx::Int) = info.call_results[idx]

"""
    info::UnionSplitInfo <: CallInfo

If inference decides to partition the method search space by splitting unions,
it will issue a method lookup query for each such partition. This info indicates
that such partitioning happened and wraps the corresponding `MethodMatchInfo` for
each partition (`info.matches::Vector{MethodMatchInfo}`).
This info is illegal on any statement that is not a call to a generic function.
"""
struct UnionSplitInfo <: CallInfo
    split::Vector{MethodMatchInfo}
end
add_edges_impl(edges::Vector{Any}, info::UnionSplitInfo) =
    _add_edges_impl(edges, info)
_add_edges_impl(edges::Vector{Any}, info::UnionSplitInfo, mi_edge::Bool=false) =
    for split in info.split; _add_edges_impl(edges, split, mi_edge); end
nsplit_impl(info::UnionSplitInfo) = length(info.split)
getsplit_impl(info::UnionSplitInfo, idx::Int) = getsplit(info.split[idx], 1)
function getresult_impl(info::UnionSplitInfo, idx::Int)
    for split in info.split
        n = length(split.call_results)
        if idx ≤ n
            return split.call_results[idx]
        else
            idx -= n
        end
    end
end

"""
    info::MethodResultPure <: CallInfo

This struct represents a method result constant was proven to be effect-free.
"""
struct MethodResultPure <: CallInfo
    info::CallInfo
end
let instance = MethodResultPure(NoCallInfo())
    global MethodResultPure
    MethodResultPure() = instance
end
add_edges_impl(edges::Vector{Any}, info::MethodResultPure) = add_edges!(edges, info.info)

"""
    ainfo::AbstractIterationInfo

Captures all the information for abstract iteration analysis of a single value.
Each (abstract) call to `iterate`, corresponds to one entry in `ainfo.each::Vector{CallMeta}`.
"""
struct AbstractIterationInfo
    each::Vector{CallMeta}
    complete::Bool
end

const MaybeAbstractIterationInfo = Union{Nothing, AbstractIterationInfo}

"""
    info::ApplyCallInfo <: CallInfo

This info applies to any call of `_apply_iterate(...)` and captures both the
info of the actual call being applied and the info for any implicit call
to the `iterate` function. Note that it is possible for the call itself
to be yet another `_apply_iterate`, in which case the `info.call` field will
be another `ApplyCallInfo`. This info is illegal on any statement that is
not an `_apply_iterate` call.
"""
struct ApplyCallInfo <: CallInfo
    # The info for the call itself
    call::CallInfo
    # AbstractIterationInfo for each argument, if applicable
    arginfo::Vector{MaybeAbstractIterationInfo}
end
function add_edges_impl(edges::Vector{Any}, info::ApplyCallInfo)
    add_edges!(edges, info.call)
    for arg in info.arginfo
        arg === nothing && continue
        for edge in arg.each
            add_edges!(edges, edge.info)
        end
    end
end

"""
    info::UnionSplitApplyCallInfo <: CallInfo

Like `UnionSplitInfo`, but for `ApplyCallInfo` rather than `MethodMatchInfo`.
This info is illegal on any statement that is not an `_apply_iterate` call.
"""
struct UnionSplitApplyCallInfo <: CallInfo
    infos::Vector{ApplyCallInfo}
end
add_edges_impl(edges::Vector{Any}, info::UnionSplitApplyCallInfo) =
    for split in info.infos; add_edges!(edges, split); end

"""
    info::InvokeCICallInfo

Represents a resolved call to `Core.invoke` targeting a `Core.CodeInstance`
"""
struct InvokeCICallInfo <: CallInfo
    edge::CodeInstance
end
add_edges_impl(edges::Vector{Any}, info::InvokeCICallInfo) =
    add_inlining_edge!(edges, info.edge)
nsplit_impl(::InvokeCICallInfo) = 0

"""
    info::InvokeCallInfo

Represents a resolved call to `Core.invoke`, carrying the `info.match::MethodMatch` of
the method that has been processed.
Optionally keeps `info.result::InferenceResult` that keeps constant information.
"""
struct InvokeCallInfo <: CallInfo
    edge::Union{Nothing,CodeInstance}
    match::MethodMatch
    result::Union{Nothing,InferredCallResult}
    atype # ::Type
end
add_edges_impl(edges::Vector{Any}, info::InvokeCallInfo) =
    _add_edges_impl(edges, info)
function _add_edges_impl(edges::Vector{Any}, info::InvokeCallInfo, mi_edge::Bool=false)
    edge = info.edge
    if edge === nothing
        edge = mi_edge ? specialize_method(info.match) : info.match.method
    end
    add_invoke_edge!(edges, info.atype, edge)
    nothing
end
function add_invoke_edge!(edges::Vector{Any}, @nospecialize(atype), edge::Union{MethodInstance,Method})
    for i in 2:length(edges)
        edgeᵢ = edges[i]
        edgeᵢ isa CodeInstance && (edgeᵢ = edgeᵢ.def)
        edgeᵢ isa MethodInstance || edgeᵢ isa Method || continue
        if edgeᵢ === edge
            edge_minus_1 = edges[i-1]
            if edge_minus_1 isa Type && edge_minus_1 == atype
                return # found existing covered edge
            end
        end
    end
    push!(edges, atype)
    push!(edges, edge)
    nothing
end
function add_invoke_edge!(edges::Vector{Any}, @nospecialize(atype), edge::CodeInstance)
    for i in 2:length(edges)
        edgeᵢ_orig = edgeᵢ = edges[i]
        edgeᵢ isa CodeInstance && (edgeᵢ = edgeᵢ.def)
        if ((edgeᵢ isa MethodInstance && edgeᵢ === edge.def) ||
            (edgeᵢ isa Method && edgeᵢ === edge.def.def))
            edge_minus_1 = edges[i-1]
            if edge_minus_1 isa Type && edge_minus_1 == atype
                if edgeᵢ_orig isa MethodInstance || edgeᵢ_orig isa Method
                    # found edge we can upgrade
                    edges[i] = edge
                    return
                elseif true # XXX compare `CodeInstance` identify?
                    return
                end
            end
        end
    end
    push!(edges, atype)
    push!(edges, edge)
    nothing
end

function add_inlining_edge!(edges::Vector{Any}, edge::MethodInstance)
    # check if we already have an edge to this code
    i = 1
    while i <= length(edges)
        edgeᵢ = edges[i]
        if edgeᵢ isa Method && edgeᵢ === edge.def
            # found edge we can upgrade
            edges[i] = edge
            return
        end
        edgeᵢ isa CodeInstance && (edgeᵢ = edgeᵢ.def)
        if edgeᵢ isa MethodInstance && edgeᵢ === edge
            return # found existing covered edge
        end
        i += 1
    end
    # add_invoke_edge alone
    push!(edges, (edge.def::Method).sig)
    push!(edges, edge)
    nothing
end
function add_inlining_edge!(edges::Vector{Any}, edge::CodeInstance)
    # check if we already have an edge to this code
    i = 1
    while i <= length(edges)
        edgeᵢ = edges[i]
        if edgeᵢ isa Method && edgeᵢ === edge.def.def
            # found edge we can upgrade
            edges[i] = edge
            return
        end
        if edgeᵢ isa MethodInstance && edgeᵢ === edge.def
            # found edge we can upgrade
            edges[i] = edge
            return
        end
        if edgeᵢ isa CodeInstance && edgeᵢ.def === edge.def
            # found existing edge
            # XXX compare `CodeInstance` identify?
            return
        end
        i += 1
    end
    # add_invoke_edge alone
    push!(edges, (get_ci_mi(edge).def::Method).sig)
    push!(edges, edge)
    nothing
end

nsplit_impl(::InvokeCallInfo) = 1
getsplit_impl(info::InvokeCallInfo, idx::Int) = (@assert idx == 1; MethodLookupResult(Core.MethodMatch[info.match],
    WorldRange(typemin(UInt), typemax(UInt)), false))
getresult_impl(info::InvokeCallInfo, idx::Int) = (@assert idx == 1; info.result)

"""
    info::OpaqueClosureCallInfo

Represents a resolved call of opaque closure, carrying the `info.match::MethodMatch` of
the method that has been processed.
Optionally keeps `info.result::InferenceResult` that keeps constant information.
"""
struct OpaqueClosureCallInfo <: CallInfo
    edge::Union{Nothing,CodeInstance}
    match::MethodMatch
    result::Union{Nothing,InferredCallResult}
end
function add_edges_impl(edges::Vector{Any}, info::OpaqueClosureCallInfo)
    edge = info.edge
    if edge !== nothing
        add_one_edge!(edges, edge)
    end
    nothing
end

"""
    info::OpaqueClosureCreateInfo <: CallInfo

This info may be constructed upon opaque closure construction, with `info.unspec::CallMeta`
carrying out inference result of an unreal, partially specialized call (i.e. specialized on
the closure environment, but not on the argument types of the opaque closure) in order to
allow the optimizer to rewrite the return type parameter of the `OpaqueClosure` based on it.
"""
struct OpaqueClosureCreateInfo <: CallInfo
    unspec::CallMeta
    function OpaqueClosureCreateInfo(unspec::CallMeta)
        @assert isa(unspec.info, Union{OpaqueClosureCallInfo, NoCallInfo})
        return new(unspec)
    end
end
# merely creating the object implies edges for OC, unlike normal objects,
# since calling them doesn't normally have edges in contrast
add_edges_impl(edges::Vector{Any}, info::OpaqueClosureCreateInfo) = add_edges!(edges, info.unspec.info)

# Stmt infos that are used by external consumers, but not by optimization.
# These are not produced by default and must be explicitly opted into by
# the AbstractInterpreter.

"""
    info::ReturnTypeCallInfo <: CallInfo

Represents a resolved call of `Core.Compiler.return_type`.
`info.call` wraps the info corresponding to the call that `Core.Compiler.return_type` call
was supposed to analyze.
"""
struct ReturnTypeCallInfo <: CallInfo
    info::CallInfo
end
add_edges_impl(edges::Vector{Any}, info::ReturnTypeCallInfo) = add_edges!(edges, info.info)

"""
    info::FinalizerInfo <: CallInfo

Represents the information of a potential (later) call to the finalizer on the given
object type.
"""
struct FinalizerInfo <: CallInfo
    info::CallInfo   # the callinfo for the finalizer call
    effects::Effects # the effects for the finalizer call
end
# merely allocating a finalizer does not imply edges (unless it gets inlined later)
add_edges_impl(::Vector{Any}, ::FinalizerInfo) = nothing

"""
    info::ModifyOpInfo <: CallInfo

Represents a resolved call of one of:
 - `modifyfield!(obj, name, op, x, [order])`
 - `modifyglobal!(mod, var, op, x, order)`
 - `memoryrefmodify!(memref, op, x, order, boundscheck)`
 - `Intrinsics.atomic_pointermodify(ptr, op, x, order)`

`info.info` wraps the call information of `op(getval(), x)`.
"""
struct ModifyOpInfo <: CallInfo
    info::CallInfo # the callinfo for the `op(getval(), x)` call
end
add_edges_impl(edges::Vector{Any}, info::ModifyOpInfo) = add_edges!(edges, info.info)

struct VirtualMethodMatchInfo <: CallInfo
    info::Union{MethodMatchInfo,UnionSplitInfo,InvokeCallInfo}
end
add_edges_impl(edges::Vector{Any}, info::VirtualMethodMatchInfo) =
    _add_edges_impl(edges, info.info, #=mi_edge=#true)

"""
    info::GlobalAccessInfo <: CallInfo

Represents access to a global through runtime reflection, rather than as a manifest
`GlobalRef` in the source code. Used for builtins (getglobal/setglobal/etc.) that
perform such accesses.
"""
struct GlobalAccessInfo <: CallInfo
    b::Core.Binding
end
function add_edges_impl(edges::Vector{Any}, info::GlobalAccessInfo)
    push!(edges, info.b)
end

@specialize
