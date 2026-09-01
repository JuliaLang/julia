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

# InferredCallResult is defined in types.jl with LocalInferenceResult

struct ConcreteResult <: InferredCallResult
    edge::Union{Nothing,CodeInstance}
    effects::Effects
    proof::Union{Nothing,InferenceProof}
    result
    function ConcreteResult(edge::Union{Nothing,CodeInstance}, effects::Effects;
                            proof::Union{Nothing,InferenceProof}=nothing)
        @assert edge !== nothing || proof !== nothing
        return new(edge, effects, proof)
    end
    function ConcreteResult(edge::Union{Nothing,CodeInstance}, effects::Effects,
                            @nospecialize(val);
                            proof::Union{Nothing,InferenceProof}=nothing)
        @assert edge !== nothing || proof !== nothing
        return new(edge, effects, proof, val)
    end
end

struct SemiConcreteResult <: InferredCallResult
    edge::CodeInstance
    ir::IRCode
    effects::Effects
    spec_info::SpecInfo
    proof::Union{Nothing,InferenceProof}
    SemiConcreteResult(edge::CodeInstance, ir::IRCode, effects::Effects, spec_info::SpecInfo;
                       proof::Union{Nothing,InferenceProof}=nothing) =
        new(edge, ir, effects, spec_info, proof)
end

inference_proof(result::ConcreteResult) =
    result.proof === nothing ? result.edge::CodeInstance : result.proof
inference_proof(result::SemiConcreteResult) = something(result.proof, result.edge)

function record_invoke_edge!(invokes::IdDict{Any,Vector{Any}},
                             @nospecialize(signature), @nospecialize(target))
    signatures = get!(Vector{Any}, invokes, target)
    for previous in signatures
        previous == signature && return false
    end
    push!(signatures, signature)
    return true
end

function _materialize_inference_edges!(edges::Vector{Any}, source,
                                       seen_proofs::IdSet{LocalInferenceProof},
                                       standalone::IdSet{Any},
                                       invokes::IdDict{Any,Vector{Any}})
    i = 1
    while i <= length(source)
        edge = source[i]
        if edge isa LocalInferenceProof
            i += 1
            edge in seen_proofs && continue
            push!(seen_proofs, edge)
            _materialize_inference_edges!(
                edges, edge.edges, seen_proofs, standalone, invokes)
        elseif edge isa Int
            # Encoded lookup groups are positional: `nmatches, atype, matches...`.
            # Preserve each complete group, while recording its targets so an
            # identical standalone proof edge later in the stream can be omitted.
            n = abs(edge)
            last = i + 1 + n
            @assert last <= length(source)
            for j = i:last
                push!(edges, source[j])
            end
            for j = i + 2:last
                target = source[j]
                if target isa Union{Method,MethodInstance,CodeInstance,Core.Binding}
                    push!(standalone, target)
                end
            end
            i = last + 1
        elseif edge isa Union{Method,MethodInstance,CodeInstance,Core.Binding}
            i += 1
            edge in standalone && continue
            push!(standalone, edge)
            push!(edges, edge)
        else
            # Everything else is an invoke signature paired with its target. Keep
            # distinct signatures, and deduplicate only the identical pair.
            @assert i < length(source)
            target = source[i + 1]
            i += 2
            record_invoke_edge!(invokes, edge, target) || continue
            push!(edges, edge, target)
        end
    end
    return nothing
end

function materialize_inference_edges(source)
    has_local_proof = false
    for edge in source
        if edge isa LocalInferenceProof
            has_local_proof = true
            break
        end
    end
    if !has_local_proof
        return source isa SimpleVector ? source : Core.svec(source...)
    end
    edges = Any[]
    sizehint!(edges, length(source))
    _materialize_inference_edges!(edges, source,
        IdSet{LocalInferenceProof}(), IdSet{Any}(), IdDict{Any,Vector{Any}}())
    return Core.svec(edges...)
end

function add_inference_proof!(edges::Vector{Any}, proof::CodeInstance,
                              @nospecialize(paired_edge=nothing))
    # This CI certifies the inferred facts of its defining method; it does not
    # necessarily represent ordinary dispatch on its MethodInstance signature
    # (for example, it may have been reached through `invoke`). Encode the proof
    # as an identity edge to that method, just as we do for an inlined CI.
    proof === paired_edge || add_inlining_edge!(edges, proof)
    return nothing
end

function add_inference_proof!(edges::Vector{Any}, proof::LocalInferenceProof,
                              @nospecialize(paired_edge=nothing))
    any(edge -> edge === proof, edges) || push!(edges, proof)
    return nothing
end

function add_result_proof!(edges::Vector{Any}, result::Union{Nothing,InferredCallResult},
                           @nospecialize(paired_edge=nothing))
    result === nothing && return nothing
    add_inference_proof!(edges, inference_proof(result), paired_edge)
    return nothing
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
    needs_mi_edges::BitVector
    call_results::Vector{Union{Nothing,InferredCallResult}}
    function MethodMatchInfo(
        results::MethodLookupResult, mt::MethodTable, @nospecialize(atype), fullmatch::Bool)
        edges = fill!(Vector{Union{Nothing,CodeInstance}}(undef, length(results)), nothing)
        needs_mi_edges = falses(length(results))
        call_results = fill!(Vector{Union{Nothing,InferredCallResult}}(undef, length(results)), nothing)
        return new(results, mt, atype, fullmatch, edges, needs_mi_edges, call_results)
    end
end
add_edges_impl(edges::Vector{Any}, info::MethodMatchInfo) = _add_edges_impl(edges, info)

function method_match_edge(info::MethodMatchInfo, i::Int, mi_edge::Bool)
    edge = info.edges[i]
    edge !== nothing && return edge
    match = info.results[i]
    # A proof-carrying result may carry facts about this specialization without an
    # executable CI of its own. The same is true when a scheduled call consumed
    # provisional SCC facts before a CI or local result was available. A completed
    # tombstone similarly carries its proof separately from the dispatch target. Keep
    # the MethodInstance as an invalidation target in these cases; a bare Method in an
    # encoded lookup is intentionally ignored by the backedge iterator and therefore
    # cannot certify those facts.
    return (mi_edge || info.needs_mi_edges[i] || info.call_results[i] !== nothing) ?
        specialize_method(match) : match.method
end

function has_encoded_lookup(edges::Vector{Any}, info::MethodMatchInfo,
                            encoded_nmatches::Int, mi_edge::Bool, marked::Bool)
    i = 1
    while i <= length(edges)
        entry = edges[i]
        if entry isa Int
            n = abs(entry)
            next_i = i + 2 + n
            if next_i - 1 <= length(edges) && entry === encoded_nmatches
                atypeᵢ = edges[i + 1]
                markedᵢ = atypeᵢ isa PossiblyAmbiguous
                markedᵢ && (atypeᵢ = (atypeᵢ::PossiblyAmbiguous).sig)
                matches = atypeᵢ == info.atype
                for j = 1:n
                    matches || break
                    if edges[i + 1 + j] !== method_match_edge(info, j, mi_edge)
                        matches = false
                    end
                end
                if matches
                    # Every call site with this signature in the caller shares this group,
                    # and a marked group relaxes invalidation for all of them. A call site
                    # inferred without the ambiguity pessimization cannot share that
                    # relaxation, so the group loses its marker; the plain form is valid for
                    # both kinds of site.
                    markedᵢ && !marked && (edges[i + 1] = info.atype)
                    return true
                end
            end
            i = next_i
        else
            i += 1
        end
    end
    return false
end

function add_method_match_proofs!(edges::Vector{Any}, info::MethodMatchInfo,
                                  mi_edge::Bool)
    for i = 1:length(info.call_results)
        add_result_proof!(edges, info.call_results[i], method_match_edge(info, i, mi_edge))
    end
    return nothing
end

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
    # A call inferred for a possible ambiguity records its group with the signature
    # wrapped in `Core.PossiblyAmbiguous` (see boot.jl); the single-edge format has no
    # signature slot, so such calls always use a group.
    marked = any_ambig(info)
    if !marked && nmatches == length(info.edges) == 1 && fully_covering(info)
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
            add_result_proof!(edges, info.call_results[1], edge)
            return nothing
        end
    end
    # add check for whether this lookup already existed in the edges list
    # encode nmatches as negative if fully_covers is false
    encoded_nmatches = fully_covering(info) ? nmatches : -nmatches
    if !has_encoded_lookup(edges, info, encoded_nmatches, mi_edge, marked)
        push!(edges, encoded_nmatches, marked ? PossiblyAmbiguous(info.atype) : info.atype)
        for i = 1:nmatches
            edge = method_match_edge(info, i, mi_edge)
            if edge isa CodeInstance
                @assert edge.def.def === info.results[i].method
            end
            push!(edges, edge)
        end
    end
    add_method_match_proofs!(edges, info, mi_edge)
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
            elseif edgeᵢ_orig === edge
                # Only the identical CodeInstance certifies the same inference proof.
                return
            end
            # Different CodeInstances for the same MethodInstance may certify
            # different inference facts. Keep both, irrespective of whether their
            # current forward-edge streams happen to compare equal.
        end
        i += 1
    end
    push!(edges, edge)
    nothing
end
nsplit_impl(::MethodMatchInfo) = 1
getsplit_impl(info::MethodMatchInfo, idx::Int) = (@assert idx == 1; info.results)
getresult_impl(info::MethodMatchInfo, idx::Int) = info.call_results[idx]
getedge_impl(info::MethodMatchInfo, idx::Int) = info.edges[idx]

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
function getedge_impl(info::UnionSplitInfo, idx::Int)
    for split in info.split
        n = length(split.edges)
        if idx ≤ n
            return split.edges[idx]
        else
            idx -= n
        end
    end
    return nothing
end

"""
    info::MethodResultPure <: CallInfo

This struct represents a method result that was proven to be a pure (effect-free) constant.
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
Optionally keeps a proof-carrying `info.result` with constant information.
"""
struct InvokeCallInfo <: CallInfo
    edge::Union{Nothing,CodeInstance}
    match::MethodMatch
    result::Union{Nothing,InferredCallResult}
    atype # ::Type
    needs_mi_edge::Bool # targetless body-derived facts require an invalidation target
end
InvokeCallInfo(edge::Union{Nothing,CodeInstance}, match::MethodMatch,
               result::Union{Nothing,InferredCallResult}, @nospecialize(atype)) =
    InvokeCallInfo(edge, match, result, atype, false)
add_edges_impl(edges::Vector{Any}, info::InvokeCallInfo) =
    _add_edges_impl(edges, info)
function _add_edges_impl(edges::Vector{Any}, info::InvokeCallInfo, mi_edge::Bool=false)
    edge = info.edge
    if edge === nothing
        edge = (mi_edge || info.needs_mi_edge || info.result !== nothing) ?
            specialize_method(info.match) : info.match.method
    end
    add_invoke_edge!(edges, info.atype, edge)
    add_result_proof!(edges, info.result, edge)
    nothing
end
function add_invoke_edge!(edges::Vector{Any}, @nospecialize(atype), edge::Union{MethodInstance,Method})
    i = 1
    while i <= length(edges)
        edgeᵢ = edges[i]
        if edgeᵢ isa Int
            i += 2 + abs(edgeᵢ)
            continue
        end
        edgeᵢ isa CodeInstance && (edgeᵢ = edgeᵢ.def)
        if !(edgeᵢ isa MethodInstance || edgeᵢ isa Method)
            i += 1
            continue
        end
        if edgeᵢ === edge
            i == 1 && (i += 1; continue)
            edge_minus_1 = edges[i - 1]
            if edge_minus_1 isa Type && edge_minus_1 == atype
                return # found existing covered edge
            end
        end
        i += 1
    end
    push!(edges, atype)
    push!(edges, edge)
    nothing
end
function add_invoke_edge!(edges::Vector{Any}, @nospecialize(atype), edge::CodeInstance)
    i = 1
    while i <= length(edges)
        edgeᵢ_orig = edgeᵢ = edges[i]
        if edgeᵢ isa Int
            i += 2 + abs(edgeᵢ)
            continue
        end
        edgeᵢ isa CodeInstance && (edgeᵢ = edgeᵢ.def)
        if ((edgeᵢ isa MethodInstance && edgeᵢ === edge.def) ||
            (edgeᵢ isa Method && edgeᵢ === edge.def.def))
            i == 1 && (i += 1; continue)
            edge_minus_1 = edges[i - 1]
            if edge_minus_1 isa Type && edge_minus_1 == atype
                if edgeᵢ_orig isa MethodInstance || edgeᵢ_orig isa Method
                    # found edge we can upgrade
                    edges[i] = edge
                    return
                elseif edgeᵢ_orig === edge
                    return
                end
                # A distinct CodeInstance for the same MethodInstance may carry a
                # distinct proof, so retain it as a separate invoke edge.
            end
        end
        i += 1
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
        if edgeᵢ isa Int
            i += 2 + abs(edgeᵢ)
            continue
        end
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
        if edgeᵢ isa Int
            i += 2 + abs(edgeᵢ)
            continue
        end
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
        if edgeᵢ === edge
            # found the identical existing edge
            return
        end
        # A distinct CodeInstance for the same MethodInstance may carry a
        # distinct proof, so do not deduplicate it by `def` alone.
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
getedge_impl(info::InvokeCallInfo, idx::Int) = (@assert idx == 1; info.edge)

"""
    info::OpaqueClosureCallInfo

Represents a resolved call of opaque closure, carrying the `info.match::MethodMatch` of
the method that has been processed.
Optionally keeps a proof-carrying `info.result` with constant information.
"""
struct OpaqueClosureCallInfo <: CallInfo
    edge::Union{Nothing,CodeInstance}
    match::MethodMatch
    result::Union{Nothing,InferredCallResult}
    needs_mi_edge::Bool # targetless body-derived facts require an invalidation target
end
OpaqueClosureCallInfo(edge::Union{Nothing,CodeInstance}, match::MethodMatch,
                      result::Union{Nothing,InferredCallResult}) =
    OpaqueClosureCallInfo(edge, match, result, false)
function add_edges_impl(edges::Vector{Any}, info::OpaqueClosureCallInfo)
    edge = info.edge
    if edge === nothing && (info.needs_mi_edge || info.result !== nothing)
        edge = specialize_method(info.match)
    end
    if edge !== nothing
        add_one_edge!(edges, edge)
    end
    add_result_proof!(edges, info.result, edge)
    nothing
end
getedge_impl(info::OpaqueClosureCallInfo, idx::Int) = (@assert idx == 1; info.edge)

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

"""
    info::TaskCallInfo <: CallInfo

Represents a resolved call of `Core._task(f, size)`. `info.info` wraps the call
information of the deferred `f()` call that runs when the created task is scheduled.
"""
struct TaskCallInfo <: CallInfo
    info::CallInfo # the callinfo for the deferred `f()` call
end
add_edges_impl(edges::Vector{Any}, info::TaskCallInfo) = add_edges!(edges, info.info)

struct VirtualMethodMatchInfo <: CallInfo
    info::Union{MethodMatchInfo,UnionSplitInfo,InvokeCallInfo}
end
add_edges_impl(edges::Vector{Any}, info::VirtualMethodMatchInfo) =
    _add_edges_impl(edges, info.info, #=mi_edge=#true)
nsplit_impl(info::VirtualMethodMatchInfo) = nsplit(info.info)
getsplit_impl(info::VirtualMethodMatchInfo, idx::Int) = getsplit(info.info, idx)
getresult_impl(info::VirtualMethodMatchInfo, idx::Int) = getresult(info.info, idx)
getedge_impl(info::VirtualMethodMatchInfo, idx::Int) = getedge(info.info, idx)

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
