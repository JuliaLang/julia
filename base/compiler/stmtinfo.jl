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
end
function add_edges_impl(edges::Vector{Any}, info::MethodMatchInfo)
    matches = info.results.matches
    if !fully_covering(info)
        # add legacy-style missing backedge info also
        exists = false
        for i in 1:length(edges)
            if edges[i] === info.mt && edges[i + 1] == info.atype
                exists = true
                break
            end
        end
        if !exists
            push!(edges, info.mt)
            push!(edges, info.atype)
        end
    end
    if length(matches) == 1
        # try the optimized format for the representation, if possible and applicable
        # if this doesn't succeed, the backedge will be less precise,
        # but the forward edge will maintain the precision
        m = matches[1]::Core.MethodMatch
        mi = specialize_method(m)
        if mi.specTypes === m.spec_types
            add_one_edge!(edges, mi)
            return nothing
        end
    end
    # add check for whether this lookup already existed in the edges list
    for i in 1:length(edges)
        if edges[i] === length(matches) && edges[i + 1] == info.atype
            return nothing
        end
    end
    push!(edges, length(matches))
    push!(edges, info.atype)
    for m in matches
        mi = specialize_method(m::Core.MethodMatch)
        push!(edges, mi)
    end
    nothing
end
function add_one_edge!(edges::Vector{Any}, mi::MethodInstance)
    for i in 1:length(edges)
        if edges[i] === mi && !(i > 1 && edges[i - 1] isa Type)
            return
        end
    end
    push!(edges, mi)
    nothing
end
nsplit_impl(info::MethodMatchInfo) = 1
getsplit_impl(info::MethodMatchInfo, idx::Int) = (@assert idx == 1; info.results)
getresult_impl(::MethodMatchInfo, ::Int) = nothing
function add_uncovered_edges_impl(edges::Vector{Any}, info::MethodMatchInfo, @nospecialize(atype))
    fully_covering(info) || push!(edges, info.mt, atype)
    nothing
end

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
    for split in info.split; add_edges!(edges, split); end
nsplit_impl(info::UnionSplitInfo) = length(info.split)
getsplit_impl(info::UnionSplitInfo, idx::Int) = getsplit(info.split[idx], 1)
getresult_impl(::UnionSplitInfo, ::Int) = nothing
function add_uncovered_edges_impl(edges::Vector{Any}, info::UnionSplitInfo, @nospecialize(atype))
    all(fully_covering, info.split) && return nothing
    # add mt backedges with removing duplications
    for mt in uncovered_method_tables(info)
        push!(edges, mt, atype)
    end
end
function uncovered_method_tables(info::UnionSplitInfo)
    mts = MethodTable[]
    for mminfo in info.split
        fully_covering(mminfo) && continue
        any(mt′::MethodTable->mt′===mminfo.mt, mts) && continue
        push!(mts, mminfo.mt)
    end
    return mts
end

abstract type ConstResult end

struct ConstPropResult <: ConstResult
    result::InferenceResult
end

struct ConcreteResult <: ConstResult
    mi::MethodInstance
    effects::Effects
    result
    ConcreteResult(mi::MethodInstance, effects::Effects) = new(mi, effects)
    ConcreteResult(mi::MethodInstance, effects::Effects, @nospecialize val) = new(mi, effects, val)
end

struct SemiConcreteResult <: ConstResult
    mi::MethodInstance
    ir::IRCode
    effects::Effects
    spec_info::SpecInfo
end

# XXX Technically this does not represent a result of constant inference, but rather that of
#     regular edge inference. It might be more appropriate to rename `ConstResult` and
#     `ConstCallInfo` to better reflect the fact that they represent either of local or
#     volatile inference result.
struct VolatileInferenceResult <: ConstResult
    inf_result::InferenceResult
end

"""
    info::ConstCallInfo <: CallInfo

The precision of this call was improved using constant information.
In addition to the original call information `info.call`, this info also keeps the results
of constant inference `info.results::Vector{Union{Nothing,ConstResult}}`.
"""
struct ConstCallInfo <: CallInfo
    call::Union{MethodMatchInfo,UnionSplitInfo}
    results::Vector{Union{Nothing,ConstResult}}
end
add_edges_impl(edges::Vector{Any}, info::ConstCallInfo) = add_edges!(edges, info.call)
nsplit_impl(info::ConstCallInfo) = nsplit(info.call)
getsplit_impl(info::ConstCallInfo, idx::Int) = getsplit(info.call, idx)
getresult_impl(info::ConstCallInfo, idx::Int) = info.results[idx]
add_uncovered_edges_impl(edges::Vector{Any}, info::ConstCallInfo, @nospecialize(atype)) = add_uncovered_edges!(edges, info.call, atype)

"""
    info::MethodResultPure <: CallInfo

This struct represents a method result constant was proven to be
effect-free, including being no-throw (typically because the value was computed
by calling an `@pure` function).
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
# N.B. `ApplyCallInfo` doesn't need to implement the interfaces for the inlining,
# since `Core._apply_iterate` is handled by the special case

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
# N.B. `UnionSplitApplyCallInfo` doesn't need to implement the interfaces for the inlining,
# since `Core._apply_iterate` is handled by the special case

"""
    info::InvokeCallInfo

Represents a resolved call to `Core.invoke`, carrying the `info.match::MethodMatch` of
the method that has been processed.
Optionally keeps `info.result::InferenceResult` that keeps constant information.
"""
struct InvokeCallInfo <: CallInfo
    match::MethodMatch
    result::Union{Nothing,ConstResult}
    atype # ::Type
end
add_edges_impl(edges::Vector{Any}, info::InvokeCallInfo) =
    add_invoke_edge!(edges, info.atype, specialize_method(info.match))
function add_invoke_edge!(edges::Vector{Any}, @nospecialize(atype), mi::MethodInstance)
    for i in 2:length(edges)
        if edges[i] === mi && edges[i - 1] isa Type && edges[i - 1] == atype
            return
        end
    end
    push!(edges, atype)
    push!(edges, mi)
    nothing
end
# N.B. `InvokeCallInfo` doesn't need to implement the interfaces for the inlining,
# since `Core.invoke` is handled by the special case

"""
    info::OpaqueClosureCallInfo

Represents a resolved call of opaque closure, carrying the `info.match::MethodMatch` of
the method that has been processed.
Optionally keeps `info.result::InferenceResult` that keeps constant information.
"""
struct OpaqueClosureCallInfo <: CallInfo
    match::MethodMatch
    result::Union{Nothing,ConstResult}
end
add_edges_impl(edges::Vector{Any}, info::OpaqueClosureCallInfo) =
    add_one_edge!(edges, specialize_method(info.match))
# N.B. `OpaqueClosureCallInfo` doesn't need to implement the interfaces for the inlining,
# since `Core.invoke` is handled by the special case

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
        @assert isa(unspec.info, OpaqueClosureCallInfo)
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

@specialize
