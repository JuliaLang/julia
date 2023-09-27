# This file is a part of Julia. License is MIT: https://julialang.org/license

@nospecialize

"""
    call::CallMeta

A simple struct that captures both the return type (`call.rt`)
and any additional information (`call.info`) for a given generic call.
"""
struct CallMeta
    rt::Any
    effects::Effects
    info::CallInfo
end

struct NoCallInfo <: CallInfo end

"""
    info::MethodMatchInfo <: CallInfo

Captures the result of a `:jl_matching_methods` lookup for the given call (`info.results`).
This info may then be used by the optimizer to inline the matches, without having
to re-consult the method table. This info is illegal on any statement that is
not a call to a generic function.
"""
struct MethodMatchInfo <: CallInfo
    results::MethodLookupResult
end
nsplit_impl(info::MethodMatchInfo) = 1
getsplit_impl(info::MethodMatchInfo, idx::Int) = (@assert idx == 1; info.results)
getresult_impl(::MethodMatchInfo, ::Int) = nothing

"""
    info::UnionSplitInfo <: CallInfo

If inference decides to partition the method search space by splitting unions,
it will issue a method lookup query for each such partition. This info indicates
that such partitioning happened and wraps the corresponding `MethodMatchInfo` for
each partition (`info.matches::Vector{MethodMatchInfo}`).
This info is illegal on any statement that is not a call to a generic function.
"""
struct UnionSplitInfo <: CallInfo
    matches::Vector{MethodMatchInfo}
end

nmatches(info::MethodMatchInfo) = length(info.results)
function nmatches(info::UnionSplitInfo)
    n = 0
    for mminfo in info.matches
        n += nmatches(mminfo)
    end
    return n
end
nsplit_impl(info::UnionSplitInfo) = length(info.matches)
getsplit_impl(info::UnionSplitInfo, idx::Int) = getsplit_impl(info.matches[idx], 1)
getresult_impl(::UnionSplitInfo, ::Int) = nothing

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
nsplit_impl(info::ConstCallInfo) = nsplit(info.call)
getsplit_impl(info::ConstCallInfo, idx::Int) = getsplit(info.call, idx)
getresult_impl(info::ConstCallInfo, idx::Int) = info.results[idx]

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
    call::Any
    # AbstractIterationInfo for each argument, if applicable
    arginfo::Vector{MaybeAbstractIterationInfo}
end

"""
    info::UnionSplitApplyCallInfo <: CallInfo

Like `UnionSplitInfo`, but for `ApplyCallInfo` rather than `MethodMatchInfo`.
This info is illegal on any statement that is not an `_apply_iterate` call.
"""
struct UnionSplitApplyCallInfo <: CallInfo
    infos::Vector{ApplyCallInfo}
end

"""
    info::InvokeCallInfo

Represents a resolved call to `Core.invoke`, carrying the `info.match::MethodMatch` of
the method that has been processed.
Optionally keeps `info.result::InferenceResult` that keeps constant information.
"""
struct InvokeCallInfo <: CallInfo
    match::MethodMatch
    result::Union{Nothing,ConstResult}
end

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

"""
    info::FinalizerInfo <: CallInfo

Represents the information of a potential (later) call to the finalizer on the given
object type.
"""
struct FinalizerInfo <: CallInfo
    info::CallInfo   # the callinfo for the finalizer call
    effects::Effects # the effects for the finalizer call
end

"""
    info::ModifyFieldInfo <: CallInfo

Represents a resolved all of `modifyfield!(obj, name, op, x, [order])`.
`info.info` wraps the call information of `op(getfield(obj, name), x)`.
"""
struct ModifyFieldInfo <: CallInfo
    info::CallInfo # the callinfo for the `op(getfield(obj, name), x)` call
end

@specialize
