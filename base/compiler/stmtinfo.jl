# This file is a part of Julia. License is MIT: https://julialang.org/license

@nospecialize

"""
    call::CallMeta

A simple struct that captures both the return type (`call.rt`)
and any additional information (`call.info`) for a given generic call.
"""
struct CallMeta
    rt::Any
    info::Any
end

"""
    info::MethodMatchInfo

Captures the result of a `:jl_matching_methods` lookup for the given call (`info.results`).
This info may then be used by the optimizer to inline the matches, without having
to re-consult the method table. This info is illegal on any statement that is
not a call to a generic function.
"""
struct MethodMatchInfo
    results::MethodLookupResult
end

"""
    info::UnionSplitInfo

If inference decides to partition the method search space by splitting unions,
it will issue a method lookup query for each such partition. This info indicates
that such partitioning happened and wraps the corresponding `MethodMatchInfo` for
each partition (`info.matches::Vector{MethodMatchInfo}`).
This info is illegal on any statement that is not a call to a generic function.
"""
struct UnionSplitInfo
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

struct ConstPropResult
    result::InferenceResult
end

struct ConcreteResult
    mi::MethodInstance
    effects::Effects
    result
    ConcreteResult(mi::MethodInstance, effects::Effects) = new(mi, effects)
    ConcreteResult(mi::MethodInstance, effects::Effects, @nospecialize val) = new(mi, effects, val)
end

const ConstResult = Union{ConstPropResult,ConcreteResult}

"""
    info::ConstCallInfo

The precision of this call was improved using constant information.
In addition to the original call information `info.call`, this info also keeps the results
of constant inference `info.results::Vector{Union{Nothing,ConstResult}}`.
"""
struct ConstCallInfo
    call::Union{MethodMatchInfo,UnionSplitInfo}
    results::Vector{Union{Nothing,ConstResult}}
end

"""
    info::MethodResultPure

This struct represents a method result constant was proven to be
effect-free, including being no-throw (typically because the value was computed
by calling an `@pure` function).
"""
struct MethodResultPure
    info::Union{MethodMatchInfo,UnionSplitInfo,Bool}
end
let instance = MethodResultPure(false)
    global MethodResultPure
    MethodResultPure() = instance
end

"""
    info::AbstractIterationInfo

Captures all the information for abstract iteration analysis of a single value.
Each (abstract) call to `iterate`, corresponds to one entry in `info.each::Vector{CallMeta}`.
"""
struct AbstractIterationInfo
    each::Vector{CallMeta}
end

const MaybeAbstractIterationInfo = Union{Nothing, AbstractIterationInfo}

"""
    info::ApplyCallInfo

This info applies to any call of `_apply_iterate(...)` and captures both the
info of the actual call being applied and the info for any implicit call
to the `iterate` function. Note that it is possible for the call itself
to be yet another `_apply_iterate`, in which case the `info.call` field will
be another `ApplyCallInfo`. This info is illegal on any statement that is
not an `_apply_iterate` call.
"""
struct ApplyCallInfo
    # The info for the call itself
    call::Any
    # AbstractIterationInfo for each argument, if applicable
    arginfo::Vector{MaybeAbstractIterationInfo}
end

"""
    info::UnionSplitApplyCallInfo

Like `UnionSplitInfo`, but for `ApplyCallInfo` rather than `MethodMatchInfo`.
This info is illegal on any statement that is not an `_apply_iterate` call.
"""
struct UnionSplitApplyCallInfo
    infos::Vector{ApplyCallInfo}
end

"""
    info::InvokeCallInfo

Represents a resolved call to `Core.invoke`, carrying the `info.match::MethodMatch` of
the method that has been processed.
Optionally keeps `info.result::InferenceResult` that keeps constant information.
"""
struct InvokeCallInfo
    match::MethodMatch
    result::Union{Nothing,ConstResult}
end

"""
    info::OpaqueClosureCallInfo

Represents a resolved call of opaque closure, carrying the `info.match::MethodMatch` of
the method that has been processed.
Optionally keeps `info.result::InferenceResult` that keeps constant information.
"""
struct OpaqueClosureCallInfo
    match::MethodMatch
    result::Union{Nothing,ConstResult}
end

"""
    info::OpaqueClosureCreateInfo

This info may be constructed upon opaque closure construction, with `info.unspec::CallMeta`
carrying out inference result of an unreal, partially specialized call (i.e. specialized on
the closure environment, but not on the argument types of the opaque closure) in order to
allow the optimizer to rewrite the return type parameter of the `OpaqueClosure` based on it.
"""
struct OpaqueClosureCreateInfo
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
    info::ReturnTypeCallInfo

Represents a resolved call of `Core.Compiler.return_type`.
`info.call` wraps the info corresponding to the call that `Core.Compiler.return_type` call
was supposed to analyze.
"""
struct ReturnTypeCallInfo
    info::Any
end

@specialize
