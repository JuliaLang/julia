# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    struct MethodMatchInfo

Captures the result of a `method_matches` lookup for the given call. This
info may then be used by the optimizer to inline the matches, without having
to re-consult the method table. This info is illegal on any statement that is
not a call to a generic function.
"""
struct MethodMatchInfo
    results::MethodLookupResult
end

"""
    struct MethodResultPure

This struct represents a method result constant was proven to be
effect-free, including being no-throw (typically because the value was computed
by calling an `@pure` function).
"""
struct MethodResultPure
    info::Any
end
let instance = MethodResultPure(false)
    global MethodResultPure
    MethodResultPure() = instance
end

"""
    struct UnionSplitInfo

If inference decides to partition the method search space by splitting unions,
it will issue a method lookup query for each such partition. This info indicates
that such partitioning happened and wraps the corresponding MethodMatchInfo for
each partition. This info is illegal on any statement that is not a call to a
generic function.
"""
struct UnionSplitInfo
    matches::Vector{MethodMatchInfo}
end

"""
    struct CallMeta

A simple struct that captures both the return type (`rt`) and any additional information
(`info`) for a given generic call.
"""
struct CallMeta
    rt::Any
    info::Any
end

"""
    struct AbstractIterationInfo

Captures all the information for abstract iteration analysis of a single value.
Each (abstract) call to `iterate`, corresponds to one entry in `each`.
"""
struct AbstractIterationInfo
    each::Vector{CallMeta}
end

"""
    struct ApplyCallInfo

This info applies to any call of `_apply_iterate(...)` and captures both the
info of the actual call being applied and the info for any implicit call
to the `iterate` function. Note that it is possible for the call itself
to be yet another `_apply_iterate`, in which case the `.call` field will
be another `ApplyCallInfo`. This info is illegal on any statement that is
not an `_apply_iterate` call.
"""
struct ApplyCallInfo
    # The info for the call itself
    call::Any
    # AbstractIterationInfo for each argument, if applicable
    arginfo::Vector{Union{Nothing, AbstractIterationInfo}}
end

"""
    struct UnionSplitApplyCallInfo

Like `UnionSplitInfo`, but for `ApplyCallInfo` rather than MethodMatchInfo.
This info is illegal on any statement that is not an `_apply_iterate` call.
"""
struct UnionSplitApplyCallInfo
    infos::Vector{ApplyCallInfo}
end

"""
    call::ConstCallInfo

The precision of this call was improved using constant information.
In addition to the original call information `call.call`, `call` also keeps
the inference results with constant information `call.results::Vector{Union{Nothing,InferenceResult}}`.
"""
struct ConstCallInfo
    call::Union{MethodMatchInfo,UnionSplitInfo}
    results::Vector{Union{Nothing,InferenceResult}}
end

"""
    call::InvokeCallInfo

Represents a resolved call to `invoke`, carrying the `call.match::MethodMatch` of the
method that has been processed.
Optionally keeps `result::InferenceResult` that carries constant information.
"""
struct InvokeCallInfo
    match::MethodMatch
    result::Union{Nothing,InferenceResult}
end

"""
    call::OpaqueClosureCallInfo

Represents a resolved call of opaque closure, carrying the `call.match::MethodMatch` of the
method that has been processed.
Optionally keeps `result::InferenceResult` that carries constant information.
"""
struct OpaqueClosureCallInfo
    match::MethodMatch
    result::Union{Nothing,InferenceResult}
end

struct OpaqueClosureCreateInfo
    unspec::CallMeta
end

# Stmt infos that are used by external consumers, but not by optimization.
# These are not produced by default and must be explicitly opted into by
# the AbstractInterpreter.

struct ReturnTypeCallInfo
    # The info corresponding to the call that return_type was supposed to
    # analyze.
    info::Any
end
