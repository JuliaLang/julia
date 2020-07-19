"""
    struct MethodMatchInfo

Captures the result of a `method_matches` lookup for the given call. This
info may then be used by the optimizer to inline the matches, without having
to re-consult the method table. This info is illegal on any statement that is
not a call to a generic function.
"""
struct MethodMatchInfo
    applicable::Any
    ambig::Bool
end

"""
    struct MethodMatchInfo

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

A simple struct that captures both the return type any any additional `info`
for a given generic call.
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

This info applies to any call of _apply_iterate(...) and captures both the
info of the actual call being applied and the info for any implicit call
to the `iterate` function. Note that it is possible for the call itself
to be yet another `_apply_iterate`, in which case the `.call` field will
be another ApplyCallInfo. This info is illegal on any statement that is
not an _apply_iterate call.
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
This info is illegal on any statement that is not an _apply_iterate call.
"""
struct UnionSplitApplyCallInfo
    infos::Vector{ApplyCallInfo}
end


