# This file is a part of Julia. License is MIT: https://julialang.org/license

_findall_matches(interp::AbstractInterpreter, @nospecialize(tt)) = findall(tt, method_table(interp))
_default_interp(world::UInt) = NativeInterpreter(world)

_may_throw_methoderror(matches::MethodLookupResult) =
    matches.ambig || !any(match::Core.MethodMatch->match.fully_covers, matches.matches)

function _infer_exception_type(interp::AbstractInterpreter, @nospecialize(tt), optimize::Bool)
    matches = _findall_matches(interp, tt)
    matches === nothing && return nothing
    exct = Union{}
    if _may_throw_methoderror(matches)
        # account for the fact that we may encounter a MethodError with a non-covered or ambiguous signature.
        exct = MethodError
    end
    for match in matches.matches
        match = match::Core.MethodMatch
        frame = typeinf_frame(interp, match, #=run_optimizer=#optimize)
        frame === nothing && return Any
        exct = tmerge(exct, widenconst(frame.result.exc_result))
    end
    return exct
end

function _infer_effects(interp::AbstractInterpreter, @nospecialize(tt), optimize::Bool)
    matches = _findall_matches(interp, tt)
    matches === nothing && return nothing
    effects = EFFECTS_TOTAL
    if _may_throw_methoderror(matches)
        # account for the fact that we may encounter a MethodError with a non-covered or ambiguous signature.
        effects = Effects(effects; nothrow=false)
    end
    for match in matches.matches
        match = match::Core.MethodMatch
        frame = typeinf_frame(interp, match, #=run_optimizer=#optimize)
        frame === nothing && return Effects()
        effects = merge_effects(effects, frame.result.ipo_effects)
    end
    return effects
end

function statement_costs!(interp::AbstractInterpreter, cost::Vector{Int}, body::Vector{Any}, src::Union{CodeInfo, IRCode}, match::Core.MethodMatch)
    params = OptimizationParams(interp)
    sptypes = VarState[VarState(sp, false) for sp in match.sparams]
    return statement_costs!(cost, body, src, sptypes, params)
end

function findsup_mt(@nospecialize(tt), world, method_table)
    if method_table === nothing
        table = InternalMethodTable(world)
    elseif method_table isa Core.MethodTable
        table = OverlayMethodTable(world, method_table)
    else
        table = method_table
    end
    return findsup(tt, table)
end
