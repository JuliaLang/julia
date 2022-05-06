# TODO this file contains many duplications with the inlining analysis code, factor them out

import Core.Compiler:
    MethodInstance, InferenceResult, Signature, ConstPropResult, ConcreteResult,
    MethodResultPure, MethodMatchInfo, UnionSplitInfo, ConstCallInfo, InvokeCallInfo,
    call_sig, argtypes_to_type, is_builtin, is_return_type, istopfunction, validate_sparams,
    specialize_method, invoke_rewrite

const Linfo = Union{MethodInstance,InferenceResult}
struct CallInfo
    linfos::Vector{Linfo}
    nothrow::Bool
end

function resolve_call(ir::IRCode, stmt::Expr, @nospecialize(info))
    sig = call_sig(ir, stmt)
    if sig === nothing
        return missing
    end
    # TODO handle _apply_iterate
    if is_builtin(sig) && sig.f !== invoke
        return false
    end
    # handling corresponding to late_inline_special_case!
    (; f, argtypes) = sig
    if length(argtypes) == 3 && istopfunction(f, :!==)
        return true
    elseif length(argtypes) == 3 && istopfunction(f, :(>:))
        return true
    elseif f === TypeVar && 2 ≤ length(argtypes) ≤ 4 && (argtypes[2] ⊑ Symbol)
        return true
    elseif f === UnionAll && length(argtypes) == 3 && (argtypes[2] ⊑ TypeVar)
        return true
    elseif is_return_type(f)
        return true
    end
    if info isa MethodResultPure
        return true
    elseif info === false
        return missing
    end
    # TODO handle OpaqueClosureCallInfo
    if sig.f === invoke
        isa(info, InvokeCallInfo) || return missing
        return analyze_invoke_call(sig, info)
    elseif isa(info, ConstCallInfo)
        return analyze_const_call(sig, info)
    elseif isa(info, MethodMatchInfo)
        infos = MethodMatchInfo[info]
    elseif isa(info, UnionSplitInfo)
        infos = info.matches
    else # isa(info, ReturnTypeCallInfo), etc.
        return missing
    end
    return analyze_call(sig, infos)
end

function analyze_invoke_call(sig::Signature, info::InvokeCallInfo)
    match = info.match
    if !match.fully_covers
        # TODO: We could union split out the signature check and continue on
        return missing
    end
    result = info.result
    if isa(result, ConstPropResult)
        return CallInfo(Linfo[result.result], true)
    else
        argtypes = invoke_rewrite(sig.argtypes)
        mi = analyze_match(match, length(argtypes))
        mi === nothing && return missing
        return CallInfo(Linfo[mi], true)
    end
end

function analyze_const_call(sig::Signature, cinfo::ConstCallInfo)
    linfos = Linfo[]
    (; call, results) = cinfo
    infos = isa(call, MethodMatchInfo) ? MethodMatchInfo[call] : call.matches
    local nothrow = true # required to account for potential escape via MethodError
    local j = 0
    for i in 1:length(infos)
        meth = infos[i].results
        nothrow &= !meth.ambig
        nmatch = Core.Compiler.length(meth)
        if nmatch == 0 # No applicable methods
            # mark this call may potentially throw, and the try next union split
            nothrow = false
            continue
        end
        for i = 1:nmatch
            j += 1
            result = results[j]
            match = Core.Compiler.getindex(meth, i)
            if result === nothing
                mi = analyze_match(match, length(sig.argtypes))
                mi === nothing && return missing
                push!(linfos, mi)
            elseif isa(result, ConcreteResult)
                # TODO we may want to feedback information that this call always throws if !isdefined(result, :result)
                push!(linfos, result.mi)
            elseif isa(result, ConstPropResult)
                push!(linfos, result.result)
            end
            nothrow &= match.fully_covers
        end
    end
    return CallInfo(linfos, nothrow)
end

function analyze_call(sig::Signature, infos::Vector{MethodMatchInfo})
    linfos = Linfo[]
    local nothrow = true # required to account for potential escape via MethodError
    for i in 1:length(infos)
        meth = infos[i].results
        nothrow &= !meth.ambig
        nmatch = Core.Compiler.length(meth)
        if nmatch == 0 # No applicable methods
            # mark this call may potentially throw, and the try next union split
            nothrow = false
            continue
        end
        for i = 1:nmatch
            match = Core.Compiler.getindex(meth, i)
            mi = analyze_match(match, length(sig.argtypes))
            mi === nothing && return missing
            push!(linfos, mi)
            nothrow &= match.fully_covers
        end
    end
    return CallInfo(linfos, nothrow)
end

function analyze_match(match::MethodMatch, npassedargs::Int)
    method = match.method
    na = Int(method.nargs)
    if na != npassedargs && !(na > 0 && method.isva)
        # we have a method match only because an earlier
        # inference step shortened our call args list, even
        # though we have too many arguments to actually
        # call this function
        return nothing
    end

    # Bail out if any static parameters are left as TypeVar
    # COMBAK is this needed for escape analysis?
    validate_sparams(match.sparams) || return nothing

    # See if there exists a specialization for this method signature
    mi = specialize_method(match; preexisting=true) # Union{Nothing, MethodInstance}
    return mi
end
