# This file is a part of Julia. License is MIT: https://julialang.org/license

const EMPTY_VECTOR = Vector{Any}()

mutable struct InferenceResult
    linfo::MethodInstance
    args::Vector{Any}
    vargs::Vector{Any} # Memoize vararg type info w/Consts here when calling get_argtypes
                       # on the InferenceResult, so that the optimizer can use this info
                       # later during inlining.
    result # ::Type, or InferenceState if WIP
    src #::Union{CodeInfo, OptimizationState, Nothing} # if inferred copy is available
    function InferenceResult(linfo::MethodInstance)
        if isdefined(linfo, :inferred_const)
            result = Const(linfo.inferred_const)
        else
            result = linfo.rettype
        end
        return new(linfo, EMPTY_VECTOR, Any[], result, nothing)
    end
end

function get_argtypes(result::InferenceResult)
    result.args === EMPTY_VECTOR || return result.args # already cached
    argtypes, vargs = get_argtypes(result.linfo)
    result.args = argtypes
    if vargs !== nothing
        result.vargs = vargs
    end
    return argtypes
end

function get_argtypes(linfo::MethodInstance)
    toplevel = !isa(linfo.def, Method)
    atypes::SimpleVector = unwrap_unionall(linfo.specTypes).parameters
    nargs::Int = toplevel ? 0 : linfo.def.nargs
    args = Vector{Any}(undef, nargs)
    vargs = nothing
    if !toplevel && linfo.def.isva
        if linfo.specTypes == Tuple
            if nargs > 1
                atypes = svec(Any[ Any for i = 1:(nargs - 1) ]..., Tuple.parameters[1])
            end
            vararg_type = Tuple
        else
            laty = length(atypes)
            if nargs > laty
                va = atypes[laty]
                if isvarargtype(va)
                    new_va = rewrap_unionall(unconstrain_vararg_length(va), linfo.specTypes)
                    vararg_type_vec = Any[new_va]
                    vararg_type = Tuple{new_va}
                else
                    vararg_type_vec = Any[]
                    vararg_type = Tuple{}
                end
            else
                vararg_type_vec = Any[]
                for p in atypes[nargs:laty]
                    p = isvarargtype(p) ? unconstrain_vararg_length(p) : p
                    push!(vararg_type_vec, rewrap_unionall(p, linfo.specTypes))
                end
                vararg_type = tuple_tfunc(Tuple{vararg_type_vec...})
                for i in 1:length(vararg_type_vec)
                    atyp = vararg_type_vec[i]
                    if isa(atyp, DataType) && isdefined(atyp, :instance)
                        # replace singleton types with their equivalent Const object
                        vararg_type_vec[i] = Const(atyp.instance)
                    elseif isconstType(atyp)
                        vararg_type_vec[i] = Const(atyp.parameters[1])
                    end
                end
            end
            vargs = vararg_type_vec
        end
        args[nargs] = vararg_type
        nargs -= 1
    end
    laty = length(atypes)
    if laty > 0
        if laty > nargs
            laty = nargs
        end
        local lastatype
        atail = laty
        for i = 1:laty
            atyp = atypes[i]
            if i == laty && isvarargtype(atyp)
                atyp = unwrapva(atyp)
                atail -= 1
            end
            while isa(atyp, TypeVar)
                atyp = atyp.ub
            end
            if isa(atyp, DataType) && isdefined(atyp, :instance)
                # replace singleton types with their equivalent Const object
                atyp = Const(atyp.instance)
            elseif isconstType(atyp)
                atyp = Const(atyp.parameters[1])
            else
                atyp = rewrap_unionall(atyp, linfo.specTypes)
            end
            i == laty && (lastatype = atyp)
            args[i] = atyp
        end
        for i = (atail + 1):nargs
            args[i] = lastatype
        end
    else
        @assert nargs == 0 "invalid specialization of method" # wrong number of arguments
    end
    return args, vargs
end

function cache_lookup(code::MethodInstance, argtypes::Vector{Any}, cache::Vector{InferenceResult})
    method = code.def::Method
    nargs::Int = method.nargs
    method.isva && (nargs -= 1)
    for cache_code in cache
        # try to search cache first
        cache_args = cache_code.args
        cache_vargs = cache_code.vargs
        if cache_code.linfo === code && length(argtypes) === (length(cache_vargs) + nargs)
            cache_match = true
            for i in 1:length(argtypes)
                a = maybe_widen_conditional(argtypes[i])
                ca = i <= nargs ? cache_args[i] : cache_vargs[i - nargs]
                # verify that all Const argument types match between the call and cache
                if (isa(a, Const) || isa(ca, Const)) && !(a === ca)
                    cache_match = false
                    break
                end
            end
            cache_match || continue
            return cache_code
        end
    end
    return nothing
end
