# This file is a part of Julia. License is MIT: https://julialang.org/license

const EMPTY_VECTOR = Vector{Any}()

mutable struct InferenceResult
    linfo::MethodInstance
    args::Vector{Any}
    result # ::Type, or InferenceState if WIP
    src #::Union{CodeInfo, OptimizationState, Nothing} # if inferred copy is available
    function InferenceResult(linfo::MethodInstance)
        if isdefined(linfo, :inferred_const)
            result = Const(linfo.inferred_const)
        else
            result = linfo.rettype
        end
        return new(linfo, EMPTY_VECTOR, result, nothing)
    end
end

function get_argtypes(result::InferenceResult)
    result.args === EMPTY_VECTOR || return result.args # already cached
    argtypes = get_argtypes(result.linfo)
    result.args = argtypes
    return argtypes
end

function get_argtypes(linfo::MethodInstance)
    toplevel = !isa(linfo.def, Method)
    atypes::SimpleVector = unwrap_unionall(linfo.specTypes).parameters
    nargs::Int = toplevel ? 0 : linfo.def.nargs
    args = Vector{Any}(undef, nargs)
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
                for i in 1:length(vararg_type_vec)
                    atyp = vararg_type_vec[i]
                    if isa(atyp, DataType) && isdefined(atyp, :instance)
                        # replace singleton types with their equivalent Const object
                        vararg_type_vec[i] = Const(atyp.instance)
                    elseif isconstType(atyp)
                        vararg_type_vec[i] = Const(atyp.parameters[1])
                    end
                end
                vararg_type = tuple_tfunc(vararg_type_vec)
            end
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
    return args
end

function cache_lookup(code::MethodInstance, argtypes::Vector{Any}, cache::Vector{InferenceResult})
    method = code.def::Method
    nargs::Int = method.nargs
    method.isva && (nargs -= 1)
    for cache_code in cache
        # try to search cache first
        cache_args = cache_code.args
        if cache_code.linfo === code && length(argtypes) >= nargs
            cache_match = true
            for i in 1:nargs
                a = maybe_widen_conditional(argtypes[i])
                ca = cache_args[i]
                # verify that all Const argument types match between the call and cache
                if (isa(a, Const) || isa(ca, Const)) && !(a === ca)
                    cache_match = false
                    break
                end
            end
            if method.isva
                last_arg = cache_args[end]
                for i in (nargs + 1):length(argtypes)
                    a = maybe_widen_conditional(argtypes[i])
                    if isa(last_arg, PartialTuple)
                        ca = last_arg.fields[i - nargs]
                    elseif isa(last_arg, Const) && isa(last_arg.val, Tuple)
                        ca = Const(last_arg.val[i - nargs])
                    else
                        ca = nothing # not Const
                    end
                    if (isa(a, Const) || isa(ca, Const)) && !(a === ca)
                        cache_match = false
                        break
                    end
                end
            end
            cache_match || continue
            return cache_code
        end
    end
    return nothing
end
