# This file is a part of Julia. License is MIT: https://julialang.org/license

const EMPTY_VECTOR = Vector{Any}()

mutable struct InferenceResult
    linfo::MethodInstance
    argtypes::Vector{Any}
    result # ::Type, or InferenceState if WIP
    src #::Union{CodeInfo, OptimizationState, Nothing} # if inferred copy is available
    function InferenceResult(linfo::MethodInstance)
        if isdefined(linfo, :inferred_const)
            result = Const(linfo.inferred_const)
        else
            result = linfo.rettype
        end
        return new(linfo, compute_inf_result_argtypes(linfo), result, nothing)
    end
end

function compute_inf_result_argtypes(linfo::MethodInstance)
    toplevel = !isa(linfo.def, Method)
    linfo_argtypes::SimpleVector = unwrap_unionall(linfo.specTypes).parameters
    nargs::Int = toplevel ? 0 : linfo.def.nargs
    result_argtypes = Vector{Any}(undef, nargs)
    # First, if we're dealing with a varargs method, then we set the last element of `args`
    # to the appropriate `Tuple` type or `PartialTuple` instance.
    if !toplevel && linfo.def.isva
        if linfo.specTypes == Tuple
            if nargs > 1
                linfo_argtypes = svec(Any[ Any for i = 1:(nargs - 1) ]..., Tuple.parameters[1])
            end
            vararg_type = Tuple
        else
            linfo_argtypes_length = length(linfo_argtypes)
            if nargs > linfo_argtypes_length
                va = linfo_argtypes[linfo_argtypes_length]
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
                for p in linfo_argtypes[nargs:linfo_argtypes_length]
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
        result_argtypes[nargs] = vararg_type
        nargs -= 1
    end
    # Now, we propagate type info from `linfo_argtypes` into `result_argtypes`, improving some
    # type info as we go (where possible). Note that if we're dealing with a varargs method,
    # we already handled the last element of `result_argtypes` (and decremented `nargs` so that
    # we don't overwrite the result of that work here).
    linfo_argtypes_length = length(linfo_argtypes)
    if linfo_argtypes_length > 0
        if linfo_argtypes_length > nargs
            linfo_argtypes_length = nargs
        end
        local lastatype
        tail_index = linfo_argtypes_length
        for i = 1:linfo_argtypes_length
            atyp = linfo_argtypes[i]
            if i == linfo_argtypes_length && isvarargtype(atyp)
                atyp = unwrapva(atyp)
                tail_index -= 1
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
            i == linfo_argtypes_length && (lastatype = atyp)
            result_argtypes[i] = atyp
        end
        for i = (tail_index + 1):nargs
            result_argtypes[i] = lastatype
        end
    else
        @assert nargs == 0 "invalid specialization of method" # wrong number of arguments
    end
    return result_argtypes
end

function cache_lookup(code::MethodInstance, argtypes::Vector{Any}, cache::Vector{InferenceResult})
    method = code.def::Method
    nargs::Int = method.nargs
    method.isva && (nargs -= 1)
    for cache_code in cache
        # try to search cache first
        cache_argtypes = cache_code.argtypes
        if cache_code.linfo === code && length(argtypes) >= nargs
            cache_match = true
            for i in 1:nargs
                a = maybe_widen_conditional(argtypes[i])
                ca = cache_argtypes[i]
                # verify that all Const argument types match between the call and cache
                if (isa(a, Const) || isa(ca, Const)) && !(a === ca)
                    cache_match = false
                    break
                end
            end
            if method.isva
                last_argtype = cache_argtypes[end]
                for i in (nargs + 1):length(argtypes)
                    a = maybe_widen_conditional(argtypes[i])
                    if isa(last_argtype, PartialTuple)
                        ca = last_argtype.fields[i - nargs]
                    elseif isa(last_argtype, Const) && isa(last_argtype.val, Tuple)
                        ca = Const(last_argtype.val[i - nargs])
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
