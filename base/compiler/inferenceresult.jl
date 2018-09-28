# This file is a part of Julia. License is MIT: https://julialang.org/license

const EMPTY_VECTOR = Vector{Any}()

mutable struct InferenceResult
    linfo::MethodInstance
    argtypes::Vector{Any}
    result # ::Type, or InferenceState if WIP
    src #::Union{CodeInfo, OptimizationState, Nothing} # if inferred copy is available
    function InferenceResult(linfo::MethodInstance, given_argtypes = nothing)
        if isdefined(linfo, :inferred_const)
            result = Const(linfo.inferred_const)
        else
            result = linfo.rettype
        end
        return new(linfo, matching_cache_argtypes(linfo, given_argtypes), result, nothing)
    end
end

# In theory, there could be a `cache` containing a matching `InferenceResult`
# for the provided `linfo` and `given_argtypes`. The purpose of this function is
# to return a valid value for `cache_lookup(linfo, argtypes, cache).argtypes`,
# so that we can construct cache-correct `InferenceResult`s in the first place.
function matching_cache_argtypes(linfo::MethodInstance, given_argtypes::Vector)
    @assert isa(linfo.def, Method) # !toplevel
    nargs::Int = linfo.def.nargs
    @assert length(given_argtypes) >= (nargs - 1)
    result_argtypes = Any[maybe_widen_conditional(a) for a in given_argtypes]
    if linfo.def.isva
        isva_result_argtypes = Vector{Any}(undef, nargs)
        for i = 1:(nargs - 1)
            isva_result_argtypes[i] = result_argtypes[i]
        end
        isva_result_argtypes[nargs] = tuple_tfunc(result_argtypes[nargs:end])
        return isva_result_argtypes
    end
    @assert length(result_argtypes) === nargs
    return result_argtypes
end

function matching_cache_argtypes(linfo::MethodInstance, ::Nothing)
    toplevel = !isa(linfo.def, Method)
    linfo_argtypes = Any[unwrap_unionall(linfo.specTypes).parameters...]
    nargs::Int = toplevel ? 0 : linfo.def.nargs
    result_argtypes = Vector{Any}(undef, nargs)
    # First, if we're dealing with a varargs method, then we set the last element of `args`
    # to the appropriate `Tuple` type or `PartialTuple` instance.
    if !toplevel && linfo.def.isva
        if linfo.specTypes == Tuple
            if nargs > 1
                linfo_argtypes = svec(Any[Any for i = 1:(nargs - 1)]..., Tuple.parameters[1])
            end
            vargtype = Tuple
        else
            linfo_argtypes_length = length(linfo_argtypes)
            if nargs > linfo_argtypes_length
                va = linfo_argtypes[linfo_argtypes_length]
                if isvarargtype(va)
                    new_va = rewrap_unionall(unconstrain_vararg_length(va), linfo.specTypes)
                    vargtype_elements = Any[new_va]
                    vargtype = Tuple{new_va}
                else
                    vargtype_elements = Any[]
                    vargtype = Tuple{}
                end
            else
                vargtype_elements = Any[]
                for p in linfo_argtypes[nargs:linfo_argtypes_length]
                    p = isvarargtype(p) ? unconstrain_vararg_length(p) : p
                    push!(vargtype_elements, rewrap(p, linfo.specTypes))
                end
                for i in 1:length(vargtype_elements)
                    atyp = vargtype_elements[i]
                    if isa(atyp, DataType) && isdefined(atyp, :instance)
                        # replace singleton types with their equivalent Const object
                        vargtype_elements[i] = Const(atyp.instance)
                    elseif isconstType(atyp)
                        vargtype_elements[i] = Const(atyp.parameters[1])
                    end
                end
                vargtype = tuple_tfunc(vargtype_elements)
            end
        end
        result_argtypes[nargs] = vargtype
        nargs -= 1
    end
    # Now, we propagate type info from `linfo_argtypes` into `result_argtypes`, improving some
    # type info as we go (where possible). Note that if we're dealing with a varargs method,
    # we already handled the last element of `result_argtypes` (and decremented `nargs` so that
    # we don't overwrite the result of that work here).
    linfo_argtypes_length = length(linfo_argtypes)
    if linfo_argtypes_length > 0
        n = linfo_argtypes_length > nargs ? nargs : linfo_argtypes_length
        tail_index = n
        local lastatype
        for i = 1:n
            atyp = linfo_argtypes[i]
            if i == n && isvarargtype(atyp)
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
                atyp = rewrap(atyp, linfo.specTypes)
            end
            i == n && (lastatype = atyp)
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

function is_argtype_mismatch(@nospecialize(a), @nospecialize(b))
    # TODO `===` is correct but too strict for PartialTuples
    return (isa(a, Const) || isa(b, Const) || isa(a, PartialTuple) || isa(b, PartialTuple)) && !(a === b)
end

function cache_lookup(linfo::MethodInstance, given_argtypes::Vector{Any}, cache::Vector{InferenceResult})
    method = linfo.def::Method
    nargs::Int = method.nargs
    method.isva && (nargs -= 1)
    for cached_result in cache
        # try to search cache first
        cache_argtypes = cached_result.argtypes
        if cached_result.linfo === linfo && length(given_argtypes) >= nargs
            cache_match = true
            for i in 1:nargs
                a = maybe_widen_conditional(given_argtypes[i])
                ca = cache_argtypes[i]
                # verify that all Const argument types match between the call and cache
                if is_argtype_mismatch(a, ca)
                    cache_match = false
                    break
                end
            end
            if method.isva && cache_match
                last_argtype = cache_argtypes[end]
                for i in (nargs + 1):length(given_argtypes)
                    a = maybe_widen_conditional(given_argtypes[i])
                    if isa(last_argtype, PartialTuple)
                        ca = last_argtype.fields[i - nargs]
                    elseif isa(last_argtype, Const) && isa(last_argtype.val, Tuple)
                        ca = Const(last_argtype.val[i - nargs])
                    else
                        ca = nothing # cache_match is false
                    end
                    if is_argtype_mismatch(a, ca)
                        cache_match = false
                        break
                    end
                end
            end
            cache_match || continue
            return cached_result
        end
    end
    return nothing
end
