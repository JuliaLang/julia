# This file is a part of Julia. License is MIT: https://julialang.org/license

function matching_cache_argtypes(𝕃::AbstractLattice, mi::MethodInstance)
    (; def, specTypes) = mi
    return most_general_argtypes(isa(def, Method) ? def : nothing, specTypes)
end

struct SimpleArgtypes
    argtypes::Vector{Any}
end

# Like `SimpleArgtypes`, but allows the argtypes to be wider than the current call.
# As a result, it is not legal to refine the cache result with information more
# precise than was it deducible from the `WidenedSimpleArgtypes`.
struct WidenedArgtypes
    argtypes::Vector{Any}
end

function matching_cache_argtypes(𝕃::AbstractLattice, mi::MethodInstance,
                                 simple_argtypes::Union{SimpleArgtypes, WidenedArgtypes},
                                 cache_argtypes::Vector{Any})
    (; argtypes) = simple_argtypes
    given_argtypes = Vector{Any}(undef, length(argtypes))
    for i = 1:length(argtypes)
        given_argtypes[i] = widenslotwrapper(argtypes[i])
    end
    given_argtypes = va_process_argtypes(𝕃, given_argtypes, mi)
    return pick_const_args!(𝕃, given_argtypes, cache_argtypes)
end

function pick_const_args!(𝕃::AbstractLattice, given_argtypes::Vector{Any}, cache_argtypes::Vector{Any})
    nargtypes = length(given_argtypes)
    @assert nargtypes == length(cache_argtypes) #= == nargs =# "invalid `given_argtypes` for `mi`"
    for i = 1:nargtypes
        given_argtype = given_argtypes[i]
        cache_argtype = cache_argtypes[i]
        if !is_argtype_match(𝕃, given_argtype, cache_argtype, false)
            # prefer the argtype we were given over the one computed from `mi`
            if (isa(given_argtype, PartialStruct) && isa(cache_argtype, Type) &&
                !⊏(𝕃, given_argtype, cache_argtype))
                # if the type information of this `PartialStruct` is less strict than
                # declared method signature, narrow it down using `tmeet`
                given_argtypes[i] = tmeet(𝕃, given_argtype, cache_argtype)
            end
        else
            given_argtypes[i] = cache_argtype
        end
    end
    return given_argtypes
end

function is_argtype_match(𝕃::AbstractLattice,
                          @nospecialize(given_argtype),
                          @nospecialize(cache_argtype),
                          overridden_by_const::Bool)
    if is_forwardable_argtype(𝕃, given_argtype)
        return is_lattice_equal(𝕃, given_argtype, cache_argtype)
    else
        return !overridden_by_const
    end
end

va_process_argtypes(𝕃::AbstractLattice, given_argtypes::Vector{Any}, mi::MethodInstance) =
    va_process_argtypes(Returns(nothing), 𝕃, given_argtypes, mi)
function va_process_argtypes(@specialize(va_handler!), 𝕃::AbstractLattice, given_argtypes::Vector{Any}, mi::MethodInstance)
    def = mi.def::Method
    isva = def.isva
    nargs = Int(def.nargs)
    if isva || isvarargtype(given_argtypes[end])
        isva_given_argtypes = Vector{Any}(undef, nargs)
        for i = 1:(nargs-isva)
            isva_given_argtypes[i] = argtype_by_index(given_argtypes, i)
        end
        if isva
            if length(given_argtypes) < nargs && isvarargtype(given_argtypes[end])
                last = length(given_argtypes)
            else
                last = nargs
            end
            isva_given_argtypes[nargs] = tuple_tfunc(𝕃, given_argtypes[last:end])
            va_handler!(isva_given_argtypes, last)
        end
        return isva_given_argtypes
    end
    @assert length(given_argtypes) == nargs "invalid `given_argtypes` for `mi`"
    return given_argtypes
end

function most_general_argtypes(method::Union{Method,Nothing}, @nospecialize(specTypes))
    toplevel = method === nothing
    isva = !toplevel && method.isva
    mi_argtypes = Any[(unwrap_unionall(specTypes)::DataType).parameters...]
    nargs::Int = toplevel ? 0 : method.nargs
    cache_argtypes = Vector{Any}(undef, nargs)
    # First, if we're dealing with a varargs method, then we set the last element of `args`
    # to the appropriate `Tuple` type or `PartialStruct` instance.
    mi_argtypes_length = length(mi_argtypes)
    if !toplevel && isva
        if specTypes::Type == Tuple
            mi_argtypes = Any[Any for i = 1:nargs]
            if nargs > 1
                mi_argtypes[end] = Tuple
            end
            vargtype = Tuple
        else
            if nargs > mi_argtypes_length
                va = mi_argtypes[mi_argtypes_length]
                if isvarargtype(va)
                    new_va = rewrap_unionall(unconstrain_vararg_length(va), specTypes)
                    vargtype = Tuple{new_va}
                else
                    vargtype = Tuple{}
                end
            else
                vargtype_elements = Any[]
                for i in nargs:mi_argtypes_length
                    p = mi_argtypes[i]
                    p = unwraptv(isvarargtype(p) ? unconstrain_vararg_length(p) : p)
                    push!(vargtype_elements, elim_free_typevars(rewrap_unionall(p, specTypes)))
                end
                for i in 1:length(vargtype_elements)
                    atyp = vargtype_elements[i]
                    if issingletontype(atyp)
                        # replace singleton types with their equivalent Const object
                        vargtype_elements[i] = Const(atyp.instance)
                    elseif isconstType(atyp)
                        vargtype_elements[i] = Const(atyp.parameters[1])
                    end
                end
                vargtype = tuple_tfunc(fallback_lattice, vargtype_elements)
            end
        end
        cache_argtypes[nargs] = vargtype
        nargs -= 1
    end
    # Now, we propagate type info from `mi_argtypes` into `cache_argtypes`, improving some
    # type info as we go (where possible). Note that if we're dealing with a varargs method,
    # we already handled the last element of `cache_argtypes` (and decremented `nargs` so that
    # we don't overwrite the result of that work here).
    if mi_argtypes_length > 0
        tail_index = nargtypes = min(mi_argtypes_length, nargs)
        local lastatype
        for i = 1:nargtypes
            atyp = mi_argtypes[i]
            if i == nargtypes && isvarargtype(atyp)
                atyp = unwrapva(atyp)
                tail_index -= 1
            end
            atyp = unwraptv(atyp)
            if issingletontype(atyp)
                # replace singleton types with their equivalent Const object
                atyp = Const(atyp.instance)
            elseif isconstType(atyp)
                atyp = Const(atyp.parameters[1])
            else
                atyp = elim_free_typevars(rewrap_unionall(atyp, specTypes))
            end
            i == nargtypes && (lastatype = atyp)
            cache_argtypes[i] = atyp
        end
        for i = (tail_index+1):nargs
            cache_argtypes[i] = lastatype
        end
    else
        @assert nargs == 0 "invalid specialization of method" # wrong number of arguments
    end
    return cache_argtypes
end

# eliminate free `TypeVar`s in order to make the life much easier down the road:
# at runtime only `Type{...}::DataType` can contain invalid type parameters, and other
# malformed types here are user-constructed type arguments given at an inference entry
# so this function will replace only the malformed `Type{...}::DataType` with `Type`
# and simply replace other possibilities with `Any`
function elim_free_typevars(@nospecialize t)
    if has_free_typevars(t)
        return isType(t) ? Type : Any
    else
        return t
    end
end

function cache_lookup(𝕃::AbstractLattice, mi::MethodInstance, given_argtypes::Vector{Any},
                      cache::Vector{InferenceResult})
    method = mi.def::Method
    nargtypes = length(given_argtypes)
    @assert nargtypes == Int(method.nargs) "invalid `given_argtypes` for `mi`"
    for cached_result in cache
        cached_result.linfo === mi || @goto next_cache
        cache_argtypes = cached_result.argtypes
        @assert length(cache_argtypes) == nargtypes "invalid `cache_argtypes` for `mi`"
        cache_overridden_by_const = cached_result.overridden_by_const::BitVector
        for i in 1:nargtypes
            if !is_argtype_match(𝕃, given_argtypes[i], cache_argtypes[i], cache_overridden_by_const[i])
                @goto next_cache
            end
        end
        return cached_result
        @label next_cache
    end
    return nothing
end
