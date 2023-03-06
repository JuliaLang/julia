# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    matching_cache_argtypes(𝕃::AbstractLattice, linfo::MethodInstance) ->
        (cache_argtypes::Vector{Any}, overridden_by_const::BitVector)

Returns argument types `cache_argtypes::Vector{Any}` for `linfo` that are in the native
Julia type domain. `overridden_by_const::BitVector` is all `false` meaning that
there is no additional extended lattice information there.

    matching_cache_argtypes(𝕃::AbstractLattice, linfo::MethodInstance, argtypes::ForwardableArgtypes) ->
        (cache_argtypes::Vector{Any}, overridden_by_const::BitVector)

Returns cache-correct extended lattice argument types `cache_argtypes::Vector{Any}`
for `linfo` given some `argtypes` accompanied by `overridden_by_const::BitVector`
that marks which argument contains additional extended lattice information.

In theory, there could be a `cache` containing a matching `InferenceResult`
for the provided `linfo` and `given_argtypes`. The purpose of this function is
to return a valid value for `cache_lookup(𝕃, linfo, argtypes, cache).argtypes`,
so that we can construct cache-correct `InferenceResult`s in the first place.
"""
function matching_cache_argtypes end

function matching_cache_argtypes(𝕃::AbstractLattice, linfo::MethodInstance)
    mthd = isa(linfo.def, Method) ? linfo.def::Method : nothing
    cache_argtypes = most_general_argtypes(mthd, linfo.specTypes)
    return cache_argtypes, falses(length(cache_argtypes))
end

struct SimpleArgtypes <: ForwardableArgtypes
    argtypes::Vector{Any}
end

"""
    matching_cache_argtypes(𝕃::AbstractLattice, linfo::MethodInstance, argtypes::SimpleArgtypes)

The implementation for `argtypes` with general extended lattice information.
This is supposed to be used for debugging and testing or external `AbstractInterpreter`
usages and in general `matching_cache_argtypes(::MethodInstance, ::ConditionalArgtypes)`
is more preferred it can forward `Conditional` information.
"""
function matching_cache_argtypes(𝕃::AbstractLattice, linfo::MethodInstance, simple_argtypes::SimpleArgtypes)
    (; argtypes) = simple_argtypes
    given_argtypes = Vector{Any}(undef, length(argtypes))
    for i = 1:length(argtypes)
        given_argtypes[i] = widenslotwrapper(argtypes[i])
    end
    given_argtypes = va_process_argtypes(𝕃, given_argtypes, linfo)
    return pick_const_args(𝕃, linfo, given_argtypes)
end

function pick_const_args(𝕃::AbstractLattice, linfo::MethodInstance, given_argtypes::Vector{Any})
    cache_argtypes, overridden_by_const = matching_cache_argtypes(𝕃, linfo)
    return pick_const_args!(𝕃, cache_argtypes, overridden_by_const, given_argtypes)
end

function pick_const_args!(𝕃::AbstractLattice, cache_argtypes::Vector{Any}, overridden_by_const::BitVector, given_argtypes::Vector{Any})
    for i = 1:length(given_argtypes)
        given_argtype = given_argtypes[i]
        cache_argtype = cache_argtypes[i]
        if !is_argtype_match(𝕃, given_argtype, cache_argtype, false)
            # prefer the argtype we were given over the one computed from `linfo`
            cache_argtypes[i] = given_argtype
            overridden_by_const[i] = true
        end
    end
    return cache_argtypes, overridden_by_const
end

function is_argtype_match(𝕃::AbstractLattice,
                          @nospecialize(given_argtype),
                          @nospecialize(cache_argtype),
                          overridden_by_const::Bool)
    if is_forwardable_argtype(𝕃, given_argtype)
        return is_lattice_equal(𝕃, given_argtype, cache_argtype)
    end
    return !overridden_by_const
end

va_process_argtypes(𝕃::AbstractLattice, given_argtypes::Vector{Any}, linfo::MethodInstance) =
    va_process_argtypes(Returns(nothing), 𝕃, given_argtypes, linfo)
function va_process_argtypes(@nospecialize(va_handler!), 𝕃::AbstractLattice, given_argtypes::Vector{Any}, linfo::MethodInstance)
    def = linfo.def::Method
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
    @assert length(given_argtypes) == nargs "invalid `given_argtypes` for `linfo`"
    return given_argtypes
end

function most_general_argtypes(method::Union{Method, Nothing}, @nospecialize(specTypes),
    withfirst::Bool = true)
    toplevel = method === nothing
    isva = !toplevel && method.isva
    linfo_argtypes = Any[(unwrap_unionall(specTypes)::DataType).parameters...]
    nargs::Int = toplevel ? 0 : method.nargs
    # For opaque closure, the closure environment is processed elsewhere
    withfirst || (nargs -= 1)
    cache_argtypes = Vector{Any}(undef, nargs)
    # First, if we're dealing with a varargs method, then we set the last element of `args`
    # to the appropriate `Tuple` type or `PartialStruct` instance.
    if !toplevel && isva
        if specTypes::Type == Tuple
            if nargs > 1
                linfo_argtypes = Any[Any for i = 1:nargs]
                linfo_argtypes[end] = Vararg{Any}
            end
            vargtype = Tuple
        else
            linfo_argtypes_length = length(linfo_argtypes)
            if nargs > linfo_argtypes_length
                va = linfo_argtypes[linfo_argtypes_length]
                if isvarargtype(va)
                    new_va = rewrap_unionall(unconstrain_vararg_length(va), specTypes)
                    vargtype = Tuple{new_va}
                else
                    vargtype = Tuple{}
                end
            else
                vargtype_elements = Any[]
                for i in nargs:linfo_argtypes_length
                    p = linfo_argtypes[i]
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
    # Now, we propagate type info from `linfo_argtypes` into `cache_argtypes`, improving some
    # type info as we go (where possible). Note that if we're dealing with a varargs method,
    # we already handled the last element of `cache_argtypes` (and decremented `nargs` so that
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
            atyp = unwraptv(atyp)
            if issingletontype(atyp)
                # replace singleton types with their equivalent Const object
                atyp = Const(atyp.instance)
            elseif isconstType(atyp)
                atyp = Const(atyp.parameters[1])
            else
                atyp = elim_free_typevars(rewrap_unionall(atyp, specTypes))
            end
            i == n && (lastatype = atyp)
            cache_argtypes[i] = atyp
        end
        for i = (tail_index + 1):nargs
            cache_argtypes[i] = lastatype
        end
    else
        @assert nargs == 0 "invalid specialization of method" # wrong number of arguments
    end
    cache_argtypes
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

function cache_lookup(lattice::AbstractLattice, linfo::MethodInstance, given_argtypes::Vector{Any}, cache::Vector{InferenceResult})
    method = linfo.def::Method
    nargs::Int = method.nargs
    method.isva && (nargs -= 1)
    length(given_argtypes) >= nargs || return nothing
    for cached_result in cache
        cached_result.linfo === linfo || continue
        cache_match = true
        cache_argtypes = cached_result.argtypes
        cache_overridden_by_const = cached_result.overridden_by_const
        for i in 1:nargs
            if !is_argtype_match(lattice, widenmustalias(given_argtypes[i]),
                                 cache_argtypes[i],
                                 cache_overridden_by_const[i])
                cache_match = false
                break
            end
        end
        if method.isva && cache_match
            cache_match = is_argtype_match(lattice, tuple_tfunc(lattice, given_argtypes[(nargs + 1):end]),
                                           cache_argtypes[end],
                                           cache_overridden_by_const[end])
        end
        cache_match || continue
        return cached_result
    end
    return nothing
end
