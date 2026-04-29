# This file is a part of Julia. License is MIT: https://julialang.org/license

function matching_cache_argtypes(::AbstractLattice, mi::MethodInstance)
    (; def, specTypes) = mi
    return most_general_argtypes(isa(def, Method) ? def : nothing, specTypes)
end

# For `@nospecializeinfer` methods, widen the `@nospecialize`'d argument positions back to
# `cache_argtypes` values to respect the `@nospecializeinfer` semantics.
# This also ensures that the constprop `argtypes` have the same length as `cache_argtypes`.
function get_nospecializeinfer_argtypes(argtypes::Vector{Any}, cache_argtypes::Vector{Any},
                                        method::Method)
    is_nospecializeinfer(method) || return argtypes
    nargs = Int(method.nargs)
    new_argtypes = Vector{Any}(undef, length(cache_argtypes))
    for i = 1:length(cache_argtypes)
        i_arg = min(i - 1, nargs - 1) # 0-indexed, 0 is the function slot
        if i_arg > 0 && !iszero(method.nospecialize & (1 << (i_arg - 1)))
            new_argtypes[i] = cache_argtypes[i]
        else
            new_argtypes[i] = argtypes[i]
        end
    end
    return new_argtypes
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

function matching_cache_argtypes(𝕃::AbstractLattice, ::MethodInstance,
                                 simple_argtypes::Union{SimpleArgtypes, WidenedArgtypes},
                                 cache_argtypes::Vector{Any})
    (; argtypes) = simple_argtypes
    given_argtypes = Vector{Any}(undef, length(argtypes))
    for i = 1:length(argtypes)
        given_argtypes[i] = widenslotwrapper(argtypes[i])
    end
    return pick_const_args!(𝕃, given_argtypes, cache_argtypes)
end

function pick_const_arg(𝕃::AbstractLattice, @nospecialize(given_argtype), @nospecialize(cache_argtype))
    if !is_argtype_match(𝕃, given_argtype, cache_argtype, false)
        # prefer the argtype we were given over the one computed from `mi`
        if (isa(given_argtype, PartialStruct) && isa(cache_argtype, Type) &&
            !⊏(𝕃, given_argtype, cache_argtype))
            # if the type information of this `PartialStruct` is less strict than
            # declared method signature, narrow it down using `tmeet`
            given_argtype = tmeet(𝕃, given_argtype, cache_argtype)
        end
        return given_argtype
    else
        return cache_argtype
    end
end

function pick_const_args!(𝕃::AbstractLattice, given_argtypes::Vector{Any}, cache_argtypes::Vector{Any})
    ngiven = length(given_argtypes)
    ncache = length(cache_argtypes)
    if ngiven == 0 || ncache == 0
        return Any[]
    end
    given_va = given_argtypes[end]
    cache_va = cache_argtypes[end]
    if isvarargtype(given_va)
        va = unwrapva(given_va)
        if isvarargtype(cache_va)
            # Process the common prefix, then join
            nprocessargs = max(ngiven-1, ncache-1)
            resize!(given_argtypes, nprocessargs+1)
            given_argtypes[end] = Vararg{pick_const_arg(𝕃, va, unwrapva(cache_va))}
        else
            nprocessargs = ncache
            resize!(given_argtypes, nprocessargs)
        end
        for i = ngiven:nprocessargs
            given_argtypes[i] = va
        end
    elseif isvarargtype(cache_va)
        nprocessargs = ngiven
    else
        @assert ngiven == ncache
        nprocessargs = ngiven
    end
    for i = 1:nprocessargs
        given_argtype = given_argtypes[i]
        cache_argtype = argtype_by_index(cache_argtypes, i)
        given_argtypes[i] = pick_const_arg(𝕃, given_argtype, cache_argtype)
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

function va_process_argtypes(𝕃::AbstractLattice, given_argtypes::Vector{Any}, nargs::UInt, isva::Bool, mi::MethodInstance)
    nargs = Int(nargs)
    if isva || (!isempty(given_argtypes) && isvarargtype(given_argtypes[end]))
        isva_given_argtypes = Vector{Any}(undef, nargs)
        for i = 1:(nargs-isva)
            newarg = argtype_by_index(given_argtypes, i)
            if isva && has_conditional(𝕃) && isa(newarg, Conditional)
                if newarg.slot > (nargs-isva)
                    newarg = widenconditional(newarg)
                end
            end
            if isva && has_mustalias(𝕃) && isa(newarg, MustAlias)
                if newarg.slot > (nargs-isva)
                    newarg = widenmustalias(newarg)
                end
            end
            isva_given_argtypes[i] = newarg
        end
        if isva
            if length(given_argtypes) < nargs && isvarargtype(given_argtypes[end])
                last = length(given_argtypes)
            else
                last = nargs
                if has_conditional(𝕃)
                    for i = last:length(given_argtypes)
                        newarg = given_argtypes[i]
                        if isa(newarg, Conditional) && newarg.slot > (nargs-isva)
                            given_argtypes[i] = widenconditional(newarg)
                        end
                    end
                end
                if has_mustalias(𝕃)
                    for i = last:length(given_argtypes)
                        newarg = given_argtypes[i]
                        if isa(newarg, MustAlias) && newarg.slot > (nargs-isva)
                            given_argtypes[i] = widenmustalias(newarg)
                        end
                    end
                end
            end
            isva_given_argtypes[nargs] = tuple_tfunc(𝕃, given_argtypes[last:end])
        end
        return isva_given_argtypes
    end
    if length(given_argtypes) != nargs
        println(given_argtypes, " != ", nargs, " for ", mi)
        throw(AssertionError("invalid `given_argtypes` for `mi`"))
    end
    return given_argtypes
end

function most_general_argtypes(method::Union{Method,Nothing}, @nospecialize(specTypes))
    mi_argtypes = Any[(unwrap_unionall(specTypes)::DataType).parameters...]
    nargtypes = length(mi_argtypes)
    nargs = isa(method, Method) ? Int(method.nargs) : 0
    if length(mi_argtypes) < nargs && isvarargtype(mi_argtypes[end])
        resize!(mi_argtypes, nargs)
    end
    # Now, we propagate type info from `mi_argtypes` into `cache_argtypes`, improving some
    # type info as we go (where possible). Note that if we're dealing with a varargs method,
    # we already handled the last element of `cache_argtypes` (and decremented `nargs` so that
    # we don't overwrite the result of that work here).
    tail_index = min(nargtypes, nargs)
    local lastatype
    for i = 1:nargtypes
        atyp = mi_argtypes[i]
        wasva = false
        if i == nargtypes && isvarargtype(atyp)
            wasva = true
            atyp = unwrapva(atyp)
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
        mi_argtypes[i] = atyp
        if wasva
            lastatype = atyp
            mi_argtypes[end] = Vararg{widenconst(atyp)}
        end
    end
    for i = (tail_index+1):(nargs-1)
        mi_argtypes[i] = lastatype
    end
    return mi_argtypes
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

function constprop_cache_lookup(𝕃::AbstractLattice, mi::MethodInstance, given_argtypes::Vector{Any}, cache::InferenceCache)
    nargtypes = length(given_argtypes)
    indices = get_indices(cache, mi)
    found_tombstone = false
    for idx in indices
        cached_result = cache.results[idx]
        cache_argtypes = cached_result.argtypes
        @assert length(cache_argtypes) == nargtypes "invalid `cache_argtypes` for `mi`"
        cache_overridden_by_const = cached_result.overridden_by_const
        cache_overridden_by_const === nothing && continue
        cache_overridden_by_const = cache_overridden_by_const::BitVector
        for i in 1:nargtypes
            if !is_argtype_match(𝕃, given_argtypes[i], cache_argtypes[i], cache_overridden_by_const[i])
                @goto next_cache
            end
        end
        # Don't return tombstoned entries as cache items: they represent rejected work
        # (due to LimitedAccuracy). Instead, record that a tombstone was found so the
        # caller can avoid re-attempting the same const-prop that would hit the same limit.
        if cached_result.tombstone
            found_tombstone = true
            @goto next_cache
        end
        return cached_result
        @label next_cache
    end
    return found_tombstone ? missing : nothing
end
