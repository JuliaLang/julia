# This file is a part of Julia. License is MIT: https://julialang.org/license

## all & any

"""
    any(itr)::Bool

Test whether any elements of a boolean collection are `true`, returning `true` as
soon as the first `true` value in `itr` is encountered (short-circuiting). To
short-circuit on `false`, use [`all`](@ref).

If the input contains [`missing`](@ref) values, return `missing` if all non-missing
values are `false` (or equivalently, if the input contains no `true` value), following
[three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic).

See also: [`all`](@ref), [`count`](@ref), [`sum`](@ref), [`|`](@ref), [`||`](@ref).

# Examples
```jldoctest
julia> a = [true,false,false,true]
4-element Vector{Bool}:
 1
 0
 0
 1

julia> any(a)
true

julia> any((println(i); v) for (i, v) in enumerate(a))
1
true

julia> any([missing, true])
true

julia> any([false, missing])
missing
```
"""
any(itr) = any(identity, itr)

"""
    all(itr)::Bool

Test whether all elements of a boolean collection are `true`, returning `false` as
soon as the first `false` value in `itr` is encountered (short-circuiting). To
short-circuit on `true`, use [`any`](@ref).

If the input contains [`missing`](@ref) values, return `missing` if all non-missing
values are `true` (or equivalently, if the input contains no `false` value), following
[three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic).

See also: [`all!`](@ref), [`any`](@ref), [`count`](@ref), [`&`](@ref), [`&&`](@ref), [`allunique`](@ref).

# Examples
```jldoctest
julia> a = [true,false,false,true]
4-element Vector{Bool}:
 1
 0
 0
 1

julia> all(a)
false

julia> all((println(i); v) for (i, v) in enumerate(a))
1
2
false

julia> all([missing, false])
false

julia> all([true, missing])
missing
```
"""
all(itr) = all(identity, itr)

"""
    any(p, itr)::Bool

Determine whether predicate `p` returns `true` for any elements of `itr`, returning
`true` as soon as the first item in `itr` for which `p` returns `true` is encountered
(short-circuiting). To short-circuit on `false`, use [`all`](@ref).

If the input contains [`missing`](@ref) values, return `missing` if all non-missing
values are `false` (or equivalently, if the input contains no `true` value), following
[three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic).

# Examples
```jldoctest
julia> any(i->(4<=i<=6), [3,5,7])
true

julia> any(i -> (println(i); i > 3), 1:10)
1
2
3
4
true

julia> any(i -> i > 0, [1, missing])
true

julia> any(i -> i > 0, [-1, missing])
missing

julia> any(i -> i > 0, [-1, 0])
false
```
"""
any(f, itr) = _any(f, itr, :)

for ItrT = (Tuple,Any)
    # define a generic method and a specialized version for `Tuple`,
    # whose method bodies are identical, while giving better effects to the later
    @eval function _any(f, itr::$ItrT, ::Colon)
        $(ItrT === Tuple ? :(@_terminates_locally_meta) : :nothing)
        anymissing = false
        for x in itr
            v = f(x)
            if ismissing(v)
                anymissing = true
            else
                v && return true
            end
        end
        return anymissing ? missing : false
    end
end

# When the function is side effect-free, we may avoid short-circuiting to help
# vectorize the loop.
function _any(::typeof(identity), itr::Tuple{Vararg{Bool}}, ::Colon)
    @_terminates_locally_meta
    r = false
    for i in eachindex(itr)
        # Avoid bounds checking to help vectorization. Use `getfield` directly,
        # instead of `@inbounds itr[i]`, for better effects.
        v = getfield(itr, i, false)
        r |= v
    end
    r
end

# Specialized versions of any(f, ::Tuple)
# We fall back to the for loop implementation all elements have the same type or
# if the tuple is too large.
function any(f, itr::Tuple)
    if itr isa NTuple || length(itr) > 32
        return _any(f, itr, :)
    end
    _any_tuple(f, false, itr...)
end

@inline function _any_tuple(f, anymissing, x, rest...)
    v = f(x)
    if ismissing(v)
        anymissing = true
    elseif v
        return true
    end
    return _any_tuple(f, anymissing, rest...)
end
@inline _any_tuple(f, anymissing) = anymissing ? missing : false

"""
    all(p, itr)::Bool

Determine whether predicate `p` returns `true` for all elements of `itr`, returning
`false` as soon as the first item in `itr` for which `p` returns `false` is encountered
(short-circuiting). To short-circuit on `true`, use [`any`](@ref).

If the input contains [`missing`](@ref) values, return `missing` if all non-missing
values are `true` (or equivalently, if the input contains no `false` value), following
[three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic).

# Examples
```jldoctest
julia> all(i->(4<=i<=6), [4,5,6])
true

julia> all(i -> (println(i); i < 3), 1:10)
1
2
3
false

julia> all(i -> i > 0, [1, missing])
missing

julia> all(i -> i > 0, [-1, missing])
false

julia> all(i -> i > 0, [1, 2])
true
```
"""
all(f, itr) = _all(f, itr, :)

for ItrT = (Tuple,Any)
    # define a generic method and a specialized version for `Tuple`,
    # whose method bodies are identical, while giving better effects to the later
    @eval function _all(f, itr::$ItrT, ::Colon)
        $(ItrT === Tuple ? :(@_terminates_locally_meta) : :nothing)
        anymissing = false
        for x in itr
            v = f(x)
            if ismissing(v)
                anymissing = true
            else
                v || return false
            end
        end
        return anymissing ? missing : true
    end
end

# When the function is side effect-free, we may avoid short-circuiting to help
# vectorize the loop.
function _all(::typeof(identity), itr::Tuple{Vararg{Bool}}, ::Colon)
    @_terminates_locally_meta
    r = true
    for i in eachindex(itr)
        # Avoid bounds checking to help vectorization. Use `getfield` directly,
        # instead of `@inbounds itr[i]`, for better effects.
        v = getfield(itr, i, false)
        r &= v
    end
    r
end

# Specialized versions of all(f, ::Tuple),
# This is similar to any(f, ::Tuple) defined above.
function all(f, itr::Tuple)
    if itr isa NTuple || length(itr) > 32
        return _all(f, itr, :)
    end
    _all_tuple(f, false, itr...)
end

@inline function _all_tuple(f, anymissing, x, rest...)
    v = f(x)
    if ismissing(v)
        anymissing = true
    # this syntax allows throwing a TypeError for non-Bool, for consistency with any
    elseif v
        nothing
    else
        return false
    end
    return _all_tuple(f, anymissing, rest...)
end
@inline _all_tuple(f, anymissing) = anymissing ? missing : true
