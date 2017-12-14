# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Some{T}

A wrapper type used in `Union{Some{T}, Void}` to distinguish between the absence
of a value ([`nothing`](@ref)) and the presence of a `nothing` value (i.e. `Some(nothing)`).

Use [`coalesce`](@ref) to access the value wrapped by a `Some` object.
"""
struct Some{T}
    value::T
end

promote_rule(::Type{Some{S}}, ::Type{Some{T}}) where {S,T} = Some{promote_type(S, T)}
promote_rule(::Type{Some{T}}, ::Type{Void}) where {T} = Union{Some{T}, Void}

convert(::Type{Some{T}}, x::Some) where {T} = Some{T}(convert(T, x.value))
convert(::Type{Union{Some{T}, Void}}, x::Some) where {T} = convert(Some{T}, x)

convert(::Type{Union{T, Void}}, x::Any) where {T} = convert(T, x)
convert(::Type{Void}, x::Any) = throw(MethodError(convert, (Void, x)))
convert(::Type{Void}, x::Void) = nothing

function show(io::IO, x::Some)
    if get(io, :typeinfo, Any) == typeof(x)
        show(io, x.value)
    else
        print(io, "Some(")
        show(io, x.value)
        print(io, ')')
    end
end

"""
    coalesce(x, y...)

Return the first value in the arguments which is not equal to `nothing`,
or `nothing` if all arguments are `nothing`. Unwrap arguments of type
[`Some`](@ref).

# Examples

```jldoctest
julia> coalesce(nothing, 1)
1

julia> coalesce(1, nothing)
1

julia> coalesce(nothing, nothing)
nothing

julia> coalesce(Some(1))
1

julia> coalesce(nothing, Some(1))
1
```
"""
function coalesce end

coalesce(x::Any) = x
coalesce(x::Some) = x.value
coalesce(x::Void) = nothing
coalesce(x::Any, y...) = x
coalesce(x::Some, y...) = x.value
coalesce(x::Void, y...) = coalesce(y...)

"""
    notnothing(x)

Throw an error if `x == nothing`, and return `x` if not.
"""
notnothing(x::Any) = x
notnothing(::Void) = throw(ArgumentError("nothing passed to notnothing"))