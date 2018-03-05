# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Some{T}

A wrapper type used in `Union{Some{T}, Nothing}` to distinguish between the absence
of a value ([`nothing`](@ref)) and the presence of a `nothing` value (i.e. `Some(nothing)`).

Use [`coalesce`](@ref) to access the value wrapped by a `Some` object.
"""
struct Some{T}
    value::T
end

promote_rule(::Type{Some{S}}, ::Type{Some{T}}) where {S,T} = Some{promote_type(S, T)}
promote_rule(::Type{Some{T}}, ::Type{Nothing}) where {T} = Union{Some{T}, Nothing}

convert(::Type{Some{T}}, x::Some) where {T} = Some{T}(convert(T, x.value))
convert(::Type{Union{Some{T}, Nothing}}, x::Some) where {T} = convert(Some{T}, x)

convert(::Type{Union{T, Nothing}}, x::Any) where {T} = convert(T, x)
convert(::Type{Nothing}, x::Any) = throw(MethodError(convert, (Nothing, x)))
convert(::Type{Nothing}, x::Nothing) = nothing

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

Return the first value in the arguments which is not equal to
either [`nothing`](@ref) or [`missing`](@ref), or the last argument.
Unwrap arguments of type [`Some`](@ref).

# Examples

```jldoctest
julia> coalesce(nothing, 1)
1

julia> coalesce(missing, 1)
1

julia> coalesce(1, nothing)
1

julia> coalesce(nothing, nothing) # returns nothing, but not printed in the REPL

julia> coalesce(Some(1))
1

julia> coalesce(nothing, Some(1))
1
```
"""
function coalesce end

coalesce(x::Any) = x
coalesce(x::Some) = x.value
coalesce(x::Nothing) = nothing
coalesce(x::Missing) = missing
coalesce(x::Any, y...) = x
coalesce(x::Some, y...) = x.value
coalesce(x::Union{Nothing, Missing}, y...) = coalesce(y...)

"""
    notnothing(x)

Throw an error if `x == nothing`, and return `x` if not.
"""
notnothing(x::Any) = x
notnothing(::Nothing) = throw(ArgumentError("nothing passed to notnothing"))
