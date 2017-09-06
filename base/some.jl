# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Some{T}

A wrapper type used with `Union{Some{T}, Void}` to distinguish between the absence
of a value ([`nothing`](@ref)) and the presence of a `nothing` value (i.e. `Some(nothing)`).
It can also be used to force users of a function argument or of an object field
to explicitly handle the possibility of a field or argument being `nothing`,
either manually or by calling [`get`](@ref) before actually using the wrapped
value.
"""
struct Some{T}
    value::T
end

eltype(::Type{Some{T}}) where {T} = T

promote_rule(::Type{Some{S}}, ::Type{Some{T}}) where {S,T} = Some{promote_type(S, T)}
promote_rule(::Type{Some{T}}, ::Type{Void}) where {T} = Union{Some{T}, Void}

convert(::Type{Some{T}}, x::Some) where {T} = Some{T}(convert(T, x.value))
convert(::Type{Union{Some{T}, Void}}, x::Some) where {T} = convert(Some{T}, x)

convert(::Type{Union{T, Void}}, x::Any) where {T} = convert(T, x)
convert(::Type{Void}, x::Any) = throw(MethodError(convert, (Void, x)))
convert(::Type{Void}, x::Void) = nothing

function show(io::IO, x::Some)
    if get(io, :compact, false)
        show(io, x.value)
    else
        print(io, "Some(")
        show(io, x.value)
        print(io, ')')
    end
end

"""
    get(x::Some[, y])
    get(x::Void[, y])

Attempt to access the value wrapped in `x`. Return the value if
`x` is not [`nothing`](@ref) (i.e. it is a [`Some`](@ref) object).
If `x` is `nothing`, return `y` if provided, or throw a `MethodError` if not.

# Examples
```jldoctest
julia> get(Some(5))
5

julia> get(nothing)
ERROR: MethodError()
[...]

julia> get(Some(1), 0)
1

julia> get(nothing, 0)
0

```
"""
function get end

get(x::Some) = x.value
get(x::Some, y) = x.value
get(x::Void, y) = y
