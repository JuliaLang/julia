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

promote_rule(::Type{Some{T}}, ::Type{Some{S}}) where {T, S<:T} = Some{T}
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
either [`nothing`](@ref) or [`missing`](@ref), or throw an error
if no argument matches this condition.
Unwrap arguments of type [`Some`](@ref).

If both `nothing` and `missing` appear in the arguments, only equal to the one
which appears first are skipped. Pass `nothing` or `missing` as the first argument
to ensure that only arguments equal to this value are skipped.

To indicate that `nothing` or `missing` should be returned when all arguments are
`nothing` or `missing` rather than throwing an error, pass `Some(nothing)` or
`Some(missing)` as the last argument.


# Examples

```jldoctest
julia> coalesce(nothing, 1)
1

julia> coalesce(missing, 1)
1

julia> coalesce(1, nothing)
1

julia> coalesce(nothing, nothing, 1)
1

julia> coalesce(nothing, nothing)
ERROR: ArgumentError: coalesce requires that least one argument differs from `nothing` or `missing`.
Pass `Some(nothing)` as the last argument to force returning `nothing`.
Stacktrace:
[...]

julia> coalesce(Some(1))
1

julia> coalesce(nothing, Some(1))
1

julia> coalesce(nothing, missing)
missing

julia> coalesce(missing, Some(missing))
missing
```
"""
function coalesce end

coalesce(x::Any) = x
_throw_coalesce_error(x) =
    throw(ArgumentError(
        """coalesce requires that least one argument differs from `nothing` or `missing`.
           Pass `Some($x)` as the last argument to force returning `$x`."""))
coalesce(x::Nothing) = _throw_coalesce_error(x)
coalesce(x::Missing) = _throw_coalesce_error(x)
coalesce(x::Any, y...) = x
coalesce(x::Some, y...) = x.value
coalesce(x::Union{Nothing, Missing}, y...) = coalesce(y...)
coalesce(x::Nothing, y::Missing, args...) = missing
coalesce(x::Missing, y::Nothing, args...) = nothing

"""
    notnothing(x)

Throw an error if `x === nothing`, and return `x` if not.
"""
notnothing(x::Any) = x
notnothing(::Nothing) = throw(ArgumentError("nothing passed to notnothing"))
