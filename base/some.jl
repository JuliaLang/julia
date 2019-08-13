# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Some{T}

A wrapper type used in `Union{Some{T}, Nothing}` to distinguish between the absence
of a value ([`nothing`](@ref)) and the presence of a `nothing` value (i.e. `Some(nothing)`).

Use [`something`](@ref) to access the value wrapped by a `Some` object.
"""
struct Some{T}
    value::T
end

promote_rule(::Type{Some{T}}, ::Type{Some{S}}) where {T, S<:T} = Some{T}
promote_rule(::Type{Some{T}}, ::Type{Nothing}) where {T} = Union{Some{T}, Nothing}

convert(::Type{Some{T}}, x::Some) where {T} = Some{T}(convert(T, x.value))
convert(::Type{Some{T}}, x::Some{T}) where {T} = x
convert(::Type{Union{Some{T}, Nothing}}, x::Some) where {T} = convert(Some{T}, x)

convert(::Type{Union{T, Nothing}}, x::Union{T, Nothing}) where {T} = x
convert(::Type{Union{T, Nothing}}, x::Any) where {T} = convert(T, x)
convert(::Type{Nothing}, x::Nothing) = nothing
convert(::Type{Nothing}, x::Any) = throw(MethodError(convert, (Nothing, x)))

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
    notnothing(x)

Throw an error if `x === nothing`, and return `x` if not.
"""
notnothing(x::Any) = x
notnothing(::Nothing) = throw(ArgumentError("nothing passed to notnothing"))

"""
    isnothing(x)

Return `true` if `x === nothing`, and return `false` if not.

!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.
"""
isnothing(::Any) = false
isnothing(::Nothing) = true


"""
    something(x, y...)

Return the first value in the arguments which is not equal to [`nothing`](@ref),
if any. Otherwise throw an error.
Arguments of type [`Some`](@ref) are unwrapped.

# Examples
```jldoctest
julia> something(nothing, 1)
1

julia> something(Some(1), nothing)
1

julia> something(missing, nothing)
missing

julia> something(nothing, nothing)
ERROR: ArgumentError: No value arguments present
```
"""
function something end

something() = throw(ArgumentError("No value arguments present"))
something(x::Nothing, y...) = something(y...)
something(x::Some, y...) = x.value
something(x::Any, y...) = x
