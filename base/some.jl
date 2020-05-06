# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Some{T}

A wrapper type used in `Union{Some{T}, Nothing}` to distinguish between the absence
of a value ([`nothing`](@ref)) and the presence of a `nothing` value (i.e. `Some(nothing)`).

`Some` is also used in broadcasting to treat it's enclosed value as a scalar.

Use [`something`](@ref) to access the value wrapped by a `Some` object.
"""
struct Some{T}
    value::T
end

promote_rule(::Type{Some{T}}, ::Type{Some{S}}) where {T, S<:T} = Some{T}

nonnothingtype(::Type{T}) where {T} = Core.Compiler.typesubtract(T, Nothing)
promote_rule(T::Type{Nothing}, S::Type) = Union{S, Nothing}
function promote_rule(T::Type{>:Nothing}, S::Type)
    R = nonnothingtype(T)
    R >: T && return Any
    T = R
    R = promote_type(T, S)
    return Union{R, Nothing}
end

function nonnothingtype_checked(T::Type)
    R = nonnothingtype(T)
    R >: T && error("could not compute non-nothing type")
    return R
end

convert(::Type{T}, x::T) where {T>:Nothing} = x
convert(::Type{T}, x) where {T>:Nothing} = convert(nonnothingtype_checked(T), x)
convert(::Type{Some{T}}, x::Some{T}) where {T} = x
convert(::Type{Some{T}}, x::Some) where {T} = Some{T}(convert(T, x.value))

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

See also [`coalesce`](@ref).

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

#Methods for broadcast
getindex(s::Some) = s.value
getindex(s::Some, ::CartesianIndex{0}) = s.value

iterate(s::Some) = (s.value, nothing)
iterate( ::Some, s) = nothing

ndims(::Some) = 0
ndims(::Type{<:Some}) = 0

length(::Some) = 1
size(::Some) = ()
axes(::Some) = ()

IteratorSize(::Type{<:Some}) = Base.HasShape{0}()
broadcastable(s::Some) = s

eltype(::Some{T})       where {T} = T
eltype(::Type{Some{T}}) where {T} = T
