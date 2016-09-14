# This file is a part of Julia. License is MIT: http://julialang.org/license

"""
    NullException()

An attempted access to a [`Nullable`](:obj:`Nullable`) with no defined value.
"""
immutable NullException <: Exception
end

"""
    Nullable(x, isnull::Bool=false)

Wrap value `x` in an object of type `Nullable`, which indicates whether a value is present.
`Nullable(x)` yields a non-empty wrapper, and `Nullable{T}()` yields an empty instance of a
wrapper that might contain a value of type `T`.

```jldoctest
julia> Nullable()
Nullable{Union{}}()

julia> Nullable(2)
Nullable{Int64}(2)

julia> Nullable(0, true)
Nullable{Int64}()

julia> Nullable(0, false)
Nullable{Int64}(0)
```
"""
Nullable{T}(value::T, isnull::Bool=false) = Nullable{T}(value, isnull)
Nullable() = Nullable{Union{}}()

eltype{T}(::Type{Nullable{T}}) = T

convert{T}(::Type{Nullable{T}}, x::Nullable{T}) = x
convert(   ::Type{Nullable   }, x::Nullable   ) = x

convert{T}(t::Type{Nullable{T}}, x::Any) = convert(t, convert(T, x))

function convert{T}(::Type{Nullable{T}}, x::Nullable)
    return isnull(x) ? Nullable{T}() : Nullable{T}(convert(T, get(x)))
end

convert{T}(::Type{Nullable{T}}, x::T) = Nullable{T}(x)
convert{T}(::Type{Nullable   }, x::T) = Nullable{T}(x)

convert{T}(::Type{Nullable{T}}, ::Void) = Nullable{T}()
convert(   ::Type{Nullable   }, ::Void) = Nullable{Union{}}()

promote_rule{S,T}(::Type{Nullable{S}}, ::Type{T}) = Nullable{promote_type(S, T)}
promote_rule{S,T}(::Type{Nullable{S}}, ::Type{Nullable{T}}) = Nullable{promote_type(S, T)}
promote_op{S,T}(op::Any, ::Type{Nullable{S}}, ::Type{Nullable{T}}) = Nullable{promote_op(op, S, T)}

function show{T}(io::IO, x::Nullable{T})
    if get(io, :compact, false)
        if isnull(x)
            print(io, "#NULL")
        else
            show(io, x.value)
        end
    else
        print(io, "Nullable{")
        showcompact(io, eltype(x))
        print(io, "}(")
        if !isnull(x)
            showcompact(io, x.value)
        end
        print(io, ')')
    end
end

"""
    get(x::Nullable[, y])

Attempt to access the value of `x`. Returns the value if it is present;
otherwise, returns `y` if provided, or throws a `NullException` if not.
"""
@inline function get{S,T}(x::Nullable{S}, y::T)
    if isbits(S)
        ifelse(x.isnull, y, x.value)
    else
        x.isnull ? y : x.value
    end
end

get(x::Nullable) = x.isnull ? throw(NullException()) : x.value


"""
    isnull(x::Nullable) -> Bool

Is the [`Nullable`](:obj:`Nullable`) object `x` null, i.e. missing a value?

```jldoctest
julia> x = Nullable(1, false)
Nullable{Int64}(1)

julia> isnull(x)
false

julia> x = Nullable(1, true)
Nullable{Int64}()

julia> isnull(x)
true
```
"""
isnull(x::Nullable) = x.isnull


## Operators

"""
    null_safe_op(f::Any, ::Type, ::Type...)::Bool

Returns whether an operation `f` can safely be applied to any value of the passed type(s).
Returns `false` by default.

Custom types should implement methods for some or all operations `f` when applicable:
returning `true` means that the operation may be called on any bit pattern without
throwing an error (though returning invalid or nonsensical results is not a problem).
In particular, this means that the operation can be applied on the whole domain of the
type *and on uninitialized objects*. As a general rule, these properties are only true for
safe operations on `isbits` types.

Types declared as safe can benefit from higher performance for operations on nullable: by
always computing the result even for null values, a branch is avoided, which helps
vectorization.
"""
null_safe_op(f::Any, ::Type, ::Type...) = false

typealias NullSafeSignedInts Union{Int128, Int16, Int32, Int64, Int8}
typealias NullSafeUnsignedInts Union{Bool, UInt128, UInt16, UInt32, UInt64, UInt8}
typealias NullSafeInts Union{NullSafeSignedInts, NullSafeUnsignedInts}
typealias NullSafeFloats Union{Float16, Float32, Float64}
typealias NullSafeTypes Union{NullSafeInts, NullSafeFloats}

null_safe_op{S<:NullSafeTypes,
             T<:NullSafeTypes}(::typeof(isequal), ::Type{S}, ::Type{T}) = true
null_safe_op{S<:NullSafeTypes,
             T<:NullSafeTypes}(::typeof(isequal), ::Type{Complex{S}}, ::Type{Complex{T}}) = true
null_safe_op{S<:NullSafeTypes,
             T<:NullSafeTypes}(::typeof(isequal), ::Type{Rational{S}}, ::Type{Rational{T}}) = true

"""
    isequal(x::Nullable, y::Nullable)

If neither `x` nor `y` is null, compare them according to their values
(i.e. `isequal(get(x), get(y))`). Else, return `true` if both arguments are null,
and `false` if one is null but not the other: nulls are considered equal.
"""
@inline function isequal{S,T}(x::Nullable{S}, y::Nullable{T})
    if null_safe_op(isequal, S, T)
        (x.isnull & y.isnull) | (!x.isnull & !y.isnull & isequal(x.value, y.value))
    else
        (x.isnull & y.isnull) || (!x.isnull & !y.isnull && isequal(x.value, y.value))
    end
end

isequal(x::Nullable{Union{}}, y::Nullable{Union{}}) = true
isequal(x::Nullable{Union{}}, y::Nullable) = y.isnull
isequal(x::Nullable, y::Nullable{Union{}}) = x.isnull

null_safe_op{S<:NullSafeTypes,
             T<:NullSafeTypes}(::typeof(isless), ::Type{S}, ::Type{T}) = true
null_safe_op{S<:NullSafeTypes,
             T<:NullSafeTypes}(::typeof(isless), ::Type{Complex{S}}, ::Type{Complex{T}}) = true
null_safe_op{S<:NullSafeTypes,
             T<:NullSafeTypes}(::typeof(isless), ::Type{Rational{S}}, ::Type{Rational{T}}) = true

"""
    isless(x::Nullable, y::Nullable)

If neither `x` nor `y` is null, compare them according to their values
(i.e. `isless(get(x), get(y))`). Else, return `true` if only `y` is null, and `false`
otherwise: nulls are always considered greater than non-nulls, but not greater than
another null.
"""
@inline function isless{S,T}(x::Nullable{S}, y::Nullable{T})
    # NULL values are sorted last
    if null_safe_op(isless, S, T)
        (!x.isnull & y.isnull) | (!x.isnull & !y.isnull & isless(x.value, y.value))
    else
        (!x.isnull & y.isnull) || (!x.isnull & !y.isnull && isless(x.value, y.value))
    end
end

isless(x::Nullable{Union{}}, y::Nullable{Union{}}) = false
isless(x::Nullable{Union{}}, y::Nullable) = false
isless(x::Nullable, y::Nullable{Union{}}) = !x.isnull

==(x::Nullable, y::Nullable) = throw(NullException())

const nullablehash_seed = UInt === UInt64 ? 0x932e0143e51d0171 : 0xe51d0171

function hash(x::Nullable, h::UInt)
    if x.isnull
        return h + nullablehash_seed
    else
        return hash(x.value, h + nullablehash_seed)
    end
end
