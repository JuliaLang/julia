# This file is a part of Julia. License is MIT: http://julialang.org/license

immutable NullException <: Exception
end

"""
    Nullable(x, hasvalue::Bool=true)

Wrap value `x` in an object of type `Nullable`, which indicates whether a value is present.
`Nullable(x)` yields a non-empty wrapper and `Nullable{T}()` yields an empty instance of a
wrapper that might contain a value of type `T`.

`Nullable(x, false)` yields `Nullable{typeof(x)}()` with `x` stored in the result's `value`
field.

# Examples

```jldoctest
julia> Nullable(1)
Nullable{Int64}(1)

julia> Nullable{Int64}()
Nullable{Int64}()

julia> Nullable(1, false)
Nullable{Int64}()

julia> dump(Nullable(1, false))
Nullable{Int64}
  hasvalue: Bool false
  value: Int64 1
```
"""
Nullable{T}(value::T, hasvalue::Bool=true) = Nullable{T}(value, hasvalue)
Nullable() = Nullable{Union{}}()

eltype{T}(::Type{Nullable{T}}) = T

convert{T}(::Type{Nullable{T}}, x::Nullable{T}) = x
convert(   ::Type{Nullable   }, x::Nullable   ) = x

convert{T}(t::Type{Nullable{T}}, x::Any) = convert(t, convert(T, x))

function convert{T}(::Type{Nullable{T}}, x::Nullable)
    return isnull(x) ? Nullable{T}() : Nullable{T}(convert(T, get(x)))
end

convert{T<:Nullable}(::Type{Nullable{T}}, x::T) = Nullable{T}(x)
convert{T}(::Type{Nullable{T}}, x::T) = Nullable{T}(x)
convert{T}(::Type{Nullable   }, x::T) = Nullable{T}(x)

convert{T}(::Type{Nullable{T}}, ::Void) = Nullable{T}()
convert(   ::Type{Nullable   }, ::Void) = Nullable{Union{}}()

promote_rule{S,T}(::Type{Nullable{S}}, ::Type{T}) = Nullable{promote_type(S, T)}
promote_rule{S,T}(::Type{Nullable{S}}, ::Type{Nullable{T}}) = Nullable{promote_type(S, T)}
promote_op{S,T}(op::Any, ::Type{Nullable{S}}, ::Type{Nullable{T}}) = Nullable{promote_op(op, S, T)}
promote_op{S,T}(op::Type, ::Type{Nullable{S}}, ::Type{Nullable{T}}) = Nullable{promote_op(op, S, T)}

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
        ifelse(isnull(x), y, x.value)
    else
        isnull(x) ? y : x.value
    end
end

get(x::Nullable) = isnull(x) ? throw(NullException()) : x.value

"""
    unsafe_get(x)

Return the value of `x` for [`Nullable`](@ref) `x`; return `x` for
all other `x`.

This method does not check whether or not `x` is null before attempting to
access the value of `x` for `x::Nullable` (hence "unsafe").

```jldoctest
julia> x = Nullable(1)
Nullable{Int64}(1)

julia> unsafe_get(x)
1

julia> x = Nullable{String}()
Nullable{String}()

julia> unsafe_get(x)
ERROR: UndefRefError: access to undefined reference
Stacktrace:
 [1] unsafe_get(::Nullable{String}) at ./nullable.jl:125

julia> x = 1
1

julia> unsafe_get(x)
1
```
"""
unsafe_get(x::Nullable) = x.value
unsafe_get(x) = x

"""
    isnull(x)

Return whether or not `x` is null for [`Nullable`](@ref) `x`; return
`false` for all other `x`.

# Examples

```jldoctest
julia> x = Nullable(1, false)
Nullable{Int64}()

julia> isnull(x)
true

julia> x = Nullable(1, true)
Nullable{Int64}(1)

julia> isnull(x)
false

julia> x = 1
1

julia> isnull(x)
false
```
"""
isnull(x::Nullable) = !x.hasvalue
isnull(x) = false

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

typealias NullSafeSignedInts Union{Type{Int128}, Type{Int16}, Type{Int32},
                                   Type{Int64}, Type{Int8}}
typealias NullSafeUnsignedInts Union{Type{Bool}, Type{UInt128}, Type{UInt16},
                                     Type{UInt32}, Type{UInt64}, Type{UInt8}}
typealias NullSafeInts Union{NullSafeSignedInts, NullSafeUnsignedInts}
typealias NullSafeFloats Union{Type{Float16}, Type{Float32}, Type{Float64}}
typealias NullSafeTypes Union{NullSafeInts, NullSafeFloats}
typealias EqualOrLess Union{typeof(isequal), typeof(isless)}

null_safe_op{T}(::typeof(identity), ::Type{T}) = isbits(T)

eltypes() = Tuple{}
eltypes(x, xs...) = Tuple{eltype(x), eltypes(xs...).parameters...}

@pure null_safe_eltype_op(op, xs...) =
    null_safe_op(op, eltypes(xs...).parameters...)

null_safe_op(f::EqualOrLess, ::NullSafeTypes, ::NullSafeTypes) = true
null_safe_op{S,T}(f::EqualOrLess, ::Type{Rational{S}}, ::Type{T}) =
    null_safe_op(f, T, S)
# complex numbers can be compared for equality but not in general ordered
null_safe_op{S,T}(::typeof(isequal), ::Type{Complex{S}}, ::Type{T}) =
    null_safe_op(isequal, T, S)

"""
    isequal(x::Nullable, y::Nullable)

If neither `x` nor `y` is null, compare them according to their values
(i.e. `isequal(get(x), get(y))`). Else, return `true` if both arguments are null,
and `false` if one is null but not the other: nulls are considered equal.
"""
@inline function isequal{S,T}(x::Nullable{S}, y::Nullable{T})
    if null_safe_op(isequal, S, T)
        (isnull(x) & isnull(y)) | (!isnull(x) & !isnull(y) & isequal(x.value, y.value))
    else
        (isnull(x) & isnull(y)) || (!isnull(x) & !isnull(y) && isequal(x.value, y.value))
    end
end

isequal(x::Nullable{Union{}}, y::Nullable{Union{}}) = true
isequal(x::Nullable{Union{}}, y::Nullable) = isnull(y)
isequal(x::Nullable, y::Nullable{Union{}}) = isnull(x)

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
        (!isnull(x) & isnull(y)) | (!isnull(x) & !isnull(y) & isless(x.value, y.value))
    else
        (!isnull(x) & isnull(y)) || (!isnull(x) & !isnull(y) && isless(x.value, y.value))
    end
end

isless(x::Nullable{Union{}}, y::Nullable{Union{}}) = false
isless(x::Nullable{Union{}}, y::Nullable) = false
isless(x::Nullable, y::Nullable{Union{}}) = !isnull(x)

==(x::Nullable, y::Nullable) = throw(NullException())

const nullablehash_seed = UInt === UInt64 ? 0x932e0143e51d0171 : 0xe51d0171

function hash(x::Nullable, h::UInt)
    if isnull(x)
        return h + nullablehash_seed
    else
        return hash(x.value, h + nullablehash_seed)
    end
end

# higher-order functions
"""
    filter(p, x::Nullable)

Return null if either `x` is null or `p(get(x))` is false, and `x` otherwise.
"""
function filter{T}(p, x::Nullable{T})
    if isbits(T)
        val = unsafe_get(x)
        Nullable{T}(val, !isnull(x) && p(val))
    else
        isnull(x) || p(unsafe_get(x)) ? x : Nullable{T}()
    end
end

"""
Return the given type if it is concrete, and `Union{}` otherwise.
"""
nullable_returntype{T}(::Type{T}) = isleaftype(T) ? T : Union{}

"""
    map(f, x::Nullable)

Return `f` applied to the value of `x` if it has one, as a `Nullable`. If `x`
is null, then return a null value of type `Nullable{S}`. `S` is guaranteed to
be either `Union{}` or a concrete type. Whichever of these is chosen is an
implementation detail, but typically the choice that maximizes performance
would be used. If `x` has a value, then the return type is guaranteed to be of
type `Nullable{typeof(f(x))}`.
"""
function map{T}(f, x::Nullable{T})
    S = promote_op(f, T)
    if isleaftype(S) && null_safe_op(f, T)
        Nullable(f(unsafe_get(x)), !isnull(x))
    else
        if isnull(x)
            Nullable{nullable_returntype(S)}()
        else
            Nullable(f(unsafe_get(x)))
        end
    end
end

# We need the following function and specializations because LLVM cannot
# optimize !any(isnull, t) without further guidance.
hasvalue(x::Nullable) = x.hasvalue
hasvalue(x) = true
all(f::typeof(hasvalue), t::Tuple) = f(t[1]) & all(f, tail(t))
all(f::typeof(hasvalue), t::Tuple{}) = true

# Overloads of null_safe_op
# Unary operators

# Note this list does not include sqrt since it can raise a DomainError
for op in (+, -, abs, abs2)
    null_safe_op(::typeof(op), ::NullSafeTypes) = true
    null_safe_op{S}(::typeof(op), ::Type{Complex{S}}) = null_safe_op(op, S)
    null_safe_op{S}(::typeof(op), ::Type{Rational{S}}) = null_safe_op(op, S)
end

null_safe_op(::typeof(~), ::NullSafeInts) = true
null_safe_op(::typeof(!), ::Type{Bool}) = true

# Binary operators

# Note this list does not include ^, ÷ and %
# Operations between signed and unsigned types are not safe: promotion to unsigned
# gives an InexactError for negative numbers
for op in (+, -, *, /, &, |, <<, >>, >>>,
           scalarmin, scalarmax)
    # to fix ambiguities
    null_safe_op(::typeof(op), ::NullSafeFloats, ::NullSafeFloats) = true
    null_safe_op(::typeof(op), ::NullSafeSignedInts, ::NullSafeSignedInts) = true
    null_safe_op(::typeof(op), ::NullSafeUnsignedInts, ::NullSafeUnsignedInts) = true
end
for op in (+, -, *, /)
    null_safe_op{S,T}(::typeof(op), ::Type{Complex{S}}, ::Type{T}) =
        null_safe_op(op, T, S)
    null_safe_op{S,T}(::typeof(op), ::Type{Rational{S}}, ::Type{T}) =
        null_safe_op(op, T, S)
end
