# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    NullException()

An attempted access to a [`Nullable`](@ref) with no defined value.

# Examples
```jldoctest
julia> a = Nullable{Int}()
Nullable{Int64}()

julia> get(a)
ERROR: NullException()
Stacktrace:
[...]
```
"""
struct NullException <: Exception
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
Nullable(value::T, hasvalue::Bool=true) where {T} = Nullable{T}(value, hasvalue)
Nullable() = Nullable{Union{}}()

eltype(::Type{Nullable{T}}) where {T} = T

convert(::Type{T}, x::T) where {T<:Nullable} = x
convert(::Type{Nullable   }, x::Nullable   ) = x

function convert(::Type{Nullable{T}}, x::Nullable) where T
    return isnull(x) ? Nullable{T}() : Nullable{T}(convert(T, get(x)))
end

convert(::Type{T}, x::Any ) where {T<:Nullable} = T(x)
convert(::Type{T}, x::Void) where {T<:Nullable} = T()

promote_rule(::Type{Nullable{S}}, ::Type{T}) where {S,T} = Nullable{promote_type(S, T)}
promote_rule(::Type{Nullable{S}}, ::Type{Nullable{T}}) where {S,T} = Nullable{promote_type(S, T)}
promote_op(op::Any, ::Type{Nullable{S}}, ::Type{Nullable{T}}) where {S,T} = Nullable{promote_op(op, S, T)}
promote_op(op::Type, ::Type{Nullable{S}}, ::Type{Nullable{T}}) where {S,T} = Nullable{promote_op(op, S, T)}

function show(io::IO, x::Nullable)
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

# Examples
```jldoctest
julia> get(Nullable(5))
5

julia> get(Nullable())
ERROR: NullException()
Stacktrace:
[...]
```
"""
@inline function get(x::Nullable{T}, y) where T
    if isbits(T)
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

# Examples
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
[...]

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

const NullSafeSignedInts = Union{Type{Int128}, Type{Int16}, Type{Int32},
                                 Type{Int64}, Type{Int8}}
const NullSafeUnsignedInts = Union{Type{Bool}, Type{UInt128}, Type{UInt16},
                                   Type{UInt32}, Type{UInt64}, Type{UInt8}}
const NullSafeInts = Union{NullSafeSignedInts, NullSafeUnsignedInts}
const NullSafeFloats = Union{Type{Float16}, Type{Float32}, Type{Float64}}
const NullSafeTypes = Union{NullSafeInts, NullSafeFloats}
const EqualOrLess = Union{typeof(isequal), typeof(isless)}

null_safe_op(::typeof(identity), ::Type{T}) where {T} = isbits(T)

null_safe_op(f::EqualOrLess, ::NullSafeTypes, ::NullSafeTypes) = true
null_safe_op(f::EqualOrLess, ::Type{Rational{S}}, ::Type{T}) where {S,T} =
    null_safe_op(f, T, S)
# complex numbers can be compared for equality but not in general ordered
null_safe_op(::typeof(isequal), ::Type{Complex{S}}, ::Type{T}) where {S,T} =
    null_safe_op(isequal, T, S)

"""
    isequal(x::Nullable, y::Nullable)

If neither `x` nor `y` is null, compare them according to their values
(i.e. `isequal(get(x), get(y))`). Else, return `true` if both arguments are null,
and `false` if one is null but not the other: nulls are considered equal.

# Examples
```jldoctest
julia> isequal(Nullable(5), Nullable(5))
true

julia> isequal(Nullable(5), Nullable(4))
false

julia> isequal(Nullable(5), Nullable())
false

julia> isequal(Nullable(), Nullable())
true
```
"""
@inline function isequal(x::Nullable{S}, y::Nullable{T}) where {S,T}
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

# Examples
```jldoctest
julia> isless(Nullable(6), Nullable(5))
false

julia> isless(Nullable(5), Nullable(6))
true

julia> isless(Nullable(5), Nullable(4))
false

julia> isless(Nullable(5), Nullable())
true

julia> isless(Nullable(), Nullable())
false

julia> isless(Nullable(), Nullable(5))
false
```
"""
@inline function isless(x::Nullable{S}, y::Nullable{T}) where {S,T}
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

# Examples
```jldoctest
julia> filter(isodd, Nullable(5))
Nullable{Int64}(5)

julia> filter(isodd, Nullable(4))
Nullable{Int64}()

julia> filter(isodd, Nullable{Int}())
Nullable{Int64}()
```
"""
function filter(p, x::Nullable{T}) where T
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
nullable_returntype(::Type{T}) where {T} = _isleaftype(T) ? T : Union{}

"""
    map(f, x::Nullable)

Return `f` applied to the value of `x` if it has one, as a `Nullable`. If `x`
is null, then return a null value of type `Nullable{S}`. `S` is guaranteed to
be either `Union{}` or a concrete type. Whichever of these is chosen is an
implementation detail, but typically the choice that maximizes performance
would be used. If `x` has a value, then the return type is guaranteed to be of
type `Nullable{typeof(f(x))}`.

# Examples
```jldoctest
julia> map(isodd, Nullable(1))
Nullable{Bool}(true)

julia> map(isodd, Nullable(2))
Nullable{Bool}(false)

julia> map(isodd, Nullable{Int}())
Nullable{Bool}()
```
"""
function map(f, x::Nullable{T}) where T
    S = promote_op(f, T)
    if _isleaftype(S) && null_safe_op(f, T)
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
    global null_safe_op
    null_safe_op(::typeof(op), ::NullSafeTypes) = true
    null_safe_op(::typeof(op), ::Type{Complex{S}}) where {S} = null_safe_op(op, S)
    null_safe_op(::typeof(op), ::Type{Rational{S}}) where {S} = null_safe_op(op, S)
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
    global null_safe_op
    null_safe_op(::typeof(op), ::NullSafeFloats, ::NullSafeFloats) = true
    null_safe_op(::typeof(op), ::NullSafeSignedInts, ::NullSafeSignedInts) = true
    null_safe_op(::typeof(op), ::NullSafeUnsignedInts, ::NullSafeUnsignedInts) = true
end
for op in (+, -, *, /)
    global null_safe_op
    null_safe_op(::typeof(op), ::Type{Complex{S}}, ::Type{T}) where {S,T} =
        null_safe_op(op, T, S)
    null_safe_op(::typeof(op), ::Type{Rational{S}}, ::Type{T}) where {S,T} =
        null_safe_op(op, T, S)
end
