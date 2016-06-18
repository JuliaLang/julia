# This file is a part of Julia. License is MIT: http://julialang.org/license

immutable NullException <: Exception
end

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

isnull(x::Nullable) = x.isnull

const nullablehash_seed = UInt === UInt64 ? 0x932e0143e51d0171 : 0xe51d0171

function hash(x::Nullable, h::UInt)
    if x.isnull
        return h + nullablehash_seed
    else
        return hash(x.value, h + nullablehash_seed)
    end
end


## Operators

"""
    null_safe_op(f::Any, ::Type)::Bool
    null_safe_op(f::Any, ::Type, ::Type)::Bool

Returns whether an operation `f` can safely be applied to any value of the passed type(s).
Returns `false` by default.

Custom types should implement methods for some or all operations `f` when applicable:
returning `true` means that the operation may be called on any value without
throwing an error. In particular, this means that the operation can be applied on
the whole domain of the type *and on uninitialized objects*. Though returning invalid or
nonsensical results is not a problem. As a general rule, these proporties are only true for
unchecked operations on `isbits` types.

Types declared as safe can benefit from higher performance for operations on nullable: by
always computing the result even for null values, a branch is avoided, which helps
vectorization.
"""
null_safe_op(f::Any, ::Type) = false
null_safe_op(f::Any, ::Type, ::Type) = false

typealias UncheckedTypes Union{Checked.SignedInt, Checked.UnsignedInt}

# Unary operators

for op in (:+, :-, :~)
    @eval begin
        null_safe_op{T<:UncheckedTypes}(::typeof($op), ::Type{T}) = true
    end
end

null_safe_op(::typeof(!), ::Type{Bool}) = true

for op in (:+, :-, :!, :~)
    @eval begin
        @inline function $op{S}(x::Nullable{S})
            R = promote_op($op, S)
            if null_safe_op($op, S)
                Nullable{R}($op(x.value), x.isnull)
            else
                x.isnull ? Nullable{R}() :
                           Nullable{R}($op(x.value))
            end
        end
    end
end

# Binary operators

# Note this list does not include ^, ÷ and %
# Operations between signed and unsigned types are not safe: promotion to unsigned
# gives an InexactError for negative numbers
for op in (:+, :-, :*, :/, :&, :|, :<<, :>>, :(>>>),
           :(==), :<, :>, :<=, :>=)
    @eval begin
        null_safe_op{S<:Checked.SignedInt,
                     T<:Checked.SignedInt}(::typeof($op), ::Type{S}, ::Type{T}) = true
        null_safe_op{S<:Checked.UnsignedInt,
                     T<:Checked.UnsignedInt}(::typeof($op), ::Type{S}, ::Type{T}) = true
    end
end

for op in (:+, :-, :*, :/, :%, :÷, :&, :|, :^, :<<, :>>, :(>>>),
           :(==), :<, :>, :<=, :>=)
    @eval begin
        @inline function $op{S,T}(x::Nullable{S}, y::Nullable{T})
            R = promote_op($op, S, T)
            if null_safe_op($op, S, T)
                Nullable{R}($op(x.value, y.value), x.isnull | y.isnull)
            else
                (x.isnull | y.isnull) ? Nullable{R}() :
                                        Nullable{R}($op(x.value, y.value))
            end
        end
    end
end

@inline function isequal{S,T}(x::Nullable{S}, y::Nullable{T})
    if null_safe_op(isequal, S, T)
        (x.isnull & y.isnull) | ((!x.isnull & !y.isnull) & isequal(x.value, y.value))
    else
        (x.isnull & y.isnull) || ((!x.isnull & !y.isnull) && isequal(x.value, y.value))
    end
end
