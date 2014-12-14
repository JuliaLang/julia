# TODO
# Comprehensive arithmetic operators
# Comprehensive elementary math functions
# Many cases of Number should actually be {Bool, Int32, ...} for all Optionable types
# Subclass Option <: Number - this causes many ambiguities
#   zero, one, inverse functions after Number subclass

import Base
import Base.show
import Base.sin
import Base.promote_rule
import Base.convert

immutable Option{T<:Number}
    val::T
end

const _na_bool = 2
const _na_int32 = -2147483648               # Smallest integer
const _na_int64 = -9223372036854775808      # Smallest integer
const _na_float32 = 0x7f8007a2              # NaN + 1954 mantissa
const _na_float64 = 0x7ff00000000007a2      # NaN + 1954 mantissa

NAof(::Bool) = _na_bool
NAof(::Int32) = _na_int32
NAof(::Int64) = _na_int64
NAof(::Float32) = _na_float32
NAof(::Float64) = _na_float64
NAof(x::Complex) = Complex(NAof(x.real), NAof(x.imag))

isNA(v::Any) = false
isNA(v::Option) = v.val == NAof(v.val)


function show(io::IO, x::Option)
    if isNA(x)
        print(io, "NA")
    else
        show(io, x.val)
    end
end


function convert{T<:Number}(::Type{Option{T}}, x::Number)
    if x == NAof(x)  # This should be very rare
        throw(TypeError("Value overlaps with NA sentinel value"))
    else
        return Option{T}(x)
    end
end


# Not certain if all of these are necessary
promote_rule{T<:Number, S<:Number}(::Type{Option{T}}, ::Type{Option{S}}) =
    Option{promote_rule(T, S)}
promote_rule{T<:Number, S<:Number}(::Type{T}, ::Type{Option{S}}) =
    Option{promote_rule(T, S)}
promote_rule{T<:Number, S<:Number}(::Type{Option{T}}, ::Type{S}) =
    Option{promote_rule(T, S)}


# Remove NA values from an Option array
# > x = Array{Option{Int32}}([1, 2, NA, 4])
# > nonnull(x)
# [1, 2, 4]
function nonnull{T}(arr::Array{Option{T}})
    result = Array{T}([])
    for item in x
        if !isNA(item)
            push!(result, item.val)
        end
    end
    return result
end


# Fill an Option array with concrete values
# > x = Array{Option{Int32}}([1, 2, NA, 4])
# > fill(x, -1)
# [1, 2, -1, 4]
function fill{T, S}(arr::Array{Option{T}}, fillvalue::S)
    result = Array{promote_type(T, S)}([])
    for item in x
        if isNA(item)
            push!(result, fillvalue)
        else
            push!(result, item.val)
        end
    end
    return result
end


# Arithmetic binary operators
^(x::Option, y::Integer) = isNA(x) ? x : $op(x.val, y)  # To avoid an ambiguity
for op in [:+, :-, :/, :*, :^, :%]
    @eval function $op(x::Option, y::Option)  # and make sure this is replicated
        if isNA(x)
            return x
        elseif isNA(y)
            return y
        else
            return Option($op(x.val, y.val))
        end
    end
    @eval $op(x::Option, y::Number) = isNA(x) ? x : Option($op(x.val, y))
    @eval $op(x::Number, y::Option) = isNA(y) ? y : Option($op(x, y.val))
end


# Boolean binary operators
for op in [:(Base.(:(==))), :(Base.(:(<=))), :(Base.(:(>=))), :(Base.(:(<))),
    :(Base.(:(>)))]
    @eval function $op(x::Option, y::Option)  # and make sure this is replicated
        if isNA(x) | isNA(y)
            result = NA_Bool
        else
            result = x.val == y.val
        end
        return Option(result)
    end
    @eval $op(x::Option, y::Number) = isNA(x) ? x : Option{Bool}($op(x.val, y))
    @eval $op(x::Number, y::Option) = isNA(y) ? y : Option{Bool}($op(x, y.val))
end


# Unary operators
!(x::Option{Bool}) = isNA(x) ? x : Option{Bool}(!x.val)
-(x::Option{Number}) = isNA(x) ? x : Option(-x.val)


# Mathematical Unary Functions -- TODO: need more extensive list
for op in [:(Base.sin), :(Base.cos), :(Base.tan), :(Base.exp), :(Base.log)]
    @eval $op(x::Option) = isNA(x) ? x : Option($op(x.val))
end


const NA_Bool = Option{Bool}(_na_bool)
const NA_Int32 = Option{Int32}(_na_int32)
const NA_Int64 = Option{Int64}(_na_int64)
const NA_Float32 = Option{Float32}(_na_float32)
const NA_Float64 = Option{Float64}(_na_float64)


# Singleton NA value
# x = Array{Option{Int32}}([1, 2, NA, 4])
# Should this share an abstract `Optional` type with `Option`?
immutable NA_Type end

const NA = NA_Type()
isNA(::NA_Type) = true

promote_rule{T}(::Type{NA_Type}, ::Type{Option{T}}) = Option{T}

convert{T}(::Type{Option{T}}, ::NA_Type) = Option{T}(NAof(T(0)))
function show(io::IO, x::NA_Type)
    print(io, "NA")
end
