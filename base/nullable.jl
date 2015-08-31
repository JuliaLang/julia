# This file is a part of Julia. License is MIT: http://julialang.org/license

immutable NullException <: Exception
end

Nullable{T}(value::T, isnull::Bool=false) = Nullable{T}(value, isnull)
Nullable() = Nullable{Union{}}()

eltype{T}(::Type{Nullable{T}}) = T

convert{T}(::Type{Nullable{T}}, x::Nullable{T}) = x

function convert{T}(::Type{Nullable{T}}, x::Nullable)
    return isnull(x) ? Nullable{T}() : Nullable{T}(convert(T, get(x)))
end

convert{T}(::Type{Nullable{T}}, x::T) = Nullable{T}(x)

convert{T}(::Type{Nullable{T}}, ::Void) = Nullable{T}()
convert(   ::Type{Nullable   }, ::Void) = Nullable{Union{}}()

function show{T}(io::IO, x::Nullable{T})
    if x.isnull
        print(io, "Nullable{"); show(io, T); print(io, "}()")
    else
        print(io, "Nullable("); show(io, x.value); print(io, ")")
    end
end

get(x::Nullable) = x.isnull ? throw(NullException()) : x.value

@inline function get{T}(x::Nullable{T}, y)
    if isbits(T)
        ifelse(x.isnull, convert(T, y), x.value)
    else
        x.isnull ? convert(T, y) : x.value
    end
end

isnull(x::Nullable) = x.isnull

function isequal(x::Nullable, y::Nullable)
    if x.isnull && y.isnull
        return true
    elseif x.isnull || y.isnull
        return false
    else
        return isequal(x.value, y.value)
    end
end

==(x::Nullable, y::Nullable) = throw(NullException())

const nullablehash_seed = UInt === UInt64 ? 0x932e0143e51d0171 : 0xe51d0171

function hash(x::Nullable, h::UInt)
    if x.isnull
        return h + nullablehash_seed
    else
        return hash(x.value, h + nullablehash_seed)
    end
end
