immutable Nullable{T}
    isnull::Bool
    value::T

    Nullable() = new(true)
    Nullable(value::T) = new(false, value)
end

immutable NullException <: Exception
end

Nullable{T}(value::T) = Nullable{T}(value)

eltype{T}(::Type{Nullable{T}}) = T

eltype{T}(x::Nullable{T}) = T

function convert{S, T}(::Type{Nullable{T}}, x::Nullable{S})
    return isnull(x) ? Nullable{T}() : Nullable(convert(T, get(x)))
end

function show{T}(io::IO, x::Nullable{T})
    if x.isnull
        @printf(io, "Nullable{%s}()", repr(T))
    else
        @printf(io, "Nullable(%s)", repr(x.value))
    end
end

get(x::Nullable) = x.isnull ? throw(NullException()) : x.value

get{S, T}(x::Nullable{S}, y::T) = x.isnull ? convert(S, y) : x.value

isnull(x::Nullable) = x.isnull

function isequal{S, T}(x::Nullable{S}, y::Nullable{T})
    if x.isnull && y.isnull
        return true
    elseif x.isnull || y.isnull
        return false
    else
        return isequal(x.value, y.value)
    end
end

=={S, T}(x::Nullable{S}, y::Nullable{T}) = throw(NullException())

const nullablehash_seed = UInt === UInt64 ? 0x932e0143e51d0171 : 0xe51d0171

function hash(x::Nullable, h::UInt)
    if x.isnull
        return h + nullablehash_seed
    else
        return hash(x.value, h + nullablehash_seed)
    end
end
