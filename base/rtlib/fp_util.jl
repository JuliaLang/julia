# This file is a part of Julia. License is MIT: http://julialang.org/license
import Base: @pure

typealias RTLIB_FLOAT Union{Float16, Float32, Float64}
const CHAR_BIT = 8

fptoui(::Type{Float16}) = UInt16
fptoui(::Type{Float32}) = UInt32
fptoui(::Type{Float64}) = UInt64
# fptoui(::Type{Float128}) = UInt128

nbits{T<:RTLIB_FLOAT}(::Type{T}) = sizeof(T) * CHAR_BIT

significand_bits(::Type{Float16}) = 10
significand_bits(::Type{Float32}) = 23
significand_bits(::Type{Float64}) = 52
#significand_bits(::Type{Float128}) = 112

@pure exponent_bits{T<:RTLIB_FLOAT}(::Type{T}) = nbits(T) - significand_bits(T) - 1
@pure exponent_inf{T<:RTLIB_FLOAT}(::Type{T}) = (one(fptoui(T)) << exponent_bits(T)) - one(fptoui(T))
@pure exponent_bias{T<:RTLIB_FLOAT}(::Type{T}) = exponent_inf(T) >> 1

@pure sign_mask{T<:RTLIB_FLOAT}(::Type{T}) = one(fptoui(T)) << (significand_bits(T) + exponent_bits(T))