# This file is a part of Julia. License is MIT: http://julialang.org/license
import Base: @pure

typealias RTLIB_FLOAT Union{Float16, Float32, Float64}
const CHAR_BIT = 8

fptoui(::Type{Float16}) = UInt16
fptoui(::Type{Float32}) = UInt32
fptoui(::Type{Float64}) = UInt64
# fptoui(::Type{Float128}) = UInt128

fptosi(::Type{Float16}) = Int16
fptosi(::Type{Float32}) = Int32
fptosi(::Type{Float64}) = Int64
# fptosi(::Type{Float128}) = Int128

signed(::Type{UInt8}) = Int8
signed(::Type{UInt16}) = Int16
signed(::Type{UInt32}) = Int32
signed(::Type{UInt64}) = Int64
signed(::Type{UInt128}) = Int128

unsigned(::Type{Int8}) = UInt8
unsigned(::Type{Int16}) = UInt16
unsigned(::Type{Int32}) = UInt32
unsigned(::Type{Int64}) = UInt64
unsigned(::Type{Int128}) = UInt128

nbits{T}(::Type{T}) = sizeof(T) * CHAR_BIT

significand_bits(::Type{Float16}) = 10
significand_bits(::Type{Float32}) = 23
significand_bits(::Type{Float64}) = 52
#significand_bits(::Type{Float128}) = 112

@pure exponent_bits{T<:RTLIB_FLOAT}(::Type{T}) = nbits(T) - significand_bits(T) - 1
@pure exponent_inf{T<:RTLIB_FLOAT}(::Type{T}) = (one(fptoui(T)) << exponent_bits(T)) - one(fptoui(T))
@pure exponent_bias{T<:RTLIB_FLOAT}(::Type{T}) = exponent_inf(T) >> 1

@pure sign_mask{T<:RTLIB_FLOAT}(::Type{T}) = one(fptoui(T)) << (significand_bits(T) + exponent_bits(T))

