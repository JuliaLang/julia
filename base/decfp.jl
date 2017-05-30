# This file is a part of Julia. License is MIT: http://julialang.org/license

# These define the types for IEEE 754-2008 Decimal Floating Point formats
# Support for these types is available via packages, such as DecFP.jl

"""Abstract numeric encoding format"""
abstract NumericFormat

"""Binary floating point format"""
abstract BinaryFmt <: NumericFormat
"""Decimal floating point (binary) format"""
abstract BinDecFmt <: NumericFormat
"""Decimal floating point (packed) format"""
abstract PackedFmt <: NumericFormat

"""Parameterized abstract floating point type"""
abstract AbstractFlt{fmt} <: AbstractFloat

"""Binary floating point"""
typealias BinaryFloat AbstractFlt{BinaryFmt}
"""Decimal floating point"""
typealias DecimalFloat Union{AbstractFlt{BinDecFmt},AbstractFlt{PackedFmt}}

"""IEEE 754-2008 32-bit Decimal Floating Point, binary format"""
bitstype 32  DecimalB32  <: AbstractFlt{BinDecFmt}
"""IEEE 754-2008 64-bit Decimal Floating Point, binary format"""
bitstype 64  DecimalB64  <: AbstractFlt{BinDecFmt}
"""IEEE 754-2008 128-bit Decimal Floating Point, binary format"""
bitstype 128 DecimalB128 <: AbstractFlt{BinDecFmt}

"""IEEE 754-2008 32-bit Decimal Floating Point, packed format"""
bitstype 32  DecimalP32  <: AbstractFlt{PackedFmt}
"""IEEE 754-2008 64-bit Decimal Floating Point, packed format"""
bitstype 64  DecimalP64  <: AbstractFlt{PackedFmt}
"""IEEE 754-2008 128-bit Decimal Floating Point, packed format"""
bitstype 128 DecimalP128 <: AbstractFlt{PackedFmt}
