# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Base.Parsers

Parsing values from bytes. [`parsevalue`](@ref Base.Parsers.parsevalue) parses a value from
all of a byte vector and returns a [`ParseFailure`](@ref Base.Parsers.ParseFailure) instead
of throwing, for callers that already know where a field starts and ends. [`parse`](@ref)
and [`tryparse`](@ref) are built on the same code.
"""
module Parsers

using ..Base: BitInteger, CodeUnits, DenseUTF8String, FastContiguousSubArray,
    require_one_based_indexing
using ..Base.Checked: add_with_overflow, mul_with_overflow

"""
    Base.Parsers.ParseFailure

Returned by [`parsevalue`](@ref Base.Parsers.parsevalue) instead of a value. It is
[`INVALID`](@ref Base.Parsers.INVALID) or [`OVERFLOW`](@ref Base.Parsers.OVERFLOW); more kinds
may be added, so check `x isa ParseFailure` to test for any failure.
"""
struct ParseFailure
    kind::UInt8
end

"""
    Base.Parsers.INVALID

The [`ParseFailure`](@ref Base.Parsers.ParseFailure) for bytes that are not a valid value.
"""
const INVALID = ParseFailure(0x00)

"""
    Base.Parsers.OVERFLOW

The [`ParseFailure`](@ref Base.Parsers.ParseFailure) for a well-formed value that is out of
range for the type.
"""
const OVERFLOW = ParseFailure(0x01)

Base.show(io::IO, x::ParseFailure) =
    print(io, "Base.Parsers.", ("INVALID", "OVERFLOW")[x.kind + 1])

include("parsers/ints.jl")

end # module Parsers
