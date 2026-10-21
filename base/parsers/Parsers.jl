# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Base.Parsers

Parsing values from bytes. [`parsevalue`](@ref Base.Parsers.parsevalue) parses a value from
all of a byte vector and returns a [`ParseFailure`](@ref Base.Parsers.ParseFailure) instead
of throwing, for callers that already know where a field starts and ends. [`parse`](@ref)
and [`tryparse`](@ref) are built on the same code.

!!! compat "Julia 1.15"
    `Base.Parsers` requires at least Julia 1.15.
"""
module Parsers

using ..Base: BitInteger, CodeUnits, DenseUTF8String, FastContiguousSubArray, IEEEFloat,
    exponent_bias, exponent_raw_max, require_one_based_indexing, significand_bits, uinttype
using ..Base.Checked: add_with_overflow, mul_with_overflow

public parsevalue, ParseFailure, INVALID, OVERFLOW, UNDERFLOW

"""
    Base.Parsers.ParseFailure

Returned by [`parsevalue`](@ref Base.Parsers.parsevalue) instead of a value. It is
[`INVALID`](@ref Base.Parsers.INVALID), [`OVERFLOW`](@ref Base.Parsers.OVERFLOW) or
[`UNDERFLOW`](@ref Base.Parsers.UNDERFLOW); more kinds may be added, so check
`x isa ParseFailure` to test for any failure.
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

The [`ParseFailure`](@ref Base.Parsers.ParseFailure) for a well-formed value too large in
magnitude for the type.
"""
const OVERFLOW = ParseFailure(0x01)

"""
    Base.Parsers.UNDERFLOW

The [`ParseFailure`](@ref Base.Parsers.ParseFailure) for a nonzero floating-point value that
rounds to zero in the type.
"""
const UNDERFLOW = ParseFailure(0x02)

Base.show(io::IO, x::ParseFailure) =
    print(io, "Base.Parsers.", ("INVALID", "OVERFLOW", "UNDERFLOW")[x.kind + 1])

"""
    Base.Parsers.parsevalue(T, bytes; kw...) -> Union{T, ParseFailure}

Parse all of `bytes`, a one-based `AbstractVector{UInt8}`, as a `T`, or return a
[`ParseFailure`](@ref Base.Parsers.ParseFailure) instead of throwing. Pass a view, such as
`view(buf, i:j)`, to parse part of a buffer.

For integer types, the bytes are an optional `-` or `+` (signed types only) followed by
digits in `base` (keyword, 2 to 62, default 10), with letters for digits above 9 as in
[`parse`](@ref).

For `Float64`, `Float32` and `Float16`, the bytes are a number as [`parse`](@ref) reads it,
without surrounding whitespace: an optional sign and a decimal or hexadecimal
floating-point number, or `inf`, `infinity` or `nan` in any case. A number is rounded to
the nearest value, ties to even; one too large for the type is `OVERFLOW`, and a nonzero
one that rounds to zero is `UNDERFLOW`. `nan(n)` puts the unsigned integer `n` (decimal,
`0x` hexadecimal or `0` octal) in the NaN's payload. The keyword `decimal` (default `'.'`)
is the decimal point; it can be any ASCII character other than a letter, digit, `+` or `-`.

For `UUID`, the bytes are 32 hexadecimal digits in groups of 8, 4, 4, 4 and 12 separated
by `-`, as [`string`](@ref) writes them, in either case.

!!! compat "Julia 1.15"
    `parsevalue` requires at least Julia 1.15.

# Examples
```jldoctest
julia> Base.Parsers.parsevalue(Int, codeunits("-42"))
-42

julia> Base.Parsers.parsevalue(UInt8, view(codeunits("x=256;"), 3:5))
Base.Parsers.OVERFLOW

julia> Base.Parsers.parsevalue(Int, UInt8[0x37, 0x66]; base = 16)
127

julia> Base.Parsers.parsevalue(Float64, codeunits("-1.5e-3"))
-0.0015

julia> Base.Parsers.parsevalue(Float32, codeunits("3,25"); decimal = ',')
3.25f0
```
"""
function parsevalue end

include("parsers/ints.jl")
include("parsers/floats.jl")

end # module Parsers
