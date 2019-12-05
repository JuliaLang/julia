module Ryu

import .Base: significand_bits, significand_mask, exponent_bits, exponent_mask, exponent_bias, exponent_max, uinttype

include("utils.jl")
include("shortest.jl")
include("fixed.jl")
include("exp.jl")

"""
    Ryu.neededdigits(T)

Number of digits necessary to represent type `T` in fixed-precision decimal.
"""
neededdigits(::Type{Float64}) = 309 + 17
neededdigits(::Type{Float32}) = 39 + 9 + 2
neededdigits(::Type{Float16}) = 9 + 5 + 9

"""
    Ryu.writeshortest(x, plus=false, space=false, hash=true, precision=-1, expchar=UInt8('e'), padexp=false, decchar=UInt8('.'), typed=false, compact=false)
    Ryu.writeshortest(buf::Vector{UInt8}, pos::Int, x, args...)

Convert a float value `x` into its "shortest" decimal string, which can be parsed back to the same value.
This function allows achieving the `%g` printf format.
Note the 2nd method allows passing in a byte buffer and position directly; callers must ensure the buffer has sufficient room to hold the entire decimal string.

Various options for the output format include:
  * `plus`: for positive `x`, prefix decimal string with a `'+'` character
  * `space`: for positive `x`, prefix decimal string with a `' '` character; overridden if `plus=true`
  * `hash`: whether the decimal point should be written, even if no additional digits are needed for precision
  * `precision`: minimum number of digits to be included in the decimal string; extra `'0'` characters will be added for padding if necessary
  * `expchar`: character to use exponent component in scientific notation
  * `padexp`: whether two digits should always be written, even for single-digit exponents (e.g. `e+1` becomes `e+01`)
  * `decchar`: decimal point character to be used
  * `typed`: whether additional type information should be printed for `Float16` / `Float32`
  * `compact`: output will be limited to 6 significant digits
"""
function writeshortest(x::T,
        plus::Bool=false,
        space::Bool=false,
        hash::Bool=true,
        precision::Integer=-1,
        expchar::UInt8=UInt8('e'),
        padexp::Bool=false,
        decchar::UInt8=UInt8('.'),
        typed::Bool=false,
        compact::Bool=false) where {T <: Base.IEEEFloat}
    buf = Base.StringVector(neededdigits(T))
    pos = writeshortest(buf, 1, x, plus, space, hash, precision, expchar, padexp, decchar, typed, compact)
    return String(resize!(buf, pos - 1))
end

"""
    Ryu.writefixed(x, precision, plus=false, space=false, hash=false, decchar=UInt8('.'), trimtrailingzeros=false)
    Ryu.writefixed(buf::Vector{UInt8}, pos::Int, x, args...)

Convert a float value `x` into a "fixed" size decimal string of the provided precision.
This function allows achieving the `%f` printf format.
Note the 2nd method allows passing in a byte buffer and position directly; callers must ensure the buffer has sufficient room to hold the entire decimal string.

Various options for the output format include:
  * `plus`: for positive `x`, prefix decimal string with a `'+'` character
  * `space`: for positive `x`, prefix decimal string with a `' '` character; overridden if `plus=true`
  * `hash`: whether the decimal point should be written, even if no additional digits are needed for precision
  * `precision`: minimum number of significant digits to be included in the decimal string; extra `'0'` characters will be added for padding if necessary
  * `decchar`: decimal point character to be used
  * `trimtrailingzeros`: whether trailing zeros should be removed
"""
function writefixed(x::T,
    precision::Integer,
    plus::Bool=false,
    space::Bool=false,
    hash::Bool=false,
    decchar::UInt8=UInt8('.'),
    trimtrailingzeros::Bool=false) where {T <: Base.IEEEFloat}
    buf = Base.StringVector(precision + neededdigits(T))
    pos = writefixed(buf, 1, x, precision, plus, space, hash, decchar, trimtrailingzeros)
    return String(resize!(buf, pos - 1))
end

"""
    Ryu.writeexp(x, precision, plus=false, space=false, hash=false, expchar=UInt8('e'), decchar=UInt8('.'), trimtrailingzeros=false)
    Ryu.writeexp(buf::Vector{UInt8}, pos::Int, x, args...)

Convert a float value `x` into a scientific notation decimal string.
This function allows achieving the `%e` printf format.
Note the 2nd method allows passing in a byte buffer and position directly; callers must ensure the buffer has sufficient room to hold the entire decimal string.

Various options for the output format include:
  * `plus`: for positive `x`, prefix decimal string with a `'+'` character
  * `space`: for positive `x`, prefix decimal string with a `' '` character; overridden if `plus=true`
  * `hash`: whether the decimal point should be written, even if no additional digits are needed for precision
  * `precision`: minimum number of significant digits to be included in the decimal string; extra `'0'` characters will be added for padding if necessary
  * `expchar`: character to use exponent component in scientific notation
  * `decchar`: decimal point character to be used
  * `trimtrailingzeros`: whether trailing zeros should be removed
"""
function writeexp(x::T,
    precision::Integer,
    plus::Bool=false,
    space::Bool=false,
    hash::Bool=false,
    expchar::UInt8=UInt8('e'),
    decchar::UInt8=UInt8('.'),
    trimtrailingzeros::Bool=false) where {T <: Base.IEEEFloat}
    buf = Base.StringVector(precision + neededdigits(T))
    pos = writeexp(buf, 1, x, precision, plus, space, hash, expchar, decchar, trimtrailingzeros)
    return String(resize!(buf, pos - 1))
end

function Base.show(io::IO, x::T, forceuntyped::Bool=false, fromprint::Bool=false) where {T <: Base.IEEEFloat}
    compact = get(io, :compact, false)
    buf = Base.StringVector(neededdigits(T))
    typed = !forceuntyped && !compact && get(io, :typeinfo, Any) != typeof(x)
    pos = writeshortest(buf, 1, x, false, false, true, -1,
        (x isa Float32 && !fromprint) ? UInt8('f') : UInt8('e'), false, UInt8('.'), typed, compact)
    write(io, resize!(buf, pos - 1))
    return
end

function Base.string(x::T) where {T <: Base.IEEEFloat}
    buf = Base.StringVector(neededdigits(T))
    pos = writeshortest(buf, 1, x, false, false, true, -1,
        UInt8('e'), false, UInt8('.'), false, false)
    return String(resize!(buf, pos - 1))
end

Base.print(io::IO, x::Union{Float16, Float32}) = show(io, x, true, true)

end # module
