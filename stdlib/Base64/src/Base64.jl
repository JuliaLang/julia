# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

module Base64

export
    Base64EncodePipe,
    base64encode,
    Base64DecodePipe,
    base64decode,
    stringmime

# Base64EncodePipe is a pipe-like IO object, which converts into base64 data
# sent to a stream. (You must close the pipe to complete the encode, separate
# from closing the target stream).  We also have a function base64encode(f,
# args...) which works like sprint except that it produces base64-encoded data,
# along with base64encode(args...)  which is equivalent to base64encode(write,
# args...), to return base64 strings.  A Base64DecodePipe object can be used to
# decode base64-encoded data read from a stream , while function base64decode is
# useful for decoding strings

include("buffer.jl")
include("encode.jl")
include("decode.jl")

"""
    stringmime(mime, x, context::Pair{Symbol,<:Any}...)

Returns an `AbstractString` containing the representation of `x` in the
requested `mime` type. This is similar to [`repr(mime, x)`](@ref) except
that binary data is base64-encoded as an ASCII string.

If `context` pairs are given, the IO buffer used to capture `show` output
is wrapped in an [`IOContext`](@ref) object with those context pairs.
"""
stringmime(m::MIME, x, context::Pair{Symbol}...) = istextmime(m) ? repr(m, x, context...) : _binstringmime(m, x, context...)
stringmime(m::AbstractString, x, context::Pair{Symbol}...) = stringmime(MIME(m), x, context...)

_binstringmime(m::MIME, x) = Base64.base64encode(show, m, x)
_binstringmime(m::MIME, x, context::Pair{Symbol}...) = Base64.base64encode(io -> show(IOContext(io, m, x, context...)))
_binstringmime(m::MIME, x::Vector{UInt8}, context::Pair{Symbol}...) = Base64.base64encode(write, x)

end
