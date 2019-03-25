# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Base64

Functionality for base-64 encoded strings and IO.
"""
module Base64

using Base: require_one_based_indexing

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
    stringmime(mime, x; context=nothing)

Returns an `AbstractString` containing the representation of `x` in the
requested `mime` type. This is similar to [`repr(mime, x)`](@ref) except
that binary data is base64-encoded as an ASCII string.

The optional keyword argument `context` can be set to `:key=>value` pair
or an `IO` or [`IOContext`](@ref) object whose attributes are used for the I/O
stream passed to [`show`](@ref).
"""
stringmime(m::MIME, x; context=nothing) = istextmime(m) ? Base.Multimedia._textrepr(m, x, context) : _binstringmime(m, x, context)
stringmime(m::AbstractString, x; context=nothing) = stringmime(MIME(m), x; context=context)

_binstringmime(m::MIME, x, context) = Base64.base64encode(show, m, x; context=context)
_binstringmime(m::MIME, x::Vector{UInt8}, context) = Base64.base64encode(write, x; context=context)

end
