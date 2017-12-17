# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

module Base64

export
    Base64EncodePipe,
    base64encode,
    Base64DecodePipe,
    base64decode

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

end
