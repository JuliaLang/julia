# This file is a part of Julia. License is MIT: https://julialang.org/license

module Base64

export
    Base64EncodePipe,
    base64encode,
    Base64DecodePipe,
    base64decode

include("buffer.jl")
include("encode.jl")
include("decode.jl")

end
