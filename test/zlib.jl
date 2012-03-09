load ("../jl/zlib.jl")

s = "This is a test string"
cs = deflate(s)
us = inflate(cs)

@assert us == s