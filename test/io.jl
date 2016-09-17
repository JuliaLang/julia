# This file is a part of Julia. License is MIT: http://julialang.org/license

b = IOBuffer()
@test invoke(write,(IO,IO), b, IOBuffer("abc")) == 3
@test takebuf_string(b) == "abc"
