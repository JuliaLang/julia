# This file is a part of Julia. License is MIT: http://julialang.org/license

cmp(a::Symbol, b::Symbol) = Int(sign(ccall(:strcmp, Int32, (Cstring, Cstring), a, b)))
isless(a::Symbol, b::Symbol) = cmp(a,b) < 0

