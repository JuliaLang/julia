# This file is a part of Julia. License is MIT: https://julialang.org/license

# MWE from https://github.com/JuliaLang/julia/issues/49501
N = 1_000_000  # or larger
T = BigFloat

struct Q{T}
    a::T
    b::T
end

# Memoy use is ~512MB
let
    A = [Q(rand(T), rand(T)) for _ in 1:N]
end

GC.gc()
