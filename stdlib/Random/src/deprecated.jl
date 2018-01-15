# This file is a part of Julia. License is MIT: https://julialang.org/license

# PR #21359

@deprecate srand(r::MersenneTwister, filename::AbstractString, n::Integer=4) srand(r, read!(filename, Vector{UInt32}(uninitialized, Int(n))))
@deprecate srand(filename::AbstractString, n::Integer=4) srand(read!(filename, Vector{UInt32}(uninitialized, Int(n))))
@deprecate MersenneTwister(filename::AbstractString)  srand(MersenneTwister(0), read!(filename, Vector{UInt32}(uninitialized, Int(4))))

function randjump(mt::MersenneTwister, jumps::Integer, jumppoly::AbstractString)
    depwarn("`randjump(rng, jumps, jumppoly::AbstractString)` is deprecated; use `randjump(rng, steps, jumps)` instead", :randjump)
    Base.Random._randjump(mt, dSFMT.GF2X(jumppoly), jumps)
end

@deprecate randjump(mt::MersenneTwister, jumps::Integer)  randjump(mt, big(10)^20, jumps)

@deprecate convert(::Type{UInt128},     u::UUID)     UInt128(u)
@deprecate convert(::Type{UUID}, s::AbstractString)  UUID(s)
