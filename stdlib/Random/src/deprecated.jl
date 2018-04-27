# This file is a part of Julia. License is MIT: https://julialang.org/license

# PR #25567
Base.@deprecate_binding dSFMT DSFMT

# PR #21359

@deprecate srand(r::MersenneTwister, filename::AbstractString, n::Integer=4) srand(r, read!(filename, Vector{UInt32}(undef, Int(n))))
@deprecate srand(filename::AbstractString, n::Integer=4) srand(read!(filename, Vector{UInt32}(undef, Int(n))))
@deprecate MersenneTwister(filename::AbstractString)  srand(MersenneTwister(0), read!(filename, Vector{UInt32}(undef, Int(4))))

function randjump(mt::MersenneTwister, jumps::Integer, jumppoly::AbstractString)
    depwarn("`randjump(rng, jumps, jumppoly::AbstractString)` is deprecated; use `randjump(rng, steps, jumps)` instead", :randjump)
    Base.Random._randjump(mt, DSFMT.GF2X(jumppoly), jumps)
end

@deprecate randjump(mt::MersenneTwister, jumps::Integer)  randjump(mt, big(10)^20, jumps)

# PR #25429
@deprecate rand(r::AbstractRNG, dims::Dims) rand(r, Float64, dims)
@deprecate rand(                dims::Dims) rand(Float64, dims)

# PR #25668
@deprecate RandomDevice(unlimited::Bool) RandomDevice(; unlimited=unlimited)
