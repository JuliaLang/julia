# This file is a part of Julia. License is MIT: https://julialang.org/license

# PR #25567
Base.@deprecate_binding dSFMT DSFMT

# PR #21359

@deprecate srand(r::MersenneTwister, filename::AbstractString, n::Integer=4) srand(r, read!(filename, Vector{UInt32}(undef, Int(n))))
@deprecate srand(filename::AbstractString, n::Integer=4) srand(read!(filename, Vector{UInt32}(undef, Int(n))))
@deprecate MersenneTwister(filename::AbstractString)  srand(MersenneTwister(0), read!(filename, Vector{UInt32}(undef, Int(4))))

function randjump(mt::MersenneTwister, jumps::Integer, jumppoly::AbstractString)
    Base.depwarn("`randjump(rng, jumps, [jumppoly::AbstractString])` is deprecated; use `Future.randjump` and `accumulate` instead", :randjump)
    _randjump(mt, DSFMT.GF2X(jumppoly), jumps)
end

@deprecate randjump(mt::MersenneTwister, jumps::Integer)  randjump(mt, big(10)^20, jumps)

function randjump(r::MersenneTwister, steps::Integer, len::Integer)
    Base.depwarn("`randjump(rng, steps::Integer, len::Integer` is deprecated; use `Future.randjump` and `accumulate` instead", :randjump)
    _randjump(r, DSFMT.calc_jump(steps), len)
end

function _randjump(mt::MersenneTwister, jumppoly::DSFMT.GF2X, len::Integer)
    mts = MersenneTwister[]
    push!(mts, mt)
    for i in 1:len-1
        cmt = mts[end]
        push!(mts, _randjump(cmt, jumppoly))
    end
    return mts
end

# PR #25429
@deprecate rand(r::AbstractRNG, dims::Dims) rand(r, Float64, dims)
@deprecate rand(                dims::Dims) rand(Float64, dims)

# PR #25668
@deprecate RandomDevice(unlimited::Bool) RandomDevice(; unlimited=unlimited)
