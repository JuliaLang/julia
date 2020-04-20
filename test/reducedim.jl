# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random

# main tests

function safe_mapslices(op, A, region)
    newregion = intersect(region, 1:ndims(A))
    return isempty(newregion) ? A : mapslices(op, A, dims = newregion)
end
safe_sum(A::Array{T}, region) where {T} = safe_mapslices(sum, A, region)
safe_prod(A::Array{T}, region) where {T} = safe_mapslices(prod, A, region)
safe_maximum(A::Array{T}, region) where {T} = safe_mapslices(maximum, A, region)
safe_minimum(A::Array{T}, region) where {T} = safe_mapslices(minimum, A, region)
safe_sumabs(A::Array{T}, region) where {T} = safe_mapslices(sum, abs.(A), region)
safe_sumabs2(A::Array{T}, region) where {T} = safe_mapslices(sum, abs2.(A), region)
safe_maxabs(A::Array{T}, region) where {T} = safe_mapslices(maximum, abs.(A), region)
safe_minabs(A::Array{T}, region) where {T} = safe_mapslices(minimum, abs.(A), region)

@testset "test reductions over region: $region" for region in Any[
    1, 2, 3, 4, 5, (1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4),
    (1, 2, 3), (1, 3, 4), (2, 3, 4), (1, 2, 3, 4)]
    Areduc = rand(3, 4, 5, 6)
    r = fill(NaN, map(length, Base.reduced_indices(axes(Areduc), region)))
    @test sum!(r, Areduc) ≈ safe_sum(Areduc, region)
    @test prod!(r, Areduc) ≈ safe_prod(Areduc, region)
    @test maximum!(r, Areduc) ≈ safe_maximum(Areduc, region)
    @test minimum!(r, Areduc) ≈ safe_minimum(Areduc, region)
    @test sum!(abs, r, Areduc) ≈ safe_sumabs(Areduc, region)
    @test sum!(abs2, r, Areduc) ≈ safe_sumabs2(Areduc, region)
    @test maximum!(abs, r, Areduc) ≈ safe_maxabs(Areduc, region)
    @test minimum!(abs, r, Areduc) ≈ safe_minabs(Areduc, region)

    # With init=false
    r2 = similar(r)
    fill!(r, 1)
    @test sum!(r, Areduc, init=false) ≈ safe_sum(Areduc, region) .+ 1
    fill!(r, 2.2)
    @test prod!(r, Areduc, init=false) ≈ safe_prod(Areduc, region)*2.2
    fill!(r, 1.8)
    @test maximum!(r, Areduc, init=false) ≈ fill!(r2, 1.8)
    fill!(r, -0.2)
    @test minimum!(r, Areduc, init=false) ≈ fill!(r2, -0.2)
    fill!(r, 8.1)
    @test sum!(abs, r, Areduc, init=false) ≈ safe_sumabs(Areduc, region) .+ 8.1
    fill!(r, 8.1)
    @test sum!(abs2, r, Areduc, init=false) ≈ safe_sumabs2(Areduc, region) .+ 8.1
    fill!(r, 1.5)
    @test maximum!(abs, r, Areduc, init=false) ≈ fill!(r2, 1.5)
    fill!(r, -1.5)
    @test minimum!(abs, r, Areduc, init=false) ≈ fill!(r2, -1.5)

    @test @inferred(sum(Areduc, dims=region)) ≈ safe_sum(Areduc, region)
    @test @inferred(prod(Areduc, dims=region)) ≈ safe_prod(Areduc, region)
    @test @inferred(maximum(Areduc, dims=region)) ≈ safe_maximum(Areduc, region)
    @test @inferred(minimum(Areduc, dims=region)) ≈ safe_minimum(Areduc, region)
    @test @inferred(sum(abs, Areduc, dims=region)) ≈ safe_sumabs(Areduc, region)
    @test @inferred(sum(abs2, Areduc, dims=region)) ≈ safe_sumabs2(Areduc, region)
    @test @inferred(maximum(abs, Areduc, dims=region)) ≈ safe_maxabs(Areduc, region)
    @test @inferred(minimum(abs, Areduc, dims=region)) ≈ safe_minabs(Areduc, region)
end

# Test reduction along first dimension; this is special-cased for
# size(A, 1) >= 16
Breduc = rand(64, 3)
r = fill(NaN, map(length, Base.reduced_indices(axes(Breduc), 1)))
@test sum!(r, Breduc) ≈ safe_sum(Breduc, 1)
@test sum!(abs, r, Breduc) ≈ safe_sumabs(Breduc, 1)
@test sum!(abs2, r, Breduc) ≈ safe_sumabs2(Breduc, 1)
@test sum(Breduc, dims=1) ≈ safe_sum(Breduc, 1)
@test sum(abs, Breduc, dims=1) ≈ safe_sumabs(Breduc, 1)
@test sum(abs2, Breduc, dims=1) ≈ safe_sumabs2(Breduc, 1)

fill!(r, 4.2)
@test sum!(r, Breduc, init=false) ≈ safe_sum(Breduc, 1) .+ 4.2
fill!(r, -6.3)
@test sum!(abs, r, Breduc, init=false) ≈ safe_sumabs(Breduc, 1) .- 6.3
fill!(r, -1.1)
@test sum!(abs2, r, Breduc, init=false) ≈ safe_sumabs2(Breduc, 1) .- 1.1

# Small arrays with init=false
let A = reshape(1:15, 3, 5)
    R = fill(1, 3)
    @test sum!(R, A, init=false) == [36,41,46]
    R = fill(1, 1, 5)
    @test sum!(R, A, init=false) == [7 16 25 34 43]
end
let R = [2]
    A = reshape(1:6, 3, 2)
    @test prod!(R, A, init=false) == [1440]

    # min/max
    @test reduce(max, A, dims=1) == [3 6]
    @test reduce(min, A, dims=2) == reshape([1,2,3], 3, 1)
end

# Small integers
@test @inferred(sum(Int8[1], dims=1)) == [1]
@test @inferred(sum(UInt8[1], dims=1)) == [1]

# Complex types
@test typeof(@inferred(sum([1.0+1.0im], dims=1))) == Vector{ComplexF64}
@test typeof(@inferred(Base.sum(abs, [1.0+1.0im], dims=1))) == Vector{Float64}
@test typeof(@inferred(Base.sum(abs2, [1.0+1.0im], dims=1))) == Vector{Float64}
@test typeof(@inferred(prod([1.0+1.0im], dims=1))) == Vector{ComplexF64}
@test typeof(@inferred(Base.prod(abs, [1.0+1.0im], dims=1))) == Vector{Float64}
@test typeof(@inferred(Base.prod(abs2, [1.0+1.0im], dims=1))) == Vector{Float64}

@testset "heterogeneously typed arrays" begin
    for x in (sum(Union{Float32, Float64}[1.0], dims=1),
              prod(Union{Float32, Float64}[1.0], dims=1))
        @test x == [1.0]
        @test x isa Vector{Float64}
    end

    x = sum(Real[1.0], dims=1)
    @test x == [1.0]
    @test x isa Vector{Real}

    x = mapreduce(cos, +, Union{Int,Missing}[1, 2], dims=1)
    @test x == mapreduce(cos, +, [1, 2], dims=1)
    @test x isa Vector{Float64}
end

@test reduce((a,b) -> a|b, [true false; false false], dims=1, init=false) == [true false]
let R = reduce((a,b) -> a+b, [1 2; 3 4], dims=2, init=0.0)
    @test eltype(R) == Float64
    @test R ≈ [3,7]
end
@test reduce((a,b) -> a+b, [1 2; 3 4], dims=1, init=0) == [4 6]

# inferred return types
@test typeof(@inferred(reduce(+, ones(3,3,3), dims=1, init=0.0))) == Array{Float64, 3}

@testset "empty cases" begin
    A = Matrix{Int}(undef, 0,1)
    @test sum(A) === 0
    @test prod(A) === 1
    @test_throws ArgumentError minimum(A)
    @test_throws ArgumentError maximum(A)

    @test isequal(sum(A, dims=1), zeros(Int, 1, 1))
    @test isequal(sum(A, dims=2), zeros(Int, 0, 1))
    @test isequal(sum(A, dims=(1, 2)), zeros(Int, 1, 1))
    @test isequal(sum(A, dims=3), zeros(Int, 0, 1))
    @test isequal(prod(A, dims=1), fill(1, 1, 1))
    @test isequal(prod(A, dims=2), fill(1, 0, 1))
    @test isequal(prod(A, dims=(1, 2)), fill(1, 1, 1))
    @test isequal(prod(A, dims=3), fill(1, 0, 1))

    for f in (minimum, maximum)
        @test_throws ArgumentError f(A, dims=1)
        @test isequal(f(A, dims=2), zeros(Int, 0, 1))
        @test_throws ArgumentError f(A, dims=(1, 2))
        @test isequal(f(A, dims=3), zeros(Int, 0, 1))
    end
    for f in (findmin, findmax)
        @test_throws ArgumentError f(A, dims=1)
        @test isequal(f(A, dims=2), (zeros(Int, 0, 1), zeros(Int, 0, 1)))
        @test_throws ArgumentError f(A, dims=(1, 2))
        @test isequal(f(A, dims=3), (zeros(Int, 0, 1), zeros(Int, 0, 1)))
    end

end
## findmin/findmax/minimum/maximum

A = [1.0 5.0 6.0;
     5.0 2.0 4.0]
for (tup, rval, rind) in [((1,), [1.0 2.0 4.0], [CartesianIndex(1,1) CartesianIndex(2,2) CartesianIndex(2,3)]),
                          ((2,), reshape([1.0,2.0], 2, 1), reshape([CartesianIndex(1,1),CartesianIndex(2,2)], 2, 1)),
                          ((1,2), fill(1.0,1,1),fill(CartesianIndex(1,1),1,1))]
    @test findmin(A, dims=tup) == (rval, rind)
    @test findmin!(similar(rval), similar(rind), A) == (rval, rind)
    @test isequal(minimum(A, dims=tup), rval)
    @test isequal(minimum!(similar(rval), A), rval)
    @test isequal(minimum!(copy(rval), A, init=false), rval)
end

for (tup, rval, rind) in [((1,), [5.0 5.0 6.0], [CartesianIndex(2,1) CartesianIndex(1,2) CartesianIndex(1,3)]),
                          ((2,), reshape([6.0,5.0], 2, 1), reshape([CartesianIndex(1,3),CartesianIndex(2,1)], 2, 1)),
                          ((1,2), fill(6.0,1,1),fill(CartesianIndex(1,3),1,1))]
    @test findmax(A, dims=tup) == (rval, rind)
    @test findmax!(similar(rval), similar(rind), A) == (rval, rind)
    @test isequal(maximum(A, dims=tup), rval)
    @test isequal(maximum!(similar(rval), A), rval)
    @test isequal(maximum!(copy(rval), A, init=false), rval)
end

#issue #23209

A = [1.0 3.0 6.0;
     NaN 2.0 4.0]
for (tup, rval, rind) in [((1,), [NaN 2.0 4.0], [CartesianIndex(2,1) CartesianIndex(2,2) CartesianIndex(2,3)]),
                          ((2,), reshape([1.0, NaN], 2, 1), reshape([CartesianIndex(1,1),CartesianIndex(2,1)], 2, 1)),
                          ((1,2), fill(NaN,1,1),fill(CartesianIndex(2,1),1,1))]
    @test isequal(findmin(A, dims=tup), (rval, rind))
    @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(minimum(A, dims=tup), rval)
    @test isequal(minimum!(similar(rval), A), rval)
    @test isequal(minimum!(copy(rval), A, init=false), rval)
    @test isequal(Base.reducedim!(min, copy(rval), A), rval)
end

for (tup, rval, rind) in [((1,), [NaN 3.0 6.0], [CartesianIndex(2,1) CartesianIndex(1,2) CartesianIndex(1,3)]),
                          ((2,), reshape([6.0, NaN], 2, 1), reshape([CartesianIndex(1,3),CartesianIndex(2,1)], 2, 1)),
                          ((1,2), fill(NaN,1,1),fill(CartesianIndex(2,1),1,1))]
    @test isequal(findmax(A, dims=tup), (rval, rind))
    @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(maximum(A, dims=tup), rval)
    @test isequal(maximum!(similar(rval), A), rval)
    @test isequal(maximum!(copy(rval), A, init=false), rval)
    @test isequal(Base.reducedim!(max, copy(rval), A), rval)
end

# issue #28320
@testset "reducedim issue with abstract complex arrays" begin
let A = Complex[1.5 0.5]
    @test mapreduce(abs2, +, A, dims=2) == reshape([2.5], 1, 1)
    @test sum(abs2, A, dims=2) == reshape([2.5], 1, 1)
    @test prod(abs2, A, dims=2) == reshape([0.5625], 1, 1)
    @test maximum(abs2, A, dims=2) == reshape([2.25], 1, 1)
    @test minimum(abs2, A, dims=2) == reshape([0.25], 1, 1)
end
end

A = [1.0 NaN 6.0;
     NaN 2.0 4.0]
for (tup, rval, rind) in [((1,), [NaN NaN 4.0], [CartesianIndex(2,1) CartesianIndex(1,2) CartesianIndex(2,3)]),
                          ((2,), reshape([NaN, NaN], 2, 1), reshape([CartesianIndex(1,2),CartesianIndex(2,1)], 2, 1)),
                          ((1,2), fill(NaN,1,1),fill(CartesianIndex(2,1),1,1))]
    @test isequal(findmin(A, dims=tup), (rval, rind))
    @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(minimum(A, dims=tup), rval)
    @test isequal(minimum!(similar(rval), A), rval)
    @test isequal(minimum!(copy(rval), A, init=false), rval)
end

for (tup, rval, rind) in [((1,), [NaN NaN 6.0], [CartesianIndex(2,1) CartesianIndex(1,2) CartesianIndex(1,3)]),
                          ((2,), reshape([NaN, NaN], 2, 1), reshape([CartesianIndex(1,2),CartesianIndex(2,1)], 2, 1)),
                          ((1,2), fill(NaN,1,1),fill(CartesianIndex(2,1),1,1))]
    @test isequal(findmax(A, dims=tup), (rval, rind))
    @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(maximum(A, dims=tup), rval)
    @test isequal(maximum!(similar(rval), A), rval)
    @test isequal(maximum!(copy(rval), A, init=false), rval)
end

A = [Inf -Inf Inf  -Inf;
     Inf  Inf -Inf -Inf]
for (tup, rval, rind) in [((1,), [Inf -Inf -Inf -Inf], [CartesianIndex(1,1) CartesianIndex(1,2) CartesianIndex(2,3) CartesianIndex(1,4)]),
                          ((2,), reshape([-Inf -Inf], 2, 1), reshape([CartesianIndex(1,2),CartesianIndex(2,3)], 2, 1)),
                          ((1,2), fill(-Inf,1,1),fill(CartesianIndex(1,2),1,1))]
    @test isequal(findmin(A, dims=tup), (rval, rind))
    @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(minimum(A, dims=tup), rval)
    @test isequal(minimum!(similar(rval), A), rval)
    @test isequal(minimum!(copy(rval), A, init=false), rval)
end

for (tup, rval, rind) in [((1,), [Inf Inf Inf -Inf], [CartesianIndex(1,1) CartesianIndex(2,2) CartesianIndex(1,3) CartesianIndex(1,4)]),
                          ((2,), reshape([Inf Inf], 2, 1), reshape([CartesianIndex(1,1),CartesianIndex(2,1)], 2, 1)),
                          ((1,2), fill(Inf,1,1),fill(CartesianIndex(1,1),1,1))]
    @test isequal(findmax(A, dims=tup), (rval, rind))
    @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(maximum(A, dims=tup), rval)
    @test isequal(maximum!(similar(rval), A), rval)
    @test isequal(maximum!(copy(rval), A, init=false), rval)
end

A = [BigInt(10)]
for (tup, rval, rind) in [((2,), [BigInt(10)], [1])]
    @test isequal(findmin(A, dims=tup), (rval, rind))
    @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(minimum(A, dims=tup), rval)
    @test isequal(minimum!(similar(rval), A), rval)
    @test isequal(minimum!(copy(rval), A, init=false), rval)
end

for (tup, rval, rind) in [((2,), [BigInt(10)], [1])]
    @test isequal(findmax(A, dims=tup), (rval, rind))
    @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(maximum(A, dims=tup), rval)
    @test isequal(maximum!(similar(rval), A), rval)
    @test isequal(maximum!(copy(rval), A, init=false), rval)
end

A = [BigInt(-10)]
for (tup, rval, rind) in [((2,), [BigInt(-10)], [1])]
    @test isequal(findmin(A, dims=tup), (rval, rind))
    @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(minimum(A, dims=tup), rval)
    @test isequal(minimum!(similar(rval), A), rval)
    @test isequal(minimum!(copy(rval), A, init=false), rval)
end

for (tup, rval, rind) in [((2,), [BigInt(-10)], [1])]
    @test isequal(findmax(A, dims=tup), (rval, rind))
    @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(maximum(A, dims=tup), rval)
    @test isequal(maximum!(similar(rval), A), rval)
    @test isequal(maximum!(copy(rval), A, init=false), rval)
end

A = [BigInt(10) BigInt(-10)]
for (tup, rval, rind) in [((2,), reshape([BigInt(-10)], 1, 1), reshape([CartesianIndex(1,2)], 1, 1))]
    @test isequal(findmin(A, dims=tup), (rval, rind))
    @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(minimum(A, dims=tup), rval)
    @test isequal(minimum!(similar(rval), A), rval)
    @test isequal(minimum!(copy(rval), A, init=false), rval)
end

for (tup, rval, rind) in [((2,), reshape([BigInt(10)], 1, 1), reshape([CartesianIndex(1,1)], 1, 1))]
    @test isequal(findmax(A, dims=tup), (rval, rind))
    @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(maximum(A, dims=tup), rval)
    @test isequal(maximum!(similar(rval), A), rval)
    @test isequal(maximum!(copy(rval), A, init=false), rval)
end

A = ["a", "b"]
for (tup, rval, rind) in [((1,), ["a"], [1])]
    @test isequal(findmin(A, dims=tup), (rval, rind))
    @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(minimum(A, dims=tup), rval)
    @test isequal(minimum!(similar(rval), A), rval)
    @test isequal(minimum!(copy(rval), A, init=false), rval)
end

for (tup, rval, rind) in [((1,), ["b"], [2])]
    @test isequal(findmax(A, dims=tup), (rval, rind))
    @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(maximum(A, dims=tup), rval)
    @test isequal(maximum!(similar(rval), A), rval)
    @test isequal(maximum!(copy(rval), A, init=false), rval)
end

# issue #6672
@test sum(Real[1 2 3; 4 5.3 7.1], dims=2) == reshape([6, 16.4], 2, 1)
@test sum(Any[1 2;3 4], dims=1) == [4 6]
@test sum(Vector{Int}[[1,2],[4,3]], dims=1)[1] == [5,5]

@testset "Issue #10461. region=$region" for region in Any[-1, 0, (-1, 2), [0, 1], (1,-2,3), [0 1;
                                                     2 3], "hello"]
    Areduc = rand(3, 4, 5, 6)

    @test_throws ArgumentError sum(Areduc, dims=region)
    @test_throws ArgumentError prod(Areduc, dims=region)
    @test_throws ArgumentError maximum(Areduc, dims=region)
    @test_throws ArgumentError minimum(Areduc, dims=region)
    @test_throws ArgumentError sum(abs, Areduc, dims=region)
    @test_throws ArgumentError sum(abs2, Areduc, dims=region)
    @test_throws ArgumentError maximum(abs, Areduc, dims=region)
    @test_throws ArgumentError minimum(abs, Areduc, dims=region)
end

# issue #26488
@testset "don't map over initial values not provided" begin
    @test sum(x->x+1, [1], dims=1)[1] === sum(x->x+1, [1]) === 2
    @test prod(x->x+1, [1], dims=1)[1] === prod(x->x+1, [1]) === 2
    @test mapreduce(x->x+1, +, [1], dims=1)[1] === mapreduce(x->x+1, +, [1]) === 2
    @test mapreduce(x->x+1, *, [1], dims=1)[1] === mapreduce(x->x+1, *, [1]) === 2
    @test mapreduce(!, &, [false], dims=1)[1] === mapreduce(!, &, [false]) === true
    @test mapreduce(!, |, [true], dims=1)[1] === mapreduce(!, |, [true]) === false
    @test mapreduce(x->1/x, max, [1], dims=1)[1] === mapreduce(x->1/x, max, [1]) === 1.0
    @test mapreduce(x->-1/x, min, [1], dims=1)[1] === mapreduce(x->-1/x, min, [1]) === -1.0
end

# check type of result
@testset "type of sum(::Array{$T}" for T in [UInt8, Int8, Int32, Int64, BigInt]
    result = sum(T[1 2 3; 4 5 6; 7 8 9], dims=2)
    @test result == hcat([6, 15, 24])
    @test eltype(result) === (T <: Base.SmallSigned ? Int :
                              T <: Base.SmallUnsigned ? UInt :
                              T)
end

@testset "argmin/argmax" begin
    B = reshape(3^3:-1:1, (3, 3, 3))
    @test B[argmax(B, dims=[2, 3])] == maximum(B, dims=[2, 3])
    @test B[argmin(B, dims=[2, 3])] == minimum(B, dims=[2, 3])
end

@testset "in-place reductions with mismatched dimensionalities" begin
    B = reshape(1:24, 4, 3, 2)
    for R in (fill(0, 4), fill(0, 4, 1), fill(0, 4, 1, 1))
        @test @inferred(maximum!(R, B)) == reshape(21:24, size(R))
        @test @inferred(minimum!(R, B)) == reshape(1:4, size(R))
    end
    for R in (fill(0, 1, 3), fill(0, 1, 3, 1))
        @test @inferred(maximum!(R, B)) == reshape(16:4:24, size(R))
        @test @inferred(minimum!(R, B)) == reshape(1:4:9, size(R))
    end
    @test_throws DimensionMismatch maximum!(fill(0, 4, 1, 1, 1), B)
    @test_throws DimensionMismatch minimum!(fill(0, 4, 1, 1, 1), B)
    @test_throws DimensionMismatch maximum!(fill(0, 1, 3, 1, 1), B)
    @test_throws DimensionMismatch minimum!(fill(0, 1, 3, 1, 1), B)
    @test_throws DimensionMismatch maximum!(fill(0, 1, 1, 2, 1), B)
    @test_throws DimensionMismatch minimum!(fill(0, 1, 1, 2, 1), B)
end

# issue #26709
@testset "dimensional reduce with custom non-bitstype types" begin
    struct Variable
        name::Symbol
    end
    struct AffExpr
        vars::Vector{Variable}
    end
    Base.zero(::Union{Variable, Type{Variable}, AffExpr}) = AffExpr(Variable[])
    Base.:+(v::Variable, w::Variable) = AffExpr([v, w])
    Base.:+(aff::AffExpr, v::Variable) = AffExpr([aff.vars; v])
    Base.:+(aff1::AffExpr, aff2::AffExpr) = AffExpr([aff1.vars; aff2.vars])
    Base.:(==)(a::Variable, b::Variable) = a.name == b.name
    Base.:(==)(a::AffExpr, b::AffExpr) = a.vars == b.vars

    @test sum([Variable(:x), Variable(:y)], dims=1) == [AffExpr([Variable(:x), Variable(:y)])]
end
