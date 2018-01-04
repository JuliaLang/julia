# This file is a part of Julia. License is MIT: https://julialang.org/license

# main tests

function safe_mapslices(op, A, region)
    newregion = intersect(region, 1:ndims(A))
    return isempty(newregion) ? A : mapslices(op, A, newregion)
end
safe_sum(A::Array{T}, region) where {T} = safe_mapslices(sum, A, region)
safe_prod(A::Array{T}, region) where {T} = safe_mapslices(prod, A, region)
safe_maximum(A::Array{T}, region) where {T} = safe_mapslices(maximum, A, region)
safe_minimum(A::Array{T}, region) where {T} = safe_mapslices(minimum, A, region)
safe_sumabs(A::Array{T}, region) where {T} = safe_mapslices(sum, abs.(A), region)
safe_sumabs2(A::Array{T}, region) where {T} = safe_mapslices(sum, abs2.(A), region)
safe_maxabs(A::Array{T}, region) where {T} = safe_mapslices(maximum, abs.(A), region)
safe_minabs(A::Array{T}, region) where {T} = safe_mapslices(minimum, abs.(A), region)

Areduc = rand(3, 4, 5, 6)
for region in Any[
    1, 2, 3, 4, 5, (1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4),
    (1, 2, 3), (1, 3, 4), (2, 3, 4), (1, 2, 3, 4)]
    # println("region = $region")
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

    @test sum(Areduc, region) ≈ safe_sum(Areduc, region)
    @test prod(Areduc, region) ≈ safe_prod(Areduc, region)
    @test maximum(Areduc, region) ≈ safe_maximum(Areduc, region)
    @test minimum(Areduc, region) ≈ safe_minimum(Areduc, region)
    @test sum(abs, Areduc, region) ≈ safe_sumabs(Areduc, region)
    @test sum(abs2, Areduc, region) ≈ safe_sumabs2(Areduc, region)
    @test maximum(abs, Areduc, region) ≈ safe_maxabs(Areduc, region)
    @test minimum(abs, Areduc, region) ≈ safe_minabs(Areduc, region)
end

# Test reduction along first dimension; this is special-cased for
# size(A, 1) >= 16
Breduc = rand(64, 3)
r = fill(NaN, map(length, Base.reduced_indices(axes(Breduc), 1)))
@test sum!(r, Breduc) ≈ safe_sum(Breduc, 1)
@test sum!(abs, r, Breduc) ≈ safe_sumabs(Breduc, 1)
@test sum!(abs2, r, Breduc) ≈ safe_sumabs2(Breduc, 1)
@test sum(Breduc, 1) ≈ safe_sum(Breduc, 1)
@test sum(abs, Breduc, 1) ≈ safe_sumabs(Breduc, 1)
@test sum(abs2, Breduc, 1) ≈ safe_sumabs2(Breduc, 1)

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
    @test reducedim(max, A, 1) == [3 6]
    @test reducedim(min, A, 2) == reshape([1,2,3], 3, 1)
end

# Small integers
@test @inferred(sum(Int8[1], 1)) == [1]
@test @inferred(sum(UInt8[1], 1)) == [1]

# Complex types
@test typeof(@inferred(sum([1.0+1.0im], 1))) == Vector{ComplexF64}
@test typeof(@inferred(Base.sum(abs, [1.0+1.0im], 1))) == Vector{Float64}
@test typeof(@inferred(Base.sum(abs2, [1.0+1.0im], 1))) == Vector{Float64}
@test typeof(@inferred(prod([1.0+1.0im], 1))) == Vector{ComplexF64}
@test typeof(@inferred(Base.prod(abs, [1.0+1.0im], 1))) == Vector{Float64}
@test typeof(@inferred(Base.prod(abs2, [1.0+1.0im], 1))) == Vector{Float64}

# Heterogeneously typed arrays
@test sum(Union{Float32, Float64}[1.0], 1) == [1.0]
@test prod(Union{Float32, Float64}[1.0], 1) == [1.0]

@test reducedim((a,b) -> a|b, [true false; false false], 1, false) == [true false]
let R = reducedim((a,b) -> a+b, [1 2; 3 4], 2, 0.0)
    @test eltype(R) == Float64
    @test R ≈ [3,7]
end
@test reducedim((a,b) -> a+b, [1 2; 3 4], 1, 0) == [4 6]

# inferred return types
let rt = Base.return_types(reducedim, Tuple{Function, Array{Float64, 3}, Int, Float64})
    @test length(rt) == 1 && rt[1] == Array{Float64, 3}
end

@testset "empty cases" begin
    A = Matrix{Int}(uninitialized, 0,1)
    @test sum(A) === 0
    @test prod(A) === 1
    @test_throws ArgumentError minimum(A)
    @test_throws ArgumentError maximum(A)
    @test var(A) === NaN

    @test isequal(sum(A, 1), zeros(Int, 1, 1))
    @test isequal(sum(A, 2), zeros(Int, 0, 1))
    @test isequal(sum(A, (1, 2)), zeros(Int, 1, 1))
    @test isequal(sum(A, 3), zeros(Int, 0, 1))
    @test isequal(prod(A, 1), fill(1, 1, 1))
    @test isequal(prod(A, 2), fill(1, 0, 1))
    @test isequal(prod(A, (1, 2)), fill(1, 1, 1))
    @test isequal(prod(A, 3), fill(1, 0, 1))
    @test isequal(var(A, 1), fill(NaN, 1, 1))
    @test isequal(var(A, 2), fill(NaN, 0, 1))
    @test isequal(var(A, (1, 2)), fill(NaN, 1, 1))
    @test isequal(var(A, 3), fill(NaN, 0, 1))

    for f in (minimum, maximum)
        @test_throws ArgumentError f(A, 1)
        @test isequal(f(A, 2), zeros(Int, 0, 1))
        @test_throws ArgumentError f(A, (1, 2))
        @test isequal(f(A, 3), zeros(Int, 0, 1))
    end
    for f in (findmin, findmax)
        @test_throws ArgumentError f(A, 1)
        @test isequal(f(A, 2), (zeros(Int, 0, 1), zeros(Int, 0, 1)))
        @test_throws ArgumentError f(A, (1, 2))
        @test isequal(f(A, 3), (zeros(Int, 0, 1), zeros(Int, 0, 1)))
    end

end
## findmin/findmax/minumum/maximum

A = [1.0 5.0 6.0;
     5.0 2.0 4.0]
for (tup, rval, rind) in [((1,), [1.0 2.0 4.0], [CartesianIndex(1,1) CartesianIndex(2,2) CartesianIndex(2,3)]),
                          ((2,), reshape([1.0,2.0], 2, 1), reshape([CartesianIndex(1,1),CartesianIndex(2,2)], 2, 1)),
                          ((1,2), fill(1.0,1,1),fill(CartesianIndex(1,1),1,1))]
    @test findmin(A, tup) == (rval, rind)
    @test findmin!(similar(rval), similar(rind), A) == (rval, rind)
    @test isequal(minimum(A, tup), rval)
    @test isequal(minimum!(similar(rval), A), rval)
    @test isequal(minimum!(copy(rval), A, init=false), rval)
end

for (tup, rval, rind) in [((1,), [5.0 5.0 6.0], [CartesianIndex(2,1) CartesianIndex(1,2) CartesianIndex(1,3)]),
                          ((2,), reshape([6.0,5.0], 2, 1), reshape([CartesianIndex(1,3),CartesianIndex(2,1)], 2, 1)),
                          ((1,2), fill(6.0,1,1),fill(CartesianIndex(1,3),1,1))]
    @test findmax(A, tup) == (rval, rind)
    @test findmax!(similar(rval), similar(rind), A) == (rval, rind)
    @test isequal(maximum(A, tup), rval)
    @test isequal(maximum!(similar(rval), A), rval)
    @test isequal(maximum!(copy(rval), A, init=false), rval)
end

#issue #23209

A = [1.0 3.0 6.0;
     NaN 2.0 4.0]
for (tup, rval, rind) in [((1,), [NaN 2.0 4.0], [CartesianIndex(2,1) CartesianIndex(2,2) CartesianIndex(2,3)]),
                          ((2,), reshape([1.0, NaN], 2, 1), reshape([CartesianIndex(1,1),CartesianIndex(2,1)], 2, 1)),
                          ((1,2), fill(NaN,1,1),fill(CartesianIndex(2,1),1,1))]
    @test isequal(findmin(A, tup), (rval, rind))
    @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(minimum(A, tup), rval)
    @test isequal(minimum!(similar(rval), A), rval)
    @test isequal(minimum!(copy(rval), A, init=false), rval)
    @test isequal(Base.reducedim!(min, copy(rval), A), rval)
end

for (tup, rval, rind) in [((1,), [NaN 3.0 6.0], [CartesianIndex(2,1) CartesianIndex(1,2) CartesianIndex(1,3)]),
                          ((2,), reshape([6.0, NaN], 2, 1), reshape([CartesianIndex(1,3),CartesianIndex(2,1)], 2, 1)),
                          ((1,2), fill(NaN,1,1),fill(CartesianIndex(2,1),1,1))]
    @test isequal(findmax(A, tup), (rval, rind))
    @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(maximum(A, tup), rval)
    @test isequal(maximum!(similar(rval), A), rval)
    @test isequal(maximum!(copy(rval), A, init=false), rval)
    @test isequal(Base.reducedim!(max, copy(rval), A), rval)
end

A = [1.0 NaN 6.0;
     NaN 2.0 4.0]
for (tup, rval, rind) in [((1,), [NaN NaN 4.0], [CartesianIndex(2,1) CartesianIndex(1,2) CartesianIndex(2,3)]),
                          ((2,), reshape([NaN, NaN], 2, 1), reshape([CartesianIndex(1,2),CartesianIndex(2,1)], 2, 1)),
                          ((1,2), fill(NaN,1,1),fill(CartesianIndex(2,1),1,1))]
    @test isequal(findmin(A, tup), (rval, rind))
    @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(minimum(A, tup), rval)
    @test isequal(minimum!(similar(rval), A), rval)
    @test isequal(minimum!(copy(rval), A, init=false), rval)
end

for (tup, rval, rind) in [((1,), [NaN NaN 6.0], [CartesianIndex(2,1) CartesianIndex(1,2) CartesianIndex(1,3)]),
                          ((2,), reshape([NaN, NaN], 2, 1), reshape([CartesianIndex(1,2),CartesianIndex(2,1)], 2, 1)),
                          ((1,2), fill(NaN,1,1),fill(CartesianIndex(2,1),1,1))]
    @test isequal(findmax(A, tup), (rval, rind))
    @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(maximum(A, tup), rval)
    @test isequal(maximum!(similar(rval), A), rval)
    @test isequal(maximum!(copy(rval), A, init=false), rval)
end

A = [Inf -Inf Inf  -Inf;
     Inf  Inf -Inf -Inf]
for (tup, rval, rind) in [((1,), [Inf -Inf -Inf -Inf], [CartesianIndex(1,1) CartesianIndex(1,2) CartesianIndex(2,3) CartesianIndex(1,4)]),
                          ((2,), reshape([-Inf -Inf], 2, 1), reshape([CartesianIndex(1,2),CartesianIndex(2,3)], 2, 1)),
                          ((1,2), fill(-Inf,1,1),fill(CartesianIndex(1,2),1,1))]
    @test isequal(findmin(A, tup), (rval, rind))
    @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(minimum(A, tup), rval)
    @test isequal(minimum!(similar(rval), A), rval)
    @test isequal(minimum!(copy(rval), A, init=false), rval)
end

for (tup, rval, rind) in [((1,), [Inf Inf Inf -Inf], [CartesianIndex(1,1) CartesianIndex(2,2) CartesianIndex(1,3) CartesianIndex(1,4)]),
                          ((2,), reshape([Inf Inf], 2, 1), reshape([CartesianIndex(1,1),CartesianIndex(2,1)], 2, 1)),
                          ((1,2), fill(Inf,1,1),fill(CartesianIndex(1,1),1,1))]
    @test isequal(findmax(A, tup), (rval, rind))
    @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(maximum(A, tup), rval)
    @test isequal(maximum!(similar(rval), A), rval)
    @test isequal(maximum!(copy(rval), A, init=false), rval)
end

A = [BigInt(10)]
for (tup, rval, rind) in [((2,), [BigInt(10)], [1])]
    @test isequal(findmin(A, tup), (rval, rind))
    @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(minimum(A, tup), rval)
    @test isequal(minimum!(similar(rval), A), rval)
    @test isequal(minimum!(copy(rval), A, init=false), rval)
end

for (tup, rval, rind) in [((2,), [BigInt(10)], [1])]
    @test isequal(findmax(A, tup), (rval, rind))
    @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(maximum(A, tup), rval)
    @test isequal(maximum!(similar(rval), A), rval)
    @test isequal(maximum!(copy(rval), A, init=false), rval)
end

A = [BigInt(-10)]
for (tup, rval, rind) in [((2,), [BigInt(-10)], [1])]
    @test isequal(findmin(A, tup), (rval, rind))
    @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(minimum(A, tup), rval)
    @test isequal(minimum!(similar(rval), A), rval)
    @test isequal(minimum!(copy(rval), A, init=false), rval)
end

for (tup, rval, rind) in [((2,), [BigInt(-10)], [1])]
    @test isequal(findmax(A, tup), (rval, rind))
    @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(maximum(A, tup), rval)
    @test isequal(maximum!(similar(rval), A), rval)
    @test isequal(maximum!(copy(rval), A, init=false), rval)
end

A = [BigInt(10) BigInt(-10)]
for (tup, rval, rind) in [((2,), reshape([BigInt(-10)], 1, 1), reshape([CartesianIndex(1,2)], 1, 1))]
    @test isequal(findmin(A, tup), (rval, rind))
    @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(minimum(A, tup), rval)
    @test isequal(minimum!(similar(rval), A), rval)
    @test isequal(minimum!(copy(rval), A, init=false), rval)
end

for (tup, rval, rind) in [((2,), reshape([BigInt(10)], 1, 1), reshape([CartesianIndex(1,1)], 1, 1))]
    @test isequal(findmax(A, tup), (rval, rind))
    @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(maximum(A, tup), rval)
    @test isequal(maximum!(similar(rval), A), rval)
    @test isequal(maximum!(copy(rval), A, init=false), rval)
end

A = ["a", "b"]
for (tup, rval, rind) in [((1,), ["a"], [1])]
    @test isequal(findmin(A, tup), (rval, rind))
    @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(minimum(A, tup), rval)
    @test isequal(minimum!(similar(rval), A), rval)
    @test isequal(minimum!(copy(rval), A, init=false), rval)
end

for (tup, rval, rind) in [((1,), ["b"], [2])]
    @test isequal(findmax(A, tup), (rval, rind))
    @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
    @test isequal(maximum(A, tup), rval)
    @test isequal(maximum!(similar(rval), A), rval)
    @test isequal(maximum!(copy(rval), A, init=false), rval)
end

# issue #6672
@test sum(Real[1 2 3; 4 5.3 7.1], 2) == reshape([6, 16.4], 2, 1)
@test std(AbstractFloat[1,2,3], 1) == [1.0]
@test sum(Any[1 2;3 4],1) == [4 6]
@test sum(Vector{Int}[[1,2],[4,3]], 1)[1] == [5,5]

# issue #10461
Areduc = rand(3, 4, 5, 6)
for region in Any[-1, 0, (-1, 2), [0, 1], (1,-2,3), [0 1;
                                                     2 3], "hello"]
    @test_throws ArgumentError sum(Areduc, region)
    @test_throws ArgumentError prod(Areduc, region)
    @test_throws ArgumentError maximum(Areduc, region)
    @test_throws ArgumentError minimum(Areduc, region)
    @test_throws ArgumentError sum(abs, Areduc, region)
    @test_throws ArgumentError sum(abs2, Areduc, region)
    @test_throws ArgumentError maximum(abs, Areduc, region)
    @test_throws ArgumentError minimum(abs, Areduc, region)
end

# check type of result
@testset "type of sum(::Array{$T}" for T in [UInt8, Int8, Int32, Int64, BigInt]
    result = sum(T[1 2 3; 4 5 6; 7 8 9], 2)
    @test result == hcat([6, 15, 24])
    @test eltype(result) === (T <: Base.SmallSigned ? Int :
                              T <: Base.SmallUnsigned ? UInt :
                              T)
end
