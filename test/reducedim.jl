# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "reducedim" begin
# main tests

function safe_mapslices(op, A, region)
    newregion = intersect(region, 1:ndims(A))
    return isempty(newregion) ? A : mapslices(op, A, newregion)
end
safe_sum{T}(A::Array{T}, region) = safe_mapslices(sum, A, region)
safe_prod{T}(A::Array{T}, region) = safe_mapslices(prod, A, region)
safe_maximum{T}(A::Array{T}, region) = safe_mapslices(maximum, A, region)
safe_minimum{T}(A::Array{T}, region) = safe_mapslices(minimum, A, region)
safe_sumabs{T}(A::Array{T}, region) = safe_mapslices(sum, abs(A), region)
safe_sumabs2{T}(A::Array{T}, region) = safe_mapslices(sum, abs2(A), region)
safe_maxabs{T}(A::Array{T}, region) = safe_mapslices(maximum, abs(A), region)
safe_minabs{T}(A::Array{T}, region) = safe_mapslices(minimum, abs(A), region)

Areduc = rand(3, 4, 5, 6)
for region in Any[
    1, 2, 3, 4, 5, (1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4),
    (1, 2, 3), (1, 3, 4), (2, 3, 4), (1, 2, 3, 4)]
    # println("region = $region")
    r = fill(NaN, Base.reduced_dims(size(Areduc), region))
    @test sum!(r, Areduc) ≈ safe_sum(Areduc, region)
    @test prod!(r, Areduc) ≈ safe_prod(Areduc, region)
    @test maximum!(r, Areduc) ≈ safe_maximum(Areduc, region)
    @test minimum!(r, Areduc) ≈ safe_minimum(Areduc, region)
    @test sumabs!(r, Areduc) ≈ safe_sumabs(Areduc, region)
    @test sumabs2!(r, Areduc) ≈ safe_sumabs2(Areduc, region)
    @test maxabs!(r, Areduc) ≈ safe_maxabs(Areduc, region)
    @test minabs!(r, Areduc) ≈ safe_minabs(Areduc, region)

    # With init=false
    r2 = similar(r)
    fill!(r, 1)
    @test sum!(r, Areduc, init=false) ≈ safe_sum(Areduc, region)+1
    fill!(r, 2.2)
    @test prod!(r, Areduc, init=false) ≈ safe_prod(Areduc, region)*2.2
    fill!(r, 1.8)
    @test maximum!(r, Areduc, init=false) ≈ fill!(r2, 1.8)
    fill!(r, -0.2)
    @test minimum!(r, Areduc, init=false) ≈ fill!(r2, -0.2)
    fill!(r, 8.1)
    @test sumabs!(r, Areduc, init=false) ≈ safe_sumabs(Areduc, region)+8.1
    fill!(r, 8.1)
    @test sumabs2!(r, Areduc, init=false) ≈ safe_sumabs2(Areduc, region)+8.1
    fill!(r, 1.5)
    @test maxabs!(r, Areduc, init=false) ≈ fill!(r2, 1.5)
    fill!(r, -1.5)
    @test minabs!(r, Areduc, init=false) ≈ fill!(r2, -1.5)

    @test sum(Areduc, region) ≈ safe_sum(Areduc, region)
    @test prod(Areduc, region) ≈ safe_prod(Areduc, region)
    @test maximum(Areduc, region) ≈ safe_maximum(Areduc, region)
    @test minimum(Areduc, region) ≈ safe_minimum(Areduc, region)
    @test sumabs(Areduc, region) ≈ safe_sumabs(Areduc, region)
    @test sumabs2(Areduc, region) ≈ safe_sumabs2(Areduc, region)
    @test maxabs(Areduc, region) ≈ safe_maxabs(Areduc, region)
    @test minabs(Areduc, region) ≈ safe_minabs(Areduc, region)
end

# Test reduction along first dimension; this is special-cased for
# size(A, 1) >= 16
Breduc = rand(64, 3)
r = fill(NaN, Base.reduced_dims(size(Breduc), 1))
@test sum!(r, Breduc) ≈ safe_sum(Breduc, 1)
@test sumabs!(r, Breduc) ≈ safe_sumabs(Breduc, 1)
@test sumabs2!(r, Breduc) ≈ safe_sumabs2(Breduc, 1)
@test sum(Breduc, 1) ≈ safe_sum(Breduc, 1)
@test sumabs(Breduc, 1) ≈ safe_sumabs(Breduc, 1)
@test sumabs2(Breduc, 1) ≈ safe_sumabs2(Breduc, 1)

fill!(r, 4.2)
@test sum!(r, Breduc, init=false) ≈ safe_sum(Breduc, 1)+4.2
fill!(r, -6.3)
@test sumabs!(r, Breduc, init=false) ≈ safe_sumabs(Breduc, 1)-6.3
fill!(r, -1.1)
@test sumabs2!(r, Breduc, init=false) ≈ safe_sumabs2(Breduc, 1)-1.1

# Small arrays with init=false
A = reshape(1:15, 3, 5)
R = ones(Int, 3)
@test sum!(R, A, init=false) == [36,41,46]
R = ones(Int, 1, 5)
@test sum!(R, A, init=false) == [7 16 25 34 43]
R = [2]
A = reshape(1:6, 3, 2)
@test prod!(R, A, init=false) == [1440]

# Small integers
@test @inferred(sum(Int8[1], 1)) == [1]
@test @inferred(sum(UInt8[1], 1)) == [1]

# Complex types
@test typeof(@inferred(sum([1.0+1.0im], 1))) == Vector{Complex128}
@test typeof(@inferred(Base.sumabs([1.0+1.0im], 1))) == Vector{Float64}
@test typeof(@inferred(Base.sumabs2([1.0+1.0im], 1))) == Vector{Float64}
@test typeof(@inferred(prod([1.0+1.0im], 1))) == Vector{Complex128}
@test typeof(@inferred(Base.prod(abs, [1.0+1.0im], 1))) == Vector{Float64}
@test typeof(@inferred(Base.prod(abs2, [1.0+1.0im], 1))) == Vector{Float64}

# min/max
@test reducedim(max, A, 1) == [3 6]
@test reducedim(min, A, 2) == reshape([1,2,3], 3, 1)

# Heterogeneously typed arrays
@test sum(Union{Float32, Float64}[1.0], 1) == [1.0]
@test prod(Union{Float32, Float64}[1.0], 1) == [1.0]

@test reducedim((a,b) -> a|b, [true false; false false], 1, false) == [true false]
R = reducedim((a,b) -> a+b, [1 2; 3 4], 2, 0.0)
@test eltype(R) == Float64
@test R ≈ [3,7]
@test reducedim((a,b) -> a+b, [1 2; 3 4], 1, 0) == [4 6]

# inferred return types
rt = Base.return_types(reducedim, Tuple{Function, Array{Float64, 3}, Int, Float64})
@test length(rt) == 1 && rt[1] == Array{Float64, 3}


## findmin/findmax
A = [1.0 3.0 6.0;
     5.0 2.0 4.0]
for (tup, rval, rind) in [((1,), [1.0 2.0 4.0], [1 4 6]),
                          ((2,), reshape([1.0,2.0], 2, 1), reshape([1,4], 2, 1)),
                          ((1,2), fill(1.0,1,1),fill(1,1,1))]
    @test findmin(A, tup) == (rval, rind)
    @test findmin!(similar(rval), similar(rind), A) == (rval, rind)
end

for (tup, rval, rind) in [((1,), [5.0 3.0 6.0], [2 3 5]),
                          ((2,), reshape([6.0,5.0], 2, 1), reshape([5,2], 2, 1)),
                          ((1,2), fill(6.0,1,1),fill(5,1,1))]
    @test findmax(A, tup) == (rval, rind)
    @test findmax!(similar(rval), similar(rind), A) == (rval, rind)
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
    @test_throws ArgumentError sumabs(Areduc, region)
    @test_throws ArgumentError sumabs2(Areduc, region)
    @test_throws ArgumentError maxabs(Areduc, region)
    @test_throws ArgumentError minabs(Areduc, region)
end
end
