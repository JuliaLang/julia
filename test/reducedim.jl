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

Areduc = rand(3, 4, 5, 6)
for region in {
    1, 2, 3, 4, 5, (1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4),
    (1, 2, 3), (1, 3, 4), (2, 3, 4), (1, 2, 3, 4)}
    # println("region = $region")
    r = fill(NaN, Base.reduced_dims(size(Areduc), region))
    @test_approx_eq sum!(r, Areduc) safe_sum(Areduc, region)
    @test_approx_eq prod!(r, Areduc) safe_prod(Areduc, region)
    @test_approx_eq maximum!(r, Areduc) safe_maximum(Areduc, region)
    @test_approx_eq minimum!(r, Areduc) safe_minimum(Areduc, region)
    @test_approx_eq Base.sumabs!(r, Areduc) safe_sumabs(Areduc, region)
    @test_approx_eq Base.sumabs2!(r, Areduc) safe_sumabs2(Areduc, region)

    @test_approx_eq sum(Areduc, region) safe_sum(Areduc, region)
    @test_approx_eq prod(Areduc, region) safe_prod(Areduc, region)
    @test_approx_eq maximum(Areduc, region) safe_maximum(Areduc, region)
    @test_approx_eq minimum(Areduc, region) safe_minimum(Areduc, region)
    @test_approx_eq Base.sumabs(Areduc, region) safe_sumabs(Areduc, region)
    @test_approx_eq Base.sumabs2(Areduc, region) safe_sumabs2(Areduc, region)
end

# Test reduction along first dimension; this is special-cased for
# size(A, 1) >= 16
Breduc = rand(64, 3)
r = fill(NaN, Base.reduced_dims(size(Breduc), 1))
@test_approx_eq sum!(r, Breduc) safe_sum(Breduc, 1)
@test_approx_eq Base.sumabs!(r, Breduc) safe_sumabs(Breduc, 1)
@test_approx_eq Base.sumabs2!(r, Breduc) safe_sumabs2(Breduc, 1)
@test_approx_eq sum(Breduc, 1) safe_sum(Breduc, 1)
@test_approx_eq Base.sumabs(Breduc, 1) safe_sumabs(Breduc, 1)
@test_approx_eq Base.sumabs2(Breduc, 1) safe_sumabs2(Breduc, 1)

@test reducedim((a,b) -> a|b, [true false; false false], 1, false) == [true false]
R = reducedim((a,b) -> a+b, [1 2; 3 4], 2, 0.0)
@test eltype(R) == Float64
@test_approx_eq R [3,7]
@test reducedim((a,b) -> a+b, [1 2; 3 4], 1, 0) == [4 6]

# inferred return types
rt = Base.return_types(reducedim, (Function, Array{Float64, 3}, Int, Float64))
@test length(rt) == 1 && rt[1] == Array{Float64, 3}

## findmin/findmax
A = [1.0 3.0 6.0;
     5.0 2.0 4.0]
@test findmin(A, (1,)) == ([1.0 2.0 4.0], [1 4 6])
@test findmin(A, (2,)) == (reshape([1.0,2.0], 2, 1), reshape([1,4], 2, 1))
@test findmin(A, (1,2)) == (fill(1.0,1,1),fill(1,1,1))
@test findmax(A, (1,)) == ([5.0 3.0 6.0], [2 3 5])
@test findmax(A, (2,)) == (reshape([6.0,5.0], 2, 1), reshape([5,2], 2, 1))
@test findmax(A, (1,2)) == (fill(6.0,1,1),fill(5,1,1))

# issue #6672
@test sum(Real[1 2 3; 4 5.3 7.1], 2) == reshape([6, 16.4], 2, 1)
@test std(FloatingPoint[1,2,3], 1) == [1.0]
@test sum({1 2;3 4},1) == [4 6]
@test sum(Vector{Int}[[1,2],[4,3]], 1)[1] == [5,5]
