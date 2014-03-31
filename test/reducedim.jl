# main tests

function safe_mapslices(op, A, region)
    newregion = intersect(region, 1:ndims(A))
    return isempty(newregion) ? A : mapslices(op, A, newregion)
end
safe_sum{T}(A::Array{T}, region) = safe_mapslices(sum, A, region)
safe_prod{T}(A::Array{T}, region) = safe_mapslices(prod, A, region)
safe_maximum{T}(A::Array{T}, region) = safe_mapslices(maximum, A, region)
safe_minimum{T}(A::Array{T}, region) = safe_mapslices(minimum, A, region)

Areduc = rand(3, 4, 5, 6)
for region in {
        1, 2, 3, 4, 5, (1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4),
    (1, 2, 3), (1, 3, 4), (2, 3, 4), (1, 2, 3, 4)}

    r = fill(NaN, Base.reduced_dims(size(Areduc), region))
    @test_approx_eq sum!(r, Areduc) safe_sum(Areduc, region)
    @test_approx_eq prod!(r, Areduc) safe_prod(Areduc, region)
    @test_approx_eq maximum!(r, Areduc) safe_maximum(Areduc, region)
    @test_approx_eq minimum!(r, Areduc) safe_minimum(Areduc, region)

    @test_approx_eq sum(Areduc, region) safe_sum(Areduc, region)
    @test_approx_eq prod(Areduc, region) safe_prod(Areduc, region)
    @test_approx_eq maximum(Areduc, region) safe_maximum(Areduc, region)
    @test_approx_eq minimum(Areduc, region) safe_minimum(Areduc, region)
end

@test reducedim((a,b) -> a|b, [true false; false false], 1, false) == [true false]
R = reducedim((a,b) -> a+b, [1 2; 3 4], 2, 0.0)
@test eltype(R) == Float64
@test_approx_eq R [3,7]
@test reducedim((a,b) -> a+b, [1 2; 3 4], 1, 0) == [4 6]

# inferred return types
rt = Base.return_types(reducedim, (Function, Array{Float64, 3}, Int, Float64))
@test length(rt) == 1 && rt[1] == Array{Float64, 3}
