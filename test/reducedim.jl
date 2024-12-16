# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random

isdefined(Main, :OffsetArrays) || @eval Main include("testhelpers/OffsetArrays.jl")
using .Main.OffsetArrays: OffsetVector, OffsetArray

# main tests

# issue #35800
# tested very early since it can be state-dependent

function my_simple_count(pred, g::Vector{T}) where {T}
    n::T = zero(T)
    for x in g
        n += pred(x)
    end
    return n
end

@test @inferred(mapreduce(x->my_simple_count(!iszero,x), +, [rand(1)]; init = 0.)) == 1.0

function safe_mapslices(op, A, region)
    newregion = intersect(region, 1:ndims(A))
    return isempty(newregion) ? A : mapslices(op, A, dims = newregion)
end
safe_sum(A::Array{T}, region) where {T} = safe_mapslices(sum, A, region)
safe_prod(A::Array{T}, region) where {T} = safe_mapslices(prod, A, region)
safe_maximum(A::Array{T}, region) where {T} = safe_mapslices(maximum, A, region)
safe_minimum(A::Array{T}, region) where {T} = safe_mapslices(minimum, A, region)
safe_count(A::AbstractArray{T}, region) where {T} = safe_mapslices(count, A, region)
safe_sumabs(A::Array{T}, region) where {T} = safe_mapslices(sum, abs.(A), region)
safe_sumabs2(A::Array{T}, region) where {T} = safe_mapslices(sum, abs2.(A), region)
safe_maxabs(A::Array{T}, region) where {T} = safe_mapslices(maximum, abs.(A), region)
safe_minabs(A::Array{T}, region) where {T} = safe_mapslices(minimum, abs.(A), region)

@testset "test reductions over region: $region" for region in Any[
    1, 2, 3, 4, 5, (1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4),
    (1, 2, 3), (1, 3, 4), (2, 3, 4), (1, 2, 3, 4)]
    Areduc = rand(3, 4, 5, 6)
    Breduc = rand(Bool, 3, 4, 5, 6)
    @assert axes(Areduc) == axes(Breduc)

    r = fill(NaN, map(length, Base.reduced_indices(axes(Areduc), region)))
    @test sum!(r, Areduc) ≈ safe_sum(Areduc, region)
    @test prod!(r, Areduc) ≈ safe_prod(Areduc, region)
    @test maximum!(r, Areduc) ≈ safe_maximum(Areduc, region)
    @test minimum!(r, Areduc) ≈ safe_minimum(Areduc, region)
    @test count!(r, Breduc) ≈ safe_count(Breduc, region)

    @test sum!(abs, r, Areduc) ≈ safe_sumabs(Areduc, region)
    @test sum!(abs2, r, Areduc) ≈ safe_sumabs2(Areduc, region)
    @test maximum!(abs, r, Areduc) ≈ safe_maxabs(Areduc, region)
    @test minimum!(abs, r, Areduc) ≈ safe_minabs(Areduc, region)
    @test count!(!, r, Breduc) ≈ safe_count(.!Breduc, region)

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
    fill!(r, 1)
    @test count!(r, Breduc, init=false) ≈ safe_count(Breduc, region) .+ 1

    fill!(r, 8.1)
    @test sum!(abs, r, Areduc, init=false) ≈ safe_sumabs(Areduc, region) .+ 8.1
    fill!(r, 8.1)
    @test sum!(abs2, r, Areduc, init=false) ≈ safe_sumabs2(Areduc, region) .+ 8.1
    fill!(r, 1.5)
    @test maximum!(abs, r, Areduc, init=false) ≈ fill!(r2, 1.5)
    fill!(r, -1.5)
    @test minimum!(abs, r, Areduc, init=false) ≈ fill!(r2, -1.5)
    fill!(r, 1)
    @test count!(!, r, Breduc, init=false) ≈ safe_count(.!Breduc, region) .+ 1

    @test @inferred(sum(Areduc, dims=region)) ≈ safe_sum(Areduc, region)
    @test @inferred(prod(Areduc, dims=region)) ≈ safe_prod(Areduc, region)
    @test @inferred(maximum(Areduc, dims=region)) ≈ safe_maximum(Areduc, region)
    @test @inferred(minimum(Areduc, dims=region)) ≈ safe_minimum(Areduc, region)
    @test @inferred(count(Breduc, dims=region)) ≈ safe_count(Breduc, region)

    @test @inferred(sum(abs, Areduc, dims=region)) ≈ safe_sumabs(Areduc, region)
    @test @inferred(sum(abs2, Areduc, dims=region)) ≈ safe_sumabs2(Areduc, region)
    @test @inferred(maximum(abs, Areduc, dims=region)) ≈ safe_maxabs(Areduc, region)
    @test @inferred(minimum(abs, Areduc, dims=region)) ≈ safe_minabs(Areduc, region)
    @test @inferred(count(!, Breduc, dims=region)) ≈ safe_count(.!Breduc, region)

    @test isequal(
        @inferred(count(Breduc, dims=region, init=0x02)),
        safe_count(Breduc, region) .% UInt8 .+ 0x02,
    )
    @test isequal(
        @inferred(count(!, Breduc, dims=region, init=Int16(0))),
        safe_count(.!Breduc, region) .% Int16,
    )
end

# Combining dims and init
A = Array{Int}(undef, 0, 3)
@test_throws "reducing over an empty collection is not allowed" maximum(A; dims=1)
@test maximum(A; dims=1, init=-1) == reshape([-1,-1,-1], 1, 3)

@test maximum(zeros(0, 2); dims=1, init=-1) == fill(-1, 1, 2)
@test minimum(zeros(0, 2); dims=1, init=1) == ones(1, 2)
@test extrema(zeros(0, 2); dims=1, init=(1, -1)) == fill((1, -1), 1, 2)

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

# issue #35199
function issue35199_test(sizes, dims)
    M = rand(Float64, sizes)
    ax = axes(M)
    n1 = @allocations Base.reduced_indices(ax, dims)
    return @test n1 == 0
end
for dims in (1, 2, (1,), (2,), (1,2))
    sizes = (64, 3)
    issue35199_test(sizes, dims)
end

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
    @test x isa Vector{<:Real}

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
    @test_throws ["reducing over an empty",
                  "consider supplying `init`"] minimum(A)
    @test_throws "consider supplying `init`" maximum(A)

    @test isequal(sum(A, dims=1), zeros(Int, 1, 1))
    @test isequal(sum(A, dims=2), zeros(Int, 0, 1))
    @test isequal(sum(A, dims=(1, 2)), zeros(Int, 1, 1))
    @test isequal(sum(A, dims=3), zeros(Int, 0, 1))
    @test isequal(prod(A, dims=1), fill(1, 1, 1))
    @test isequal(prod(A, dims=2), fill(1, 0, 1))
    @test isequal(prod(A, dims=(1, 2)), fill(1, 1, 1))
    @test isequal(prod(A, dims=3), fill(1, 0, 1))

    for f in (minimum, maximum)
        @test_throws "reducing over an empty collection is not allowed" f(A, dims=1)
        @test_throws "reducing over an empty collection is not allowed" isequal(f(A, dims=2), zeros(Int, 0, 1))
        @test_throws "reducing over an empty collection is not allowed" f(A, dims=(1, 2))
        @test_throws "reducing over an empty collection is not allowed" isequal(f(A, dims=3), zeros(Int, 0, 1))
    end
    for f in (findmin, findmax)
        @test_throws "reducing over an empty collection is not allowed" f(A, dims=1)
        @test_throws "reducing over an empty collection is not allowed" isequal(f(A, dims=2), (zeros(Int, 0, 1), zeros(Int, 0, 1)))
        @test_throws "reducing over an empty collection is not allowed" f(A, dims=(1, 2))
        @test_throws "reducing over an empty collection is not allowed" isequal(f(A, dims=3), (zeros(Int, 0, 1), zeros(Int, 0, 1)))
        @test_throws "reducing over an empty collection is not allowed" f(abs2, A, dims=1)
        @test_throws "reducing over an empty collection is not allowed" isequal(f(abs2, A, dims=2), (zeros(Int, 0, 1), zeros(Int, 0, 1)))
        @test_throws "reducing over an empty collection is not allowed" f(abs2, A, dims=(1, 2))
        @test_throws "reducing over an empty collection is not allowed" isequal(f(abs2, A, dims=3), (zeros(Int, 0, 1), zeros(Int, 0, 1)))
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

@testset "findmin/findmax transformed arguments, numeric values" begin
    A = [1.0 -5.0 -6.0;
         -5.0 2.0 4.0]
    TA = [((1,), [1.0 2.0 4.0], [CartesianIndex(1,1) CartesianIndex(2,2) CartesianIndex(2,3)]),
          ((2,), reshape([1.0, 2.0], 2, 1), reshape([CartesianIndex(1,1), CartesianIndex(2,2)], 2, 1)),
          ((1,2), fill(1.0,1,1), fill(CartesianIndex(1,1),1,1))]
    TA2 = [((1,), [1.0 4.0 16.0], [CartesianIndex(1,1) CartesianIndex(2,2) CartesianIndex(2,3)]),
           ((2,), reshape([1.0, 4.0], 2, 1), reshape([CartesianIndex(1,1), CartesianIndex(2,2)], 2, 1)),
           ((1,2), fill(1.0,1,1), fill(CartesianIndex(1,1),1,1))]
    TAc = [((1,), [0.28366218546322625 -0.4161468365471424 -0.6536436208636119], [CartesianIndex(2,1) CartesianIndex(2,2) CartesianIndex(2,3)]),
           ((2,), reshape([0.28366218546322625, -0.6536436208636119], 2, 1), reshape([CartesianIndex(1,2), CartesianIndex(2,3)], 2, 1)),
           ((1,2), fill(-0.6536436208636119,1,1), fill(CartesianIndex(2,3),1,1))]
    for (f, At) in ((abs, TA), (abs2, TA2), (cos, TAc))
        A′ = map(f, A)
        for (tup, rval, rind) in At
            (rval′, rind′) = findmin(f, A, dims=tup)
            @test all(rval′ .≈ rval)
            @test rind′ == rind
            @test findmin(f, A, dims=tup) == (rval, rind)
            @test (rval′, rind′) == findmin(A′, dims=tup)
        end
    end

    TA = [((1,), [5.0 5.0 6.0], [CartesianIndex(2,1) CartesianIndex(1,2) CartesianIndex(1,3)]),
          ((2,), reshape([6.0,5.0], 2, 1), reshape([CartesianIndex(1,3), CartesianIndex(2,1)], 2, 1)),
          ((1,2), fill(6.0,1,1),fill(CartesianIndex(1,3),1,1))]
    TA2 = [((1,), [25.0 25.0 36.0], [CartesianIndex(2,1) CartesianIndex(1,2) CartesianIndex(1,3)]),
           ((2,), reshape([36.0, 25.0], 2, 1), reshape([CartesianIndex(1,3), CartesianIndex(2,1)], 2, 1)),
           ((1,2), fill(36.0,1,1), fill(CartesianIndex(1,3),1,1))]
    TAc = [((1,), [0.5403023058681398 0.28366218546322625 0.960170286650366], [CartesianIndex(1,1) CartesianIndex(1,2) CartesianIndex(1,3)]),
           ((2,), reshape([0.960170286650366, 0.28366218546322625], 2, 1), reshape([CartesianIndex(1,3), CartesianIndex(2,1)], 2, 1)),
           ((1,2), fill(0.960170286650366,1,1), fill(CartesianIndex(1,3),1,1))]
    for (f, At) in ((abs, TA), (abs2, TA2), (cos, TAc))
        A′ = map(f, A)
        for (tup, rval, rind) in At
            (rval′, rind′) = findmax(f, A, dims=tup)
            @test all(rval′ .≈ rval)
            @test rind′ == rind
            @test findmax(f, A, dims=tup) == (rval, rind)
            @test (rval′, rind′) == findmax(A′, dims=tup)
        end
    end
end

# findmin/findmax function arguments: output type inference
@testset "findmin/findmax output type inference" begin
    A = ["1" "22"; "333" "4444"]
    for (tup, rval, rind) in [((1,), [1 2], [CartesianIndex(1, 1) CartesianIndex(1, 2)]),
                              ((2,), reshape([1, 3], 2, 1), reshape([CartesianIndex(1, 1), CartesianIndex(2, 1)], 2, 1)),
                              ((1,2), fill(1,1,1), fill(CartesianIndex(1,1),1,1))]
        rval′, rind′ = findmin(length, A, dims=tup)
        @test (rval, rind) == (rval′, rind′)
        @test typeof(rval′) == Matrix{Int}
    end
    for (tup, rval, rind) in [((1,), [3 4], [CartesianIndex(2, 1) CartesianIndex(2, 2)]),
                              ((2,), reshape([2, 4], 2, 1), reshape([CartesianIndex(1, 2), CartesianIndex(2, 2)], 2, 1)),
                              ((1,2), fill(4,1,1), fill(CartesianIndex(2,2),1,1))]
        rval′, rind′ = findmax(length, A, dims=tup)
        @test (rval, rind) == (rval′, rind′)
        @test typeof(rval) == Matrix{Int}
    end
    B = [1.5 1.0; 5.5 6.0]
    for (tup, rval, rind) in [((1,), [3//2 1//1], [CartesianIndex(1, 1) CartesianIndex(1, 2)]),
                              ((2,), reshape([1//1, 11//2], 2, 1), reshape([CartesianIndex(1, 2), CartesianIndex(2, 1)], 2, 1)),
                              ((1,2), fill(1//1,1,1), fill(CartesianIndex(1,2),1,1))]
        rval′, rind′ = findmin(Rational, B, dims=tup)
        @test (rval, rind) == (rval′, rind′)
        @test typeof(rval) == Matrix{Rational{Int}}
        rval′, rind′ = findmin(Rational ∘ abs ∘ complex, B, dims=tup)
        @test (rval, rind) == (rval′, rind′)
        @test typeof(rval) == Matrix{Rational{Int}}
    end
end


@testset "missing in findmin/findmax" begin
    B = [1.0 missing NaN;
         5.0 NaN missing]
    B′ = [1.0 missing -NaN;
          -5.0 NaN missing]
    for (tup, rval, rind) in [(1, [5.0 missing missing], [CartesianIndex(2, 1) CartesianIndex(1, 2) CartesianIndex(2, 3)]),
                              (2, [missing; missing],    [CartesianIndex(1, 2) CartesianIndex(2, 3)] |> permutedims)]
        (rval′, rind′) = findmax(B, dims=tup)
        @test all(rval′ .=== rval)
        @test all(rind′ .== rind)
        @test all(maximum(B, dims=tup) .=== rval)
        @test isequal(findmax(abs, B′, dims=tup), (rval′, rind′))
    end

    for (tup, rval, rind) in [(1, [1.0 missing missing], [CartesianIndex(1, 1) CartesianIndex(1, 2) CartesianIndex(2, 3)]),
                              (2, [missing; missing],    [CartesianIndex(1, 2) CartesianIndex(2, 3)] |> permutedims)]
        (rval′, rind′) = findmin(B, dims=tup)
        @test all(rval′ .=== rval)
        @test all(rind′ .== rind)
        @test all(minimum(B, dims=tup) .=== rval)
        @test isequal(findmin(abs, B′, dims=tup), (rval′, rind′))
    end
end

@testset "reducedim_init min/max unorderable handling" begin
    x = Any[1.0, NaN]
    y = [1, missing]
    for (v, rval1, rval2) in [(x, [NaN], x),
                              (y, [missing], y),
                              (Any[1. NaN; 1. 1.], Any[1. NaN], Any[NaN, 1.])]
        for f in (minimum, maximum)
            @test all(f(v, dims=1) .=== rval1)
            @test all(f(v, dims=2) .=== rval2)
        end
    end
end

#issue #23209

A = [1.0 3.0 6.0;
     NaN 2.0 4.0]
for (tup, rval, rind) in [((1,), [NaN 2.0 4.0], [CartesianIndex(2,1) CartesianIndex(2,2) CartesianIndex(2,3)]),
                          ((2,), reshape([1.0, NaN], 2, 1), reshape([CartesianIndex(1,1),CartesianIndex(2,1)], 2, 1)),
                          ((1,2), fill(NaN,1,1),fill(CartesianIndex(2,1),1,1))]
    @test isequal(findmin(A, dims=tup), (rval, rind))
    @test isequal(findmin(abs, A, dims=tup), (rval, rind))
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
    @test isequal(findmax(abs, A, dims=tup), (rval, rind))
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
        @test findmin(abs2, A, dims=2) == (fill(0.25, 1, 1), fill(CartesianIndex(1, 2), 1, 1))
        @test findmax(abs2, A, dims=2) == (fill(2.25, 1, 1), fill(CartesianIndex(1, 1), 1, 1))
    end
end

@testset "NaN in findmin/findmax/minimum/maximum" begin
    A = [1.0 NaN 6.0;
         NaN 2.0 4.0]
    A′ = [-1.0 NaN -6.0;
          NaN -2.0 4.0]
    for (tup, rval, rind) in [((1,), [NaN NaN 4.0], [CartesianIndex(2,1) CartesianIndex(1,2) CartesianIndex(2,3)]),
                              ((2,), reshape([NaN, NaN], 2, 1), reshape([CartesianIndex(1,2),CartesianIndex(2,1)], 2, 1)),
                              ((1,2), fill(NaN,1,1),fill(CartesianIndex(2,1),1,1))]
        @test isequal(findmin(A, dims=tup), (rval, rind))
        @test isequal(findmin(abs, A′, dims=tup), (rval, rind))
        @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
        @test isequal(minimum(A, dims=tup), rval)
        @test isequal(minimum!(similar(rval), A), rval)
        @test isequal(minimum!(copy(rval), A, init=false), rval)
    end

    for (tup, rval, rind) in [((1,), [NaN NaN 6.0], [CartesianIndex(2,1) CartesianIndex(1,2) CartesianIndex(1,3)]),
                              ((2,), reshape([NaN, NaN], 2, 1), reshape([CartesianIndex(1,2),CartesianIndex(2,1)], 2, 1)),
                              ((1,2), fill(NaN,1,1),fill(CartesianIndex(2,1),1,1))]
        @test isequal(findmax(A, dims=tup), (rval, rind))
        @test isequal(findmax(abs, A′, dims=tup), (rval, rind))
        @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
        @test isequal(maximum(A, dims=tup), rval)
        @test isequal(maximum!(similar(rval), A), rval)
        @test isequal(maximum!(copy(rval), A, init=false), rval)
    end
end

@testset "+/-Inf in findmin/findmax/minimum/maximum" begin
    A = [Inf -Inf Inf  -Inf;
         Inf  Inf -Inf -Inf]
    A′ = [1 0 1 0;
          1 1 0 0]
    for (tup, rval, rind) in [((1,), [Inf -Inf -Inf -Inf], [CartesianIndex(1,1) CartesianIndex(1,2) CartesianIndex(2,3) CartesianIndex(1,4)]),
                              ((2,), reshape([-Inf -Inf], 2, 1), reshape([CartesianIndex(1,2),CartesianIndex(2,3)], 2, 1)),
                              ((1,2), fill(-Inf,1,1),fill(CartesianIndex(1,2),1,1))]
        @test isequal(findmin(A, dims=tup), (rval, rind))
        @test isequal(findmin(x -> x == 1 ? Inf : -Inf, A′, dims=tup), (rval, rind))
        @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
        @test isequal(minimum(A, dims=tup), rval)
        @test isequal(minimum!(similar(rval), A), rval)
        @test isequal(minimum!(copy(rval), A, init=false), rval)
    end

    for (tup, rval, rind) in [((1,), [Inf Inf Inf -Inf], [CartesianIndex(1,1) CartesianIndex(2,2) CartesianIndex(1,3) CartesianIndex(1,4)]),
                              ((2,), reshape([Inf Inf], 2, 1), reshape([CartesianIndex(1,1),CartesianIndex(2,1)], 2, 1)),
                              ((1,2), fill(Inf,1,1),fill(CartesianIndex(1,1),1,1))]
        @test isequal(findmax(A, dims=tup), (rval, rind))
        @test isequal(findmax(x -> x == 1 ? Inf : -Inf, A′, dims=tup), (rval, rind))
        @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
        @test isequal(maximum(A, dims=tup), rval)
        @test isequal(maximum!(similar(rval), A), rval)
        @test isequal(maximum!(copy(rval), A, init=false), rval)
    end
end

@testset "BigInt in findmin/findmax/minimum/maximum" begin
    A = [BigInt(10)]
    A′ = [BigInt(1)]
    for (tup, rval, rind) in [((2,), [BigInt(10)], [1])]
        @test isequal(findmin(A, dims=tup), (rval, rind))
        @test isequal(findmin(x -> 10^x, A′, dims=tup), (rval, rind))
        @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
        @test isequal(minimum(A, dims=tup), rval)
        @test isequal(minimum!(similar(rval), A), rval)
        @test isequal(minimum!(copy(rval), A, init=false), rval)
    end

    for (tup, rval, rind) in [((2,), [BigInt(10)], [1])]
        @test isequal(findmax(A, dims=tup), (rval, rind))
        @test isequal(findmax(x -> 10^x, A′, dims=tup), (rval, rind))
        @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
        @test isequal(maximum(A, dims=tup), rval)
        @test isequal(maximum!(similar(rval), A), rval)
        @test isequal(maximum!(copy(rval), A, init=false), rval)
    end

    A = [BigInt(-10)]
    for (tup, rval, rind) in [((2,), [BigInt(-10)], [1])]
        @test isequal(findmin(A, dims=tup), (rval, rind))
        @test isequal(findmin(x -> -(x + 20), A, dims=tup), (rval, rind))
        @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
        @test isequal(minimum(A, dims=tup), rval)
        @test isequal(minimum!(similar(rval), A), rval)
        @test isequal(minimum!(copy(rval), A, init=false), rval)
    end

    for (tup, rval, rind) in [((2,), [BigInt(-10)], [1])]
        @test isequal(findmax(A, dims=tup), (rval, rind))
        @test isequal(findmax(x -> -(x + 20), A, dims=tup), (rval, rind))
        @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
        @test isequal(maximum(A, dims=tup), rval)
        @test isequal(maximum!(similar(rval), A), rval)
        @test isequal(maximum!(copy(rval), A, init=false), rval)
    end

    A = [BigInt(10) BigInt(-10)]
    A′ = [BigInt(1) BigInt(10)]
    for (tup, rval, rind) in [((2,), reshape([BigInt(-10)], 1, 1), reshape([CartesianIndex(1,2)], 1, 1))]
        @test isequal(findmin(A, dims=tup), (rval, rind))
        @test isequal(findmin(x -> x == 1 ? 10^x : x - 20, A′, dims=tup), (rval, rind))
        @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
        @test isequal(minimum(A, dims=tup), rval)
        @test isequal(minimum!(similar(rval), A), rval)
        @test isequal(minimum!(copy(rval), A, init=false), rval)
    end

    for (tup, rval, rind) in [((2,), reshape([BigInt(10)], 1, 1), reshape([CartesianIndex(1,1)], 1, 1))]
        @test isequal(findmax(A, dims=tup), (rval, rind))
        @test isequal(findmax(x -> x == 1 ? 10^x : x - 20, A′, dims=tup), (rval, rind))
        @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
        @test isequal(maximum(A, dims=tup), rval)
        @test isequal(maximum!(similar(rval), A), rval)
        @test isequal(maximum!(copy(rval), A, init=false), rval)
    end
end

@testset "String in findmin/findmax/minimum/maximum" begin
    A = ["a", "b"]
    for (tup, rval, rind) in [((1,), ["a"], [1])]
        @test isequal(findmin(A, dims=tup), (rval, rind))
        @test isequal(findmin(x -> (x^2)[1:1], A, dims=tup), (rval, rind))
        @test isequal(findmin!(similar(rval), similar(rind), A), (rval, rind))
        @test isequal(minimum(A, dims=tup), rval)
        @test isequal(minimum!(similar(rval), A), rval)
        @test isequal(minimum!(copy(rval), A, init=false), rval)
    end

    for (tup, rval, rind) in [((1,), ["b"], [2])]
        @test isequal(findmax(A, dims=tup), (rval, rind))
        @test isequal(findmax(x -> (x^2)[1:1], A, dims=tup), (rval, rind))
        @test isequal(findmax!(similar(rval), similar(rind), A), (rval, rind))
        @test isequal(maximum(A, dims=tup), rval)
        @test isequal(maximum!(similar(rval), A), rval)
        @test isequal(maximum!(copy(rval), A, init=false), rval)
    end
end

f44906(x) = maximum(f44906, x); f44906(x::Number) = x
g44906(x) = mapreduce(g44906, max, x); g44906(x::Number) = x
@testset "inference with minimum/maximum, issue #44906" begin
    x = [[1, 2], [3, 4]];
    @test @inferred(f44906(x)) == 4
    @test @inferred(g44906(x)) == 4
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
    @test eltype(result) === (T <: Base.BitSignedSmall ? Int :
                              T <: Base.BitUnsignedSmall ? UInt :
                              T)
end

@testset "argmin/argmax" begin
    B = reshape(3^3:-1:1, (3, 3, 3))
    @test B[argmax(B, dims=[2, 3])] == @inferred(maximum(B, dims=[2, 3]))
    @test B[argmin(B, dims=[2, 3])] == @inferred(minimum(B, dims=[2, 3]))
end

@testset "careful with @inbounds" begin
    Base.@propagate_inbounds f(x) = x == 2 ? x[-10000] : x
    Base.@propagate_inbounds op(x,y) = x[-10000] + y[-10000]
    for (arr, dims) in (([1,1,2], 1), ([1 1 2], 2), ([ones(Int,256);2], 1))
        @test_throws BoundsError mapreduce(f, +, arr)
        @test_throws BoundsError mapreduce(f, +, arr; dims)
        @test_throws BoundsError mapreduce(f, +, arr; dims, init=0)
        @test_throws BoundsError mapreduce(identity, op, arr)
        try
            #=@test_throws BoundsError=# mapreduce(identity, op, arr; dims)
        catch ex
            @test_broken ex isa BoundsError
        end
        @test_throws BoundsError mapreduce(identity, op, arr; dims, init=0)

        @test_throws BoundsError findmin(f, arr)
        @test_throws BoundsError findmin(f, arr; dims)

        @test_throws BoundsError mapreduce(f, max, arr)
        @test_throws BoundsError mapreduce(f, max, arr; dims)
        @test_throws BoundsError mapreduce(f, max, arr; dims, init=0)
    end
end

@testset "in-place reductions with mismatched dimensionalities" begin
    B = reshape(1:24, 4, 3, 2)
    for R in (fill(0, 4), fill(0, 4, 1), fill(0, 4, 1, 1))
        @test @inferred(maximum!(R, B)) == reshape(21:24, size(R))
        @test @inferred(minimum!(R, B)) == reshape(1:4, size(R))
        @test @inferred(extrema!(fill((0,0), size(R)), B)) == reshape(tuple.(1:4, 21:24), size(R))
    end
    for R in (fill(0, 1, 3), fill(0, 1, 3, 1))
        @test @inferred(maximum!(R, B)) == reshape(16:4:24, size(R))
        @test @inferred(minimum!(R, B)) == reshape(1:4:9, size(R))
        @test @inferred(extrema!(fill((0,0), size(R)), B)) == reshape(tuple.(1:4:9, 16:4:24), size(R))
    end
    for (ini, f!) in zip((0,0,(0,0)), (maximum!, minimum!, extrema!))
        @test_throws DimensionMismatch f!(fill(ini, 4, 1, 1, 1), B)
        @test_throws DimensionMismatch f!(fill(ini, 1, 3, 1, 1), B)
        @test_throws DimensionMismatch f!(fill(ini, 1, 1, 2, 1), B)
    end
end

function unordered_test_for_extrema(a; dims_test = ((), 1, 2, (1,2), 3))
    for dims in dims_test
        vext = extrema(a; dims)
        vmin, vmax = minimum(a; dims), maximum(a; dims)
        @test isequal(extrema!(similar(vext, NTuple{2, eltype(a)}), a), vext)
        @test all(x -> isequal(x[1], x[2:3]), zip(vext,vmin,vmax))
    end
    true
end
@testset "0.0,-0.0 test for extrema with dims" begin
    @test extrema([-0.0;0.0], dims = 1)[1] === (-0.0,0.0)
    @test tuple(extrema([-0.0;0.0], dims = 2)...) === ((-0.0, -0.0), (0.0, 0.0))
end
@testset "NaN/missing test for extrema with dims #43599" begin
    for sz = (3, 10, 100)
        for T in (Int, Float64, BigFloat, BigInt)
            Aₘ = Matrix{Union{T, Missing}}(rand(-sz:sz, sz, sz))
            Aₘ[rand(1:sz*sz, sz)] .= missing
            @test unordered_test_for_extrema(Aₘ)
            if T <: AbstractFloat
                Aₙ = map(i -> ismissing(i) ? T(NaN) : i, Aₘ)
                @test unordered_test_for_extrema(Aₙ)
                p = rand(1:sz*sz, sz)
                Aₘ[p] .= NaN
                @test unordered_test_for_extrema(Aₘ)
            end
        end
    end
end

@testset "minimum/maximum over dims with missing (#35308)" begin
    for T in (Int, Float64, BigInt, BigFloat)
        x = Union{T, Missing}[1 missing; 2 missing]
        @test isequal(minimum(x, dims=1), reshape([1, missing], 1, :))
        @test isequal(maximum(x, dims=1), reshape([2, missing], 1, :))
        @test isequal(minimum(x, dims=2), reshape([missing, missing], :, 1))
        @test isequal(maximum(x, dims=2), reshape([missing, missing], :, 1))
    end
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

# count
@testset "count: throw on non-bool types" begin
    @test_throws TypeError count([1], dims=1)
    @test_throws TypeError count!([1], [1])
end

@test @inferred(count(false:true, dims=:, init=0x0004)) === 0x0005
@test @inferred(count(isodd, reshape(1:9, 3, 3), dims=:, init=Int128(0))) === Int128(5)

@testset "reduced_index for BigInt (issue #39995)" begin
    for T in [Int8, Int16, Int32, Int64, Int128, BigInt]
        r = T(1):T(2)
        ax = axes(r, 1)
        axred = Base.reduced_index(ax)
        @test axred == Base.OneTo(1)
        @test typeof(axred) === typeof(ax)
        r_red = reduce(+, r, dims = 1)
        @test eltype(r_red) == T
        @test r_red == [3]
    end
end

@testset "type stability (issue #43461)" begin
    @test (@inferred maximum(Float64, reshape(1:4,2,:); dims = 2)) == reshape([3,4],2,1)
end

@testset "Min/Max initialization test" begin
    A = Vector{Union{Missing,Int}}(1:4)
    A[2] = missing
    @test_broken @inferred(minimum(exp, A; dims = 1))[1] === missing
    @test_broken @inferred(maximum(exp, A; dims = 1))[1] === missing
    @test_broken @inferred(extrema(exp, A; dims = 1))[1] === (missing, missing)
end

@testset "generic sum reductions; issue #31427" begin
    add31427(a, b) = a+b
    A = rand(1:10, 5, 5)
    @test reduce(add31427, A, dims = (1, 2)) == reduce(+, A, dims = (1, 2)) == [sum(A);;]

    As = [rand(5, 4) for _ in 1:2, _ in 1:3]
    @test reduce(hcat, reduce(vcat, As, dims=1)) == [As[1,1] As[1,2] As[1,3]; As[2,1] As[2,2] As[2,3]]
end

@testset "sum with `missing`s; issue #55213 and #32366" begin
    @test isequal(sum([0.0 1; 0.0 missing], dims=2), [1.0; missing;;])
    @test sum([0.0 1; 0.0 missing], dims=2)[1] === 1.0
    @test isequal(sum(Any[0.0 1; 0.0 missing], dims=2), [1.0; missing;;])
    @test sum(Any[0.0 1; 0.0 missing], dims=2)[1] === 1.0

    @test isequal(sum([1 0.0; 0.0 missing], dims=1), [1.0 missing])
    @test sum([1 0.0; 0.0 missing], dims=1)[1] === 1.0
    @test isequal(sum(Any[1 0.0; 0.0 missing], dims=1), [1.0 missing])
    @test sum(Any[1 0.0; 0.0 missing], dims=1)[1] === 1.0

    @test isequal(sum([true false missing], dims=2), sum([1 0 missing], dims=2))
    @test isequal(sum([true false missing], dims=2), [sum([true false missing]);;])
    @test isequal(sum([true false missing], dims=2), [missing;;])
end

@testset "issues #45566 and #47231; initializers" begin
    @test reduce(gcd, [1]; dims=1) == [reduce(gcd, [1])] == [1]

    x = reshape(1:6, 2, 3)
    @test reduce(+, x, dims=2) == [9;12;;]
    @test reduce(-, x, dims=1) == [1-2 3-4 5-6]
    @test reduce(/, x, dims=1) == [1/2 3/4 5/6]
    @test reduce(^, x, dims=1) == [1^2 3^4 5^6]
    # These have arbitrary associativity, but they shouldn't error
    @test reduce(-, x, dims=2) in ([(1-3)-5; (2-4)-6;;], [1-(3-5); 2-(4-6);;])
    @test reduce(/, x, dims=2) in ([(1/3)/5; (2/4)/6;;], [1/(3/5); 2/(4/6);;])
    @test reduce(^, x, dims=2) in ([(1^3)^5; (2^4)^6;;], [1^(3^5); 2^(4^6);;])
end

@testset "better initializations" begin
    @test mapreduce(_ -> pi, +, [1,2]; dims=1) == [mapreduce(_ -> pi, +, [1,2])] == [2pi]
    @test mapreduce(_ -> pi, +, [1 2]; dims=2) == [mapreduce(_ -> pi, +, [1 2]);;] == [2pi;;]
    @test mapreduce(_ -> pi, +, [1 2]; dims=(1,2)) == [2pi;;]
    @test mapreduce(_ -> pi, +, [1]; dims=1) == [mapreduce(_ -> pi, +, [1])] == [pi]
    @test mapreduce(_ -> pi, +, [1;;]; dims=2) == [mapreduce(_ -> pi, +, [1;;]);;] == [pi;;]
    @test_throws ArgumentError mapreduce(_ -> pi, +, []; dims=1)
    @test_throws ArgumentError mapreduce(_ -> pi, +, [;;]; dims=2)
    @test_throws ArgumentError mapreduce(_ -> pi, +, [;;]; dims=(1,2))
    @test_throws ArgumentError mapreduce(_ -> pi, +, [])
    @test_throws ArgumentError mapreduce(_ -> pi, +, [;;])

    @test mapreduce(x -> log(x-1), +, [2,3,4]; dims=1) == [mapreduce(x -> log(x-1), +, [2,3,4])] == [log(1) + log(2) + log(3)]
    @test mapreduce(x -> log(x-1), +, [2,3]; dims=1) == [mapreduce(x -> log(x-1), +, [2,3])] == [log(1) + log(2)]
    @test mapreduce(x -> log(x-1), +, [2]; dims=1) == [mapreduce(x -> log(x-1), +, [2])] == [log(1)]
    @test mapreduce(x -> log(x-1), +, [2 3 4]; dims=2) == [mapreduce(x -> log(x-1), +, [2,3,4]);;] == [log(1) + log(2) + log(3);;]
    @test mapreduce(x -> log(x-1), +, [2 3]; dims=2) == [mapreduce(x -> log(x-1), +, [2,3]);;] == [log(1) + log(2);;]
    @test mapreduce(x -> log(x-1), +, [2;;]; dims=2) == [mapreduce(x -> log(x-1), +, [2]);;] == [log(1);;]
    @test_throws ArgumentError mapreduce(x -> log(x-1), +, []; dims=1)
    @test_throws ArgumentError mapreduce(x -> log(x-1), +, [;;]; dims=2)
    @test_throws ArgumentError mapreduce(x -> log(x-1), +, [;;]; dims=(1,2))
    @test_throws ArgumentError mapreduce(x -> log(x-1), +, [])
    @test_throws ArgumentError mapreduce(x -> log(x-1), +, [;;])

    @test sum(x->sqrt(x-1), ones(5); dims=1) == [sum(x->sqrt(x-1), ones(5))] == [0.0]
    @test sum(x->sqrt(x-1), ones(1); dims=1) == [sum(x->sqrt(x-1), ones(1))] == [0.0]
    @test_throws ArgumentError sum(x->sqrt(x-1), ones(0); dims=1)
    @test_throws ArgumentError sum(x->sqrt(x-1), ones(0))
end

@testset "reductions on broadcasted; issue #41054" begin
    A = clamp.(randn(3,4), -1, 1)
    bc = Base.broadcasted(+, A, 2)
    @test sum(bc, dims=1) ≈ sum(A .+ 2, dims=1)
    @test mapreduce(sqrt, +, bc, dims=1) ≈ mapreduce(sqrt, +, A .+ 2, dims=1)

    @test sum(bc, dims=2) ≈ sum(A .+ 2, dims=2)
    @test mapreduce(sqrt, +, bc, dims=2) ≈ mapreduce(sqrt, +, A .+ 2, dims=2)

    @test sum(bc, dims=(1,2)) ≈ [sum(A .+ 2)]
    @test mapreduce(sqrt, +, bc, dims=(1,2)) ≈ [mapreduce(sqrt, +, A .+ 2)]
end

@testset "reductions over complex values; issue #54920" begin
    A = Complex{Int}.(rand(Complex{Int8}, 2, 2, 2));
    @test maximum(abs, A; dims=(1,)) == mapreduce(abs, max, A, dims=(1,)) == [maximum(abs, A[:,1,1]) maximum(abs, A[:,2,1]);;; maximum(abs, A[:,1,2]) maximum(abs, A[:,2,2])]
    @test maximum(abs, A; dims=(2,)) == mapreduce(abs, max, A, dims=(2,)) == [maximum(abs, A[1,:,1]); maximum(abs, A[2,:,1]);;; maximum(abs, A[1,:,2]); maximum(abs, A[2,:,2])]
    @test maximum(abs, A; dims=(1, 2)) == mapreduce(abs, max, A, dims=(1, 2)) == [maximum(abs, A[:,:,1]);;; maximum(abs, A[:,:,2])]
    @test maximum(abs, A; dims=(1, 2, 3)) == mapreduce(abs, max, A, dims=(1, 2, 3)) == [maximum(abs, A);;;]
end

@testset "bitwise operators on integers; part of issue #45562" begin
    @test mapreduce(identity, &, [3,3,3]; dims=1) == [mapreduce(identity, &, [3,3,3])] == [3 & 3 & 3] == [3]
    @test mapreduce(identity, |, [3,3,3]; dims=1) == [mapreduce(identity, |, [3,3,3])] == [3 | 3 | 3] == [3]
    @test mapreduce(identity, xor, [3,3,3]; dims=1) == [mapreduce(identity, xor, [3,3,3])] == [xor(xor(3, 3), 3)] == [3]

    @test mapreduce(identity, &, [3,7,6]; dims=1) == [mapreduce(identity, &, [3,7,6])] == [3 & 7 & 6] == [2]
    @test mapreduce(identity, |, [3,7,6]; dims=1) == [mapreduce(identity, |, [3,7,6])] == [3 | 7 | 6] == [7]
    @test mapreduce(identity, xor, [3,7,6]; dims=1) == [mapreduce(identity, xor, [3,7,6])] == [xor(xor(3, 7), 6)] == [2]
end

@testset "indexing" begin
    A = [1 2; 3 4]
    B = [5 6; 7 8]

    @test mapreduce(/, +, A, B) ≈ sum(A./B)
    @test mapreduce(i -> A[i]/B[i], +, eachindex(A,B)) ≈ sum(A./B)
    @test mapreduce(i -> A[i]/B'[i], +, eachindex(A,B')) ≈ sum(A./B')

    @test mapreduce(/, +, A, B; dims=1) ≈ sum(A./B; dims=1)
    @test mapreduce(i -> A[i]/B[i], +, reshape(eachindex(A,B), size(A)); dims=1) ≈ sum(A./B; dims=1)  # BoundsError
    @test mapreduce(i -> A[i]/B'[i], +, eachindex(A,B'); dims=1) ≈ sum(A./B'; dims=1)  # BoundsError
end

@testset "more related to issue #26488" begin
    @test mapreduce(x -> log10(x-1), +, [11, 101]) ≈ 3.0
    @test mapreduce(x -> log10(x-1), *, [11, 101]) ≈ 2.0
    @test mapreduce(x -> log10(x-1), +, [11, 101]; dims=1) ≈ [3.0]
    @test mapreduce(x -> log10(x-1), +, [11, 101]; init=-10) ≈ -7.0
    @test mapreduce(x -> log10(x-1), +, [11, 101]; init=-10, dims=1) ≈ [-7.0]

    @test mapreduce(_ -> pi, +, [1,2]) ≈ 2pi
    @test mapreduce(_ -> pi, *, [1,2]) ≈ pi^2
    @test mapreduce(_ -> pi, +, [1,2]; dims=1) ≈ [2pi]
end

_add(x,y) = x+y  # this avoids typeof(+) dispatch

@testset "mapreduce fast paths, dims=:, op=$op, kw=$kw" for op in (+,_add),
                                                            kw in ((;), (; init=0.0))
    @test_broken 0 == @allocated mapreduce(/, op, 1:3, 4:7; kw...)
    @test_broken 0 == @allocated mapreduce(/, op, 1:3, 4:9; kw...)  # stops early
    @test mapreduce(/, op, 1:3, 4:7; kw...) ≈ reduce(op, map(/, 1:3, 4:7); kw...)
    @test mapreduce(/, op, 1:3, 4:9; kw...) ≈ reduce(op, map(/, 1:3, 4:9); kw...)

    A = [1 2; 3 4]
    B = [5 6; 7 8]

    @test_broken 0 == @allocated mapreduce(*, op, A, B; kw...)  # LinearIndices
    @test_broken 0 == @allocated mapreduce(*, op, A, B'; kw...)  # CartesianIndices

    @test mapreduce(*, op, A, B; kw...) == reduce(op, map(*, A, B); kw...)
    @test mapreduce(*, op, A, B'; kw...) == reduce(op, map(*, A, B'); kw...)

    @test_broken 0 == @allocated mapreduce(*, op, A, 5:7; kw...)  # stops early
    @test_broken 0 == @allocated mapreduce(*, op, 1:3, B'; kw...)  # stops early
    @test mapreduce(*, op, A, 5:7; kw...) == reduce(op, map(*, A, 5:7); kw...)
    @test mapreduce(*, op, 1:3, B'; kw...) == reduce(op, map(*, 1:3, B'); kw...)
    @test mapreduce(*, op, 1:3, B', 10:20; kw...) == reduce(op, map(*, 1:3, B', 10:20); kw...)

    @test_throws DimensionMismatch map(*, A, hcat(B, 9:10))  # same ndims, does not stop early
    @test_throws DimensionMismatch mapreduce(*, op, A, hcat(B, 9:10); kw...)
end

@testset "mapreduce fast paths, dims=$dims, op=$op, kw=$kw" for dims in (1,2,[2],(1,2),3),
                                                                op in (+,*,_add),
                                                                kw in ((;), (; init=0.0))
    (kw == (;) && op == _add) && continue

    @test mapreduce(/, op, 1:3, 4:7; dims, kw...) ≈ reduce(op, map(/, 1:3, 4:7); dims, kw...)
    @test mapreduce(/, op, 1:3, 4:9; dims, kw...) ≈ reduce(op, map(/, 1:3, 4:9); dims, kw...)

    A = [1 2; 3 4]
    B = [5 6; 7 8]
    @test mapreduce(*, op, A, B; dims, kw...) == reduce(op, map(*, A, B); dims, kw...)  # LinearIndices
    @test mapreduce(*, op, A, B'; dims, kw...) == reduce(op, map(*, A, B'); dims, kw...)  # CartesianIndices

    @test_broken @allocated(mapreduce(*, op, A, B; dims, kw...)) < @allocated(reduce(op, map(*, A, B); dims, kw...))
    @test_broken @allocated(mapreduce(*, op, A, B'; dims, kw...)) < @allocated(reduce(op, map(*, A, B'); dims, kw...))

    @test mapreduce(*, op, A, 5:7; dims, kw...) == reduce(op, map(*, A, 5:7); dims, kw...)  # stops early
    @test mapreduce(*, op, 1:3, B'; dims, kw...) == reduce(op, map(*, 1:3, B'); dims, kw...)

    @test_throws DimensionMismatch mapreduce(*, +, A, hcat(B, 9:10); dims, kw...)
end

struct Infinity21097 <: Number
end
Base.:+(::Infinity21097,::Infinity21097) = Infinity21097()
@testset "don't call zero on unknown numbers, issue #21097" begin
    @test reduce(+, [Infinity21097()], init=Infinity21097()) == Infinity21097()
    @test reduce(+, [Infinity21097()], init=Infinity21097(), dims=1) == [Infinity21097()]
    @test reduce(+, [Infinity21097();;], init=Infinity21097(), dims=2) == [Infinity21097();;]
    for len in [2,15,16,17,31,32,33,63,64,65,127,128,129]
        for init in ((), (; init=Infinity21097()))
            @test reduce(+,fill(Infinity21097(), len); init...) == Infinity21097()
            @test reduce(+,fill(Infinity21097(), len, 2); init...) == Infinity21097()
            @test reduce(+,fill(Infinity21097(), len, 16); init...) == Infinity21097()
            @test reduce(+,fill(Infinity21097(), len); dims = 1, init...) == [Infinity21097()]
            @test reduce(+,fill(Infinity21097(), len, 2); dims = 1, init...) == fill(Infinity21097(), 1, 2)
            @test reduce(+,fill(Infinity21097(), len, 16); dims = 1, init...) == fill(Infinity21097(), 1, 16)
            @test reduce(+,fill(Infinity21097(), len, 2); dims = 2, init...) == fill(Infinity21097(), len, 1)
            @test reduce(+,fill(Infinity21097(), len, 16); dims = 2, init...) == fill(Infinity21097(), len, 1)
        end
    end
end

@testset "don't assume zero is the neutral element; issue #54875" begin
    A = rand(3,4)
    @test sum(I -> A[I], CartesianIndices(A); dims=1) == sum(I -> A[I], CartesianIndices(A); dims=1, init=0.)
end

@testset "cannot construct a value of type Union{} for return result; issue #43731" begin
    a = collect(1.0:4)
    f(x) = x < 3 ? missing : x
    @test ismissing(minimum(f, a))
    @test ismissing(minimum(f, a); dims = 1)
end

@testset "zero indices are not special, issue #38660" begin
    v = OffsetVector([-1, 1], 0:1)
    @test findmin(v, dims=1) == OffsetVector.(([-1], [0]), (0:0,))

    A = rand(3,4,5)
    OA = OffsetArray(A, (-1,-2,-3))
    for dims in ((), 1, 2, 3, (1,2), (1,3), (2,3))
        av, ai = findmin(A; dims)
        ov, oi = findmin(OA; dims)
        @test av == ov.parent
        @test ai == oi.parent .+ CartesianIndex(1,2,3)
    end
end
