# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Random, LinearAlgebra

@testset "middle" begin
    @test middle(3) === 3.0
    @test middle(2, 3) === 2.5
    let x = ((realmax(1.0)/4)*3)
        @test middle(x, x) === x
    end
    @test middle(1:8) === 4.5
    @test middle([1:8;]) === 4.5

    # ensure type-correctness
    for T in [Bool,Int8,Int16,Int32,Int64,Int128,UInt8,UInt16,UInt32,UInt64,UInt128,Float16,Float32,Float64]
        @test middle(one(T)) === middle(one(T), one(T))
    end
end

@testset "median" begin
    @test median([1.]) === 1.
    @test median([1.,3]) === 2.
    @test median([1.,3,2]) === 2.

    @test median([1,3,2]) === 2.0
    @test median([1,3,2,4]) === 2.5

    @test median([0.0,Inf]) == Inf
    @test median([0.0,-Inf]) == -Inf
    @test median([0.,Inf,-Inf]) == 0.0
    @test median([1.,-1.,Inf,-Inf]) == 0.0
    @test isnan(median([-Inf,Inf]))

    X = [2 3 1 -1; 7 4 5 -4]
    @test all(median(X, dims=2) .== [1.5, 4.5])
    @test all(median(X, dims=1) .== [4.5 3.5 3.0 -2.5])
    @test X == [2 3 1 -1; 7 4 5 -4] # issue #17153

    @test_throws ArgumentError median([])
    @test isnan(median([NaN]))
    @test isnan(median([0.0,NaN]))
    @test isnan(median([NaN,0.0]))
    @test isequal(median([NaN 0.0; 1.2 4.5], dims=2), reshape([NaN; 2.85], 2, 1))

    @test median!([1 2 3 4]) == 2.5
    @test median!([1 2; 3 4]) == 2.5

    @test invoke(median, Tuple{AbstractVector}, 1:10) == median(1:10) == 5.5
end

@testset "mean" begin
    @test_throws ArgumentError mean(())
    @test mean((1,2,3)) === 2.
    @test mean([0]) === 0.
    @test mean([1.]) === 1.
    @test mean([1.,3]) == 2.
    @test mean([1,2,3]) == 2.
    @test mean([0 1 2; 4 5 6], dims=1) == [2.  3.  4.]
    @test mean([1 2 3; 4 5 6], dims=1) == [2.5 3.5 4.5]
    @test mean(i->i+1, 0:2) === 2.
    @test mean(isodd, [3]) === 1.
    @test mean(x->3x, (1,1)) === 3.

    @test isnan(mean([NaN]))
    @test isnan(mean([0.0,NaN]))
    @test isnan(mean([NaN,0.0]))

    @test isnan(mean([0.,Inf,-Inf]))
    @test isnan(mean([1.,-1.,Inf,-Inf]))
    @test isnan(mean([-Inf,Inf]))
    @test isequal(mean([NaN 0.0; 1.2 4.5], dims=2), reshape([NaN; 2.85], 2, 1))

    # Check that small types are accumulated using wider type
    for T in (Int8, UInt8)
        x = [typemax(T) typemax(T)]
        g = (v for v in x)
        @test mean(x) == mean(g) == typemax(T)
        @test mean(identity, x) == mean(identity, g) == typemax(T)
        @test mean(x, dims=2) == [typemax(T)]'
    end
end

@testset "quantile" begin
    @test quantile([1,2,3,4],0.5) == 2.5
    @test quantile([1,2,3,4],[0.5]) == [2.5]
    @test quantile([1., 3],[.25,.5,.75])[2] == median([1., 3])
    @test quantile(100.0:-1.0:0.0, 0.0:0.1:1.0) == 0.0:10.0:100.0
    @test quantile(0.0:100.0, 0.0:0.1:1.0, sorted=true) == 0.0:10.0:100.0
    @test quantile(100f0:-1f0:0.0, 0.0:0.1:1.0) == 0f0:10f0:100f0
    @test quantile([Inf,Inf],0.5) == Inf
    @test quantile([-Inf,1],0.5) == -Inf
    @test quantile([0,1],1e-18) == 1e-18
    @test quantile([1, 2, 3, 4],[]) == []
    @test quantile([1, 2, 3, 4], (0.5,)) == (2.5,)
    @test quantile([4, 9, 1, 5, 7, 8, 2, 3, 5, 17, 11], (0.1, 0.2, 0.4, 0.9)) == (2.0, 3.0, 5.0, 11.0)
    @test quantile([1, 2, 3, 4], ()) == ()
end

# StatsBase issue 164
let y = [0.40003674665581906, 0.4085630862624367, 0.41662034698690303, 0.41662034698690303, 0.42189053966652057, 0.42189053966652057, 0.42553514344518345, 0.43985732442991354]
    @test issorted(quantile(y, range(0.01, stop=0.99, length=17)))
end

@testset "Issue #17153 and PR #17154" begin
    a = rand(10,10)
    b = copy(a)
    x = median(a, dims=1)
    @test b == a
    x = median(a, dims=2)
    @test b == a
    x = mean(a, dims=1)
    @test b == a
    x = mean(a, dims=2)
    @test b == a
end

# dimensional correctness
isdefined(Main, :TestHelpers) || @eval Main include("TestHelpers.jl")
using .Main.TestHelpers: Furlong
@testset "Unitful elements" begin
    r = Furlong(1):Furlong(1):Furlong(2)
    a = Vector(r)
    @test sum(r) == sum(a) == Furlong(3)
    @test cumsum(r) == Furlong.([1,3])
    @test mean(r) == mean(a) == median(a) == median(r) == Furlong(1.5)

    # Issue #21786
    A = [Furlong{1}(rand(-5:5)) for i in 1:2, j in 1:2]
    @test mean(mean(A, dims=1), dims=2)[1] === mean(A)
end

# Issue #22901
@testset "quantile of Any arrays" begin
    x = Any[1, 2, 4, 10]
    y = Any[1, 2, 4, 10//1]
    @test quantile(x, 0.5)  === 3.0
    @test quantile(x, 1//2) === 3//1
end

@testset "Mean along dimension of empty array" begin
    a0  = zeros(0)
    a00 = zeros(0, 0)
    a01 = zeros(0, 1)
    a10 = zeros(1, 0)
    @test isequal(mean(a0, dims=1)      , fill(NaN, 1))
    @test isequal(mean(a00, dims=(1, 2)), fill(NaN, 1, 1))
    @test isequal(mean(a01, dims=1)     , fill(NaN, 1, 1))
    @test isequal(mean(a10, dims=2)     , fill(NaN, 1, 1))
end
