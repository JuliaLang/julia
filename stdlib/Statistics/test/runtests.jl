# This file is a part of Julia. License is MIT: https://julialang.org/license

using Statistics, Test, Random, LinearAlgebra, SparseArrays
using Test: guardseed

Random.seed!(123)

@testset "middle" begin
    @test middle(3) === 3.0
    @test middle(2, 3) === 2.5
    let x = ((floatmax(1.0)/4)*3)
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
    @test isnan(median([NaN,0.0,1.0]))
    @test isnan(median(Any[NaN,0.0,1.0]))
    @test isequal(median([NaN 0.0; 1.2 4.5], dims=2), reshape([NaN; 2.85], 2, 1))

    @test ismissing(median([1, missing]))
    @test ismissing(median([1, 2, missing]))
    @test ismissing(median([NaN, 2.0, missing]))
    @test ismissing(median([NaN, missing]))
    @test ismissing(median([missing, NaN]))
    @test ismissing(median(Any[missing, 2.0, 3.0, 4.0, NaN]))
    @test median(skipmissing([1, missing, 2])) === 1.5

    @test median!([1 2 3 4]) == 2.5
    @test median!([1 2; 3 4]) == 2.5

    @test invoke(median, Tuple{AbstractVector}, 1:10) == median(1:10) == 5.5

    @test @inferred(median(Float16[1, 2, NaN])) === Float16(NaN)
    @test @inferred(median(Float16[1, 2, 3]))   === Float16(2)
    @test @inferred(median(Float32[1, 2, NaN])) === NaN32
    @test @inferred(median(Float32[1, 2, 3]))   === 2.0f0
end

@testset "mean" begin
    @test_throws MethodError mean(())
    @test mean((1,2,3)) === 2.
    @test mean([0]) === 0.
    @test mean([1.]) === 1.
    @test mean([1.,3]) == 2.
    @test mean([1,2,3]) == 2.
    @test mean([0 1 2; 4 5 6], dims=1) == [2.  3.  4.]
    @test mean([1 2 3; 4 5 6], dims=1) == [2.5 3.5 4.5]
    @test mean(-, [1 2 3 ; 4 5 6], dims=1) == [-2.5 -3.5 -4.5]
    @test mean(-, [1 2 3 ; 4 5 6], dims=2) == transpose([-2.0 -5.0])
    @test mean(-, [1 2 3 ; 4 5 6], dims=(1, 2)) == -3.5 .* ones(1, 1)
    @test mean(-, [1 2 3 ; 4 5 6], dims=(1, 1)) == [-2.5 -3.5 -4.5]
    @test mean(-, [1 2 3 ; 4 5 6], dims=()) == Float64[-1 -2 -3 ; -4 -5 -6]
    @test mean(i->i+1, 0:2) === 2.
    @test mean(isodd, [3]) === 1.
    @test mean(x->3x, (1,1)) === 3.

    # mean of iterables:
    n = 10; a = randn(n); b = randn(n)
    @test mean(Tuple(a)) ≈ mean(a)
    @test mean(Tuple(a + b*im)) ≈ mean(a + b*im)
    @test mean(cos, Tuple(a)) ≈ mean(cos, a)
    @test mean(x->x/2, a + b*im) ≈ mean(a + b*im) / 2.
    @test ismissing(mean(Tuple((1, 2, missing, 4, 5))))

    @test isnan(mean([NaN]))
    @test isnan(mean([0.0,NaN]))
    @test isnan(mean([NaN,0.0]))

    @test isnan(mean([0.,Inf,-Inf]))
    @test isnan(mean([1.,-1.,Inf,-Inf]))
    @test isnan(mean([-Inf,Inf]))
    @test isequal(mean([NaN 0.0; 1.2 4.5], dims=2), reshape([NaN; 2.85], 2, 1))

    @test ismissing(mean([1, missing]))
    @test ismissing(mean([NaN, missing]))
    @test ismissing(mean([missing, NaN]))
    @test isequal(mean([missing 1.0; 2.0 3.0], dims=1), [missing 2.0])
    @test mean(skipmissing([1, missing, 2])) === 1.5
    @test isequal(mean(Complex{Float64}[]), NaN+NaN*im)
    @test mean(Complex{Float64}[]) isa Complex{Float64}
    @test isequal(mean(skipmissing(Complex{Float64}[])), NaN+NaN*im)
    @test mean(skipmissing(Complex{Float64}[])) isa Complex{Float64}
    @test isequal(mean(abs, Complex{Float64}[]), NaN)
    @test mean(abs, Complex{Float64}[]) isa Float64
    @test isequal(mean(abs, skipmissing(Complex{Float64}[])), NaN)
    @test mean(abs, skipmissing(Complex{Float64}[])) isa Float64
    @test isequal(mean(Int[]), NaN)
    @test mean(Int[]) isa Float64
    @test isequal(mean(skipmissing(Int[])), NaN)
    @test mean(skipmissing(Int[])) isa Float64
    @test_throws MethodError mean([])
    @test_throws MethodError mean(skipmissing([]))
    @test_throws ArgumentError mean((1 for i in 2:1))

    # Check that small types are accumulated using wider type
    for T in (Int8, UInt8)
        x = [typemax(T) typemax(T)]
        g = (v for v in x)
        @test mean(x) == mean(g) == typemax(T)
        @test mean(identity, x) == mean(identity, g) == typemax(T)
        @test mean(x, dims=2) == [typemax(T)]'
    end
end

@testset "mean/median for ranges" begin
    for f in (mean, median)
        for n = 2:5
            @test f(2:n) == f([2:n;])
            @test f(2:0.1:n) ≈ f([2:0.1:n;])
        end
    end
    @test mean(2:1) === NaN
    @test mean(big(2):1) isa BigFloat
end

@testset "var & std" begin
    # edge case: empty vector
    # iterable; this has to throw for type stability
    @test_throws MethodError var(())
    @test_throws MethodError var((); corrected=false)
    @test_throws MethodError var((); mean=2)
    @test_throws MethodError var((); mean=2, corrected=false)
    # reduction
    @test isnan(var(Int[]))
    @test isnan(var(Int[]; corrected=false))
    @test isnan(var(Int[]; mean=2))
    @test isnan(var(Int[]; mean=2, corrected=false))
    # reduction across dimensions
    @test isequal(var(Int[], dims=1), [NaN])
    @test isequal(var(Int[], dims=1; corrected=false), [NaN])
    @test isequal(var(Int[], dims=1; mean=[2]), [NaN])
    @test isequal(var(Int[], dims=1; mean=[2], corrected=false), [NaN])

    # edge case: one-element vector
    # iterable
    @test isnan(@inferred(var((1,))))
    @test var((1,); corrected=false) === 0.0
    @test var((1,); mean=2) === Inf
    @test var((1,); mean=2, corrected=false) === 1.0
    # reduction
    @test isnan(@inferred(var([1])))
    @test var([1]; corrected=false) === 0.0
    @test var([1]; mean=2) === Inf
    @test var([1]; mean=2, corrected=false) === 1.0
    # reduction across dimensions
    @test isequal(@inferred(var([1], dims=1)), [NaN])
    @test var([1], dims=1; corrected=false) ≈ [0.0]
    @test var([1], dims=1; mean=[2]) ≈ [Inf]
    @test var([1], dims=1; mean=[2], corrected=false) ≈ [1.0]

    @test var(1:8) == 6.
    @test varm(1:8,1) == varm(Vector(1:8),1)
    @test isnan(varm(1:1,1))
    @test isnan(var(1:1))
    @test isnan(var(1:-1))

    @test @inferred(var(1.0:8.0)) == 6.
    @test varm(1.0:8.0,1.0) == varm(Vector(1.0:8.0),1)
    @test isnan(varm(1.0:1.0,1.0))
    @test isnan(var(1.0:1.0))
    @test isnan(var(1.0:-1.0))

    @test @inferred(var(1.0f0:8.0f0)) === 6.f0
    @test varm(1.0f0:8.0f0,1.0f0) == varm(Vector(1.0f0:8.0f0),1)
    @test isnan(varm(1.0f0:1.0f0,1.0f0))
    @test isnan(var(1.0f0:1.0f0))
    @test isnan(var(1.0f0:-1.0f0))

    @test varm([1,2,3], 2) ≈ 1.
    @test var([1,2,3]) ≈ 1.
    @test var([1,2,3]; corrected=false) ≈ 2.0/3
    @test var([1,2,3]; mean=0) ≈ 7.
    @test var([1,2,3]; mean=0, corrected=false) ≈ 14.0/3

    @test varm((1,2,3), 2) ≈ 1.
    @test var((1,2,3)) ≈ 1.
    @test var((1,2,3); corrected=false) ≈ 2.0/3
    @test var((1,2,3); mean=0) ≈ 7.
    @test var((1,2,3); mean=0, corrected=false) ≈ 14.0/3
    @test_throws ArgumentError var((1,2,3); mean=())

    @test var([1 2 3 4 5; 6 7 8 9 10], dims=2) ≈ [2.5 2.5]'
    @test var([1 2 3 4 5; 6 7 8 9 10], dims=2; corrected=false) ≈ [2.0 2.0]'

    @test var(collect(1:99), dims=1) ≈ [825]
    @test var(Matrix(transpose(collect(1:99))), dims=2) ≈ [825]

    @test stdm([1,2,3], 2) ≈ 1.
    @test std([1,2,3]) ≈ 1.
    @test std([1,2,3]; corrected=false) ≈ sqrt(2.0/3)
    @test std([1,2,3]; mean=0) ≈ sqrt(7.0)
    @test std([1,2,3]; mean=0, corrected=false) ≈ sqrt(14.0/3)

    @test stdm([1.0,2,3], 2) ≈ 1.
    @test std([1.0,2,3]) ≈ 1.
    @test std([1.0,2,3]; corrected=false) ≈ sqrt(2.0/3)
    @test std([1.0,2,3]; mean=0) ≈ sqrt(7.0)
    @test std([1.0,2,3]; mean=0, corrected=false) ≈ sqrt(14.0/3)

    @test std([1.0,2,3]; dims=1)[] ≈ 1.
    @test std([1.0,2,3]; dims=1, corrected=false)[] ≈ sqrt(2.0/3)
    @test std([1.0,2,3]; dims=1, mean=[0])[] ≈ sqrt(7.0)
    @test std([1.0,2,3]; dims=1, mean=[0], corrected=false)[] ≈ sqrt(14.0/3)

    @test stdm((1,2,3), 2) ≈ 1.
    @test std((1,2,3)) ≈ 1.
    @test std((1,2,3); corrected=false) ≈ sqrt(2.0/3)
    @test std((1,2,3); mean=0) ≈ sqrt(7.0)
    @test std((1,2,3); mean=0, corrected=false) ≈ sqrt(14.0/3)

    @test std([1 2 3 4 5; 6 7 8 9 10], dims=2) ≈ sqrt.([2.5 2.5]')
    @test std([1 2 3 4 5; 6 7 8 9 10], dims=2; corrected=false) ≈ sqrt.([2.0 2.0]')

    let A = ComplexF64[exp(i*im) for i in 1:10^4]
        @test varm(A, 0.) ≈ sum(map(abs2, A)) / (length(A) - 1)
        @test varm(A, mean(A)) ≈ var(A)
    end

    @test var([1//1, 2//1]) isa Rational{Int}
    @test var([1//1, 2//1], dims=1) isa Vector{Rational{Int}}

    @test std([1//1, 2//1]) isa Float64
    @test std([1//1, 2//1], dims=1) isa Vector{Float64}

    @testset "var: empty cases" begin
        A = Matrix{Int}(undef, 0,1)
        @test var(A) === NaN

        @test isequal(var(A, dims=1), fill(NaN, 1, 1))
        @test isequal(var(A, dims=2), fill(NaN, 0, 1))
        @test isequal(var(A, dims=(1, 2)), fill(NaN, 1, 1))
        @test isequal(var(A, dims=3), fill(NaN, 0, 1))
    end

    # issue #6672
    @test std(AbstractFloat[1,2,3], dims=1) == [1.0]

    for f in (var, std)
        @test ismissing(f([1, missing]))
        @test ismissing(f([NaN, missing]))
        @test ismissing(f([missing, NaN]))
        @test isequal(f([missing 1.0; 2.0 3.0], dims=1), [missing f([1.0, 3.0])])
        @test f(skipmissing([1, missing, 2])) === f([1, 2])
    end
    for f in (varm, stdm)
        @test ismissing(f([1, missing], 0))
        @test ismissing(f([1, 2], missing))
        @test ismissing(f([1, NaN], missing))
        @test ismissing(f([NaN, missing], 0))
        @test ismissing(f([missing, NaN], 0))
        @test ismissing(f([NaN, missing], missing))
        @test ismissing(f([missing, NaN], missing))
        @test f(skipmissing([1, missing, 2]), 0) === f([1, 2], 0)
    end

    @test isequal(var(Complex{Float64}[]), NaN)
    @test var(Complex{Float64}[]) isa Float64
    @test isequal(var(skipmissing(Complex{Float64}[])), NaN)
    @test var(skipmissing(Complex{Float64}[])) isa Float64
    @test_throws MethodError var([])
    @test_throws MethodError var(skipmissing([]))
    @test_throws MethodError var((1 for i in 2:1))
    @test isequal(var(Int[]), NaN)
    @test var(Int[]) isa Float64
    @test isequal(var(skipmissing(Int[])), NaN)
    @test var(skipmissing(Int[])) isa Float64
end

function safe_cov(x, y, zm::Bool, cr::Bool)
    n = length(x)
    if !zm
        x = x .- mean(x)
        y = y .- mean(y)
    end
    dot(vec(x), vec(y)) / (n - Int(cr))
end
X = [1.0  5.0;
     2.0  4.0;
     3.0  6.0;
     4.0  2.0;
     5.0  1.0]
Y = [6.0  2.0;
     1.0  7.0;
     5.0  8.0;
     3.0  4.0;
     2.0  3.0]

@testset "covariance" begin
    for vd in [1, 2], zm in [true, false], cr in [true, false]
        # println("vd = $vd: zm = $zm, cr = $cr")
        if vd == 1
            k = size(X, 2)
            Cxx = zeros(k, k)
            Cxy = zeros(k, k)
            for i = 1:k, j = 1:k
                Cxx[i,j] = safe_cov(X[:,i], X[:,j], zm, cr)
                Cxy[i,j] = safe_cov(X[:,i], Y[:,j], zm, cr)
            end
            x1 = vec(X[:,1])
            y1 = vec(Y[:,1])
        else
            k = size(X, 1)
            Cxx = zeros(k, k)
            Cxy = zeros(k, k)
            for i = 1:k, j = 1:k
                Cxx[i,j] = safe_cov(X[i,:], X[j,:], zm, cr)
                Cxy[i,j] = safe_cov(X[i,:], Y[j,:], zm, cr)
            end
            x1 = vec(X[1,:])
            y1 = vec(Y[1,:])
        end

        c = zm ? Statistics.covm(x1, 0, corrected=cr) :
                 cov(x1, corrected=cr)
        @test isa(c, Float64)
        @test c ≈ Cxx[1,1]
        @inferred cov(x1, corrected=cr)

        @test cov(X) == Statistics.covm(X, mean(X, dims=1))
        C = zm ? Statistics.covm(X, 0, vd, corrected=cr) :
                 cov(X, dims=vd, corrected=cr)
        @test size(C) == (k, k)
        @test C ≈ Cxx
        @inferred cov(X, dims=vd, corrected=cr)

        @test cov(x1, y1) == Statistics.covm(x1, mean(x1), y1, mean(y1))
        c = zm ? Statistics.covm(x1, 0, y1, 0, corrected=cr) :
                 cov(x1, y1, corrected=cr)
        @test isa(c, Float64)
        @test c ≈ Cxy[1,1]
        @inferred cov(x1, y1, corrected=cr)

        if vd == 1
            @test cov(x1, Y) == Statistics.covm(x1, mean(x1), Y, mean(Y, dims=1))
        end
        C = zm ? Statistics.covm(x1, 0, Y, 0, vd, corrected=cr) :
                 cov(x1, Y, dims=vd, corrected=cr)
        @test size(C) == (1, k)
        @test vec(C) ≈ Cxy[1,:]
        @inferred cov(x1, Y, dims=vd, corrected=cr)

        if vd == 1
            @test cov(X, y1) == Statistics.covm(X, mean(X, dims=1), y1, mean(y1))
        end
        C = zm ? Statistics.covm(X, 0, y1, 0, vd, corrected=cr) :
                 cov(X, y1, dims=vd, corrected=cr)
        @test size(C) == (k, 1)
        @test vec(C) ≈ Cxy[:,1]
        @inferred cov(X, y1, dims=vd, corrected=cr)

        @test cov(X, Y) == Statistics.covm(X, mean(X, dims=1), Y, mean(Y, dims=1))
        C = zm ? Statistics.covm(X, 0, Y, 0, vd, corrected=cr) :
                 cov(X, Y, dims=vd, corrected=cr)
        @test size(C) == (k, k)
        @test C ≈ Cxy
        @inferred cov(X, Y, dims=vd, corrected=cr)
    end

    @testset "floating point accuracy for `cov` of large numbers" begin
        A = [4.0, 7.0, 13.0, 16.0]
        C = A .+ 1.0e10
        @test cov(A, A) ≈ cov(C, C)
    end
end

function safe_cor(x, y, zm::Bool)
    if !zm
        x = x .- mean(x)
        y = y .- mean(y)
    end
    x = vec(x)
    y = vec(y)
    dot(x, y) / (sqrt(dot(x, x)) * sqrt(dot(y, y)))
end
@testset "correlation" begin
    for vd in [1, 2], zm in [true, false]
        # println("vd = $vd: zm = $zm")
        if vd == 1
            k = size(X, 2)
            Cxx = zeros(k, k)
            Cxy = zeros(k, k)
            for i = 1:k, j = 1:k
                Cxx[i,j] = safe_cor(X[:,i], X[:,j], zm)
                Cxy[i,j] = safe_cor(X[:,i], Y[:,j], zm)
            end
            x1 = vec(X[:,1])
            y1 = vec(Y[:,1])
        else
            k = size(X, 1)
            Cxx = zeros(k, k)
            Cxy = zeros(k, k)
            for i = 1:k, j = 1:k
                Cxx[i,j] = safe_cor(X[i,:], X[j,:], zm)
                Cxy[i,j] = safe_cor(X[i,:], Y[j,:], zm)
            end
            x1 = vec(X[1,:])
            y1 = vec(Y[1,:])
        end

        c = zm ? Statistics.corm(x1, 0) : cor(x1)
        @test isa(c, Float64)
        @test c ≈ Cxx[1,1]
        @inferred cor(x1)

        @test cor(X) == Statistics.corm(X, mean(X, dims=1))
        C = zm ? Statistics.corm(X, 0, vd) : cor(X, dims=vd)
        @test size(C) == (k, k)
        @test C ≈ Cxx
        @inferred cor(X, dims=vd)

        @test cor(x1, y1) == Statistics.corm(x1, mean(x1), y1, mean(y1))
        c = zm ? Statistics.corm(x1, 0, y1, 0) : cor(x1, y1)
        @test isa(c, Float64)
        @test c ≈ Cxy[1,1]
        @inferred cor(x1, y1)

        if vd == 1
            @test cor(x1, Y) == Statistics.corm(x1, mean(x1), Y, mean(Y, dims=1))
        end
        C = zm ? Statistics.corm(x1, 0, Y, 0, vd) : cor(x1, Y, dims=vd)
        @test size(C) == (1, k)
        @test vec(C) ≈ Cxy[1,:]
        @inferred cor(x1, Y, dims=vd)

        if vd == 1
            @test cor(X, y1) == Statistics.corm(X, mean(X, dims=1), y1, mean(y1))
        end
        C = zm ? Statistics.corm(X, 0, y1, 0, vd) : cor(X, y1, dims=vd)
        @test size(C) == (k, 1)
        @test vec(C) ≈ Cxy[:,1]
        @inferred cor(X, y1, dims=vd)

        @test cor(X, Y) == Statistics.corm(X, mean(X, dims=1), Y, mean(Y, dims=1))
        C = zm ? Statistics.corm(X, 0, Y, 0, vd) : cor(X, Y, dims=vd)
        @test size(C) == (k, k)
        @test C ≈ Cxy
        @inferred cor(X, Y, dims=vd)
    end

    @test cor(repeat(1:17, 1, 17))[2] <= 1.0
    @test cor(1:17, 1:17) <= 1.0
    @test cor(1:17, 18:34) <= 1.0
    @test cor(Any[1, 2], Any[1, 2]) == 1.0
    @test isnan(cor([0], Int8[81]))
    let tmp = range(1, stop=85, length=100)
        tmp2 = Vector(tmp)
        @test cor(tmp, tmp) <= 1.0
        @test cor(tmp, tmp2) <= 1.0
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
    @test quantile([4, 9, 1, 5, 7, 8, 2, 3, 5, 17, 11],
                   (0.1, 0.2, 0.4, 0.9)) == (2.0, 3.0, 5.0, 11.0)
    @test quantile(Union{Int, Missing}[4, 9, 1, 5, 7, 8, 2, 3, 5, 17, 11],
                   [0.1, 0.2, 0.4, 0.9]) == [2.0, 3.0, 5.0, 11.0]
    @test quantile(Any[4, 9, 1, 5, 7, 8, 2, 3, 5, 17, 11],
                   [0.1, 0.2, 0.4, 0.9]) == [2.0, 3.0, 5.0, 11.0]
    @test quantile([4, 9, 1, 5, 7, 8, 2, 3, 5, 17, 11],
                   Any[0.1, 0.2, 0.4, 0.9]) == [2.0, 3.0, 5.0, 11.0]
    @test quantile([4, 9, 1, 5, 7, 8, 2, 3, 5, 17, 11],
                   Any[0.1, 0.2, 0.4, 0.9]) isa Vector{Float64}
    @test quantile(Any[4, 9, 1, 5, 7, 8, 2, 3, 5, 17, 11],
                   Any[0.1, 0.2, 0.4, 0.9]) == [2, 3, 5, 11]
    @test quantile(Any[4, 9, 1, 5, 7, 8, 2, 3, 5, 17, 11],
                   Any[0.1, 0.2, 0.4, 0.9]) isa Vector{Float64}
    @test quantile([1, 2, 3, 4], ()) == ()
    @test isempty(quantile([1, 2, 3, 4], Float64[]))
    @test quantile([1, 2, 3, 4], Float64[]) isa Vector{Float64}
    @test quantile([1, 2, 3, 4], []) isa Vector{Any}
    @test quantile([1, 2, 3, 4], [0, 1]) isa Vector{Int}

    @test quantile(Any[1, 2, 3], 0.5) isa Float64
    @test quantile(Any[1, big(2), 3], 0.5) isa BigFloat
    @test quantile(Any[1, 2, 3], Float16(0.5)) isa Float16
    @test quantile(Any[1, Float16(2), 3], Float16(0.5)) isa Float16
    @test quantile(Any[1, big(2), 3], Float16(0.5)) isa BigFloat

    @test_throws ArgumentError quantile([1, missing], 0.5)
    @test_throws ArgumentError quantile([1, NaN], 0.5)
    @test quantile(skipmissing([1, missing, 2]), 0.5) === 1.5

    # make sure that type inference works correctly in normal cases
    for T in [Int, BigInt, Float64, Float16, BigFloat, Rational{Int}, Rational{BigInt}]
        for S in [Float64, Float16, BigFloat, Rational{Int}, Rational{BigInt}]
            @inferred quantile(T[1, 2, 3], S(0.5))
            @inferred quantile(T[1, 2, 3], S(0.6))
            @inferred quantile(T[1, 2, 3], S[0.5, 0.6])
            @inferred quantile(T[1, 2, 3], (S(0.5), S(0.6)))
        end
    end
    x = [3; 2; 1]
    y = zeros(3)
    @test quantile!(y, x, [0.1, 0.5, 0.9]) === y
    @test y == [1.2, 2.0, 2.8]
end

# StatsBase issue 164
let y = [0.40003674665581906, 0.4085630862624367, 0.41662034698690303, 0.41662034698690303, 0.42189053966652057, 0.42189053966652057, 0.42553514344518345, 0.43985732442991354]
    @test issorted(quantile(y, range(0.01, stop=0.99, length=17)))
end

@testset "variance of complex arrays (#13309)" begin
    z = rand(ComplexF64, 10)
    @test var(z) ≈ invoke(var, Tuple{Any}, z) ≈ cov(z) ≈ var(z,dims=1)[1] ≈ sum(abs2, z .- mean(z))/9
    @test isa(var(z), Float64)
    @test isa(invoke(var, Tuple{Any}, z), Float64)
    @test isa(cov(z), Float64)
    @test isa(var(z,dims=1), Vector{Float64})
    @test varm(z, 0.0) ≈ invoke(varm, Tuple{Any,Float64}, z, 0.0) ≈ sum(abs2, z)/9
    @test isa(varm(z, 0.0), Float64)
    @test isa(invoke(varm, Tuple{Any,Float64}, z, 0.0), Float64)
    @test cor(z) === 1.0
    v = varm([1.0+2.0im], 0; corrected = false)
    @test v ≈ 5
    @test isa(v, Float64)
end

@testset "cov and cor of complex arrays (issue #21093)" begin
    x = [2.7 - 3.3im, 0.9 + 5.4im, 0.1 + 0.2im, -1.7 - 5.8im, 1.1 + 1.9im]
    y = [-1.7 - 1.6im, -0.2 + 6.5im, 0.8 - 10.0im, 9.1 - 3.4im, 2.7 - 5.5im]
    @test cov(x, y) ≈ 4.8365 - 12.119im
    @test cov(y, x) ≈ 4.8365 + 12.119im
    @test cov(x, reshape(y, :, 1)) ≈ reshape([4.8365 - 12.119im], 1, 1)
    @test cov(reshape(x, :, 1), y) ≈ reshape([4.8365 - 12.119im], 1, 1)
    @test cov(reshape(x, :, 1), reshape(y, :, 1)) ≈ reshape([4.8365 - 12.119im], 1, 1)
    @test cov([x y]) ≈ [21.779 4.8365-12.119im;
                        4.8365+12.119im 54.548]
    @test cor(x, y) ≈ 0.14032104449218274 - 0.35160772008699703im
    @test cor(y, x) ≈ 0.14032104449218274 + 0.35160772008699703im
    @test cor(x, reshape(y, :, 1)) ≈ reshape([0.14032104449218274 - 0.35160772008699703im], 1, 1)
    @test cor(reshape(x, :, 1), y) ≈ reshape([0.14032104449218274 - 0.35160772008699703im], 1, 1)
    @test cor(reshape(x, :, 1), reshape(y, :, 1)) ≈ reshape([0.14032104449218274 - 0.35160772008699703im], 1, 1)
    @test cor([x y]) ≈ [1.0                                          0.14032104449218274-0.35160772008699703im
                        0.14032104449218274+0.35160772008699703im  1.0]
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
    x = var(a, dims=1)
    @test b == a
    x = var(a, dims=2)
    @test b == a
    x = std(a, dims=1)
    @test b == a
    x = std(a, dims=2)
    @test b == a
end

# dimensional correctness
const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :Furlongs) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "Furlongs.jl"))
using .Main.Furlongs

Statistics.middle(x::Furlong{p}) where {p} = Furlong{p}(middle(x.val))
Statistics.middle(x::Furlong{p}, y::Furlong{p}) where {p} = Furlong{p}(middle(x.val, y.val))

@testset "Unitful elements" begin
    r = Furlong(1):Furlong(1):Furlong(2)
    a = Vector(r)
    @test sum(r) == sum(a) == Furlong(3)
    @test cumsum(r) == Furlong.([1,3])
    @test mean(r) == mean(a) == median(a) == median(r) == Furlong(1.5)
    @test var(r) == var(a) == Furlong{2}(0.5)
    @test std(r) == std(a) == Furlong{1}(sqrt(0.5))

    # Issue #21786
    A = [Furlong{1}(rand(-5:5)) for i in 1:2, j in 1:2]
    @test mean(mean(A, dims=1), dims=2)[1] === mean(A)
    @test var(A, dims=1)[1] === var(A[:, 1])
    @test std(A, dims=1)[1] === std(A[:, 1])
end

# Issue #22901
@testset "var and quantile of Any arrays" begin
    x = Any[1, 2, 4, 10]
    y = Any[1, 2, 4, 10//1]
    @test var(x) === 16.25
    @test var(y) === 65//4
    @test std(x) === sqrt(16.25)
    @test quantile(x, 0.5)  === 3.0
    @test quantile(x, 1//2) === 3//1
end

@testset "Promotion in covzm. Issue #8080" begin
    A = [1 -1 -1; -1 1 1; -1 1 -1; 1 -1 -1; 1 -1 1]
    @test Statistics.covzm(A) - mean(A, dims=1)'*mean(A, dims=1)*size(A, 1)/(size(A, 1) - 1) ≈ cov(A)
    A = [1//1 -1 -1; -1 1 1; -1 1 -1; 1 -1 -1; 1 -1 1]
    @test (A'A - size(A, 1)*mean(A, dims=1)'*mean(A, dims=1))/4 == cov(A)
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

@testset "cov/var/std of Vector{Vector}" begin
    x = [[2,4,6],[4,6,8]]
    @test var(x) ≈ vec(var([x[1] x[2]], dims=2))
    @test std(x) ≈ vec(std([x[1] x[2]], dims=2))
    @test cov(x) ≈ cov([x[1] x[2]], dims=2)
end

@testset "var of sparse array" begin
    se33 = SparseMatrixCSC{Float64}(I, 3, 3)
    sA = sprandn(3, 7, 0.5)
    pA = sparse(rand(3, 7))

    for arr in (se33, sA, pA)
        farr = Array(arr)
        @test var(arr) ≈ var(farr)
        @test var(arr, dims=1) ≈ var(farr, dims=1)
        @test var(arr, dims=2) ≈ var(farr, dims=2)
        @test var(arr, dims=(1, 2)) ≈ [var(farr)]
        @test isequal(var(arr, dims=3), var(farr, dims=3))
    end

    @testset "empty cases" begin
        @test var(sparse(Int[])) === NaN
        @test isequal(var(spzeros(0, 1), dims=1), var(Matrix{Int}(I, 0, 1), dims=1))
        @test isequal(var(spzeros(0, 1), dims=2), var(Matrix{Int}(I, 0, 1), dims=2))
        @test isequal(var(spzeros(0, 1), dims=(1, 2)), var(Matrix{Int}(I, 0, 1), dims=(1, 2)))
        @test isequal(var(spzeros(0, 1), dims=3), var(Matrix{Int}(I, 0, 1), dims=3))
    end
end

# Faster covariance function for sparse matrices
# Prevents densifying the input matrix when subtracting the mean
# Test against dense implementation
# PR https://github.com/JuliaLang/julia/pull/22735
# Part of this test needed to be hacked due to the treatment
# of Inf in sparse matrix algebra
# https://github.com/JuliaLang/julia/issues/22921
# The issue will be resolved in
# https://github.com/JuliaLang/julia/issues/22733
@testset "optimizing sparse $elty covariance" for elty in (Float64, Complex{Float64})
    n = 10
    p = 5
    np2 = div(n*p, 2)
    nzvals, x_sparse = guardseed(1) do
        if elty <: Real
            nzvals = randn(np2)
        else
            nzvals = complex.(randn(np2), randn(np2))
        end
        nzvals, sparse(rand(1:n, np2), rand(1:p, np2), nzvals, n, p)
    end
    x_dense  = convert(Matrix{elty}, x_sparse)
    @testset "Test with no Infs and NaNs, vardim=$vardim, corrected=$corrected" for vardim in (1, 2),
                                                                                 corrected in (true, false)
        @test cov(x_sparse, dims=vardim, corrected=corrected) ≈
              cov(x_dense , dims=vardim, corrected=corrected)
    end

    @testset "Test with $x11, vardim=$vardim, corrected=$corrected" for x11 in (NaN, Inf),
                                                                     vardim in (1, 2),
                                                                  corrected in (true, false)
        x_sparse[1,1] = x11
        x_dense[1 ,1] = x11

        cov_sparse = cov(x_sparse, dims=vardim, corrected=corrected)
        cov_dense  = cov(x_dense , dims=vardim, corrected=corrected)
        @test cov_sparse[2:end, 2:end] ≈ cov_dense[2:end, 2:end]
        @test isfinite.(cov_sparse) == isfinite.(cov_dense)
        @test isfinite.(cov_sparse) == isfinite.(cov_dense)
    end

    @testset "Test with NaN and Inf, vardim=$vardim, corrected=$corrected" for vardim in (1, 2),
                                                                            corrected in (true, false)
        x_sparse[1,1] = Inf
        x_dense[1 ,1] = Inf
        x_sparse[2,1] = NaN
        x_dense[2 ,1] = NaN

        cov_sparse = cov(x_sparse, dims=vardim, corrected=corrected)
        cov_dense  = cov(x_dense , dims=vardim, corrected=corrected)
        @test cov_sparse[(1 + vardim):end, (1 + vardim):end] ≈
              cov_dense[ (1 + vardim):end, (1 + vardim):end]
        @test isfinite.(cov_sparse) == isfinite.(cov_dense)
        @test isfinite.(cov_sparse) == isfinite.(cov_dense)
    end
end
