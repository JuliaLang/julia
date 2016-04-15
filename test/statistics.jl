# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

# middle

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


# median
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

@test all(median([2 3 1 -1; 7 4 5 -4], 2) .== [1.5, 4.5])
@test all(median([2 3 1 -1; 7 4 5 -4], 1) .== [4.5 3.5 3.0 -2.5])

@test_throws ArgumentError median([])
@test isnan(median([NaN]))
@test isnan(median([0.0,NaN]))
@test isnan(median([NaN,0.0]))
@test isequal(median([NaN 0.0; 1.2 4.5], 2), reshape([NaN; 2.85], 2, 1))

@test median!([1 2 3 4]) == 2.5
@test median!([1 2; 3 4]) == 2.5

# mean
@test_throws ArgumentError mean(())
@test mean((1,2,3)) === 2.
@test mean([0]) === 0.
@test mean([1.]) === 1.
@test mean([1.,3]) == 2.
@test mean([1,2,3]) == 2.
@test mean([0 1 2; 4 5 6], 1) == [2.  3.  4.]
@test mean([1 2 3; 4 5 6], 1) == [2.5 3.5 4.5]

@test isnan(mean([NaN]))
@test isnan(mean([0.0,NaN]))
@test isnan(mean([NaN,0.0]))

@test isnan(mean([0.,Inf,-Inf]))
@test isnan(mean([1.,-1.,Inf,-Inf]))
@test isnan(mean([-Inf,Inf]))
@test isequal(mean([NaN 0.0; 1.2 4.5], 2), reshape([NaN; 2.85], 2, 1))

# test var & std

# edge case: empty vector
# iterable; this has to throw for type stability
@test_throws ArgumentError var(())
@test_throws ArgumentError var((); corrected=false)
@test_throws ArgumentError var((); mean=2)
@test_throws ArgumentError var((); mean=2, corrected=false)
# reduction
@test isnan(var(Int[]))
@test isnan(var(Int[]; corrected=false))
@test isnan(var(Int[]; mean=2))
@test isnan(var(Int[]; mean=2, corrected=false))
# reduction across dimensions
@test_approx_eq var(Int[], 1) [NaN]
@test_approx_eq var(Int[], 1; corrected=false) [NaN]
@test_approx_eq var(Int[], 1; mean=[2]) [NaN]
@test_approx_eq var(Int[], 1; mean=[2], corrected=false) [NaN]

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
@test_approx_eq @inferred(var([1], 1)) [NaN]
@test_approx_eq var([1], 1; corrected=false) [0.0]
@test_approx_eq var([1], 1; mean=[2]) [Inf]
@test_approx_eq var([1], 1; mean=[2], corrected=false) [1.0]

@test var(1:8) == 6.
@test varm(1:8,1) == varm(collect(1:8),1)
@test isnan(varm(1:1,1))
@test isnan(var(1:1))
@test isnan(var(1:-1))

@test_approx_eq varm([1,2,3], 2) 1.
@test_approx_eq var([1,2,3]) 1.
@test_approx_eq var([1,2,3]; corrected=false) 2.0/3
@test_approx_eq var([1,2,3]; mean=0) 7.
@test_approx_eq var([1,2,3]; mean=0, corrected=false) 14.0/3

@test_approx_eq varm((1,2,3), 2) 1.
@test_approx_eq var((1,2,3)) 1.
@test_approx_eq var((1,2,3); corrected=false) 2.0/3
@test_approx_eq var((1,2,3); mean=0) 7.
@test_approx_eq var((1,2,3); mean=0, corrected=false) 14.0/3
@test_throws ArgumentError var((1,2,3); mean=())

@test_approx_eq var([1 2 3 4 5; 6 7 8 9 10], 2) [2.5 2.5]'
@test_approx_eq var([1 2 3 4 5; 6 7 8 9 10], 2; corrected=false) [2.0 2.0]'

@test_approx_eq stdm([1,2,3], 2) 1.
@test_approx_eq std([1,2,3]) 1.
@test_approx_eq std([1,2,3]; corrected=false) sqrt(2.0/3)
@test_approx_eq std([1,2,3]; mean=0) sqrt(7.0)
@test_approx_eq std([1,2,3]; mean=0, corrected=false) sqrt(14.0/3)

@test_approx_eq stdm((1,2,3), 2) 1.
@test_approx_eq std((1,2,3)) 1.
@test_approx_eq std((1,2,3); corrected=false) sqrt(2.0/3)
@test_approx_eq std((1,2,3); mean=0) sqrt(7.0)
@test_approx_eq std((1,2,3); mean=0, corrected=false) sqrt(14.0/3)

@test_approx_eq std([1 2 3 4 5; 6 7 8 9 10], 2) sqrt([2.5 2.5]')
@test_approx_eq std([1 2 3 4 5; 6 7 8 9 10], 2; corrected=false) sqrt([2.0 2.0]')

A = Complex128[exp(i*im) for i in 1:10^4]
@test_approx_eq varm(A,0.) sum(map(abs2,A))/(length(A)-1)
@test_approx_eq varm(A,mean(A)) var(A)

# test covariance

function safe_cov(x, y, zm::Bool, cr::Bool)
    n = length(x)
    if !zm
        x = x .- mean(x)
        y = y .- mean(y)
    end
    dot(vec(x), vec(y)) / (n - Int(cr))
end

X = [1. 2. 3. 4. 5.; 5. 4. 6. 2. 1.]'
Y = [6. 1. 5. 3. 2.; 2. 7. 8. 4. 3.]'

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

    c = zm ? Base.covm(x1, 0, cr) :
             cov(x1, cr)
    @test isa(c, Float64)
    @test_approx_eq c Cxx[1,1]
    @inferred cov(x1, cr)

    @test cov(X) == Base.covm(X, mean(X, 1))
    C = zm ? Base.covm(X, 0, vd, cr) :
             cov(X, vd, cr)
    @test size(C) == (k, k)
    @test_approx_eq C Cxx
    @inferred cov(X, vd, cr)

    @test cov(x1, y1) == Base.covm(x1, mean(x1), y1, mean(y1))
    c = zm ? Base.covm(x1, 0, y1, 0, cr) :
             cov(x1, y1, cr)
    @test isa(c, Float64)
    @test_approx_eq c Cxy[1,1]
    @inferred cov(x1, y1, cr)

    if vd == 1
        @test cov(x1, Y) == Base.covm(x1, mean(x1), Y, mean(Y, 1))
    end
    C = zm ? Base.covm(x1, 0, Y, 0, vd, cr) :
             cov(x1, Y, vd, cr)
    @test size(C) == (1, k)
    @test_approx_eq C Cxy[1,:]
    @inferred cov(x1, Y, vd, cr)

    if vd == 1
        @test cov(X, y1) == Base.covm(X, mean(X, 1), y1, mean(y1))
    end
    C = zm ? Base.covm(X, 0, y1, 0, vd, cr) :
             cov(X, y1, vd, cr)
    @test size(C) == (k, 1)
    @test_approx_eq C Cxy[:,1]
    @inferred cov(X, y1, vd, cr)

    @test cov(X, Y) == Base.covm(X, mean(X, 1), Y, mean(Y, 1))
    C = zm ? Base.covm(X, 0, Y, 0, vd, cr) :
             cov(X, Y, vd, cr)
    @test size(C) == (k, k)
    @test_approx_eq C Cxy
    @inferred cov(X, Y, vd, cr)
end

# test correlation

function safe_cor(x, y, zm::Bool)
    if !zm
        x = x .- mean(x)
        y = y .- mean(y)
    end
    x = vec(x)
    y = vec(y)
    dot(x, y) / (sqrt(dot(x, x)) * sqrt(dot(y, y)))
end

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

    c = zm ? Base.corm(x1, 0) : cor(x1)
    @test isa(c, Float64)
    @test_approx_eq c Cxx[1,1]
    @inferred cor(x1)

    @test cor(X) == Base.corm(X, mean(X, 1))
    C = zm ? Base.corm(X, 0, vd) : cor(X, vd)
    @test size(C) == (k, k)
    @test_approx_eq C Cxx
    @inferred cor(X, vd)

    @test cor(x1, y1) == Base.corm(x1, mean(x1), y1, mean(y1))
    c = zm ? Base.corm(x1, 0, y1, 0) : cor(x1, y1)
    @test isa(c, Float64)
    @test_approx_eq c Cxy[1,1]
    @inferred cor(x1, y1)

    if vd == 1
        @test cor(x1, Y) == Base.corm(x1, mean(x1), Y, mean(Y, 1))
    end
    C = zm ? Base.corm(x1, 0, Y, 0, vd) : cor(x1, Y, vd)
    @test size(C) == (1, k)
    @test_approx_eq C Cxy[1,:]
    @inferred cor(x1, Y, vd)

    if vd == 1
        @test cor(X, y1) == Base.corm(X, mean(X, 1), y1, mean(y1))
    end
    C = zm ? Base.corm(X, 0, y1, 0, vd) : cor(X, y1, vd)
    @test size(C) == (k, 1)
    @test_approx_eq C Cxy[:,1]
    @inferred cor(X, y1, vd)

    @test cor(X, Y) == Base.corm(X, mean(X, 1), Y, mean(Y, 1))
    C = zm ? Base.corm(X, 0, Y, 0, vd) : cor(X, Y, vd)
    @test size(C) == (k, k)
    @test_approx_eq C Cxy
    @inferred cor(X, Y, vd)
end


# test hist

@test sum(hist([1,2,3])[2]) == 3
@test hist(Union{}[])[2] == []
@test hist([1])[2] == [1]
@test hist([1,2,3],[0,2,4]) == ([0,2,4],[2,1])
@test hist([1,2,3],0:2:4) == (0:2:4,[2,1])
@test all(hist([1:100;]/100,0.0:0.01:1.0)[2] .==1)
@test hist([1,1,1,1,1])[2][1] == 5
@test sum(hist2d(rand(100, 2))[3]) == 100
@test hist([1 2 3 4;1 2 3 4]) == (0.0:2.0:4.0, [2 2 0 0; 0 0 2 2])

@test midpoints(1.0:1.0:10.0) == 1.5:1.0:9.5
@test midpoints(1:10) == 1.5:9.5
@test midpoints(Float64[1.0:1.0:10.0;]) == Float64[1.5:1.0:9.5;]

@test quantile([1,2,3,4],0.5) == 2.5
@test quantile([1,2,3,4],[0.5]) == [2.5]
@test quantile([1., 3],[.25,.5,.75])[2] == median([1., 3])
@test quantile(100.0:-1.0:0.0, 0.0:0.1:1.0) == collect(0.0:10.0:100.0)
@test quantile(0.0:100.0, 0.0:0.1:1.0, sorted=true) == collect(0.0:10.0:100.0)
@test quantile(100f0:-1f0:0.0, 0.0:0.1:1.0) == collect(0f0:10f0:100f0)


# test invalid hist nbins argument (#9999)
@test_throws ArgumentError hist(Int[], -1)
@test hist(Int[], 0)[2] == Int[]
@test_throws ArgumentError hist([1,2,3], -1)
@test_throws ArgumentError hist([1,2,3], 0)
@test_throws ArgumentError hist([1.0,2.0,3.0], -1)
@test_throws ArgumentError hist([1.0,2.0,3.0], 0)

@test histrange([1, 2, 3, 4], 4) == 0.0:1.0:4.0
@test histrange([1, 2, 2, 4], 4) == 0.0:1.0:4.0
@test histrange([1, 10], 4) == 0.0:5.0:10.0
@test histrange([1, 20], 4) == 0.0:5.0:20.0
@test histrange([1, 600], 4) == 0.0:200.0:600.0
@test histrange([1, -1000], 4) == -1500.0:500.0:500.0

# issue #13326
l,h = extrema(histrange([typemin(Int),typemax(Int)], 10))
@test l <= typemin(Int)
@test h >= typemax(Int)

@test_throws ArgumentError histrange([1, 10], 0)
@test_throws ArgumentError histrange([1, 10], -1)
@test_throws ArgumentError histrange(Float64[],-1)

# variance of complex arrays (#13309)
let z = rand(Complex128, 10)
    @test var(z) ≈ invoke(var, (Any,), z) ≈ cov(z) ≈ var(z,1)[1] ≈ sumabs2(z - mean(z))/9
    @test isa(var(z), Float64)
    @test isa(invoke(var, (Any,), z), Float64)
    @test isa(cov(z), Float64)
    @test isa(var(z,1), Vector{Float64})
    @test varm(z, 0.0) ≈ invoke(varm, (Any,Float64), z, 0.0) ≈ sumabs2(z)/9
    @test isa(varm(z, 0.0), Float64)
    @test isa(invoke(varm, (Any,Float64), z, 0.0), Float64)
    @test cor(z) === 1.0
end
let v = varm([1.0+2.0im], 0; corrected = false)
    @test v ≈ 5
    @test isa(v, Float64)
end
