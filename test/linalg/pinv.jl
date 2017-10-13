# This file is a part of Julia. License is MIT: https://julialang.org/license

#
#  Test the pseudo-inverse
#

using Test

srand(12345)

function hilb(T::Type, n::Integer)
    a=Array{T}(n,n)
    for i=1:n
        for j=1:n
            a[j,i]=one(T)/(i+j-one(T))
        end
    end
    return a
end
hilb(n::Integer) = hilb(Float64,n)

function hilb(T::Type, m::Integer, n::Integer)
    a=Array{T}(m,n)
    for i=1:n
        for j=1:m
            a[j,i]=one(T)/(i+j-one(T))
        end
    end
    return a
end
hilb(m::Integer, n::Integer) = hilb(Float64,m,n)

function onediag(T::Type, m::Integer, n::Integer)
    a=zeros(T,m,n)
    for i=1:min(n,m)
        a[i,i]=one(T)/(float(i)^5)
    end
    a[1,1] = 0
    a[min(m,n),min(m,n)] = 0
    return a
end
onediag(m::Integer, n::Integer) = onediag(Float64, m::Integer, n::Integer)

function onediag_sparse(T::Type, n::Integer)
    a=zeros(T,n)
    for i=1:n
        a[i]=one(T)/(float(i)^5)
    end
    a[1] = 0
    a[n] = 0
    return Diagonal(a)
end
onediag_sparse(n::Integer) = onediag_sparse(Float64, n::Integer)

function tridiag(T::Type, m::Integer, n::Integer)
    a=zeros(T,m,n)
    for i=1:min(n,m)
        a[i,i]=one(T)/(float(i)^5)
    end
    for i=1:min(n,m)-1
        a[i+1,i]=2*one(T)/(float(i)^5)
        a[1,i+1]=2*one(T)/(float(i)^5)
    end
    return a
end
tridiag(m::Integer, n::Integer) = tridiag(Float64, m::Integer, n::Integer)

function randn_float64(m::Integer, n::Integer)
    a=randn(m,n)
    b=Array{Float64}(m,n)
    for i=1:n
        for j=1:m
            b[j,i]=convert(Float64,a[j,i])
        end
    end
    return b
end

function randn_float32(m::Integer, n::Integer)
    a=randn(m,n)
    b=Array{Float32}(m,n)
    for i=1:n
        for j=1:m
            b[j,i]=convert(Float32,a[j,i])
        end
    end
    return b
end


function test_pinv(a,m,n,tol1,tol2,tol3)
    apinv = @inferred pinv(a)

    @test vecnorm(a*apinv*a-a)/vecnorm(a) ≈ 0 atol=tol1
    x0 = randn(n); b = a*x0; x = apinv*b
    @test vecnorm(a*x-b)/vecnorm(b) ≈ 0 atol=tol1
    apinv = pinv(a,sqrt(eps(real(one(eltype(a))))))

    @test vecnorm(a*apinv*a-a)/vecnorm(a) ≈ 0 atol=tol2
    x0 = randn(n); b = a*x0; x = apinv*b
    @test vecnorm(a*x-b)/vecnorm(b) ≈ 0 atol=tol2
end

@testset for eltya in (Float32, Float64, Complex64, Complex128)
    @testset for (m, n) in [(1000, 100), (100, 100), (100, 1000)]
        default_tol = (real(one(eltya))) * max(m,n) * 10
        tol1 = 1e-2
        tol2 = 1e-5
        tol3 = 1e-5
        if real(eltya) == Float32
            tol1 = 1e0
            tol2 = 1e-2
            tol3 = 1e-2
        end
        @testset "dense/ill-conditioned matrix" begin
        ###    a = randn_float64(m,n) * hilb(eltya,n)
            a = hilb(eltya, m, n)
            test_pinv(a, m, n, tol1, tol2, tol3)
        end
        @testset "dense/diagonal matrix" begin
            a = onediag(eltya, m, n)
            test_pinv(a, m, n, default_tol, default_tol, default_tol)
        end
        @testset "dense/tri-diagonal matrix" begin
            a = tridiag(eltya, m, n)
            test_pinv(a, m, n, default_tol, tol2, default_tol)
        end
        @testset "Diagonal matrix" begin
            a = onediag_sparse(eltya, m)
            test_pinv(a, m, m, default_tol, default_tol, default_tol)
        end
        @testset "Vector" begin
            a = rand(eltya, m)
            apinv = @inferred pinv(a)
            @test pinv(hcat(a)) ≈ apinv
            @test apinv isa RowVector{eltya}
        end
        @testset "RowVector" begin
            a = rand(eltya, m)'
            apinv = @inferred pinv(a)
            @test pinv(vcat(a)) ≈ apinv
            @test apinv isa Vector{eltya}
        end
    end

    @testset "zero valued numbers/vectors/matrices" begin
        a = pinv(zero(eltya))
        @test a ≈ 0.0

        a = pinv([zero(eltya); zero(eltya)])
        @test a[1] ≈ 0.0
        @test a[2] ≈ 0.0

        a = pinv([zero(eltya); zero(eltya)]')
        @test a[1] ≈ 0.0
        @test a[2] ≈ 0.0

        a = pinv(Diagonal([zero(eltya); zero(eltya)]))
        @test a.diag[1] ≈ 0.0
        @test a.diag[2] ≈ 0.0
    end

    if eltya <: Base.LinAlg.BlasReal
        @testset "sub-normal numbers/vectors/matrices" begin
            a = pinv(realmin(eltya)/100)
            @test a ≈ 0.0
            # Complex subnormal
            a = pinv(realmin(eltya)/100*(1+1im))
            @test a ≈ 0.0

            a = pinv([realmin(eltya); realmin(eltya)]/100)
            @test a[1] ≈ 0.0
            @test a[2] ≈ 0.0
            # Complex subnormal
            a = pinv([realmin(eltya); realmin(eltya)]/100*(1+1im))
            @test a[1] ≈ 0.0
            @test a[2] ≈ 0.0
            a = pinv(Diagonal([realmin(eltya); realmin(eltya)]/100))
            @test a.diag[1] ≈ 0.0
            @test a.diag[2] ≈ 0.0
            # Complex subnormal
            a = pinv(Diagonal([realmin(eltya); realmin(eltya)]/100*(1+1im)))
            @test a.diag[1] ≈ 0.0
            @test a.diag[2] ≈ 0.0
        end
    end
end
