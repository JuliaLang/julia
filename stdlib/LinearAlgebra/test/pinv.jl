# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestPinv

using Test, LinearAlgebra, Random

Random.seed!(12345)

function hilb(T::Type, n::Integer)
    a = Matrix{T}(undef, n, n)
    for i=1:n
        for j=1:n
            a[j,i]=one(T)/(i+j-one(T))
        end
    end
    return a
end
hilb(n::Integer) = hilb(Float64,n)

function hilb(T::Type, m::Integer, n::Integer)
    a = Matrix{T}(undef, m, n)
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

function test_pinv(a,tol1,tol2)
    m,n = size(a)

    apinv = @inferred pinv(a)
    @test size(apinv) == (n,m)
    @test norm(a*apinv*a-a)/norm(a) ≈ 0 atol=tol1
    @test norm(apinv*a*apinv-apinv)/norm(apinv) ≈ 0 atol=tol1
    b = a*randn(n)
    x = apinv*b
    @test norm(a*x-b)/norm(b) ≈ 0 atol=tol1

    apinv = @inferred pinv(a,sqrt(eps(real(one(eltype(a))))))
    @test size(apinv) == (n,m)
    @test norm(a*apinv*a-a)/norm(a) ≈ 0 atol=tol2
    @test norm(apinv*a*apinv-apinv)/norm(apinv) ≈ 0 atol=tol2
    b = a*randn(n)
    x = apinv*b
    @test norm(a*x-b)/norm(b) ≈ 0 atol=tol2
end

@testset for eltya in (Float32, Float64, ComplexF32, ComplexF64)
    @testset for (m, n) in [(1000, 100), (100, 100), (100, 1000)]
        default_tol = (real(one(eltya))) * max(m,n) * 10
        tol1 = 1e-2
        tol2 = 1e-5
        if real(eltya) == Float32
            tol1 = 1e0
            tol2 = 1e-2
        end
        @testset "dense/ill-conditioned matrix" begin
            a = hilb(eltya, m, n)
            test_pinv(a, tol1, tol2)
        end
        @testset "dense/diagonal matrix" begin
            a = onediag(eltya, m, n)
            test_pinv(a, default_tol, default_tol)
        end
        @testset "dense/tri-diagonal matrix" begin
            a = tridiag(eltya, m, n)
            test_pinv(a, default_tol, tol2)
        end
        @testset "Diagonal matrix" begin
            a = onediag_sparse(eltya, m)
            test_pinv(a, default_tol, default_tol)
        end
        @testset "Vector" begin
            a = rand(eltya, m)
            apinv = @inferred pinv(a)
            @test pinv(hcat(a)) ≈ apinv
            @test isa(apinv, eltya <: Complex ? Adjoint{eltya} : Transpose{eltya})
        end
        @testset "Adjoint/Transpose vector" begin
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

    @testset "hermitian matrices" begin
        Q = ones(2,2)
        C = pinv(Hermitian(Q))/0.25
        @test C ≈ ones(2,2)
    end

    @testset "non-square diagonal matrices" begin
        A = eltya[1 0 ; 0 1 ; 0 0]
        B = pinv(A)
        @test A*B*A ≈ A
        @test B*A*B ≈ B

        A = eltya[1 0 0 ; 0 1 0]
        B = pinv(A)
        @test A*B*A ≈ A
        @test B*A*B ≈ B
    end

    if eltya <: LinearAlgebra.BlasReal
        @testset "sub-normal numbers/vectors/matrices" begin
            a = pinv(floatmin(eltya)/100)
            @test a ≈ 0.0
            # Complex subnormal
            a = pinv(floatmin(eltya)/100*(1+1im))
            @test a ≈ 0.0

            a = pinv([floatmin(eltya); floatmin(eltya)]/100)
            @test a[1] ≈ 0.0
            @test a[2] ≈ 0.0
            # Complex subnormal
            a = pinv([floatmin(eltya); floatmin(eltya)]/100*(1+1im))
            @test a[1] ≈ 0.0
            @test a[2] ≈ 0.0
            a = pinv(Diagonal([floatmin(eltya); floatmin(eltya)]/100))
            @test a.diag[1] ≈ 0.0
            @test a.diag[2] ≈ 0.0
            # Complex subnormal
            a = pinv(Diagonal([floatmin(eltya); floatmin(eltya)]/100*(1+1im)))
            @test a.diag[1] ≈ 0.0
            @test a.diag[2] ≈ 0.0
        end
    end
end

end # module TestPinv
