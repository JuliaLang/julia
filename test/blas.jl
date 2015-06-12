# This file is a part of Julia. License is MIT: http://julialang.org/license

import Base.LinAlg
## BLAS tests - testing the interface code to BLAS routines
for elty in [Float32, Float64, Complex64, Complex128]

    o4 = ones(elty, 4)
    z4 = zeros(elty, 4)

    I4 = eye(elty, 4)
    I43 = eye(elty, 4, 3)
    L4 = tril(ones(elty, (4,4)))
    U4 = triu(ones(elty, (4,4)))
    Z4 = zeros(elty, (4,4))

    elm1 = convert(elty, -1)
    el2 = convert(elty, 2)
    v14 = convert(Vector{elty}, [1:4;])
    v41 = convert(Vector{elty}, [4:-1:1;])

    let n = 10
        # dot
        if elty <: Real
            x1 = convert(Vector{elty}, randn(n))
            x2 = convert(Vector{elty}, randn(n))
            @test_approx_eq BLAS.dot(x1,x2) sum(x1.*x2)
            @test_throws DimensionMismatch BLAS.dot(x1,rand(elty, n + 1))
        else
            z1 = convert(Vector{elty}, complex(randn(n),randn(n)))
            z2 = convert(Vector{elty}, complex(randn(n),randn(n)))
            @test_approx_eq BLAS.dotc(z1,z2) sum(conj(z1).*z2)
            @test_approx_eq BLAS.dotu(z1,z2) sum(z1.*z2)
            @test_throws DimensionMismatch BLAS.dotc(z1,rand(elty, n + 1))
            @test_throws DimensionMismatch BLAS.dotu(z1,rand(elty, n + 1))
        end

        #iamax
        if elty <: Real
            x = convert(Vector{elty}, randn(n))
            @test BLAS.iamax(x) == indmax(abs(x))
        else
            z = convert(Vector{elty}, complex(randn(n),randn(n)))
            @test BLAS.iamax(z) == indmax(map(x -> abs(real(x)) + abs(imag(x)), z))
        end

        # axpy
        if elty <: Real
            x1 = convert(Vector{elty}, randn(n))
            x2 = convert(Vector{elty}, randn(n))
                α  = rand(elty)
            @test_approx_eq BLAS.axpy!(α,copy(x1),copy(x2)) x2 + α*x1
            @test_throws DimensionMismatch BLAS.axpy!(α, copy(x1), rand(elty, n + 1))
            @test_throws DimensionMismatch BLAS.axpy!(α, copy(x1), 1:div(n,2), copy(x2), 1:n)
            @test_throws BoundsError BLAS.axpy!(α, copy(x1), 0:div(n,2), copy(x2), 1:(div(n, 2) + 1))
            @test_throws BoundsError BLAS.axpy!(α, copy(x1), 1:div(n,2), copy(x2), 0:(div(n, 2) - 1))
        else
            z1 = convert(Vector{elty}, complex(randn(n), randn(n)))
            z2 = convert(Vector{elty}, complex(randn(n), randn(n)))
            α  = rand(elty)
            @test_approx_eq BLAS.axpy!(α, copy(z1), copy(z2)) z2 + α * z1
            @test_throws DimensionMismatch BLAS.axpy!(α, copy(z1), rand(elty, n + 1))
            @test_throws DimensionMismatch BLAS.axpy!(α, copy(z1), 1:div(n, 2), copy(z2), 1:(div(n, 2) + 1))
            @test_throws BoundsError BLAS.axpy!(α, copy(z1), 0:div(n,2), copy(z2), 1:(div(n, 2) + 1))
            @test_throws BoundsError BLAS.axpy!(α, copy(z1), 1:div(n,2), copy(z2), 0:(div(n, 2) - 1))
        end

        # trsv
        A = triu(rand(elty,n,n))
        x = rand(elty,n)
        @test_approx_eq A\x BLAS.trsv('U','N','N',A,x)
        @test_throws DimensionMismatch BLAS.trsv('U','N','N',A,ones(elty,n+1))

        # copy
        x1 = convert(Vector{elty}, randn(n))
        x2 = convert(Vector{elty}, randn(n))
        BLAS.copy!(x2, 1:n, x1, 1:n)
        @test x2 == x1
        @test_throws DimensionMismatch BLAS.copy!(x2, 1:n, x1, 1:(n - 1))
        @test_throws BoundsError BLAS.copy!(x1, 0:div(n, 2), x2, 1:(div(n, 2) + 1))
        @test_throws BoundsError BLAS.copy!(x1, 1:(div(n, 2) + 1), x2, 0:div(n, 2))
    end

    # gemv
    @test all(BLAS.gemv('N', I4, o4) .== o4)
    @test all(BLAS.gemv('T', I4, o4) .== o4)
    @test all(BLAS.gemv('N', el2, I4, o4) .== el2 * o4)
    @test all(BLAS.gemv('T', el2, I4, o4) .== el2 * o4)
    @test_throws DimensionMismatch BLAS.gemv('N',I43,o4)
    o4cp = copy(o4)
    @test_throws DimensionMismatch BLAS.gemv!('T',one(elty),I43,o4,one(elty),o4cp)
    @test_throws DimensionMismatch BLAS.gemv!('C',one(elty),I43,o4,one(elty),o4cp)
    @test all(BLAS.gemv!('N', one(elty), I4, o4, elm1, o4cp) .== z4)
    @test all(o4cp .== z4)
    o4cp[:] = o4
    @test all(BLAS.gemv!('T', one(elty), I4, o4, elm1, o4cp) .== z4)
    @test all(o4cp .== z4)
    @test all(BLAS.gemv('N', U4, o4) .== v41)
    @test all(BLAS.gemv('N', U4, o4) .== v41)

    # gemm
    @test all(BLAS.gemm('N', 'N', I4, I4) .== I4)
    @test all(BLAS.gemm('N', 'T', I4, I4) .== I4)
    @test all(BLAS.gemm('T', 'N', I4, I4) .== I4)
    @test all(BLAS.gemm('T', 'T', I4, I4) .== I4)
    @test all(BLAS.gemm('N', 'N', el2, I4, I4) .== el2 * I4)
    @test all(BLAS.gemm('N', 'T', el2, I4, I4) .== el2 * I4)
    @test all(BLAS.gemm('T', 'N', el2, I4, I4) .== el2 * I4)
    @test all(LinAlg.BLAS.gemm('T', 'T', el2, I4, I4) .== el2 * I4)
    I4cp = copy(I4)
    @test all(BLAS.gemm!('N', 'N', one(elty), I4, I4, elm1, I4cp) .== Z4)
    @test all(I4cp .== Z4)
    I4cp[:] = I4
    @test all(BLAS.gemm!('N', 'T', one(elty), I4, I4, elm1, I4cp) .== Z4)
    @test all(I4cp .== Z4)
    I4cp[:] = I4
    @test all(BLAS.gemm!('T', 'N', one(elty), I4, I4, elm1, I4cp) .== Z4)
    @test all(I4cp .== Z4)
    I4cp[:] = I4
    @test all(BLAS.gemm!('T', 'T', one(elty), I4, I4, elm1, I4cp) .== Z4)
    @test all(I4cp .== Z4)
    @test all(BLAS.gemm('N', 'N', I4, U4) .== U4)
    @test all(BLAS.gemm('N', 'T', I4, U4) .== L4)
    @test_throws DimensionMismatch BLAS.gemm!('N','N', one(elty), I4, I4, elm1, eye(elty,5))

    # gemm compared to (sy)(he)rk
    if iseltype(elm1,Complex)
        @test all(triu(BLAS.herk('U', 'N', U4)) .== triu(BLAS.gemm('N', 'T', U4, U4)))
        @test all(tril(BLAS.herk('L', 'N', U4)) .== tril(BLAS.gemm('N', 'T', U4, U4)))
        @test all(triu(BLAS.herk('U', 'N', L4)) .== triu(BLAS.gemm('N', 'T', L4, L4)))
        @test all(tril(BLAS.herk('L', 'N', L4)) .== tril(BLAS.gemm('N', 'T', L4, L4)))
        @test all(triu(BLAS.herk('U', 'C', U4)) .== triu(BLAS.gemm('T', 'N', U4, U4)))
        @test all(tril(BLAS.herk('L', 'C', U4)) .== tril(BLAS.gemm('T', 'N', U4, U4)))
        @test all(triu(BLAS.herk('U', 'C', L4)) .== triu(BLAS.gemm('T', 'N', L4, L4)))
        @test all(tril(BLAS.herk('L', 'C', L4)) .== tril(BLAS.gemm('T', 'N', L4, L4)))
        ans = similar(L4)
        @test all(tril(BLAS.herk('L','C', L4)) .== tril(BLAS.herk!('L', 'C', real(one(elty)), L4, real(zero(elty)), ans)))
        @test all(Base.LinAlg.copytri!(ans, 'L') .== LinAlg.BLAS.gemm('T', 'N', L4, L4))
        @test_throws DimensionMismatch BLAS.herk!('L','N',real(one(elty)),eye(elty,5),real(one(elty)),eye(elty,6))
    else
        @test all(triu(BLAS.syrk('U', 'N', U4)) .== triu(BLAS.gemm('N', 'T', U4, U4)))
        @test all(tril(BLAS.syrk('L', 'N', U4)) .== tril(BLAS.gemm('N', 'T', U4, U4)))
        @test all(triu(BLAS.syrk('U', 'N', L4)) .== triu(BLAS.gemm('N', 'T', L4, L4)))
        @test all(tril(BLAS.syrk('L', 'N', L4)) .== tril(BLAS.gemm('N', 'T', L4, L4)))
        @test all(triu(BLAS.syrk('U', 'T', U4)) .== triu(BLAS.gemm('T', 'N', U4, U4)))
        @test all(tril(BLAS.syrk('L', 'T', U4)) .== tril(BLAS.gemm('T', 'N', U4, U4)))
        @test all(triu(BLAS.syrk('U', 'T', L4)) .== triu(BLAS.gemm('T', 'N', L4, L4)))
        @test all(tril(BLAS.syrk('L', 'T', L4)) .== tril(BLAS.gemm('T', 'N', L4, L4)))
        ans = similar(L4)
        @test all(tril(BLAS.syrk('L','T', L4)) .== tril(BLAS.syrk!('L', 'T', one(elty), L4, zero(elty), ans)))
        @test all(Base.LinAlg.copytri!(ans, 'L') .== BLAS.gemm('T', 'N', L4, L4))
        @test_throws DimensionMismatch BLAS.syrk!('L','N',one(elty),eye(elty,5),one(elty),eye(elty,6))
    end

end
