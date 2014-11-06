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
    v14 = convert(Vector{elty}, [1:4])
    v41 = convert(Vector{elty}, [4:-1:1])

    # dot
    if elty <: Real
        x1 = convert(Vector{elty}, randn(10))
        x2 = convert(Vector{elty}, randn(10))
        @test_approx_eq BLAS.dot(x1,x2) sum(x1.*x2)
    else
        z1 = convert(Vector{elty}, complex(randn(10),randn(10)))
        z2 = convert(Vector{elty}, complex(randn(10),randn(10)))
        @test_approx_eq BLAS.dotc(z1,z2) sum(conj(z1).*z2)
        @test_approx_eq BLAS.dotu(z1,z2) sum(z1.*z2)
    end

    # gemv
    @test all(BLAS.gemv('N', I4, o4) .== o4)
    @test all(BLAS.gemv('T', I4, o4) .== o4)
    @test all(BLAS.gemv('N', el2, I4, o4) .== el2 * o4)
    @test all(BLAS.gemv('T', el2, I4, o4) .== el2 * o4)
    @test_throws DimensionMismatch BLAS.gemv('N',I43,o4)
    o4cp = copy(o4)
    @test_throws DimensionMismatch BLAS.gemv!('T',one(elty),I43,o4,one(elty),o4cp)
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
        @test all(tril(BLAS.herk('L','C', L4)) .== tril(BLAS.herk!('L', 'C', one(elty), L4, zero(elty), ans)))
        @test all(Base.LinAlg.copytri!(ans, 'L') .== LinAlg.BLAS.gemm('T', 'N', L4, L4))
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
    end
end
