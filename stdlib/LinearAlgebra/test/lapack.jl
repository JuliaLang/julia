# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestLAPACK

using Test, LinearAlgebra, Random
using LinearAlgebra: BlasInt

@test_throws ArgumentError LinearAlgebra.LAPACK.chkuplo('Z')
@test_throws ArgumentError LinearAlgebra.LAPACK.chkside('Z')
@test_throws ArgumentError LinearAlgebra.LAPACK.chkdiag('Z')
@test_throws ArgumentError LinearAlgebra.LAPACK.chktrans('Z')
@test_throws ArgumentError LinearAlgebra.LAPACK.chkvalidparam(1, "job", 2, (0,1))

@testset "syevr" begin
    Random.seed!(123)
    Ainit = randn(5,5)
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        if elty == ComplexF32 || elty == ComplexF64
            A = complex.(Ainit, Ainit)
        else
            A = Ainit
        end
        A = convert(Array{elty, 2}, A)
        Asym = A'A
        vals, Z = LAPACK.syevr!('V', copy(Asym))
        @test Z*(Diagonal(vals)*Z') ≈ Asym
        @test all(vals .> 0.0)
        @test LAPACK.syevr!('N', 'V', 'U', copy(Asym), 0.0, 1.0, 4, 5, -1.0)[1] ≈ vals[vals .< 1.0]
        @test LAPACK.syevr!('N', 'I', 'U', copy(Asym), 0.0, 1.0, 4, 5, -1.0)[1] ≈ vals[4:5]
        @test vals ≈ LAPACK.syev!('N', 'U', copy(Asym))
        @test vals ≈ LAPACK.syevd!('N', 'U', copy(Asym))
        vals_test, Z_test = LAPACK.syev!('V', 'U', copy(Asym))
        @test vals_test ≈ vals
        @test Z_test*(Diagonal(vals)*Z_test') ≈ Asym
        vals_test, Z_test = LAPACK.syevd!('V', 'U', copy(Asym))
        @test vals_test ≈ vals
        @test Z_test*(Diagonal(vals)*Z_test') ≈ Asym
        @test_throws DimensionMismatch LAPACK.sygvd!(1, 'V', 'U', copy(Asym), zeros(elty, 6, 6))

        @test_throws "jobz must be one of ('N', 'V'), but 'X' was passed" LAPACK.syevr!('X', Asym)
        @test_throws "jobz must be one of ('N', 'V'), but 'X' was passed" LAPACK.syev!('X', 'U', Asym)
        @test_throws "uplo argument must be 'U' (upper) or 'L' (lower), got 'M'" LAPACK.syev!('N', 'M', Asym)
        @test_throws "jobz must be one of ('N', 'V'), but 'X' was passed" LAPACK.syevd!('X', 'U', Asym)
        @test_throws "uplo argument must be 'U' (upper) or 'L' (lower), got 'M'" LAPACK.syevd!('N', 'M', Asym)
    end
end

@testset "gglse" begin
    let
        @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
            A = convert(Array{elty, 2}, [1 1 1 1; 1 3 1 1; 1 -1 3 1; 1 1 1 3; 1 1 1 -1])
            c = convert(Array{elty, 1}, [2, 1, 6, 3, 1])
            B = convert(Array{elty, 2}, [1 1 1 -1; 1 -1 1 1; 1 1 -1 1])
            d = convert(Array{elty, 1}, [1, 3, -1])
            @test LAPACK.gglse!(A, c, B, d)[1] ≈ convert(Array{elty}, [0.5, -0.5, 1.5, 0.5])
        end
    end
end

@testset "gebrd, bdsqr, throw for bdsdc" begin
    let
        n = 10
        @testset for elty in (Float32, Float64)
            d, e = convert(Vector{elty}, randn(n)), convert(Vector{elty}, randn(n - 1))
            U, Vt, C = Matrix{elty}(I, n, n), Matrix{elty}(I, n, n), Matrix{elty}(I, n, n)
            s, _ = LAPACK.bdsqr!('U', copy(d), copy(e), Vt, U, C)
            @test Array(Bidiagonal(d, e, :U)) ≈ U*Diagonal(s)*Vt

            @test_throws ArgumentError LAPACK.bdsqr!('A', d, e, Vt, U, C)
            @test_throws DimensionMismatch LAPACK.bdsqr!('U', d, [e; 1], Vt, U, C)
            @test_throws DimensionMismatch LAPACK.bdsqr!('U', d, e, Vt[1:end - 1, :], U, C)
            @test_throws DimensionMismatch LAPACK.bdsqr!('U', d, e, Vt, U[:,1:end - 1], C)
            @test_throws DimensionMismatch LAPACK.bdsqr!('U', d, e, Vt, U, C[1:end - 1, :])

            @test_throws ArgumentError LAPACK.bdsdc!('U','Z',d,e)

            A = rand(elty,n,n)
            B = copy(A)
            B, d, e, tauq, taup = LAPACK.gebrd!(B)
            U, Vt, C = Matrix{elty}(I, n, n), Matrix{elty}(I, n, n), Matrix{elty}(I, n, n)
            s, _ = LAPACK.bdsqr!('U',d,e[1:n-1],Vt, U, C)
            @test s ≈ svdvals(A)
        end
    end
end

@testset "Issue #7886" begin
    let
        x, r = LAPACK.gelsy!([0 1; 0 2; 0 3.], [2, 4, 6.])
        @test x ≈ [0,2]
        @test r == 1
    end
end

@testset "geqrt(3)" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        A = rand(elty,10,10)
        B = copy(A)
        C,T = LAPACK.geqrt!(A,zeros(elty,10,10))
        D,S = LAPACK.geqrt3!(A,zeros(elty,10,10))
        @test C ≈ D
    end
end

@testset "gbtrf and gbtrs" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        d = rand(elty,6)
        dl = rand(elty,5)
        du = rand(elty,5)
        dl2 = rand(elty,4)
        AB = zeros(elty,6,6)
        AB[6,1:4] = dl2
        AB[5,1:5] = dl
        AB[4,:] = d
        AB[3,2:6] = du
        AB,ipiv = LAPACK.gbtrf!(2,1,6,AB)
        C = rand(elty,6,6)
        D = copy(C)
        D = LAPACK.gbtrs!('N',2,1,6,AB,ipiv,D)
        A = diagm(-2 => dl2, -1 => dl, 0 => d, 1 => du)
        @test A\C ≈ D
        M = Matrix{elty}(undef,7,6)
        @test_throws DimensionMismatch LAPACK.gbtrs!('N',2,1,6,AB,ipiv,M)
        @test_throws ArgumentError LAPACK.gbtrs!('M',2,1,6,AB,ipiv,M)
        @test_throws LinearAlgebra.LAPACKException LAPACK.gbtrf!(2,1,6,zeros(elty,6,6))
    end
end


@testset "geqp3, geqrt error handling" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        x10, x11 = Vector{elty}.(undef, (10, 11))
        y10, y11 = Vector{LinearAlgebra.BlasInt}.(undef, (10, 11))
        A10x10, A11x10, A10x11, A11x11 = Matrix{elty}.(undef, ((10,10), (11,10), (10,11), (11,11)))
        @test_throws DimensionMismatch LAPACK.geqlf!(A10x10, x11)
        @test_throws DimensionMismatch LAPACK.gelqf!(A10x10, x11)
        @test_throws DimensionMismatch LAPACK.geqp3!(A10x10, y11, x10)
        @test_throws DimensionMismatch LAPACK.geqp3!(A10x10, y10, x11)
        @test_throws ArgumentError LAPACK.geqrt!(A10x10, A11x10)
        @test_throws DimensionMismatch LAPACK.geqrt3!(A10x10, A11x10)
        @test_throws DimensionMismatch LAPACK.geqrt3!(A10x11, A11x11)
        @test_throws DimensionMismatch LAPACK.geqrf!(A10x10, x11)
        @test_throws DimensionMismatch LAPACK.gerqf!(A10x10, x11)
    end
end

@testset "gels, gesv, getrs, getri error handling" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        A10x10, B11x11 = Matrix{elty}.(undef, ((10,10), (11,11)))
        x10, x11 = Vector{LinearAlgebra.BlasInt}.(undef, (10, 11))
        @test_throws DimensionMismatch LAPACK.gels!('N',A10x10,B11x11)
        @test_throws DimensionMismatch LAPACK.gels!('T',A10x10,B11x11)
        @test_throws ArgumentError LAPACK.gels!('X',A10x10,B11x11)
        @test_throws DimensionMismatch LAPACK.gesv!(A10x10,B11x11)
        @test_throws DimensionMismatch LAPACK.getrs!('N',A10x10,x10,B11x11)
        @test_throws DimensionMismatch LAPACK.getrs!('T',A10x10,x10,B11x11)
        @test_throws ArgumentError LAPACK.getrs!('X',A10x10,x10,B11x11)
        @test_throws DimensionMismatch LAPACK.getri!(A10x10,x11)
    end
end

@testset "gelsy, gelsd" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        A = rand(elty, 10, 10)
        B = rand(elty, 10, 10)
        C, j = LAPACK.gelsd!(copy(A),copy(B))
        D, k = LAPACK.gelsy!(copy(A),copy(B))
        @test C ≈ D rtol=4*eps(cond(A))
        @test_throws DimensionMismatch LAPACK.gelsd!(A,rand(elty,12,10))
        @test_throws DimensionMismatch LAPACK.gelsy!(A,rand(elty,12,10))
    end
end

@testset "gglse errors" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        A = rand(elty,10,10)
        @test_throws DimensionMismatch LAPACK.gglse!(A,zeros(elty,10),rand(elty,12,11),zeros(elty,12))
        @test_throws DimensionMismatch LAPACK.gglse!(A,zeros(elty,11),rand(elty,10,10),zeros(elty,10))
        @test_throws DimensionMismatch LAPACK.gglse!(A,zeros(elty,10),rand(elty,10,10),zeros(elty,11))
    end
end

@testset "gesvd, ggsvd" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        A = rand(elty,10,5)
        U,S,V = svd(A)
        lU,lS,lVt = LAPACK.gesvd!('S','S',A)
        @test U ≈ lU
        @test S ≈ lS
        @test V' ≈ lVt
        @test_throws ArgumentError LAPACK.gesvd!('X','S',A)
        @test_throws ArgumentError LAPACK.gesvd!('S','X',A)
        B = rand(elty,10,10)
        # xggsvd3 replaced xggsvd in LAPACK 3.6.0
        if LAPACK.version() < v"3.6.0"
            @test_throws DimensionMismatch LAPACK.ggsvd!('N','N','N',A,B)
            @test_throws ArgumentError LAPACK.ggsvd!('X','N','N',A,B)
            @test_throws ArgumentError LAPACK.ggsvd!('N','X','N',A,B)
            @test_throws ArgumentError LAPACK.ggsvd!('N','N','X',A,B)
        else
            @test_throws DimensionMismatch LAPACK.ggsvd3!('N','N','N',A,B)
            @test_throws ArgumentError LAPACK.ggsvd3!('X','N','N',A,B)
            @test_throws ArgumentError LAPACK.ggsvd3!('N','X','N',A,B)
            @test_throws ArgumentError LAPACK.ggsvd3!('N','N','X',A,B)
        end
    end
end

@testset "geevx, ggev, ggev3 errors" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        A = rand(elty,10,10)
        B = rand(elty,10,10)
        @test_throws ArgumentError LAPACK.geevx!('M','N','N','N',A)
        @test_throws ArgumentError LAPACK.geevx!('N','Z','N','N',A)
        @test_throws ArgumentError LAPACK.geevx!('N','N','Z','N',A)
        @test_throws ArgumentError LAPACK.geevx!('N','N','N','Z',A)
        @test_throws ArgumentError LAPACK.ggev!('N','B',A,B)
        @test_throws ArgumentError LAPACK.ggev!('B','N',A,B)
        @test_throws DimensionMismatch LAPACK.ggev!('N','N',A,zeros(elty,12,12))
        @test_throws ArgumentError LAPACK.ggev3!('N','B',A,B)
        @test_throws ArgumentError LAPACK.ggev3!('B','N',A,B)
        @test_throws DimensionMismatch LAPACK.ggev3!('N','N',A,zeros(elty,12,12))
    end
end

@testset "gebal/gebak" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        typescale = log10(eps(real(elty))) / 3 * 2
        A = rand(elty,10,10) * Diagonal(exp10.(range(typescale, stop=-typescale, length=10)))
        B = copy(A)
        ilo, ihi, scale = LAPACK.gebal!('S',B)
        Bvs = eigvecs(B)
        Avs = eigvecs(A)
        Bvs = LAPACK.gebak!('S','R',ilo,ihi,scale,Bvs)
        @test norm(diff(Avs ./ Bvs, dims=1)) < 100 * eps(abs(float(one(elty))))
    end
end

@testset "gels" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        Random.seed!(913)
        A = rand(elty,10,10)
        X = rand(elty,10)
        B,Y,z = LAPACK.gels!('N',copy(A),copy(X))
        @test A\X ≈ Y
        @test_throws ArgumentError LAPACK.gels!('X',A,X)
    end
end

@testset "getrf/getri" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        A = rand(elty,10,10)
        iA = inv(A)
        A, ipiv, info = LAPACK.getrf!(A)
        A = LAPACK.getri!(A, ipiv)
        @test A ≈ iA

        B = rand(elty,10,10)
        iB = inv(B)
        ipiv = rand(BlasInt,10)
        B, ipiv, info = LAPACK.getrf!(B, ipiv)
        B = LAPACK.getri!(B, ipiv)
        @test B ≈ iB
    end
end

@testset "geev" begin
    # complex is easier for now
    @testset for elty in (ComplexF32, ComplexF64)
        A = rand(elty,10,10)
        Aw, Avl, Avr = LAPACK.geev!('N','V',copy(A))
        fA = eigen(A, sortby=nothing)
        @test fA.values  ≈ Aw
        @test fA.vectors ≈ Avr

        @test_throws ArgumentError LAPACK.geev!('X','V',A)
        @test_throws ArgumentError LAPACK.geev!('N','X',A)
    end
end

@testset "gtsv" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        du = rand(elty,9)
        d  = rand(elty,10)
        dl = rand(elty,9)
        b  = rand(elty,10)
        c = Tridiagonal(dl,d,du) \ b
        b = LAPACK.gtsv!(dl,d,du,b)
        @test b ≈ c
        @test_throws DimensionMismatch LAPACK.gtsv!(zeros(elty,11),d,du,b)
        @test_throws DimensionMismatch LAPACK.gtsv!(dl,d,zeros(elty,11),b)
        @test_throws DimensionMismatch LAPACK.gtsv!(dl,d,du,zeros(elty,11))
        @test LAPACK.gtsv!(elty[],elty[],elty[],elty[]) == elty[]
    end
end

@testset "gttrs,gttrf errors" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        du = rand(elty,9)
        d  = rand(elty,10)
        dl = rand(elty,9)
        b  = rand(elty,10)
        y10 = Vector{BlasInt}(undef, 10)
        x9, x11 = Vector{elty}.(undef, (9, 11))
        @test_throws DimensionMismatch LAPACK.gttrf!(x11, d, du)
        @test_throws DimensionMismatch LAPACK.gttrf!(dl, d, x11)
        @test_throws DimensionMismatch LAPACK.gttrs!('N', x11, d, du, x9, y10, b)
        @test_throws DimensionMismatch LAPACK.gttrs!('N', dl, d, x11, x9, y10, b)
        @test_throws DimensionMismatch LAPACK.gttrs!('N', dl, d, du, x9, y10, x11)
        @test_throws ArgumentError LAPACK.gttrs!('X', dl, d, du, x9, y10, x11)
        A = lu(Tridiagonal(dl,d,du))
        b  = rand(elty,10,5)
        c = copy(b)
        dl,d,du,du2,ipiv = LAPACK.gttrf!(dl,d,du)
        c = LAPACK.gttrs!('N',dl,d,du,du2,ipiv,c)
        @test A\b ≈ c
    end
end

@testset "orglq and friends errors" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        A = rand(elty,10,10)
        A,tau = LAPACK.gelqf!(A)
        @test_throws DimensionMismatch LAPACK.orglq!(A,tau,11)
        temp = rand(elty,11,11)
        @test_throws DimensionMismatch LAPACK.ormlq!('R','N',A,tau,temp)
        @test_throws DimensionMismatch LAPACK.ormlq!('L','N',A,tau,temp)
        @test_throws ArgumentError LAPACK.ormlq!('X','N',A,tau,temp)
        @test_throws ArgumentError LAPACK.ormlq!('R','X',A,tau,temp)
        temp = zeros(elty,11)
        B = copy(A)
        @test_throws DimensionMismatch LAPACK.ormlq!('R','N',A,temp,B)
        @test_throws DimensionMismatch LAPACK.ormlq!('L','N',A,temp,B)
        @test_throws ArgumentError LAPACK.ormlq!('X','N',A,temp,B)
        @test_throws ArgumentError LAPACK.ormlq!('L','X',A,temp,B)

        B = copy(A)
        C = LAPACK.orglq!(B,tau)
        @test LAPACK.ormlq!('R','N',A,tau, Matrix{elty}(I, 10, 10)) ≈ C

        A = rand(elty,10,10)
        A,tau = LAPACK.geqrf!(A)
        @test_throws DimensionMismatch LAPACK.orgqr!(A,tau,11)
        B = copy(A)
        @test LAPACK.orgqr!(B,tau) ≈ LAPACK.ormqr!('R','N',A,tau,Matrix{elty}(I, 10, 10))
        temp = rand(elty,11,11)
        @test_throws DimensionMismatch LAPACK.ormqr!('R','N',A,tau,temp)
        @test_throws DimensionMismatch LAPACK.ormqr!('L','N',A,tau,temp)
        @test_throws ArgumentError LAPACK.ormqr!('X','N',A,tau,temp)
        @test_throws ArgumentError LAPACK.ormqr!('L','X',A,tau,temp)
        B = copy(A)
        temp = zeros(elty,11)
        @test_throws DimensionMismatch LAPACK.ormqr!('R','N',A,temp,B)
        @test_throws DimensionMismatch LAPACK.ormqr!('L','N',A,temp,B)
        @test_throws ArgumentError LAPACK.ormqr!('X','N',A,temp,B)
        @test_throws ArgumentError LAPACK.ormqr!('L','X',A,temp,B)

        A = rand(elty,10,10)
        A,tau = LAPACK.geqlf!(A)
        @test_throws DimensionMismatch LAPACK.orgql!(A,tau,11)
        B = copy(A)
        @test LAPACK.orgql!(B,tau) ≈ LAPACK.ormql!('R','N',A,tau,Matrix{elty}(I, 10, 10))
        temp = rand(elty,11,11)
        @test_throws DimensionMismatch LAPACK.ormql!('R','N',A,tau,temp)
        @test_throws DimensionMismatch LAPACK.ormql!('L','N',A,tau,temp)
        @test_throws ArgumentError LAPACK.ormql!('X','N',A,tau,temp)
        @test_throws ArgumentError LAPACK.ormql!('L','X',A,tau,temp)
        temp = zeros(elty,11)
        B = copy(A)
        @test_throws DimensionMismatch LAPACK.ormql!('R','N',A,temp,B)
        @test_throws DimensionMismatch LAPACK.ormql!('L','N',A,temp,B)
        @test_throws ArgumentError LAPACK.ormql!('X','N',A,temp,B)
        @test_throws ArgumentError LAPACK.ormql!('L','X',A,temp,B)

        A = rand(elty,10,10)
        A,tau = LAPACK.gerqf!(A)
        @test_throws DimensionMismatch LAPACK.orgrq!(A,tau,11)
        B = copy(A)
        @test LAPACK.orgrq!(B,tau) ≈ LAPACK.ormrq!('R','N',A,tau,Matrix{elty}(I, 10, 10))
        temp = rand(elty,11,11)
        @test_throws DimensionMismatch LAPACK.ormrq!('R','N',A,tau,temp)
        @test_throws DimensionMismatch LAPACK.ormrq!('L','N',A,tau,temp)
        @test_throws ArgumentError LAPACK.ormrq!('X','N',A,tau,temp)
        @test_throws ArgumentError LAPACK.ormrq!('L','X',A,tau,temp)
        B = copy(A)
        temp = zeros(elty,11)
        @test_throws DimensionMismatch LAPACK.ormrq!('R','N',A,temp,B)
        @test_throws DimensionMismatch LAPACK.ormrq!('L','N',A,temp,B)
        @test_throws ArgumentError LAPACK.ormrq!('X','N',A,temp,B)
        @test_throws ArgumentError LAPACK.ormrq!('L','X',A,temp,B)

        A = rand(elty,10,11)
        Q = copy(A)
        Q,tau = LAPACK.gerqf!(Q)
        R = triu(Q[:,2:11])
        LAPACK.orgrq!(Q,tau)
        @test Q*Q' ≈ Matrix(I, 10, 10)
        @test R*Q ≈ A
        @test_throws DimensionMismatch LAPACK.orgrq!(zeros(elty,11,10),zeros(elty,10))

        C = rand(elty,10,10)
        V = rand(elty,10,10)
        T = zeros(elty,10,11)
        @test_throws DimensionMismatch LAPACK.gemqrt!('L','N',V,T,C)
        @test_throws DimensionMismatch LAPACK.gemqrt!('R','N',V,T,C)
        @test_throws ArgumentError LAPACK.gemqrt!('X','N',V,T,C)
        @test_throws ArgumentError LAPACK.gemqrt!('R','X',V,T,C)

        C = rand(elty,10,10)
        V = rand(elty,11,10)
        T = zeros(elty,10,10)
        @test_throws DimensionMismatch LAPACK.gemqrt!('R','N',V,T,C)
        @test_throws DimensionMismatch LAPACK.gemqrt!('L','N',V,T,C)
        @test_throws ArgumentError LAPACK.gemqrt!('X','N',V,T,C)
        @test_throws ArgumentError LAPACK.gemqrt!('L','X',V,T,C)

        # test size(T) = (nb,k) ensures 1 <= nb <= k
        T = zeros(elty,10,10)
        V = rand(elty,5,10)
        @test_throws DimensionMismatch LAPACK.gemqrt!('L','N',V,T,C)
        @test_throws ArgumentError LAPACK.gemqrt!('X','N',V,T,C)
        @test_throws ArgumentError LAPACK.gemqrt!('L','X',V,T,C)
        C = rand(elty,10,10)
        V = rand(elty,10,10)
        T = zeros(elty,11,10)
        @test_throws DimensionMismatch LAPACK.gemqrt!('R','N',V,T,C)
        @test_throws ArgumentError LAPACK.gemqrt!('X','N',V,T,C)
        @test_throws ArgumentError LAPACK.gemqrt!('R','X',V,T,C)

        @test_throws DimensionMismatch LAPACK.orghr!(1, 10, C, zeros(elty,11))
    end
end

@testset "sytri, sytrs, and sytrf" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        A = rand(elty,10,10)
        A = A + transpose(A) #symmetric!
        B = copy(A)
        B,ipiv = LAPACK.sytrf!('U',B)
        @test_throws ArgumentError LAPACK.sytrf!('X',B)
        @test triu(inv(A)) ≈ triu(LAPACK.sytri!('U',B,ipiv)) rtol=eps(cond(A))
        @test_throws ArgumentError LAPACK.sytri!('X',B,ipiv)
        temp = rand(elty,11,5)
        @test_throws DimensionMismatch LAPACK.sytrs!('U',B,ipiv,temp)
        @test_throws ArgumentError LAPACK.sytrs!('X',B,ipiv,temp)
        @test LAPACK.sytrf!('U',zeros(elty,0,0)) == (zeros(elty,0,0),zeros(BlasInt,0),zero(BlasInt))
    end

    # Rook-pivoting variants
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        A = rand(elty, 10, 10)
        A = A + transpose(A) #symmetric!
        B = copy(A)
        B,ipiv = LAPACK.sytrf_rook!('U', B)
        @test triu(inv(A)) ≈ triu(LAPACK.sytri_rook!('U', B, ipiv)) rtol=eps(cond(A))
        @test_throws ArgumentError LAPACK.sytri_rook!('X', B, ipiv)
        temp = rand(elty, 11, 5)
        @test_throws DimensionMismatch LAPACK.sytrs_rook!('U', B, ipiv, temp)
        @test_throws ArgumentError LAPACK.sytrs_rook!('X', B, ipiv, temp)
        @test LAPACK.sytrf_rook!('U',zeros(elty, 0, 0)) == (zeros(elty, 0, 0),zeros(BlasInt, 0),zero(BlasInt))
        A = rand(elty, 10, 10)
        A = A + transpose(A) #symmetric!
        b = rand(elty, 10)
        c = A \ b
        cnd = cond(A)
        b,A = LAPACK.sysv_rook!('U', A, b)
        @test b ≈ c rtol=eps(cnd)
        temp = rand(elty,11)
        @test_throws DimensionMismatch LAPACK.sysv_rook!('U',A,temp)
        @test_throws ArgumentError LAPACK.sysv_rook!('X',A,temp)

        # syconvf_rook error handling
        # way argument is wrong
        @test_throws ArgumentError LAPACK.syconvf_rook!('U', 'U', A, rand(BlasInt, 10))
        # ipiv has wrong length
        @test_throws ArgumentError LAPACK.syconvf_rook!('U', 'R', A, rand(BlasInt, 9))
        # e has wrong length
        @test_throws ArgumentError LAPACK.syconvf_rook!('U', 'R', A, rand(BlasInt, 10), rand(elty, 9))
    end
end

@testset "hetrf, hetrs" begin
    @testset for elty in (ComplexF32, ComplexF64)
        A = rand(elty,10,10)
        A = A + A' #hermitian!
        B = copy(A)
        B,ipiv = LAPACK.hetrf!('U',B)
        temp = rand(elty,11,5)
        @test_throws DimensionMismatch LAPACK.hetrs!('U',B,ipiv,temp)
        @test_throws ArgumentError LAPACK.hetrs!('X',B,ipiv,temp)
        @test_throws DimensionMismatch LAPACK.hetrs_rook!('U',B,ipiv,temp)
        @test_throws ArgumentError LAPACK.hetrs_rook!('X',B,ipiv,temp)
    end
end

@testset "stev, stebz, stein, stegr" begin
    @testset for elty in (Float32, Float64)
        d = rand(elty,10)
        e = rand(elty,9)
        temp = rand(elty,11)
        @test_throws DimensionMismatch LAPACK.stev!('N',d,temp)
        @test_throws ArgumentError LAPACK.stev!('X',d,temp)
        temp = rand(elty,10)
        @test_throws DimensionMismatch LAPACK.stebz!('A','B',zero(elty),zero(elty),0,0,-1.,d,temp)
        @test_throws ArgumentError LAPACK.stebz!('X','B',zero(elty),zero(elty),0,0,-1.,d,temp)
        @test_throws ArgumentError LAPACK.stebz!('A','X',zero(elty),zero(elty),0,0,-1.,d,temp)
        temp11 = rand(elty,11)
        @test_throws DimensionMismatch LAPACK.stegr!('N','A',d,temp11,zero(elty),zero(elty),0,0)
        @test_throws ArgumentError LAPACK.stegr!('X','A',d,temp11,zero(elty),zero(elty),0,0)
        @test_throws ArgumentError LAPACK.stegr!('N','X',d,temp11,zero(elty),zero(elty),0,0)
        tempblasint10 = zeros(BlasInt,10)
        tempblasint10_2 = zeros(BlasInt,10)
        @test_throws DimensionMismatch LAPACK.stein!(d,temp11,temp,tempblasint10,tempblasint10_2)
        @test_throws DimensionMismatch LAPACK.stein!(d,e,temp11,tempblasint10,tempblasint10_2)
    end
end

@testset "trtri & trtrs" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        A = rand(elty,10,10)
        A = triu(A)
        B = copy(A)
        @test inv(A) ≈ LAPACK.trtri!('U','N',B)
        @test_throws ArgumentError LAPACK.trtri!('X','N',B)
        @test_throws ArgumentError LAPACK.trtri!('U','X',B)
        temp = zeros(elty,11,10)
        @test_throws DimensionMismatch LAPACK.trtrs!('U','N','N',B,temp)
        @test_throws ArgumentError LAPACK.trtrs!('X','N','N',B,temp)
        @test_throws ArgumentError LAPACK.trtrs!('U','X','N',B,temp)
        @test_throws ArgumentError LAPACK.trtrs!('U','N','X',B,temp)
    end
end

@testset "larfg & larf" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        ## larfg
        Random.seed!(0)
        x  = rand(elty, 5)
        v  = copy(x)
        τ = LinearAlgebra.LAPACK.larfg!(v)
        H = (I - τ*v*v')
        # for complex input, LAPACK wants a conjugate transpose of H (check clarfg docs)
        y = elty <: Complex ? H'*x : H*x
        # we have rotated a vector
        @test norm(y) ≈ norm(x)
        # an annihilated almost all the first column
        @test norm(y[2:end], Inf) < 10*eps(real(one(elty)))

        ## larf
        C = rand(elty, 5, 5)
        C_norm = norm(C, 2)
        v = C[1:end, 1]
        τ = LinearAlgebra.LAPACK.larfg!(v)
        LinearAlgebra.LAPACK.larf!('L', v, conj(τ), C)
        # we have applied a unitary transformation
        @test norm(C, 2) ≈ C_norm
        # an annihilated almost all the first column
        @test norm(C[2:end, 1], Inf) < 10*eps(real(one(elty)))

        # apply left and right
        C1 = rand(elty, 5, 5)
        C2 = rand(elty, 5, 5)
        C = C2*C1

        v = C1[1:end, 1]
        τ = LinearAlgebra.LAPACK.larfg!(v)
        LinearAlgebra.LAPACK.larf!('L', v,      τ, C1)
        LinearAlgebra.LAPACK.larf!('R', v, conj(τ), C2)
        @test C ≈ C2*C1

        @test_throws ArgumentError LAPACK.larf!('X', v,      τ, C1)
    end
end

@testset "tgsen, tzrzf, & trsyl" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        Z = zeros(elty,10,10)
        @test_throws DimensionMismatch LAPACK.tgsen!(zeros(BlasInt,10),Z,zeros(elty,11,11),Z,Z)
        @test_throws DimensionMismatch LAPACK.tgsen!(zeros(BlasInt,10),Z,Z,zeros(elty,11,11),Z)
        @test_throws DimensionMismatch LAPACK.tgsen!(zeros(BlasInt,10),Z,Z,Z,zeros(elty,11,11))
        @test_throws DimensionMismatch LAPACK.trsyl!('N','N',Z,Z,zeros(elty,11,11))
        @test_throws ArgumentError LAPACK.trsyl!('X','N',Z,Z,zeros(elty,11,11))
        @test_throws ArgumentError LAPACK.trsyl!('N','X',Z,Z,zeros(elty,11,11))
        @test_throws DimensionMismatch LAPACK.tzrzf!(zeros(elty,10,5))

        A = triu(rand(elty,4,4))
        V = view(A, 1:2, :)
        M = Matrix(V)
        @test LAPACK.tzrzf!(V) == LAPACK.tzrzf!(M)
    end
end

@testset "sysv" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        Random.seed!(123)
        A = rand(elty,10,10)
        A = A + transpose(A) #symmetric!
        b = rand(elty,10)
        c = A \ b
        b,A = LAPACK.sysv!('U',A,b)
        @test b ≈ c
        @test_throws DimensionMismatch LAPACK.sysv!('U',A,rand(elty,11))
        @test_throws ArgumentError LAPACK.sysv!('X',A,rand(elty,11))
    end
end

@testset "hesv" begin
    @testset for elty in (ComplexF32, ComplexF64)
        Random.seed!(935)
        A = rand(elty,10,10)
        A = A + A' #hermitian!
        b = rand(elty,10)
        c = A \ b
        b,A = LAPACK.hesv!('U',A,b)
        @test b ≈ c
        temp = rand(elty,11)
        @test_throws DimensionMismatch LAPACK.hesv!('U',A,temp)
        @test_throws ArgumentError LAPACK.hesv!('X',A,temp)
        A = rand(elty,10,10)
        A = A + A' #hermitian!
        b = rand(elty,10)
        c = A \ b
        b,A = LAPACK.hesv_rook!('U',A,b)
        @test b ≈ c
        @test_throws DimensionMismatch LAPACK.hesv_rook!('U',A,temp)
        @test_throws ArgumentError LAPACK.hesv_rook!('X',A,temp)
    end
end

@testset "ptsv" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        dv = fill(elty(1),10)
        ev = zeros(elty,9)
        rdv = real(dv)
        A = SymTridiagonal(dv,ev)
        if elty <: Complex
            A = Tridiagonal(conj(ev),dv,ev)
        end
        B = rand(elty,10,10)
        C = copy(B)
        @test A\B ≈ LAPACK.ptsv!(rdv,ev,C)
        @test_throws DimensionMismatch LAPACK.ptsv!(rdv,Vector{elty}(undef,10),C)
        @test_throws DimensionMismatch LAPACK.ptsv!(rdv,ev,Matrix{elty}(undef,11,11))
    end
end

@testset "pttrf and pttrs" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        dv = fill(elty(1),10)
        ev = zeros(elty,9)
        rdv = real(dv)
        A = SymTridiagonal(dv,ev)
        if elty <: Complex
            A = Tridiagonal(conj(ev),dv,ev)
        end
        rdv,ev = LAPACK.pttrf!(rdv,ev)
        @test_throws DimensionMismatch LAPACK.pttrf!(rdv,dv)
        B = rand(elty,10,10)
        C = copy(B)
        if elty <: Complex
            @test A\B ≈ LAPACK.pttrs!('U',rdv,ev,C)
            tempvec = Vector{elty}(undef,10)
            tempmat = Matrix{elty}(undef,11,11)
            @test_throws DimensionMismatch LAPACK.pttrs!('U',rdv,tempvec,C)
            @test_throws DimensionMismatch LAPACK.pttrs!('U',rdv,ev,tempmat)
            @test_throws ArgumentError LAPACK.pttrs!('X',rdv,tempvec,C)
            @test_throws ArgumentError LAPACK.pttrs!('X',rdv,ev,tempmat)
        else
            @test A\B ≈ LAPACK.pttrs!(rdv,ev,C)
            @test_throws DimensionMismatch LAPACK.pttrs!(rdv,Vector{elty}(undef,10),C)
            @test_throws DimensionMismatch LAPACK.pttrs!(rdv,ev,Matrix{elty}(undef,11,11))
        end
    end
end

@testset "posv and some errors for friends" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        local n = 10
        A = rand(elty,n,n)/100
        A += real(diagm(0 => n*real(rand(elty,n))))
        if elty <: Complex
            A = A + A'
        else
            A = A + transpose(A)
        end
        B = rand(elty,n,n)
        D = copy(A)
        C = copy(B)
        D,C = LAPACK.posv!('U',D,C)
        @test A\B ≈ C
        offsizemat = Matrix{elty}(undef, n+1, n+1)
        @test_throws DimensionMismatch LAPACK.posv!('U', D, offsizemat)
        @test_throws DimensionMismatch LAPACK.potrs!('U', D, offsizemat)
        @test_throws ArgumentError LAPACK.posv!('X', D, offsizemat)
        @test_throws ArgumentError LAPACK.potrs!('X', D, offsizemat)

        @test LAPACK.potrs!('U',Matrix{elty}(undef,0,0),elty[]) == elty[]
    end
end

@testset "gesvx" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        A = rand(elty,10,10)
        B = rand(elty,10,5)
        C = copy(A)
        D = copy(B)
        X, rcond, f, b, r = LAPACK.gesvx!(C,D)
        @test X ≈ A\B rtol=inv(rcond)*eps(real(elty))
    end
end

@testset "gees, gges, gges3 error throwing" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        A = rand(elty,10,10)
        B = rand(elty,11,11)
        @test_throws DimensionMismatch LAPACK.gges!('V','V',A,B)
        @test_throws DimensionMismatch LAPACK.gges3!('V','V',A,B)
        @test_throws ArgumentError LAPACK.gges!('X','V',A,B)
        @test_throws ArgumentError LAPACK.gges3!('X','V',A,B)
        @test_throws ArgumentError LAPACK.gges!('V','X',A,B)
        @test_throws ArgumentError LAPACK.gges3!('V','X',A,B)
    end
end

@testset "trrfs & trevc" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        T = triu(rand(elty,10,10))
        v = eigvecs(T, sortby=nothing)[:,1]
        select = zeros(LinearAlgebra.BlasInt,10)
        select[1] = 1
        select,Vr = LAPACK.trevc!('R','S',select,copy(T))
        @test Vr ≈ v
        select = zeros(LinearAlgebra.BlasInt,10)
        select[1] = 1
        select,Vl = LAPACK.trevc!('L','S',select,copy(T))
        select = zeros(LinearAlgebra.BlasInt,10)
        select[1] = 1
        select,Vln,Vrn = LAPACK.trevc!('B','S',select,copy(T))
        @test Vrn ≈ v
        @test Vln ≈ Vl
        @test_throws ArgumentError LAPACK.trevc!('V','S',select,T)
        @test_throws ArgumentError LAPACK.trevc!('R','X',select,T)
        temp1010 = rand(elty,10,10)
        temp1011 = rand(elty,10,11)
        @test_throws DimensionMismatch LAPACK.trrfs!('U','N','N',T,temp1010,temp1011)
        @test_throws ArgumentError LAPACK.trrfs!('X','N','N',T,temp1010,temp1011)
        @test_throws ArgumentError LAPACK.trrfs!('U','X','N',T,temp1010,temp1011)
        @test_throws ArgumentError LAPACK.trrfs!('U','N','X',T,temp1010,temp1011)
    end
end

@testset "laic1" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        @test_throws DimensionMismatch LAPACK.laic1!(1,rand(elty,10),real(rand(elty)),rand(elty,11),rand(elty))
    end
end

@testset "trsen" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        for job in ('N', 'E', 'V', 'B')
            for c in ('V', 'N')
                A = convert(Matrix{elty}, [7 2 2 1; 1 5 2 0; 0 3 9 4; 1 1 1 4])
                T,Q,d = schur(A)
                s, sep = LinearAlgebra.LAPACK.trsen!(job,c,Array{LinearAlgebra.BlasInt}([0,1,0,0]),T,Q)[4:5]
                @test d[1] ≈ T[2,2]
                @test d[2] ≈ T[1,1]
                if c == 'V'
                    @test  Q*T*Q' ≈ A
                end
                if job == 'N' || job == 'V'
                    @test iszero(s)
                else
                    @test s ≈ 0.8080423 atol=1e-6
                end
                if job == 'N' || job == 'E'
                    @test iszero(sep)
                else
                    @test sep ≈ 2. atol=3e-1
                end
            end
        end
    end
end

@testset "trexc" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        for c in ('V', 'N')
            A = convert(Matrix{elty}, [7 2 2 1; 1 5 2 0; 0 3 9 4; 1 1 1 4])
            T,Q,d = schur(A)
            LinearAlgebra.LAPACK.trexc!(c,LinearAlgebra.BlasInt(1),LinearAlgebra.BlasInt(2),T,Q)
            @test d[1] ≈ T[2,2]
            @test d[2] ≈ T[1,1]
            if c == 'V'
                @test Q*T*Q' ≈ A
            end
        end
    end
end

@testset "lacpy!" begin
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        n = 10
        A = rand(elty, n, n)
        for uplo in ('L', 'U', 'N')
            B = zeros(elty, n, n)
            LinearAlgebra.LAPACK.lacpy!(B, A, uplo)
            C = uplo == 'L' ? tril(A) : (uplo == 'U' ? triu(A) : A)
            @test B == C
            B = zeros(elty, n+1, n+1)
            LinearAlgebra.LAPACK.lacpy!(B, A, uplo)
            C = uplo == 'L' ? tril(A) : (uplo == 'U' ? triu(A) : A)
            @test view(B, 1:n, 1:n) == C
        end
        A = rand(elty, n, n+1)
        B = zeros(elty, n, n)
        LinearAlgebra.LAPACK.lacpy!(B, A, 'L')
        @test B == view(tril(A), 1:n, 1:n)
        B = zeros(elty, n, n+1)
        LinearAlgebra.LAPACK.lacpy!(B, A, 'U')
        @test B == triu(A)
        A = rand(elty, n+1, n)
        B = zeros(elty, n, n)
        LinearAlgebra.LAPACK.lacpy!(B, A, 'U')
        @test B == view(triu(A), 1:n, 1:n)
        B = zeros(elty, n+1, n)
        LinearAlgebra.LAPACK.lacpy!(B, A, 'L')
        @test B == tril(A)
    end
end

@testset "Julia vs LAPACK" begin
    # Test our own linear algebra functionality against LAPACK
    @testset for elty in (Float32, Float64, ComplexF32, ComplexF64)
        for nn in (5,10,15)
            if elty <: Real
                A = convert(Matrix{elty}, randn(10,nn))
            else
                A = convert(Matrix{elty}, complex.(randn(10,nn),randn(10,nn)))
            end    ## LU (only equal for real because LAPACK uses different absolute value when choosing permutations)
            if elty <: Real
                FJulia  = LinearAlgebra.generic_lufact!(copy(A))
                FLAPACK = LinearAlgebra.LAPACK.getrf!(copy(A))
                @test FJulia.factors ≈ FLAPACK[1]
                @test FJulia.ipiv ≈ FLAPACK[2]
                @test FJulia.info ≈ FLAPACK[3]
            end

            ## QR
            FJulia  = LinearAlgebra.qrfactUnblocked!(copy(A))
            FLAPACK = LinearAlgebra.LAPACK.geqrf!(copy(A))
            @test FJulia.factors ≈ FLAPACK[1]
            @test FJulia.τ ≈ FLAPACK[2]
        end
    end
end

# Issue 13976
let A = [NaN 0.0 NaN; 0 0 0; NaN 0 NaN]
    @test_throws ArgumentError exp(A)
end

# Issue 14065 (and 14220)
let A = [NaN NaN; NaN NaN]
    @test_throws ArgumentError eigen(A)
end

# Issue #42762 https://github.com/JuliaLang/julia/issues/42762
# Tests geqrf! and gerqf! with null column dimensions
a = zeros(2,0), zeros(0)
@test LinearAlgebra.LAPACK.geqrf!(a...) === a
@test LinearAlgebra.LAPACK.gerqf!(a...) === a

# Issue #49489: https://github.com/JuliaLang/julia/issues/49489
# Dimension mismatch between A and ipiv causes segfaults
@testset "issue #49489" begin
    A = randn(23,23)
    b = randn(23)
    ipiv = collect(1:20)
    @test_throws DimensionMismatch LinearAlgebra.LAPACK.getrs!('N', A, ipiv, b)
end

@testset "hetrd ignore non-filled half" begin
    A = rand(3,3)
    B = copy(A)
    B[2,1] = NaN
    B[3,1] = Inf
    LAPACK.hetrd!('U', A)
    LAPACK.hetrd!('U', B)
    @test UpperTriangular(A) == UpperTriangular(B)
end

@testset "inference in syev!/syevd!" begin
    for T in (Float32, Float64), CT in (T, Complex{T})
        A = rand(CT, 4,4)
        @inferred (A -> LAPACK.syev!('N', 'U', A))(A)
        @inferred (A -> LAPACK.syev!('V', 'U', A))(A)
        @inferred (A -> LAPACK.syevd!('N', 'U', A))(A)
        @inferred (A -> LAPACK.syevd!('V', 'U', A))(A)
    end
end

end # module TestLAPACK
