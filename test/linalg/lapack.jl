# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

import Base.LinAlg.BlasInt

@test_throws ArgumentError Base.LinAlg.LAPACK.chkuplo('Z')
@test_throws ArgumentError Base.LinAlg.LAPACK.chkside('Z')
@test_throws ArgumentError Base.LinAlg.LAPACK.chkdiag('Z')
@test_throws ArgumentError Base.LinAlg.LAPACK.chktrans('Z')

@testset "syevr" begin
    let
        srand(123)
        Ainit = randn(5,5)
        @testset for elty in (Float32, Float64, Complex64, Complex128)
            if elty == Complex64 || elty == Complex128
                A = complex.(Ainit, Ainit)
            else
                A = Ainit
            end
            A = convert(Array{elty, 2}, A)
            Asym = A'A
            vals, Z = LAPACK.syevr!('V', copy(Asym))
            @test Z*(Diagonal(vals)*Z') ≈ Asym
            @test all(vals .> 0.0)
            @test LAPACK.syevr!('N','V','U',copy(Asym),0.0,1.0,4,5,-1.0)[1] ≈ vals[vals .< 1.0]
            @test LAPACK.syevr!('N','I','U',copy(Asym),0.0,1.0,4,5,-1.0)[1] ≈ vals[4:5]
            @test vals ≈ LAPACK.syev!('N','U',copy(Asym))

            @test_throws DimensionMismatch LAPACK.sygvd!(1,'V','U',copy(Asym),ones(elty,6,6))
        end
    end
end

@testset "gglse" begin
    let
        @testset for elty in (Float32, Float64, Complex64, Complex128)
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
            U, Vt, C = eye(elty, n), eye(elty, n), eye(elty, n)
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
            U, Vt, C = eye(elty, n), eye(elty, n), eye(elty, n)
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
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        A = rand(elty,10,10)
        B = copy(A)
        C,T = LAPACK.geqrt!(A,zeros(elty,10,10))
        D,S = LAPACK.geqrt3!(A,zeros(elty,10,10))
        @test C ≈ D
    end
end

@testset "gbtrf and gbtrs" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
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
        A = diagm(dl2,-2) + diagm(dl,-1) + diagm(d) + diagm(du,1)
        @test A\C ≈ D
        @test_throws DimensionMismatch LAPACK.gbtrs!('N',2,1,6,AB,ipiv,ones(elty,7,6))
        @test_throws Base.LinAlg.LAPACKException LAPACK.gbtrf!(2,1,6,zeros(AB))
    end
end


@testset "geqp3, geqrt error handling" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        A = rand(elty,10,10)
        @test_throws DimensionMismatch LAPACK.geqlf!(A,zeros(elty,11))
        @test_throws DimensionMismatch LAPACK.gelqf!(A,zeros(elty,11))
        @test_throws DimensionMismatch LAPACK.geqp3!(A,ones(Base.LinAlg.BlasInt,11),zeros(elty,10))
        @test_throws DimensionMismatch LAPACK.geqp3!(A,ones(Base.LinAlg.BlasInt,10),zeros(elty,11))
        @test_throws ArgumentError LAPACK.geqrt!(A,zeros(elty,11,10))
        @test_throws DimensionMismatch LAPACK.geqrt3!(A,zeros(elty,11,10))
        @test_throws DimensionMismatch LAPACK.geqrt3!(ones(elty,10,11),zeros(elty,11,11))
        @test_throws DimensionMismatch LAPACK.geqrf!(A,zeros(elty,11))
        @test_throws DimensionMismatch LAPACK.gerqf!(A,zeros(elty,11))
    end
end

@testset "gels, gesv, getrs, getri error handling" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        A = rand(elty,10,10)
        B = rand(elty,11,11)
        @test_throws DimensionMismatch LAPACK.gels!('N',A,B)
        @test_throws DimensionMismatch LAPACK.gels!('T',A,B)
        @test_throws DimensionMismatch LAPACK.gesv!(A,B)
        @test_throws DimensionMismatch LAPACK.getrs!('N',A,ones(Base.LinAlg.BlasInt,10),B)
        @test_throws DimensionMismatch LAPACK.getrs!('T',A,ones(Base.LinAlg.BlasInt,10),B)
        @test_throws DimensionMismatch LAPACK.getri!(A,ones(Base.LinAlg.BlasInt,11))
    end
end

@testset "gelsy, gelsd" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        A = rand(elty,10,10)
        B = rand(elty,10,10)
        C, j = LAPACK.gelsd!(copy(A),copy(B))
        D, k = LAPACK.gelsy!(copy(A),copy(B))
        @test C ≈ D
        @test_throws DimensionMismatch LAPACK.gelsd!(A,rand(elty,12,10))
        @test_throws DimensionMismatch LAPACK.gelsy!(A,rand(elty,12,10))
    end
end

@testset "gglse errors" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        A = rand(elty,10,10)
        @test_throws DimensionMismatch LAPACK.gglse!(A,zeros(elty,10),rand(elty,12,11),zeros(elty,12))
        @test_throws DimensionMismatch LAPACK.gglse!(A,zeros(elty,11),rand(elty,10,10),zeros(elty,10))
        @test_throws DimensionMismatch LAPACK.gglse!(A,zeros(elty,10),rand(elty,10,10),zeros(elty,11))
    end
end

@testset "gesvd, ggsvd" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        A = rand(elty,10,5)
        U,S,V = svd(A)
        lU,lS,lVt = LAPACK.gesvd!('S','S',A)
        @test U ≈ lU
        @test S ≈ lS
        @test V' ≈ lVt
        B = rand(elty,10,10)
        # xggsvd3 replaced xggsvd in LAPACK 3.6.0
        if LAPACK.version() < v"3.6.0"
            @test_throws DimensionMismatch LAPACK.ggsvd!('S','S','S',A,B)
        else
            @test_throws DimensionMismatch LAPACK.ggsvd3!('S','S','S',A,B)
        end
    end
end

@testset "geevx, ggev errors" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        A = rand(elty,10,10)
        B = rand(elty,10,10)
        @test_throws ArgumentError LAPACK.geevx!('M','N','N','N',A)
        @test_throws ArgumentError LAPACK.geevx!('N','Z','N','N',A)
        @test_throws ArgumentError LAPACK.geevx!('N','N','Z','N',A)
        @test_throws ArgumentError LAPACK.geevx!('N','N','N','Z',A)
        @test_throws ArgumentError LAPACK.ggev!('N','B',A,B)
        @test_throws ArgumentError LAPACK.ggev!('B','N',A,B)
        @test_throws DimensionMismatch LAPACK.ggev!('N','N',A,zeros(elty,12,12))
    end
end

@testset "gebal/gebak" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        A = rand(elty,10,10) * Diagonal(exp10.(linspace(-10,10,10)))
        B = copy(A)
        ilo, ihi, scale = LAPACK.gebal!('S',B)
        Bvs = eigvecs(B)
        Avs = eigvecs(A)
        Bvs = LAPACK.gebak!('S','R',ilo,ihi,scale,Bvs)
        @test norm(diff(Avs ./ Bvs)) < 100 * eps(abs(float(one(elty))))
    end
end

@testset "gels" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        A = rand(elty,10,10)
        X = rand(elty,10)
        B,Y,z = LAPACK.gels!('N',copy(A),copy(X))
        @test A\X ≈ Y
    end
end

@testset "getrf/getri" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        A = rand(elty,10,10)
        iA = inv(A)
        A, ipiv = LAPACK.getrf!(A)
        A = LAPACK.getri!(A, ipiv)
        @test A ≈ iA
    end
end

@testset "geev" begin
    # complex is easier for now
    @testset for elty in (Complex64, Complex128)
        A = rand(elty,10,10)
        Aw, Avl, Avr = LAPACK.geev!('N','V',copy(A))
        fA = eigfact(A)
        @test fA[:values] ≈ Aw
        @test fA[:vectors] ≈ Avr
    end
end

@testset "gtsv" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
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
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        du = rand(elty,9)
        d  = rand(elty,10)
        dl = rand(elty,9)
        b  = rand(elty,10)
        @test_throws DimensionMismatch LAPACK.gttrf!(zeros(elty,11),d,du)
        @test_throws DimensionMismatch LAPACK.gttrf!(dl,d,zeros(elty,11))
        @test_throws DimensionMismatch LAPACK.gttrs!('N',zeros(elty,11),d,du,ones(elty,9),zeros(Base.LinAlg.BlasInt,10),b)
        @test_throws DimensionMismatch LAPACK.gttrs!('N',dl,d,zeros(elty,11),ones(elty,9),zeros(Base.LinAlg.BlasInt,10),b)
        @test_throws DimensionMismatch LAPACK.gttrs!('N',dl,d,du,ones(elty,9),zeros(Base.LinAlg.BlasInt,10),zeros(elty,11))
        A = lufact(Tridiagonal(dl,d,du))
        b  = rand(elty,10,5)
        c = copy(b)
        dl,d,du,du2,ipiv = LAPACK.gttrf!(dl,d,du)
        c = LAPACK.gttrs!('N',dl,d,du,du2,ipiv,c)
        @test A\b ≈ c
    end
end

@testset "orglq and friends errors" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        A = rand(elty,10,10)
        A,tau = LAPACK.gelqf!(A)
        @test_throws DimensionMismatch LAPACK.orglq!(A,tau,11)
        @test_throws DimensionMismatch LAPACK.ormlq!('R','N',A,tau,rand(elty,11,11))
        @test_throws DimensionMismatch LAPACK.ormlq!('L','N',A,tau,rand(elty,11,11))
        @test_throws DimensionMismatch LAPACK.ormlq!('R','N',A,zeros(elty,11),rand(elty,10,10))
        @test_throws DimensionMismatch LAPACK.ormlq!('L','N',A,zeros(elty,11),rand(elty,10,10))

        B = copy(A)
        C = LAPACK.orglq!(B,tau)
        @test LAPACK.ormlq!('R','N',A,tau,eye(elty,10)) ≈ C

        A = rand(elty,10,10)
        A,tau = LAPACK.geqrf!(A)
        @test_throws DimensionMismatch LAPACK.orgqr!(A,tau,11)
        B = copy(A)
        @test LAPACK.orgqr!(B,tau) ≈ LAPACK.ormqr!('R','N',A,tau,eye(elty,10))
        @test_throws DimensionMismatch LAPACK.ormqr!('R','N',A,tau,rand(elty,11,11))
        @test_throws DimensionMismatch LAPACK.ormqr!('L','N',A,tau,rand(elty,11,11))
        @test_throws DimensionMismatch LAPACK.ormqr!('R','N',A,zeros(elty,11),rand(elty,10,10))
        @test_throws DimensionMismatch LAPACK.ormqr!('L','N',A,zeros(elty,11),rand(elty,10,10))

        A = rand(elty,10,10)
        A,tau = LAPACK.geqlf!(A)
        @test_throws DimensionMismatch LAPACK.orgql!(A,tau,11)
        B = copy(A)
        @test LAPACK.orgql!(B,tau) ≈ LAPACK.ormql!('R','N',A,tau,eye(elty,10))
        @test_throws DimensionMismatch LAPACK.ormql!('R','N',A,tau,rand(elty,11,11))
        @test_throws DimensionMismatch LAPACK.ormql!('L','N',A,tau,rand(elty,11,11))
        @test_throws DimensionMismatch LAPACK.ormql!('R','N',A,zeros(elty,11),rand(elty,10,10))
        @test_throws DimensionMismatch LAPACK.ormql!('L','N',A,zeros(elty,11),rand(elty,10,10))

        A = rand(elty,10,10)
        A,tau = LAPACK.gerqf!(A)
        @test_throws DimensionMismatch LAPACK.orgrq!(A,tau,11)
        B = copy(A)
        @test LAPACK.orgrq!(B,tau) ≈ LAPACK.ormrq!('R','N',A,tau,eye(elty,10))
        @test_throws DimensionMismatch LAPACK.ormrq!('R','N',A,tau,rand(elty,11,11))
        @test_throws DimensionMismatch LAPACK.ormrq!('L','N',A,tau,rand(elty,11,11))
        @test_throws DimensionMismatch LAPACK.ormrq!('R','N',A,zeros(elty,11),rand(elty,10,10))
        @test_throws DimensionMismatch LAPACK.ormrq!('L','N',A,zeros(elty,11),rand(elty,10,10))

        A = rand(elty,10,11)
        Q = copy(A)
        Q,tau = LAPACK.gerqf!(Q)
        R = triu(Q[:,2:11])
        LAPACK.orgrq!(Q,tau)
        @test Q*Q' ≈ eye(elty,10)
        @test R*Q ≈ A
        @test_throws DimensionMismatch LAPACK.orgrq!(zeros(elty,11,10),zeros(elty,10))

        C = rand(elty,10,10)
        V = rand(elty,10,10)
        T = zeros(elty,10,11)
        @test_throws DimensionMismatch LAPACK.gemqrt!('L','N',V,T,C)
        @test_throws DimensionMismatch LAPACK.gemqrt!('R','N',V,T,C)

        C = rand(elty,10,10)
        V = rand(elty,11,10)
        T = zeros(elty,10,10)
        @test_throws DimensionMismatch LAPACK.gemqrt!('R','N',V,T,C)
        @test_throws DimensionMismatch LAPACK.gemqrt!('L','N',V,T,C)

        # test size(T) = (nb,k) ensures 1 <= nb <= k
        T = zeros(elty,10,10)
        V = rand(elty,5,10)
        @test_throws DimensionMismatch LAPACK.gemqrt!('L','N',V,T,C)
        C = rand(elty,10,10)
        V = rand(elty,10,10)
        T = zeros(elty,11,10)
        @test_throws DimensionMismatch LAPACK.gemqrt!('R','N',V,T,C)

        @test_throws DimensionMismatch LAPACK.orghr!(1, 10, C, zeros(elty,11))
    end
end

@testset "sytri, sytrs, and sytrf" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        A = rand(elty,10,10)
        A = A + A.' #symmetric!
        B = copy(A)
        B,ipiv = LAPACK.sytrf!('U',B)
        @test triu(inv(A)) ≈ triu(LAPACK.sytri!('U',B,ipiv))
        @test_throws DimensionMismatch LAPACK.sytrs!('U',B,ipiv,rand(elty,11,5))
        @test LAPACK.sytrf!('U',zeros(elty,0,0)) == (zeros(elty,0,0),zeros(BlasInt,0))
    end

    # Rook-pivoting variants
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        A = rand(elty,10,10)
        A = A + A.' #symmetric!
        B = copy(A)
        B,ipiv = LAPACK.sytrf_rook!('U',B)
        @test triu(inv(A)) ≈ triu(LAPACK.sytri_rook!('U',B,ipiv))
        @test_throws DimensionMismatch LAPACK.sytrs_rook!('U',B,ipiv,rand(elty,11,5))
        @test LAPACK.sytrf_rook!('U',zeros(elty,0,0)) == (zeros(elty,0,0),zeros(BlasInt,0))
        A = rand(elty,10,10)
        A = A + A.' #symmetric!
        b = rand(elty,10)
        c = A \ b
        b,A = LAPACK.sysv_rook!('U',A,b)
        @test b ≈ c
        @test_throws DimensionMismatch LAPACK.sysv_rook!('U',A,rand(elty,11))
    end
end

@testset "hetrf, hetrs" begin
    @testset for elty in (Complex64, Complex128)
        A = rand(elty,10,10)
        A = A + A' #hermitian!
        B = copy(A)
        B,ipiv = LAPACK.hetrf!('U',B)
        @test_throws DimensionMismatch LAPACK.hetrs!('U',B,ipiv,rand(elty,11,5))
        @test_throws DimensionMismatch LAPACK.hetrs_rook!('U',B,ipiv,rand(elty,11,5))
    end
end

@testset "stev, stebz, stein, stegr" begin
    @testset for elty in (Float32, Float64)
        d = rand(elty,10)
        e = rand(elty,9)
        @test_throws DimensionMismatch LAPACK.stev!('U',d,rand(elty,10))
        @test_throws DimensionMismatch LAPACK.stebz!('A','B',zero(elty),zero(elty),0,0,-1.,d,rand(elty,10))
        @test_throws DimensionMismatch LAPACK.stegr!('N','A',d,rand(elty,10),zero(elty),zero(elty),0,0)
        @test_throws DimensionMismatch LAPACK.stein!(d,zeros(elty,10),zeros(d),zeros(BlasInt,10),zeros(BlasInt,10))
        @test_throws DimensionMismatch LAPACK.stein!(d,e,zeros(elty,11),zeros(BlasInt,10),zeros(BlasInt,10))
    end
end

@testset "trtri & trtrs" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        A = rand(elty,10,10)
        A = triu(A)
        B = copy(A)
        @test inv(A) ≈ LAPACK.trtri!('U','N',B)
        @test_throws DimensionMismatch LAPACK.trtrs!('U','N','N',B,zeros(elty,11,10))
    end
end

@testset "tgsen, tzrzf, & trsyl" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        Z = zeros(elty,10,10)
        @test_throws DimensionMismatch LAPACK.tgsen!(zeros(BlasInt,10),Z,zeros(elty,11,11),Z,Z)
        @test_throws DimensionMismatch LAPACK.tgsen!(zeros(BlasInt,10),Z,Z,zeros(elty,11,11),Z)
        @test_throws DimensionMismatch LAPACK.tgsen!(zeros(BlasInt,10),Z,Z,Z,zeros(elty,11,11))
        @test_throws DimensionMismatch LAPACK.trsyl!('N','N',Z,Z,zeros(elty,11,11))
        @test_throws DimensionMismatch LAPACK.tzrzf!(zeros(elty,10,5))
    end
end

@testset "sysv" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        A = rand(elty,10,10)
        A = A + A.' #symmetric!
        b = rand(elty,10)
        c = A \ b
        b,A = LAPACK.sysv!('U',A,b)
        @test b ≈ c
        @test_throws DimensionMismatch LAPACK.sysv!('U',A,rand(elty,11))
    end
end

@testset "hesv" begin
    @testset for elty in (Complex64, Complex128)
        A = rand(elty,10,10)
        A = A + A' #hermitian!
        b = rand(elty,10)
        c = A \ b
        b,A = LAPACK.hesv!('U',A,b)
        @test b ≈ c
        @test_throws DimensionMismatch LAPACK.hesv!('U',A,rand(elty,11))
        A = rand(elty,10,10)
        A = A + A' #hermitian!
        b = rand(elty,10)
        c = A \ b
        b,A = LAPACK.hesv_rook!('U',A,b)
        @test b ≈ c
        @test_throws DimensionMismatch LAPACK.hesv_rook!('U',A,rand(elty,11))
    end
end

@testset "ptsv" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        dv = ones(elty,10)
        ev = zeros(elty,9)
        rdv = real(dv)
        A = SymTridiagonal(dv,ev)
        if elty <: Complex
            A = Tridiagonal(conj(ev),dv,ev)
        end
        B = rand(elty,10,10)
        C = copy(B)
        @test A\B ≈ LAPACK.ptsv!(rdv,ev,C)
        @test_throws DimensionMismatch LAPACK.ptsv!(rdv,ones(elty,10),C)
        @test_throws DimensionMismatch LAPACK.ptsv!(rdv,ev,ones(elty,11,11))
    end
end

@testset "pttrf and pttrs" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        dv = ones(elty,10)
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
            @test_throws DimensionMismatch LAPACK.pttrs!('U',rdv,ones(elty,10),C)
            @test_throws DimensionMismatch LAPACK.pttrs!('U',rdv,ev,rand(elty,11,11))
        else
            @test A\B ≈ LAPACK.pttrs!(rdv,ev,C)
            @test_throws DimensionMismatch LAPACK.pttrs!(rdv,ones(elty,10),C)
            @test_throws DimensionMismatch LAPACK.pttrs!(rdv,ev,rand(elty,11,11))
        end
    end
end

@testset "posv and some errors for friends" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        A = rand(elty,10,10)/100
        A += real(diagm(10*real(rand(elty,10))))
        if elty <: Complex
            A = A + A'
        else
            A = A + A.'
        end
        B = rand(elty,10,10)
        D = copy(A)
        C = copy(B)
        D,C = LAPACK.posv!('U',D,C)
        @test A\B ≈ C
        @test_throws DimensionMismatch LAPACK.posv!('U',D,ones(elty,12,12))
        @test_throws DimensionMismatch LAPACK.potrs!('U',D,ones(elty,12,12))

        @test LAPACK.potrs!('U',zeros(elty,0,0),ones(elty,0)) == ones(elty,0)
    end
end

@testset "gesvx" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        A = rand(elty,10,10)
        B = rand(elty,10,5)
        C = copy(A)
        D = copy(B)
        X, rcond, f, b, r = LAPACK.gesvx!(C,D)
        @test X ≈ A\B
    end
end

@testset "gees, gges error throwing" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        A = rand(elty,10,10)
        B = rand(elty,11,11)
        @test_throws DimensionMismatch LAPACK.gges!('V','V',A,B)
    end
end

@testset "trrfs & trevc" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        T = triu(rand(elty,10,10))
        S = copy(T)
        select = zeros(Base.LinAlg.BlasInt,10)
        select[1] = 1
        select,Vr = LAPACK.trevc!('R','S',select,copy(T))
        @test Vr ≈ eigvecs(S)[:,1]
        select = zeros(Base.LinAlg.BlasInt,10)
        select[1] = 1
        select,Vl = LAPACK.trevc!('L','S',select,copy(T))
        select = zeros(Base.LinAlg.BlasInt,10)
        select[1] = 1
        select,Vln,Vrn = LAPACK.trevc!('B','S',select,copy(T))
        @test Vrn ≈ eigvecs(S)[:,1]
        @test Vln ≈ Vl
        @test_throws ArgumentError LAPACK.trevc!('V','S',select,copy(T))
        @test_throws DimensionMismatch LAPACK.trrfs!('U','N','N',T,rand(elty,10,10),rand(elty,10,11))
    end
end

@testset "laic1" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        @test_throws DimensionMismatch LAPACK.laic1!(1,rand(elty,10),real(rand(elty)),rand(elty,11),rand(elty))
    end
end

@testset "trsen" begin
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        for job in ('N', 'E', 'V', 'B')
            for c in ('V', 'N')
                A = convert(Matrix{elty}, [7 2 2 1; 1 5 2 0; 0 3 9 4; 1 1 1 4])
                T,Q,d = schur(A)
                s, sep = Base.LinAlg.LAPACK.trsen!(job,c,Array{LinAlg.BlasInt}([0,1,0,0]),T,Q)[4:5]
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
    @testset for elty in (Float32, Float64, Complex64, Complex128)
        for c in ('V', 'N')
            A = convert(Matrix{elty}, [7 2 2 1; 1 5 2 0; 0 3 9 4; 1 1 1 4])
            T,Q,d = schur(A)
            Base.LinAlg.LAPACK.trexc!(c,LinAlg.BlasInt(1),LinAlg.BlasInt(2),T,Q)
            @test d[1] ≈ T[2,2]
            @test d[2] ≈ T[1,1]
            if c == 'V'
                @test Q*T*Q' ≈ A
            end
        end
    end
end

@testset "Julia vs LAPACK" begin
    # Test our own linear algebra functionality against LAPACK
    @testset for elty in (Float32, Float64, Complex{Float32}, Complex{Float64})
        for nn in (5,10,15)
            if elty <: Real
                A = convert(Matrix{elty}, randn(10,nn))
            else
                A = convert(Matrix{elty}, complex.(randn(10,nn),randn(10,nn)))
            end    ## LU (only equal for real because LAPACK uses different absolute value when choosing permutations)
            if elty <: Real
                FJulia  = Base.LinAlg.generic_lufact!(copy(A))
                FLAPACK = Base.LinAlg.LAPACK.getrf!(copy(A))
                @test FJulia.factors ≈ FLAPACK[1]
                @test FJulia.ipiv ≈ FLAPACK[2]
                @test FJulia.info ≈ FLAPACK[3]
            end

            ## QR
            FJulia  = LinAlg.qrfactUnblocked!(copy(A))
            FLAPACK = Base.LinAlg.LAPACK.geqrf!(copy(A))
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
    @test_throws ArgumentError eigfact(A)
end
