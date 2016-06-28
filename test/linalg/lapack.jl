# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "lapack" begin
import Base.LinAlg.BlasInt

@test_throws ArgumentError Base.LinAlg.LAPACK.chkside('Z')
@test_throws ArgumentError Base.LinAlg.LAPACK.chkdiag('Z')
@test_throws ArgumentError Base.LinAlg.LAPACK.chktrans('Z')

let # syevr
    srand(123)
    Ainit = randn(5,5)
    for elty in (Float32, Float64, Complex64, Complex128)
        if elty == Complex64 || elty == Complex128
            A = complex(Ainit, Ainit)
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

let # Test gglse
    for elty in (Float32, Float64, Complex64, Complex128)
        A = convert(Array{elty, 2}, [1 1 1 1; 1 3 1 1; 1 -1 3 1; 1 1 1 3; 1 1 1 -1])
        c = convert(Array{elty, 1}, [2, 1, 6, 3, 1])
        B = convert(Array{elty, 2}, [1 1 1 -1; 1 -1 1 1; 1 1 -1 1])
        d = convert(Array{elty, 1}, [1, 3, -1])
        @test LAPACK.gglse!(A, c, B, d)[1] ≈ convert(Array{elty}, [0.5, -0.5, 1.5, 0.5])
    end
end

let # gebrd, bdsqr & throw for bdsdc
    n = 10
    for elty in (Float32, Float64)
        d, e = convert(Vector{elty}, randn(n)), convert(Vector{elty}, randn(n - 1))
        U, Vt, C = eye(elty, n), eye(elty, n), eye(elty, n)
        s, _ = LAPACK.bdsqr!('U', copy(d), copy(e), Vt, U, C)
        @test full(Bidiagonal(d, e, true)) ≈ U*Diagonal(s)*Vt

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

let # Issue #7886
    x, r = LAPACK.gelsy!([0 1; 0 2; 0 3.], [2, 4, 6.])
    @test x ≈ [0,2]
    @test r == 1
end

#geqrt(3)
for elty in (Float32, Float64, Complex64, Complex128)
    A = rand(elty,10,10)
    B = copy(A)
    C,T = LAPACK.geqrt!(A,zeros(elty,10,10))
    D,S = LAPACK.geqrt3!(A,zeros(elty,10,10))
    @test C ≈ D
end

#gbtrf and gbtrs
for elty in (Float32, Float64, Complex64, Complex128)
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

#geqp3, geqrt, error handling!
for elty in (Float32, Float64, Complex64, Complex128)
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

#gels, gesv, getrs, getri error handling!
for elty in (Float32, Float64, Complex64, Complex128)
    A = rand(elty,10,10)
    B = rand(elty,11,11)
    @test_throws DimensionMismatch LAPACK.gels!('N',A,B)
    @test_throws DimensionMismatch LAPACK.gels!('T',A,B)
    @test_throws DimensionMismatch LAPACK.gesv!(A,B)
    @test_throws DimensionMismatch LAPACK.getrs!('N',A,ones(Base.LinAlg.BlasInt,10),B)
    @test_throws DimensionMismatch LAPACK.getrs!('T',A,ones(Base.LinAlg.BlasInt,10),B)
    @test_throws DimensionMismatch LAPACK.getri!(A,ones(Base.LinAlg.BlasInt,11))
end

#gelsy,gelsd
for elty in (Float32, Float64, Complex64, Complex128)
    A = rand(elty,10,10)
    B = rand(elty,10,10)
    C, j = LAPACK.gelsd!(copy(A),copy(B))
    D, k = LAPACK.gelsy!(copy(A),copy(B))
    @test C ≈ D
    @test_throws DimensionMismatch LAPACK.gelsd!(A,rand(elty,12,10))
    @test_throws DimensionMismatch LAPACK.gelsy!(A,rand(elty,12,10))
end

#gglse errors
for elty in (Float32, Float64, Complex64, Complex128)
    A = rand(elty,10,10)
    @test_throws DimensionMismatch LAPACK.gglse!(A,zeros(elty,10),rand(elty,12,11),zeros(elty,12))
    @test_throws DimensionMismatch LAPACK.gglse!(A,zeros(elty,11),rand(elty,10,10),zeros(elty,10))
    @test_throws DimensionMismatch LAPACK.gglse!(A,zeros(elty,10),rand(elty,10,10),zeros(elty,11))
end

#gesvd, ggsvd
for elty in (Float32, Float64, Complex64, Complex128)
    A = rand(elty,10,5)
    U,S,V = svd(A)
    lU,lS,lVt = LAPACK.gesvd!('S','S',A)
    @test U ≈ lU
    @test S ≈ lS
    @test V' ≈ lVt
    B = rand(elty,10,10)
    # xggsvd3 replaced xggsvd in LAPACK 3.6.0
    if LAPACK.laver() < (3, 6, 0)
        @test_throws DimensionMismatch LAPACK.ggsvd!('S','S','S',A,B)
    else
        @test_throws DimensionMismatch LAPACK.ggsvd3!('S','S','S',A,B)
    end
end

#geevx, ggev errors
for elty in (Float32, Float64, Complex64, Complex128)
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

#gebal/gebak
for elty in (Float32, Float64, Complex64, Complex128)
    A = rand(elty,10,10) * Diagonal(exp10(linspace(-10,10,10)))
    B = copy(A)
    ilo, ihi, scale = LAPACK.gebal!('S',B)
    Bvs = eigvecs(B)
    Avs = eigvecs(A)
    Bvs = LAPACK.gebak!('S','R',ilo,ihi,scale,Bvs)
    @test norm(diff(Avs ./ Bvs)) < 100 * eps(abs(float(one(elty))))
end

#gels
for elty in (Float32, Float64, Complex64, Complex128)
    A = rand(elty,10,10)
    X = rand(elty,10)
    B,Y,z = LAPACK.gels!('N',copy(A),copy(X))
    @test A\X ≈ Y
end

#getrf/getri
for elty in (Float32, Float64, Complex64, Complex128)
    A = rand(elty,10,10)
    iA = inv(A)
    A, ipiv = LAPACK.getrf!(A)
    A = LAPACK.getri!(A, ipiv)
    @test A ≈ iA
end

#geev - complex is easier for now
for elty in (Complex64, Complex128)
    A = rand(elty,10,10)
    Aw, Avl, Avr = LAPACK.geev!('N','V',copy(A))
    fA = eigfact(A)
    @test fA[:values] ≈ Aw
    @test fA[:vectors] ≈ Avr
end

#gtsv
for elty in (Float32, Float64, Complex64, Complex128)
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

#gttrs,gttrf errors
for elty in (Float32, Float64, Complex64, Complex128)
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

#orglq and friends errors
for elty in (Float32, Float64, Complex64, Complex128)
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

#sytri, sytrs, and sytrf
for elty in (Float32, Float64, Complex64, Complex128)
    A = rand(elty,10,10)
    A = A + A.' #symmetric!
    B = copy(A)
    B,ipiv = LAPACK.sytrf!('U',B)
    @test triu(inv(A)) ≈ triu(LAPACK.sytri!('U',B,ipiv))
    @test_throws DimensionMismatch LAPACK.sytrs!('U',B,ipiv,rand(elty,11,5))
    @test LAPACK.sytrf!('U',zeros(elty,0,0)) == (zeros(elty,0,0),zeros(BlasInt,0))
end

for elty in (Float32, Float64, Complex64, Complex128)
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

# hetrs
for elty in (Complex64, Complex128)
    A = rand(elty,10,10)
    A = A + A' #hermitian!
    B = copy(A)
    B,ipiv = LAPACK.hetrf!('U',B)
    @test_throws DimensionMismatch LAPACK.hetrs!('U',B,ipiv,rand(elty,11,5))
    @test_throws DimensionMismatch LAPACK.hetrs_rook!('U',B,ipiv,rand(elty,11,5))
end

# stev, stebz, stein, stegr
for elty in (Float32, Float64)
    d = rand(elty,10)
    e = rand(elty,9)
    @test_throws DimensionMismatch LAPACK.stev!('U',d,rand(elty,10))
    @test_throws DimensionMismatch LAPACK.stebz!('A','B',zero(elty),zero(elty),0,0,-1.,d,rand(elty,10))
    @test_throws DimensionMismatch LAPACK.stegr!('N','A',d,rand(elty,10),zero(elty),zero(elty),0,0)
    @test_throws DimensionMismatch LAPACK.stein!(d,zeros(elty,10),zeros(d),zeros(BlasInt,10),zeros(BlasInt,10))
    @test_throws DimensionMismatch LAPACK.stein!(d,e,zeros(elty,11),zeros(BlasInt,10),zeros(BlasInt,10))
end

#trtri & trtrs
for elty in (Float32, Float64, Complex64, Complex128)
    A = rand(elty,10,10)
    A = triu(A)
    B = copy(A)
    @test inv(A) ≈ LAPACK.trtri!('U','N',B)
    @test_throws DimensionMismatch LAPACK.trtrs!('U','N','N',B,zeros(elty,11,10))
end

#tgsen, tzrzf, & trsyl
for elty in (Float32, Float64, Complex64, Complex128)
    Z = zeros(elty,10,10)
    @test_throws DimensionMismatch LAPACK.tgsen!(zeros(BlasInt,10),Z,zeros(elty,11,11),Z,Z)
    @test_throws DimensionMismatch LAPACK.tgsen!(zeros(BlasInt,10),Z,Z,zeros(elty,11,11),Z)
    @test_throws DimensionMismatch LAPACK.tgsen!(zeros(BlasInt,10),Z,Z,Z,zeros(elty,11,11))
    @test_throws DimensionMismatch LAPACK.trsyl!('N','N',Z,Z,zeros(elty,11,11))
    @test_throws DimensionMismatch LAPACK.tzrzf!(zeros(elty,10,5))
end

#sysv
for elty in (Float32, Float64, Complex64, Complex128)
    A = rand(elty,10,10)
    A = A + A.' #symmetric!
    b = rand(elty,10)
    c = A \ b
    b,A = LAPACK.sysv!('U',A,b)
    @test b ≈ c
    @test_throws DimensionMismatch LAPACK.sysv!('U',A,rand(elty,11))
end

#hesv
for elty in (Complex64, Complex128)
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

#ptsv
for elty in (Float32, Float64, Complex64, Complex128)
    dv = real(ones(elty,10))
    ev = zeros(elty,9)
    A = SymTridiagonal(dv,ev)
    if elty <: Complex
        A = Tridiagonal(conj(ev),dv,ev)
    end
    B = rand(elty,10,10)
    C = copy(B)
    @test A\B ≈ LAPACK.ptsv!(dv,ev,C)
    @test_throws DimensionMismatch LAPACK.ptsv!(dv,ones(elty,10),C)
    @test_throws DimensionMismatch LAPACK.ptsv!(dv,ev,ones(elty,11,11))
end

#pttrf and pttrs
for elty in (Float32, Float64, Complex64, Complex128)
    dv = real(ones(elty,10))
    ev = zeros(elty,9)
    A = SymTridiagonal(dv,ev)
    if elty <: Complex
        A = Tridiagonal(conj(ev),dv,ev)
    end
    dv,ev = LAPACK.pttrf!(dv,ev)
    @test_throws DimensionMismatch LAPACK.pttrf!(dv,ones(elty,10))
    B = rand(elty,10,10)
    C = copy(B)
    if elty <: Complex
        @test A\B ≈ LAPACK.pttrs!('U',dv,ev,C)
        @test_throws DimensionMismatch LAPACK.pttrs!('U',dv,ones(elty,10),C)
        @test_throws DimensionMismatch LAPACK.pttrs!('U',dv,ev,rand(elty,11,11))
    else
        @test A\B ≈ LAPACK.pttrs!(dv,ev,C)
        @test_throws DimensionMismatch LAPACK.pttrs!(dv,ones(elty,10),C)
        @test_throws DimensionMismatch LAPACK.pttrs!(dv,ev,rand(elty,11,11))
    end
end

#posv and some errors for friends
for elty in (Float32, Float64, Complex64, Complex128)
    A = 0.01*rand(elty,10,10)
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

#gesvx
for elty in (Float32, Float64, Complex64, Complex128)
    A = rand(elty,10,10)
    B = rand(elty,10,5)
    C = copy(A)
    D = copy(B)
    X, rcond, f, b, r = LAPACK.gesvx!(C,D)
    @test X ≈ A\B
end

#gees, gges error throwing
for elty in (Float32, Float64, Complex64, Complex128)
    A = rand(elty,10,10)
    B = rand(elty,11,11)
    @test_throws DimensionMismatch LAPACK.gges!('V','V',A,B)
end

# trrfs & trevc
for elty in (Float32, Float64, Complex64, Complex128)
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

# laic1
for elty in (Float32, Float64, Complex64, Complex128)
    @test_throws DimensionMismatch LAPACK.laic1!(1,rand(elty,10),real(rand(elty)),rand(elty,11),rand(elty))
end

#trexc
for elty in (Float32, Float64, Complex64, Complex128)
    for c in ('V', 'N')
        A = convert(Matrix{elty}, [7 2 2 1; 1 5 2 0; 0 3 9 4; 1 1 1 4])
        T,Q,d = schur(A)
        Base.LinAlg.LAPACK.trsen!('N',c,Array{LinAlg.BlasInt}([0,1,0,0]),T,Q)
        @test d[1] ≈ T[2,2]
        @test d[2] ≈ T[1,1]
        if c == 'V'
            @test  Q*T*Q' ≈ A
        end
    end
end

#trexc and trsen
for elty in (Float32, Float64, Complex64, Complex128)
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

# Test our own linear algebra functionaly against LAPACK
for elty in (Float32, Float64, Complex{Float32}, Complex{Float64})
    for nn in (5,10,15)
        if elty <: Real
            A = convert(Matrix{elty}, randn(10,nn))
        else
            A = convert(Matrix{elty}, complex(randn(10,nn),randn(10,nn)))
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

# Issue 13976
let A = [NaN 0.0 NaN; 0 0 0; NaN 0 NaN]
    @test_throws ArgumentError expm(A)
end

# Issue 14065 (and 14220)
let A = [NaN NaN; NaN NaN]
    @test_throws ArgumentError eigfact(A)
end
end
