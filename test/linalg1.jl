debug = false

import Base.LinAlg
import Base.LinAlg: BlasComplex, BlasFloat, BlasReal

n     = 10
srand(1234321)

a = rand(n,n)
for elty in (Float32, Float64, Complex64, Complex128)
    a = convert(Matrix{elty}, a)
    # cond
    @test_approx_eq_eps cond(a, 1) 4.837320054554436e+02 0.01
    @test_approx_eq_eps cond(a, 2) 1.960057871514615e+02 0.01
    @test_approx_eq_eps cond(a, Inf) 3.757017682707787e+02 0.01
    @test_approx_eq_eps cond(a[:,1:5]) 10.233059337453463 0.01
end

areal = randn(n,n)/2
aimg  = randn(n,n)/2
breal = randn(n,2)/2
bimg  = randn(n,2)/2
for eltya in (Float32, Float64, Complex32, Complex64, Complex128, BigFloat, Int)
    for eltyb in (Float32, Float64, Complex32, Complex64, Complex128, Int)
        a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex(areal, aimg) : areal)
        b = eltyb == Int ? rand(1:5, n, 2) : convert(Matrix{eltyb}, eltyb <: Complex ? complex(breal, bimg) : breal)
        asym = a'+a                  # symmetric indefinite
        apd  = a'*a                 # symmetric positive-definite

        εa = eps(abs(float(one(eltya))))
        εb = eps(abs(float(one(eltyb))))
        ε = max(εa,εb)

debug && println("\ntype of a: ", eltya, " type of b: ", eltyb, "\n")

debug && println("(Automatic) upper Cholesky factor")
    if eltya != BigFloat && eltyb != BigFloat # Note! Need to implement cholesky decomposition in julia
        capd  = factorize(apd)
        r     = capd[:U]
        κ     = cond(apd) #condition number

        #Test error bound on reconstruction of matrix: LAWNS 14, Lemma 2.1
        E = abs(apd - r'*r)
        for i=1:n, j=1:n
            @test E[i,j] <= (n+1)ε/(1-(n+1)ε)*real(sqrt(apd[i,i]*apd[j,j]))
        end

        #Test error bound on linear solver: LAWNS 14, Theorem 2.1
        #This is a surprisingly loose bound...
        x = capd\b
        @test norm(x-apd\b)/norm(x) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ
        @test norm(apd*x-b)/norm(b) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ

        @test_approx_eq apd * inv(capd) eye(n)
        @test norm(a*(capd\(a'*b)) - b)/norm(b) <= ε*κ*n # Ad hoc, revisit
        @test abs((det(capd) - det(apd))/det(capd)) <= ε*κ*n # Ad hoc, but statistically verified, revisit
        @test_approx_eq logdet(capd) log(det(capd)) # logdet is less likely to overflow

debug && println("lower Cholesky factor")
        l = cholfact(apd, :L)[:L] 
        @test_approx_eq l*l' apd
    end

debug && println("pivoted Choleksy decomposition")
    if eltya != BigFloat && eltyb != BigFloat # Note! Need to implement pivoted cholesky decomposition in julia
        cpapd = cholfact(apd, pivot=true)
        @test rank(cpapd) == n
        @test all(diff(diag(real(cpapd.UL))).<=0.) # diagonal should be non-increasing
        @test norm(apd * (cpapd\b) - b)/norm(b) <= ε*κ*n # Ad hoc, revisit
        if isreal(apd)
            @test_approx_eq apd * inv(cpapd) eye(n)
        end
    end

debug && println("(Automatic) Bunch-Kaufman factor of indefinite matrix")
    if eltya != BigFloat && eltyb != BigFloat # Not implemented for BigFloat and I don't think it will.
        bc1 = factorize(asym)
        @test_approx_eq inv(bc1) * asym eye(n)
        @test_approx_eq_eps asym * (bc1\b) b 600ε

debug && println("Bunch-Kaufman factors of a pos-def matrix")
        bc2 = bkfact(apd)
        @test_approx_eq inv(bc2) * apd eye(n)
        @test_approx_eq_eps apd * (bc2\b) b 60000ε
    end

debug && println("(Automatic) Square LU decomposition")
    κ     = cond(a,1)
    lua   = factorize(a)
    l,u,p = lua[:L], lua[:U], lua[:p]
    @test_approx_eq l*u a[p,:]
    @test_approx_eq l[invperm(p),:]*u a
    @test_approx_eq a * inv(lua) eye(n)
    @test norm(a*(lua\b) - b, 1) < ε*κ*n*2 # Two because the right hand side has two columns

debug && println("Thin LU")
    lua   = lufact(a[:,1:5])
    @test_approx_eq lua[:L]*lua[:U] lua[:P]*a[:,1:5]

debug && println("Fat LU")
    lua   = lufact(a[1:5,:])
    @test_approx_eq lua[:L]*lua[:U] lua[:P]*a[1:5,:]

debug && println("QR decomposition (without pivoting)")
    qra   = qrfact(a, pivot=false)
    q,r   = qra[:Q], qra[:R]
    @test_approx_eq q'*full(q, thin=false) eye(n)
    @test_approx_eq q*full(q, thin=false)' eye(n)
    @test_approx_eq q*r a
    @test_approx_eq_eps a*(qra\b) b 3000ε

debug && println("(Automatic) Fat (pivoted) QR decomposition") # Pivoting is only implemented for BlasFloats
    qrpa  = factorize(a[1:5,:])
    q,r = qrpa[:Q], qrpa[:R]
    if isa(qrpa,QRPivoted) p = qrpa[:p] end # Reconsider if pivoted QR gets implemented in julia
    @test_approx_eq q'*full(q, thin=false) eye(5)
    @test_approx_eq q*full(q, thin=false)' eye(5)
    @test_approx_eq q*r isa(qrpa,QRPivoted) ? a[1:5,p] : a[1:5,:]
    @test_approx_eq isa(qrpa, QRPivoted) ? q*r[:,invperm(p)] : q*r a[1:5,:]
    @test_approx_eq_eps a[1:5,:]*(qrpa\b[1:5]) b[1:5] 5000ε

debug && println("(Automatic) Thin (pivoted) QR decomposition") # Pivoting is only implemented for BlasFloats
    qrpa  = factorize(a[:,1:5])
    q,r = qrpa[:Q], qrpa[:R]
    if isa(qrpa, QRPivoted) p = qrpa[:p] end # Reconsider if pivoted QR gets implemented in julia
    @test_approx_eq q'*full(q, thin=false) eye(n)
    @test_approx_eq q*full(q, thin=false)' eye(n)
    @test_approx_eq q*r isa(qrpa, QRPivoted) ? a[:,p] : a[:,1:5]
    @test_approx_eq isa(qrpa, QRPivoted) ? q*r[:,invperm(p)] : q*r a[:,1:5]

debug && println("symmetric eigen-decomposition")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        d,v   = eig(asym)
        @test_approx_eq asym*v[:,1] d[1]*v[:,1]
        @test_approx_eq v*scale(d,v') asym
        @test isequal(eigvals(asym[1]), eigvals(asym[1:1,1:1]))
    end

debug && println("non-symmetric eigen decomposition")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        d,v   = eig(a)
        for i in 1:size(a,2) @test_approx_eq a*v[:,i] d[i]*v[:,i] end
    end

debug && println("symmetric generalized eigenproblem")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        a610 = a[:,6:10]
        f = eigfact(asym[1:5,1:5], a610'a610)
        @test_approx_eq asym[1:5,1:5]*f[:vectors] scale(a610'a610*f[:vectors], f[:values])
        @test_approx_eq f[:values] eigvals(asym[1:5,1:5], a610'a610)
        @test_approx_eq_eps prod(f[:values]) prod(eigvals(asym[1:5,1:5]/(a610'a610))) 200ε
    end
 
debug && println("Non-symmetric generalized eigenproblem")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        f = eigfact(a[1:5,1:5], a[6:10,6:10])
        @test_approx_eq a[1:5,1:5]*f[:vectors] scale(a[6:10,6:10]*f[:vectors], f[:values])
        @test_approx_eq f[:values] eigvals(a[1:5,1:5], a[6:10,6:10])
        @test_approx_eq_eps prod(f[:values]) prod(eigvals(a[1:5,1:5]/a[6:10,6:10])) 50000ε
    end

debug && println("Schur")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        f = schurfact(a)
        @test_approx_eq f[:vectors]*f[:Schur]*f[:vectors]' a
        @test_approx_eq sort(real(f[:values])) sort(real(d))
        @test_approx_eq sort(imag(f[:values])) sort(imag(d))
        @test istriu(f[:Schur]) || iseltype(a,Real)
    end

debug && println("Generalized Schur")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        f = schurfact(a[1:5,1:5], a[6:10,6:10]) 
        @test_approx_eq f[:Q]*f[:S]*f[:Z]' a[1:5,1:5]
        @test_approx_eq f[:Q]*f[:T]*f[:Z]' a[6:10,6:10]
        @test istriu(f[:S]) || iseltype(a,Real)
        @test istriu(f[:T]) || iseltype(a,Real)
    end

debug && println("singular value decomposition")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        usv = svdfact(a)
        @test_approx_eq usv[:U]*scale(usv[:S],usv[:Vt]) a
    end

debug && println("Generalized svd")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        gsvd = svdfact(a,a[1:5,:])
        @test_approx_eq gsvd[:U]*gsvd[:D1]*gsvd[:R]*gsvd[:Q]' a
        @test_approx_eq gsvd[:V]*gsvd[:D2]*gsvd[:R]*gsvd[:Q]' a[1:5,:]
    end

debug && println("Solve square general system of equations")
    κ = cond(a,1)
    x = a \ b
    @test_throws b'\b
    @test_throws b\b'
    @test norm(a*x - b, 1)/norm(b) < ε*κ*n*2 # Ad hoc, revisit!

debug && println("Solve upper triangular system")
    x = triu(a) \ b

    #Test forward error [JIN 5705] if this is not a BigFloat
    γ = n*ε/(1-n*ε)
    if eltya != BigFloat
        bigA = big(triu(a))
        ̂x = bigA \ b
        for i=1:size(b, 2)
            @test norm(̂x[:,i]-x[:,i], Inf)/norm(x[:,i], Inf) <= abs(condskeel(bigA, x[:,i])*γ/(1-condskeel(bigA)*γ))
        end
    end
    #Test backward error [JIN 5705]
    for i=1:size(b, 2)
        @test norm(abs(b[:,i] - triu(a)*x[:,i]), Inf) <= γ * norm(triu(a), Inf) * norm(x[:,i], Inf)
    end

debug && println("Solve lower triangular system")
    x = tril(a)\b

    #Test forward error [JIN 5705] if this is not a BigFloat
    γ = n*ε/(1-n*ε)
    if eltya != BigFloat
        bigA = big(tril(a))
        ̂x = bigA \ b
        for i=1:size(b, 2)
            @test norm(̂x[:,i]-x[:,i], Inf)/norm(x[:,i], Inf) <= abs(condskeel(bigA, x[:,i])*γ/(1-condskeel(bigA)*γ))
        end
    end
    #Test backward error [JIN 5705]
    for i=1:size(b, 2)
        @test norm(abs(b[:,i] - tril(a)*x[:,i]), Inf) <= γ * norm(tril(a), Inf) * norm(x[:,i], Inf)
    end

debug && println("Test null")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        a15null = null(a[:,1:5]')
        @test rank([a[:,1:5] a15null]) == 10
        @test_approx_eq_eps norm(a[:,1:5]'a15null, Inf) zero(eltya) 300ε
        @test_approx_eq_eps norm(a15null'a[:,1:5], Inf) zero(eltya) 400ε
        @test size(null(b), 2) == 0
    end

debug && println("Test pinv")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        pinva15 = pinv(a[:,1:5])
        @test_approx_eq a[:,1:5]*pinva15*a[:,1:5] a[:,1:5]
        @test_approx_eq pinva15*a[:,1:5]*pinva15 pinva15
    end

    # if isreal(a)
debug && println("Matrix square root")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        asq = sqrtm(a)
        @test_approx_eq asq*asq a
        asymsq = sqrtm(asym)
        @test_approx_eq asymsq*asymsq asym
    end
end
end

## Least squares solutions
a = [ones(20) 1:20 1:20]
b = reshape(eye(8, 5), 20, 2)
for elty in (Float32, Float64, Complex64, Complex128)
    a = convert(Matrix{elty}, a)
    b = convert(Matrix{elty}, b)

    # Vector rhs
    x = a[:,1:2]\b[:,1]
    @test_approx_eq ((a[:,1:2]*x-b[:,1])'*(a[:,1:2]*x-b[:,1]))[1] convert(elty, 2.546616541353384)

    # Matrix rhs
    x = a[:,1:2]\b
    @test_approx_eq det((a[:,1:2]*x-b)'*(a[:,1:2]*x-b)) convert(elty, 4.437969924812031)

    # Rank deficient
    x = a\b
    @test_approx_eq det((a*x-b)'*(a*x-b)) convert(elty, 4.437969924812031)

    # Underdetermined minimum norm
    x = convert(Matrix{elty}, [1 0 0; 0 1 -1]) \ convert(Vector{elty}, [1,1])
    @test_approx_eq x convert(Vector{elty}, [1, 0.5, -0.5])

    # symmetric, positive definite
    @test_approx_eq inv(convert(Matrix{elty}, [6. 2; 2 1])) convert(Matrix{elty}, [0.5 -1; -1 3])

    # symmetric, indefinite
    @test_approx_eq inv(convert(Matrix{elty}, [1. 2; 2 1])) convert(Matrix{elty}, [-1. 2; 2 -1]/3)
end

## Test Julia fallbacks to BLAS routines

# matrices with zero dimensions
@test ones(0,5)*ones(5,3) == zeros(0,3)
@test ones(3,5)*ones(5,0) == zeros(3,0)
@test ones(3,0)*ones(0,4) == zeros(3,4)
@test ones(0,5)*ones(5,0) == zeros(0,0)
@test ones(0,0)*ones(0,4) == zeros(0,4)
@test ones(3,0)*ones(0,0) == zeros(3,0)
@test ones(0,0)*ones(0,0) == zeros(0,0)

# 2x2
A = [1 2; 3 4]
B = [5 6; 7 8]
@test A*B == [19 22; 43 50]
@test At_mul_B(A, B) == [26 30; 38 44]
@test A_mul_Bt(A, B) == [17 23; 39 53]
@test At_mul_Bt(A, B) == [23 31; 34 46]
Ai = A+(0.5*im).*B
Bi = B+(2.5*im).*A[[2,1],[2,1]]
@test Ai*Bi == [-21+53.5im -4.25+51.5im; -12+95.5im 13.75+85.5im]
@test Ac_mul_B(Ai, Bi) == [68.5-12im 57.5-28im; 88-3im 76.5-25im]
@test A_mul_Bc(Ai, Bi) == [64.5+5.5im 43+31.5im; 104-18.5im 80.5+31.5im]
@test Ac_mul_Bc(Ai, Bi) == [-28.25-66im 9.75-58im; -26-89im 21-73im]

# 3x3
A = [1 2 3; 4 5 6; 7 8 9].-5
B = [1 0 5; 6 -10 3; 2 -4 -1]
@test A*B == [-26 38 -27; 1 -4 -6; 28 -46 15]
@test Ac_mul_B(A, B) == [-6 2 -25; 3 -12 -18; 12 -26 -11]
@test A_mul_Bc(A, B) == [-14 0 6; 4 -3 -3; 22 -6 -12]
@test Ac_mul_Bc(A, B) == [6 -8 -6; 12 -9 -9; 18 -10 -12]
Ai = A+(0.5*im).*B
Bi = B+(2.5*im).*A[[2,1,3],[2,3,1]]
@test Ai*Bi == [-44.75+13im 11.75-25im -38.25+30im; -47.75-16.5im -51.5+51.5im -56+6im; 16.75-4.5im -53.5+52im -15.5im]
@test Ac_mul_B(Ai, Bi) == [-21+2im -1.75+49im -51.25+19.5im; 25.5+56.5im -7-35.5im 22+35.5im; -3+12im -32.25+43im -34.75-2.5im]
@test A_mul_Bc(Ai, Bi) == [-20.25+15.5im -28.75-54.5im 22.25+68.5im; -12.25+13im -15.5+75im -23+27im; 18.25+im 1.5+94.5im -27-54.5im]
@test Ac_mul_Bc(Ai, Bi) == [1+2im 20.75+9im -44.75+42im; 19.5+17.5im -54-36.5im 51-14.5im; 13+7.5im 11.25+31.5im -43.25-14.5im]

# Generic integer matrix multiplication
A = [1 2 3; 4 5 6] .- 3
B = [2 -2; 3 -5; -4 7]
@test A*B == [-7 9; -4 9]
@test At_mul_Bt(A, B) == [-6 -11 15; -6 -13 18; -6 -15 21]
A = ones(Int, 2, 100)
B = ones(Int, 100, 3)
@test A*B == [100 100 100; 100 100 100]
A = rand(1:20, 5, 5) .- 10
B = rand(1:20, 5, 5) .- 10
@test At_mul_B(A, B) == A'*B
@test A_mul_Bt(A, B) == A*B'
 
# Preallocated
C = Array(Int, size(A, 1), size(B, 2))
@test A_mul_B!(C, A, B) == A*B
@test At_mul_B!(C, A, B) == A'*B
@test A_mul_Bt!(C, A, B) == A*B'
@test At_mul_Bt!(C, A, B) == A'*B'

# matrix algebra with subarrays of floats (stride != 1)
A = reshape(float64(1:20),5,4)
Aref = A[1:2:end,1:2:end]
Asub = sub(A, 1:2:5, 1:2:4)
b = [1.2,-2.5]
@test (Aref*b) == (Asub*b)
@test At_mul_B(Asub, Asub) == At_mul_B(Aref, Aref)
@test A_mul_Bt(Asub, Asub) == A_mul_Bt(Aref, Aref)
Ai = A .+ im
Aref = Ai[1:2:end,1:2:end]
Asub = sub(Ai, 1:2:5, 1:2:4)
@test Ac_mul_B(Asub, Asub) == Ac_mul_B(Aref, Aref)
@test A_mul_Bc(Asub, Asub) == A_mul_Bc(Aref, Aref)

# syrk & herk
A = reshape(1:1503, 501, 3).-750.0
res = float64([135228751 9979252 -115270247; 9979252 10481254 10983256; -115270247 10983256 137236759])
@test At_mul_B(A, A) == res
@test A_mul_Bt(A',A') == res
cutoff = 501
A = reshape(1:6*cutoff,2*cutoff,3).-(6*cutoff)/2
Asub = sub(A, 1:2:2*cutoff, 1:3)
Aref = A[1:2:2*cutoff, 1:3]
@test At_mul_B(Asub, Asub) == At_mul_B(Aref, Aref)
Ai = A .- im
Asub = sub(Ai, 1:2:2*cutoff, 1:3)
Aref = Ai[1:2:2*cutoff, 1:3]
@test Ac_mul_B(Asub, Asub) == Ac_mul_B(Aref, Aref)

# Matrix exponential
for elty in (Float32, Float64, Complex64, Complex128)
        A1  = convert(Matrix{elty}, [4 2 0; 1 4 1; 1 1 4])
        eA1 = convert(Matrix{elty}, [147.866622446369 127.781085523181  127.781085523182;
        183.765138646367 183.765138646366  163.679601723179;
        71.797032399996  91.8825693231832 111.968106246371]')
        @test_approx_eq expm(A1) eA1

        A2  = convert(Matrix{elty}, 
            [29.87942128909879    0.7815750847907159 -2.289519314033932;
            0.7815750847907159 25.72656945571064    8.680737820540137;
            -2.289519314033932   8.680737820540137  34.39400925519054])
        eA2 = convert(Matrix{elty},
            [  5496313853692458.0 -18231880972009236.0 -30475770808580460.0;
             -18231880972009252.0  60605228702221920.0 101291842930249760.0;
             -30475770808580480.0 101291842930249728.0 169294411240851968.0])
        @test_approx_eq expm(A2) eA2

        A3  = convert(Matrix{elty}, [-131 19 18;-390 56 54;-387 57 52])
        eA3 = convert(Matrix{elty}, [-1.50964415879218 -5.6325707998812  -4.934938326092;
        0.367879439109187 1.47151775849686  1.10363831732856;
        0.135335281175235 0.406005843524598 0.541341126763207]')
        @test_approx_eq expm(A3) eA3

        # issue 5116
        A4  = [0 10 0 0; -1 0 0 0; 0 0 0 0; -2 0 0 0]
        eA4 = [-0.999786072879326  -0.065407069689389   0.0   0.0
                0.006540706968939  -0.999786072879326   0.0   0.0
                0.0                 0.0                 1.0   0.0
                0.013081413937878  -3.999572145758650   0.0   1.0]
        @test_approx_eq expm(A4) eA4

        # issue 5116
        A5  = [ 0. 0. 0. 0. ; 0. 0. -im 0.; 0. im 0. 0.; 0. 0. 0. 0.]
        eA5 = [ 1.0+0.0im   0.0+0.0im                 0.0+0.0im                0.0+0.0im
                0.0+0.0im   1.543080634815244+0.0im   0.0-1.175201193643801im  0.0+0.0im
                0.0+0.0im   0.0+1.175201193643801im   1.543080634815243+0.0im  0.0+0.0im
                0.0+0.0im   0.0+0.0im                 0.0+0.0im                1.0+0.0im]
        @test_approx_eq expm(A5) eA5

        # Hessenberg
        @test_approx_eq hessfact(A1)[:H] convert(Matrix{elty}, 
                        [4.000000000000000  -1.414213562373094  -1.414213562373095
                        -1.414213562373095   4.999999999999996  -0.000000000000000
                                         0  -0.000000000000002   3.000000000000000])
end

# Hermitian matrix exponential
A1 = randn(4,4) + im*randn(4,4)
A2 = A1 + A1'
@test_approx_eq expm(A2) expm(Hermitian(A2))

# matmul for types w/o sizeof (issue #1282)
A = Array(Complex{Int},10,10)
A[:] = complex(1,1)
A2 = A^2
@test A2[1,1] == 20im

# test sparse matrix norms
Ac = sprandn(10,10,.1) + im* sprandn(10,10,.1)
Ar = sprandn(10,10,.1)
Ai = int(ceil(Ar*100))
@test_approx_eq norm(Ac,1)     norm(full(Ac),1)
@test_approx_eq norm(Ac,Inf)   norm(full(Ac),Inf)
@test_approx_eq vecnorm(Ac)    vecnorm(full(Ac))
@test_approx_eq norm(Ar,1)     norm(full(Ar),1)
@test_approx_eq norm(Ar,Inf)   norm(full(Ar),Inf)
@test_approx_eq vecnorm(Ar)    vecnorm(full(Ar))
@test_approx_eq norm(Ai,1)     norm(full(Ai),1)
@test_approx_eq norm(Ai,Inf)   norm(full(Ai),Inf)
@test_approx_eq vecnorm(Ai)    vecnorm(full(Ai))

# scale real matrix by complex type
@test_throws scale!([1.0], 2.0im)
@test isequal(scale([1.0], 2.0im),             Complex{Float64}[2.0im])
@test isequal(scale(Float32[1.0], 2.0f0im),    Complex{Float32}[2.0im])
@test isequal(scale(Float32[1.0], 2.0im),      Complex{Float64}[2.0im])
@test isequal(scale(Float64[1.0], 2.0f0im),    Complex{Float64}[2.0im])
@test isequal(scale(Float32[1.0], big(2.0)im), Complex{BigFloat}[2.0im])
@test isequal(scale(Float64[1.0], big(2.0)im), Complex{BigFloat}[2.0im])
@test isequal(scale(BigFloat[1.0], 2.0im),     Complex{BigFloat}[2.0im])
@test isequal(scale(BigFloat[1.0], 2.0f0im),   Complex{BigFloat}[2.0im])
