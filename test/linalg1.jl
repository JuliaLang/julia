debug = false

import Base.LinAlg: BlasComplex, BlasFloat, BlasReal, QRPivoted

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
for eltya in (Float32, Float64, Complex64, Complex128, BigFloat, Int)
    for eltyb in (Float32, Float64, Complex64, Complex128, Int)
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
        @test_approx_eq v*Diagonal(d)*v' asym
        @test isequal(eigvals(asym[1]), eigvals(asym[1:1,1:1]))
        @test_approx_eq abs(eigfact(Hermitian(asym), 1:2)[:vectors]'v[:,1:2]) eye(eltya, 2)
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
    @test_throws DimensionMismatch b'\b
    @test_throws DimensionMismatch b\b'
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
