# This file is a part of Julia. License is MIT: http://julialang.org/license

debug = false
using Base.Test

using Base.LinAlg: BlasComplex, BlasFloat, BlasReal, QRPivoted

n = 10

# Split n into 2 parts for tests needing two matrices
n1 = div(n, 2)
n2 = 2*n1

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
a2real = randn(n,n)/2
a2img  = randn(n,n)/2
breal = randn(n,2)/2
bimg  = randn(n,2)/2

for eltya in (Float32, Float64, Complex64, Complex128, BigFloat, Int)
    a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex(areal, aimg) : areal)
    a2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex(a2real, a2img) : a2real)
    asym = a'+a                  # symmetric indefinite
    apd  = a'*a                 # symmetric positive-definite
    ε = εa = eps(abs(float(one(eltya))))

    for eltyb in (Float32, Float64, Complex64, Complex128, Int)
        b = eltyb == Int ? rand(1:5, n, 2) : convert(Matrix{eltyb}, eltyb <: Complex ? complex(breal, bimg) : breal)
        εb = eps(abs(float(one(eltyb))))
        ε = max(εa,εb)

debug && println("\ntype of a: ", eltya, " type of b: ", eltyb, "\n")

debug && println("(Automatic) Bunch-Kaufman factor of indefinite matrix")
    if eltya != BigFloat && eltyb != BigFloat # Not implemented for BigFloat and I don't think it will.
        bc1 = factorize(asym)
        @test_approx_eq inv(bc1) * asym eye(n)
        @test_approx_eq_eps asym * (bc1\b) b 1000ε

debug && println("Bunch-Kaufman factors of a pos-def matrix")
        bc2 = bkfact(apd)
        @test_approx_eq inv(bc2) * apd eye(n)
        @test_approx_eq_eps apd * (bc2\b) b 150000ε
    end

debug && println("QR decomposition (without pivoting)")
    for i = 1:2
        let a = i == 1 ? a : sub(a, 1:n - 1, 1:n - 1), b = i == 1 ? b : sub(b, 1:n - 1), n = i == 1 ? n : n - 1
            qra   = @inferred qrfact(a)
            @inferred qr(a)
            q, r  = qra[:Q], qra[:R]
            @test_throws KeyError qra[:Z]
            @test_approx_eq q'*full(q, thin = false) eye(n)
            @test_approx_eq q*full(q, thin = false)' eye(n)
            @test_approx_eq q*r a
            @test_approx_eq_eps a*(qra\b) b 3000ε
            @test_approx_eq full(qra) a

debug && println("Thin QR decomposition (without pivoting)")
            qra   = @inferred qrfact(a[:,1:n1], Val{false})
            @inferred qr(a[:,1:n1], Val{false})
            q,r   = qra[:Q], qra[:R]
            @test_throws KeyError qra[:Z]
            @test_approx_eq q'*full(q, thin=false) eye(n)
            @test_approx_eq q'*full(q) eye(n, n1)
            @test_approx_eq q*r a[:,1:n1]
            @test_approx_eq_eps q*b[1:n1] full(q)*b[1:n1] 100ε
            @test_approx_eq_eps q*b full(q, thin=false)*b 100ε
            @test_throws DimensionMismatch q*b[1:n1 + 1]

debug && println("(Automatic) Fat (pivoted) QR decomposition") # Pivoting is only implemented for BlasFloats
            if eltya <: BlasFloat
                @inferred qrfact(a, Val{true})
                @inferred qr(a, Val{true})
            end
            qrpa  = factorize(a[1:n1,:])
            q,r = qrpa[:Q], qrpa[:R]
            @test_throws KeyError qrpa[:Z]
            if isa(qrpa,QRPivoted) p = qrpa[:p] end # Reconsider if pivoted QR gets implemented in julia
            @test_approx_eq q'*full(q, thin=false) eye(n1)
            @test_approx_eq q*full(q, thin=false)' eye(n1)
            @test_approx_eq q*r isa(qrpa,QRPivoted) ? a[1:n1,p] : a[1:n1,:]
            @test_approx_eq isa(qrpa, QRPivoted) ? q*r[:,invperm(p)] : q*r a[1:n1,:]
            @test_approx_eq_eps a[1:n1,:]*(qrpa\b[1:n1]) b[1:n1] 5000ε
            @test_approx_eq full(qrpa) a[1:5,:]

debug && println("(Automatic) Thin (pivoted) QR decomposition") # Pivoting is only implemented for BlasFloats
            qrpa  = factorize(a[:,1:n1])
            q,r = qrpa[:Q], qrpa[:R]
            @test_throws KeyError qrpa[:Z]
            if isa(qrpa, QRPivoted) p = qrpa[:p] end # Reconsider if pivoted QR gets implemented in julia
            @test_approx_eq q'*full(q, thin=false) eye(n)
            @test_approx_eq q*full(q, thin=false)' eye(n)
            @test_approx_eq q*r isa(qrpa, QRPivoted) ? a[:,p] : a[:,1:n1]
            @test_approx_eq isa(qrpa, QRPivoted) ? q*r[:,invperm(p)] : q*r a[:,1:n1]
            @test_approx_eq full(qrpa) a[:,1:5]
        end
    end

debug && println("Matmul with QR factorizations")
    if eltya != Int
        qrpa  = factorize(a[:,1:n1])
        q,r = qrpa[:Q], qrpa[:R]
        @test_approx_eq A_mul_B!(full(q,thin=false)',q) eye(n)
        @test_throws DimensionMismatch A_mul_B!(eye(eltya,n+1),q)
        @test_approx_eq A_mul_Bc!(full(q,thin=false),q) eye(n)
        @test_throws DimensionMismatch A_mul_Bc!(eye(eltya,n+1),q)
        @test_throws BoundsError size(q,-1)

        qra   = qrfact(a[:,1:n1], Val{false})
        q,r   = qra[:Q], qra[:R]
        @test_approx_eq A_mul_B!(full(q,thin=false)',q) eye(n)
        @test_throws DimensionMismatch A_mul_B!(eye(eltya,n+1),q)
        @test_approx_eq A_mul_Bc!(full(q,thin=false),q) eye(n)
        @test_throws DimensionMismatch A_mul_Bc!(eye(eltya,n+1),q)
        @test_throws BoundsError size(q,-1)

        @test_throws DimensionMismatch q * eye(Int8,n+4)
    end

debug && println("non-symmetric eigen decomposition")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        d,v   = eig(a)
        for i in 1:size(a,2) @test_approx_eq a*v[:,i] d[i]*v[:,i] end
    end

debug && println("symmetric generalized eigenproblem")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        asym_sg = asym[1:n1, 1:n1]
        a_sg = a[:,n1+1:n2]
        f = eigfact(asym_sg, a_sg'a_sg)
        eig(asym_sg, a_sg'a_sg) # same result, but checks that method works
        @test_approx_eq asym_sg*f[:vectors] scale(a_sg'a_sg*f[:vectors], f[:values])
        @test_approx_eq f[:values] eigvals(asym_sg, a_sg'a_sg)
        @test_approx_eq_eps prod(f[:values]) prod(eigvals(asym_sg/(a_sg'a_sg))) 200ε
        @test eigvecs(asym_sg, a_sg'a_sg) == f[:vectors]
        @test_throws KeyError f[:Z]
    end

debug && println("Non-symmetric generalized eigenproblem")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        a1_nsg = a[1:n1, 1:n1]
        a2_nsg = a[n1+1:n2, n1+1:n2]
        f = eigfact(a1_nsg, a2_nsg)
        eig(a1_nsg, a2_nsg) # same result, but checks that method works
        @test_approx_eq a1_nsg*f[:vectors] scale(a2_nsg*f[:vectors], f[:values])
        @test_approx_eq f[:values] eigvals(a1_nsg, a2_nsg)
        @test_approx_eq_eps prod(f[:values]) prod(eigvals(a1_nsg/a2_nsg)) 50000ε
        @test eigvecs(a1_nsg, a2_nsg) == f[:vectors]
        @test_throws KeyError f[:Z]
    end

debug && println("Schur")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        f = schurfact(a)
        @test_approx_eq f[:vectors]*f[:Schur]*f[:vectors]' a
        @test_approx_eq sort(real(f[:values])) sort(real(d))
        @test_approx_eq sort(imag(f[:values])) sort(imag(d))
        @test istriu(f[:Schur]) || iseltype(a,Real)
        @test_approx_eq full(f) a
        @test_throws KeyError f[:A]
    end

debug && println("Reorder Schur")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        # use asym for real schur to enforce tridiag structure
        # avoiding partly selection of conj. eigenvalues
        ordschura = eltya <: Complex ? a : asym
        S = schurfact(ordschura)
        select = bitrand(n)
        O = ordschur(S, select)
        sum(select) != 0 && @test_approx_eq S[:values][find(select)] O[:values][1:sum(select)]
        @test_approx_eq O[:vectors]*O[:Schur]*O[:vectors]' ordschura
        @test_throws KeyError f[:A]
    end

debug && println("Generalized Schur")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        a1_sf = a[1:n1, 1:n1]
        a2_sf = a[n1+1:n2, n1+1:n2]
        f = schurfact(a1_sf, a2_sf)
        @test_approx_eq f[:Q]*f[:S]*f[:Z]' a1_sf
        @test_approx_eq f[:Q]*f[:T]*f[:Z]' a2_sf
        @test istriu(f[:S]) || iseltype(a,Real)
        @test istriu(f[:T]) || iseltype(a,Real)
        @test_throws KeyError f[:A]
    end

debug && println("Reorder Generalized Schur")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in Julia
        a1_sf = a[1:n1, 1:n1]
        a2_sf = a[n1+1:n2, n1+1:n2]
        NS = schurfact(a1_sf, a2_sf)
        # Currently just testing with selecting gen eig values < 1
        select = abs2(NS[:values]) .< 1
        m = sum(select)
        S = ordschur(NS, select)
        # Make sure that the new factorization stil factors matrix
        @test_approx_eq S[:Q]*S[:S]*S[:Z]' a1_sf
        @test_approx_eq S[:Q]*S[:T]*S[:Z]' a2_sf
        # Make sure that we have sorted it correctly
        @test_approx_eq NS[:values][find(select)] S[:values][1:m]
    end

debug && println("singular value decomposition")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        usv = svdfact(a)
        @test_approx_eq usv[:U]*scale(usv[:S],usv[:Vt]) a
        @test_approx_eq full(usv) a
        @test_approx_eq usv[:Vt]' usv[:V]
        @test_throws KeyError usv[:Z]
    end

debug && println("Generalized svd")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        a_svd = a[1:n1, :]
        gsvd = svdfact(a,a_svd)
        @test_approx_eq gsvd[:U]*gsvd[:D1]*gsvd[:R]*gsvd[:Q]' a
        @test_approx_eq gsvd[:V]*gsvd[:D2]*gsvd[:R]*gsvd[:Q]' a_svd
        @test_approx_eq usv[:Vt]' usv[:V]
        @test_throws KeyError usv[:Z]
        α = eltya == Int ? -1 : rand(eltya)
        β = svdfact(α)
        @test β[:S] == [abs(α)]
        @test svdvals(α) == [abs(α)]
    end

debug && println("Solve square general system of equations")
    κ = cond(a,1)
    x = a \ b
    @test_throws DimensionMismatch b'\b
    @test_throws DimensionMismatch b\b'
    @test norm(a*x - b, 1)/norm(b) < ε*κ*n*2 # Ad hoc, revisit!

debug && println("Test nullspace")
    if eltya != BigFloat && eltyb != BigFloat # Revisit when implemented in julia
        a15null = nullspace(a[:,1:n1]')
        @test rank([a[:,1:n1] a15null]) == 10
        @test_approx_eq_eps norm(a[:,1:n1]'a15null, Inf) zero(eltya) 300ε
        @test_approx_eq_eps norm(a15null'a[:,1:n1], Inf) zero(eltya) 400ε
        @test size(nullspace(b), 2) == 0
    end
    end # for eltyb

debug && println("\ntype of a: ", eltya, "\n")

debug && println("Test pinv")
    if eltya != BigFloat # Revisit when implemented in julia
        pinva15 = pinv(a[:,1:n1])
        @test_approx_eq a[:,1:n1]*pinva15*a[:,1:n1] a[:,1:n1]
        @test_approx_eq pinva15*a[:,1:n1]*pinva15 pinva15
    end

    # if isreal(a)
debug && println("Matrix square root")
    if eltya != BigFloat # Revisit when implemented in julia
        asq = sqrtm(a)
        @test_approx_eq asq*asq a
        asymsq = sqrtm(asym)
        @test_approx_eq asymsq*asymsq asym
    end

debug && println("Lyapunov/Sylvester")
    if eltya != BigFloat
        let
            x = lyap(a, a2)
            @test_approx_eq -a2 a*x + x*a'
            x2 = sylvester(a[1:3, 1:3], a[4:n, 4:n], a2[1:3,4:n])
            @test_approx_eq -a2[1:3, 4:n] a[1:3, 1:3]*x2 + x2*a[4:n, 4:n]
        end
    end
end # for eltya

#6941
#@test (ones(10^7,4)*ones(4))[3] == 4.0

# test diff, throw ArgumentError for invalid dimension argument
let X = [3  9   5;
         7  4   2;
         2  1  10]
    @test diff(X,1) == [4  -5 -3; -5  -3  8]
    @test diff(X,2) == [6 -4; -3 -2; -1 9]
    @test_throws ArgumentError diff(X,3)
    @test_throws ArgumentError diff(X,-1)
end

x = float([1:12;])
y = [5.5; 6.3; 7.6; 8.8; 10.9; 11.79; 13.48; 15.02; 17.77; 20.81; 22.0; 22.99]
@test_approx_eq linreg(x,y) [2.5559090909090867, 1.6960139860139862]
