using Base.Test

srand(101)
debug = false #Turn on for more debugging info

#Pauli σ-matrices
for σ in map(Hermitian, Any[ eye(2), [0 1; 1 0], [0 -im; im 0], [1 0; 0 -1] ])
    @test ishermitian(σ)
end

# Hermitian matrix exponential
let A1 = randn(4,4) + im*randn(4,4)
    A2 = A1 + A1'
    @test_approx_eq expm(A2) expm(Hermitian(A2))
end

let n=10
    areal = randn(n,n)/2
    aimg  = randn(n,n)/2
    debug && println("symmetric eigendecomposition")
    for eltya in (Float32, Float64, Complex64, Complex128, BigFloat, Int)
        a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex(areal, aimg) : areal)
        asym = a'+a                 # symmetric indefinite
        ε = εa = eps(abs(float(one(eltya))))

        debug && println("\ntype of a: ", eltya, "\n")

        eltya == BigFloat && continue # Revisit when implemented in julia
        d, v = eig(asym)
        @test_approx_eq asym*v[:,1] d[1]*v[:,1]
        @test_approx_eq v*Diagonal(d)*v' asym
        @test isequal(eigvals(asym[1]), eigvals(asym[1:1,1:1]))
        @test_approx_eq abs(eigfact(Hermitian(asym), 1:2)[:vectors]'v[:,1:2]) eye(eltya, 2)
        eig(Hermitian(asym), 1:2) # same result, but checks that method works
        @test_approx_eq abs(eigfact(Hermitian(asym), d[1]-10*eps(d[1]), d[2]+10*eps(d[2]))[:vectors]'v[:,1:2]) eye(eltya, 2)
        eig(Hermitian(asym), d[1]-10*eps(d[1]), d[2]+10*eps(d[2])) # same result, but checks that method works
        @test_approx_eq eigvals(Hermitian(asym), 1:2) d[1:2]
        @test_approx_eq eigvals(Hermitian(asym), d[1]-10*eps(d[1]), d[2]+10*eps(d[2])) d[1:2]
        @test_approx_eq full(eigfact(asym)) asym

        # relation to svdvals
        @test sum(sort(abs(eigvals(Hermitian(asym))))) == sum(sort(svdvals(Hermitian(asym))))

        # cond
        @test_approx_eq cond(Hermitian(asym)) cond(asym)

        # rank
        let A = a[:,1:5]*a[:,1:5]'
            @test rank(A) == rank(Hermitian(A))
        end
    end
end

#Issue #7647: test xsyevr, xheevr, xstevr drivers
for Mi7647 in (Symmetric(diagm(1.0:3.0)),
               Hermitian(diagm(1.0:3.0)),
               Hermitian(diagm(complex(1.0:3.0))),
               SymTridiagonal([1.0:3.0;], zeros(2)))
    debug && println("Eigenvalues in interval for $(typeof(Mi7647))")
    @test eigmin(Mi7647)  == eigvals(Mi7647, 0.5, 1.5)[1] == 1.0
    @test eigmax(Mi7647)  == eigvals(Mi7647, 2.5, 3.5)[1] == 3.0
    @test eigvals(Mi7647) == eigvals(Mi7647, 0.5, 3.5) == [1.0:3.0;]
end

#Issue #7933
let A7933 = [1 2; 3 4]
    B7933 = copy(A7933)
    C7933 = full(Symmetric(A7933))
    @test A7933 == B7933
end

# Issues #8057 and #8058
for f in (eigfact, eigvals, eig)
    for A in (Symmetric([0 1; 1 0]), Hermitian([0 im; -im 0]))
        @test_throws ArgumentError f(A, 3, 2)
        @test_throws ArgumentError f(A, 1:4)
    end
end

#Issue 10671
let A = [1.0+im 2.0; 2.0 0.0]
    @test !ishermitian(A)
    @test_throws ArgumentError Hermitian(A)
end
