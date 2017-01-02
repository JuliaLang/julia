# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

srand(101)
debug = false #Turn on for more debugging info

#Pauli σ-matrices
for σ in map(Hermitian, Any[ eye(2), [0 1; 1 0], [0 -im; im 0], [1 0; 0 -1] ])
    @test ishermitian(σ)
end

# Hermitian matrix exponential/log
let A1 = randn(4,4) + im*randn(4,4)
    A2 = A1 + A1'
    @test expm(A2) ≈ expm(Hermitian(A2))
    @test logm(A2) ≈ logm(Hermitian(A2))
    A3 = A1 * A1' # posdef
    @test expm(A3) ≈ expm(Hermitian(A3))
    @test logm(A3) ≈ logm(Hermitian(A3))
end

let A1 = randn(4,4)
    A3 = A1 * A1'
    A4 = A1 + A1.'
    @test expm(A4) ≈ expm(Symmetric(A4))
    @test logm(A3) ≈ logm(Symmetric(A3))
    @test logm(A3) ≈ logm(Hermitian(A3))
end

let n=10
    areal = randn(n,n)/2
    aimg  = randn(n,n)/2
    debug && println("symmetric eigendecomposition")
    for eltya in (Float32, Float64, Complex64, Complex128, BigFloat, Int)
        a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
        asym = a'+a                 # symmetric indefinite
        ε = εa = eps(abs(float(one(eltya))))

        x = randn(n)
        y = randn(n)
        b = randn(n,n)/2
        x = eltya == Int ? rand(1:7, n) : convert(Vector{eltya}, eltya <: Complex ? complex.(x, zeros(n)) : x)
        y = eltya == Int ? rand(1:7, n) : convert(Vector{eltya}, eltya <: Complex ? complex.(y, zeros(n)) : y)
        b = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(b, zeros(n,n)) : b)

        debug && println("\ntype of a: ", eltya, "\n")

        # similar
        @test isa(similar(Symmetric(asym)), Symmetric{eltya})
        @test isa(similar(Hermitian(asym)), Hermitian{eltya})
        @test isa(similar(Symmetric(asym), Int), Symmetric{Int})
        @test isa(similar(Hermitian(asym), Int), Hermitian{Int})
        @test isa(similar(Symmetric(asym), (3,2)), Matrix{eltya})
        @test isa(similar(Hermitian(asym), (3,2)), Matrix{eltya})
        @test isa(similar(Symmetric(asym), Int, (3,2)), Matrix{Int})
        @test isa(similar(Hermitian(asym), Int, (3,2)), Matrix{Int})

        # full
        @test asym == full(Hermitian(asym))

        # parent

        @test asym == parent(Hermitian(asym))

        # getindex
        @test asym[1,1] == Hermitian(asym)[1,1]
        @test asym[1,1] == Symmetric(asym)[1,1]

        #trace
        @test trace(asym) == trace(Hermitian(asym))

        # issymmetric, ishermitian
        if eltya <: Real
            @test issymmetric(Symmetric(asym))
            @test ishermitian(Symmetric(asym))
        end
        if eltya <: Complex
            @test ishermitian(Symmetric(b + b'))
        end

        #transpose, ctranspose
        if eltya <: Real
            @test transpose(Symmetric(asym)) == asym
        else
            @test transpose(Hermitian(asym)) == transpose(asym)
        end
        @test ctranspose(Symmetric(asym)) == Symmetric(conj(asym))
        @test ctranspose(Hermitian(asym)) == asym

        #tril/triu
        for di in -n:n
            @test triu(Symmetric(a+a.'),di) == triu(a+a.',di)
            @test tril(Symmetric(a+a.'),di) == tril(a+a.',di)
            @test triu(Hermitian(asym),di) == triu(asym,di)
            @test tril(Hermitian(asym),di) == tril(asym,di)
            @test triu(Symmetric(a+a.',:L),di) == triu(a+a.',di)
            @test tril(Symmetric(a+a.',:L),di) == tril(a+a.',di)
            @test triu(Hermitian(asym,:L),di) == triu(asym,di)
            @test tril(Hermitian(asym,:L),di) == tril(asym,di)
        end

        eltya == BigFloat && continue # Revisit when implemented in julia
        d, v = eig(asym)
        @test asym*v[:,1] ≈ d[1]*v[:,1]
        @test v*Diagonal(d)*v' ≈ asym
        @test isequal(eigvals(asym[1]), eigvals(asym[1:1,1:1]))
        @test abs.(eigfact(Hermitian(asym), 1:2)[:vectors]'v[:,1:2]) ≈ eye(eltya, 2)
        eig(Hermitian(asym), 1:2) # same result, but checks that method works
        @test abs.(eigfact(Hermitian(asym), d[1] - 1, (d[2] + d[3])/2)[:vectors]'v[:,1:2]) ≈ eye(eltya, 2)
        eig(Hermitian(asym), d[1] - 1, (d[2] + d[3])/2) # same result, but checks that method works
        @test eigvals(Hermitian(asym), 1:2) ≈ d[1:2]
        @test eigvals(Hermitian(asym), d[1] - 1, (d[2] + d[3])/2) ≈ d[1:2]
        @test full(eigfact(asym)) ≈ asym

        # relation to svdvals
        @test sum(sort(abs.(eigvals(Hermitian(asym))))) == sum(sort(svdvals(Hermitian(asym))))

        # cond
        @test cond(Hermitian(asym)) ≈ cond(asym)

        # det
        @test det(asym) ≈ det(Hermitian(asym, :U))
        @test det(asym) ≈ det(Hermitian(asym, :L))
        if eltya <: Real
            @test det(asym) ≈ det(Symmetric(asym, :U))
            @test det(asym) ≈ det(Symmetric(asym, :L))
        end
        @test det(a + a.') ≈ det(Symmetric(a + a.', :U))
        @test det(a + a.') ≈ det(Symmetric(a + a.', :L))

        # rank
        let A = a[:,1:5]*a[:,1:5]'
            # Make sure A is Hermitian even in the present of rounding error
            # xianyi/OpenBLAS#729
            A = (A' + A) / 2
            @test rank(A) == rank(Hermitian(A))
        end

        # mat * vec
        if eltya <: Complex
            @test Hermitian(asym)*x+y ≈ asym*x+y
        end
        if eltya <: Real && eltya != Int
            @test Symmetric(asym)*x+y ≈ asym*x+y
        end

        C = zeros(eltya,n,n)
        # mat * mat
        if eltya <: Complex
            @test Hermitian(asym) * a ≈ asym * a
            @test a * Hermitian(asym) ≈ a * asym
            @test Hermitian(asym) * Hermitian(asym) ≈ asym*asym
            @test_throws DimensionMismatch Hermitian(asym) * ones(eltya,n+1)
            Base.LinAlg.A_mul_B!(C,a,Hermitian(asym))
            @test C ≈ a*asym
        end
        if eltya <: Real && eltya != Int
            @test Symmetric(asym) * Symmetric(asym) ≈ asym*asym
            @test Symmetric(asym) * a ≈ asym * a
            @test a * Symmetric(asym) ≈ a * asym
            @test_throws DimensionMismatch Symmetric(asym) * ones(eltya,n+1)
            Base.LinAlg.A_mul_B!(C,a,Symmetric(asym))
            @test C ≈ a*asym
        end

        # solver
        @test Hermitian(asym)\x ≈ asym\x
        if eltya <: Real
            @test Symmetric(asym)\x ≈ asym\x
        end

        #inversion
        @test inv(Hermitian(asym)) ≈ inv(asym)
        if eltya <: Real && eltya != Int
            @test inv(Symmetric(asym)) ≈ inv(asym)
        end

        # conversion
        @test Symmetric(asym) == convert(Symmetric,Symmetric(asym))
        if eltya <: Real && eltya != Int
            typs = [Float16,Float32,Float64]
            for typ in typs
                @test Symmetric(convert(Matrix{typ},asym)) == convert(Symmetric{typ,Matrix{typ}},Symmetric(asym))
            end
        end
        if eltya <: Complex && eltya != Int
            typs = [Complex64,Complex128]
            for typ in typs
                @test Hermitian(convert(Matrix{typ},asym)) == convert(Hermitian{typ,Matrix{typ}},Hermitian(asym))
            end
        end

        #unsafe_getindex
        if eltya <: Real
            @test Symmetric(asym)[1:2,1:2] == asym[1:2,1:2]
        end
        @test Hermitian(asym)[1:2,1:2] == asym[1:2,1:2]
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

# Unary minus for Symmetric matrices
let A = Symmetric(randn(5,5))
    B = -A
    @test A + B ≈ zeros(5,5)
end

# 17780
let a = randn(2,2)
    a = a'a
    b = complex.(a,a)
    c = Symmetric(b)
    @test conj(c) == conj(Array(c))
    cc = copy(c)
    @test conj!(c) == conj(Array(cc))
    c = Hermitian(b + b')
    @test conj(c) == conj(Array(c))
    cc = copy(c)
    @test conj!(c) == conj(Array(c))
end

# 19225
let X = [1 -1; -1 1]
    for T in (Symmetric, Hermitian)
        Y = T(copy(X))
        _Y = similar(Y)
        copy!(_Y, Y)
        @test _Y == Y

        W = T(copy(X), :L)
        copy!(W, Y)
        @test W.data == Y.data
        @test W.uplo != Y.uplo

        W[1,1] = 4
        @test W == T([4 -1; -1 1])
        @test_throws ArgumentError (W[1,2] = 2)

        @test Y + I == T([2 -1; -1 2])
        @test Y - I == T([0 -1; -1 0])
        @test Y * I == Y

        @test Y + 1 == T([2 0; 0 2])
        @test Y - 1 == T([0 -2; -2 0])
        @test Y * 2 == T([2 -2; -2 2])
        @test Y / 1 == Y

        @test T([true false; false true]) + true == T([2 1; 1 2])
    end

    @test_throws ArgumentError Hermitian(X) + 2im*I
    @test_throws ArgumentError Hermitian(X) - 2im*I
end
