# This file is a part of Julia. License is MIT: http://julialang.org/license

debug = false

using Base.Test

#Test equivalence of eigenvectors/singular vectors taking into account possible phase (sign) differences
function test_approx_eq_vecs{S<:Real,T<:Real}(a::StridedVecOrMat{S}, b::StridedVecOrMat{T}, error=nothing)
    n = size(a, 1)
    @test n==size(b,1) && size(a,2)==size(b,2)
    error==nothing && (error=n^3*(eps(S)+eps(T)))
    for i=1:n
        ev1, ev2 = a[:,i], b[:,i]
        deviation = min(abs(norm(ev1-ev2)),abs(norm(ev1+ev2)))
        if !isnan(deviation)
            @test_approx_eq_eps deviation 0.0 error
        end
    end
end

let n = 12 #Size of matrix problem to test
    srand(123)
    debug && println("SymTridiagonal (symmetric tridiagonal) matrices")
    for relty in (Float32, Float64), elty in (relty, Complex{relty})
        debug && println("elty is $(elty), relty is $(relty)")
        a = convert(Vector{elty}, randn(n))
        b = convert(Vector{elty}, randn(n-1))
        if elty <: Complex
            a += im*convert(Vector{elty}, randn(n))
            b += im*convert(Vector{elty}, randn(n-1))
        end

        @test_throws DimensionMismatch SymTridiagonal(a, ones(n+1))

        A = SymTridiagonal(a, b)
        fA = map(elty <: Complex ? Complex128 : Float64, full(A))

        debug && println("getindex")
        @test_throws BoundsError A[n+1,1]
        @test_throws BoundsError A[1,n+1]
        @test A[1,n] == convert(elty,0.0)
        @test A[1,1] == a[1]

        debug && println("Diagonal extraction")
        @test diag(A,1) == b
        @test diag(A,-1) == b
        @test diag(A,0) == a
        @test diag(A,n-1) == zeros(elty,1)
        @test_throws BoundsError diag(A,n+1)

        debug && println("Idempotent tests")
        for func in (conj, transpose, ctranspose)
            @test func(func(A)) == A
        end

        debug && println("Simple unary functions")
        for func in (det, inv)
            @test_approx_eq_eps func(A) func(fA) n^2*sqrt(eps(relty))
        end

        debug && println("Rounding to Ints")
        if elty <: Real
            @test round(Int,A) == round(Int,fA)
            @test trunc(Int,A) == trunc(Int,fA)
            @test ceil(Int,A) == ceil(Int,fA)
            @test floor(Int,A) == floor(Int,fA)
        end

        debug && println("Tridiagonal/SymTridiagonal mixing ops")
        B = convert(Tridiagonal{elty},A)
        @test B == A
        @test B + A == A + B
        @test B - A == A - B

        debug && println("Multiplication with strided vector")
        @test_approx_eq A*ones(n) full(A)*ones(n)

        debug && println("Multiplication with strided matrix")
        @test_approx_eq A*ones(n, 2) full(A)*ones(n, 2)

        debug && println("Eigensystems")
        if elty <: Real
            zero, infinity = convert(elty, 0), convert(elty, Inf)
            debug && println("This tests eigenvalue and eigenvector computations using stebz! and stein!")
            w, iblock, isplit = LAPACK.stebz!('V', 'B', -infinity, infinity, 0, 0, zero, a, b)
            evecs = LAPACK.stein!(a, b, w)

            (e, v) = eig(SymTridiagonal(a, b))
            @test_approx_eq e w
            test_approx_eq_vecs(v, evecs)

            debug && println("stein! call using iblock and isplit")
            w, iblock, isplit = LAPACK.stebz!('V', 'B', -infinity, infinity, 0, 0, zero, a, b)
            evecs = LAPACK.stein!(a, b, w, iblock, isplit)
            test_approx_eq_vecs(v, evecs)

            debug && println("stegr! call with index range")
            F = eigfact(SymTridiagonal(a, b),1:2)
            fF = eigfact(Symmetric(full(SymTridiagonal(a, b))),1:2)
            @test_approx_eq F[:vectors] fF[:vectors]
            @test_approx_eq F[:values] fF[:values]

            debug && println("stegr! call with value range")
            F = eigfact(SymTridiagonal(a, b),0.0,1.0)
            fF = eigfact(Symmetric(full(SymTridiagonal(a, b))),0.0,1.0)
            @test_approx_eq F[:vectors] fF[:vectors]
            @test_approx_eq F[:values] fF[:values]
        end

        debug && println("Binary operations")
        a = convert(Vector{elty}, randn(n))
        b = convert(Vector{elty}, randn(n - 1))
        if elty <: Complex
            a += im*convert(Vector{elty}, randn(n))
            b += im*convert(Vector{elty}, randn(n - 1))
        end

        B = SymTridiagonal(a, b)
        fB = map(elty <: Complex ? Complex128 : Float64, full(B))

        for op in (+, -, *)
            @test_approx_eq full(op(A, B)) op(fA, fB)
        end
        α = rand(elty)
        @test_approx_eq full(α*A) α*full(A)
        @test_approx_eq full(A*α) full(A)*α
        @test_approx_eq full(A/α) full(A)/α

        debug && println("A_mul_B!")
        @test_throws DimensionMismatch A_mul_B!(zeros(elty,n,n),B,ones(elty,n+1,n))
        @test_throws DimensionMismatch A_mul_B!(zeros(elty,n+1,n),B,ones(elty,n,n))
        @test_throws DimensionMismatch A_mul_B!(zeros(elty,n,n+1),B,ones(elty,n,n))

    end

    debug && println("Tridiagonal matrices")
    for relty in (Float32, Float64), elty in (relty, Complex{relty})
        debug && println("relty is $(relty), elty is $(elty)")
        a = convert(Vector{elty}, randn(n - 1))
        b = convert(Vector{elty}, randn(n))
        c = convert(Vector{elty}, randn(n - 1))
        if elty <: Complex
            a += im*convert(Vector{elty}, randn(n - 1))
            b += im*convert(Vector{elty}, randn(n))
            c += im*convert(Vector{elty}, randn(n - 1))
        end

        @test_throws ArgumentError Tridiagonal(a,a,a)
        A = Tridiagonal(a, b, c)
        fA = map(elty <: Complex ? Complex128 : Float64, full(A))

        debug && println("Similar, size, and copy!")
        B = similar(A)
        @test size(B) == size(A)
        copy!(B,A)
        @test B == A
        @test_throws DimensionMismatch similar(A,(n,n,2))
        @test_throws DimensionMismatch similar(A,(n+1,n))
        @test_throws DimensionMismatch similar(A,(n,n+1))
        @test size(A,3) == 1
        @test_throws ArgumentError size(A,0)

        debug && println("Diagonal extraction")
        @test diag(A,-1) == a
        @test diag(A,0) == b
        @test diag(A,1) == c
        @test diag(A,n-1) == zeros(elty,1)
        @test_throws BoundsError diag(A,n+1)

        debug && println("Simple unary functions")
        for func in (det, inv)
            @test_approx_eq_eps func(A) func(fA) n^2*sqrt(eps(relty))
        end

        debug && println("Rounding to Ints")
        if elty <: Real
            @test round(Int,A) == round(Int,fA)
            @test trunc(Int,A) == trunc(Int,fA)
            @test ceil(Int,A) == ceil(Int,fA)
            @test floor(Int,A) == floor(Int,fA)
        end

        debug && println("Binary operations")
        a = convert(Vector{elty}, randn(n - 1))
        b = convert(Vector{elty}, randn(n))
        c = convert(Vector{elty}, randn(n - 1))
        if elty <: Complex
            a += im*convert(Vector{elty}, randn(n - 1))
            b += im*convert(Vector{elty}, randn(n))
            c += im*convert(Vector{elty}, randn(n - 1))
        end

        debug && println("Multiplication with strided vector")
        @test_approx_eq A*ones(n) full(A)*ones(n)

        debug && println("Multiplication with strided matrix")
        @test_approx_eq A*ones(n, 2) full(A)*ones(n, 2)


        B = Tridiagonal(a, b, c)
        fB = map(elty <: Complex ? Complex128 : Float64, full(B))

        for op in (+, -, *)
            @test_approx_eq full(op(A, B)) op(fA, fB)
        end
        α = rand(elty)
        @test_approx_eq full(α*A) α*full(A)
        @test_approx_eq full(A*α) full(A)*α
        @test_approx_eq full(A/α) full(A)/α

        @test_throws ArgumentError convert(SymTridiagonal{elty},A)

        debug && println("A_mul_B!")
        @test_throws DimensionMismatch Base.LinAlg.A_mul_B!(zeros(fA),A,ones(elty,n,n+1))
        @test_throws DimensionMismatch Base.LinAlg.A_mul_B!(zeros(fA),A,ones(elty,n+1,n))

        debug && println("getindex")
        @test_throws BoundsError A[n+1,1]
        @test_throws BoundsError A[1,n+1]
    end
end
