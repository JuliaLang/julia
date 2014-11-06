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

n = 12 #Size of matrix problem to test

debug && println("SymTridiagonal (symmetric tridiagonal) matrices")
for relty in (Float32, Float64), elty in (relty, )#XXX Complex{relty}) doesn't work
    debug && println("elty is $(elty), relty is $(relty)")
    a = convert(Vector{elty}, randn(n))
    b = convert(Vector{elty}, randn(n-1))
    if elty <: Complex
        a += im*convert(Vector{elty}, randn(n))
        b += im*convert(Vector{elty}, randn(n-1))
    end

    A = SymTridiagonal(a, b)
    fA = (elty <: Complex ? complex128:float64)(full(A))

    debug && println("Idempotent tests")
    for func in (conj, transpose, ctranspose)
        @test func(func(A)) == A
    end

    debug && println("Simple unary functions")
    for func in (det, inv)
        @test_approx_eq_eps func(A) func(fA) n^2*sqrt(eps(relty))
    end

    debug && println("Multiplication with strided vector")
    @test_approx_eq A*ones(n) full(A)*ones(n)

    debug && println("Multiplication with strided matrix")
    @test_approx_eq A*ones(n, 2) full(A)*ones(n, 2)

    debug && println("Eigensystems")
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

    debug && println("Binary operations")
    a = convert(Vector{elty}, randn(n))
    b = convert(Vector{elty}, randn(n - 1))
    if elty <: Complex
        a += im*convert(Vector{elty}, randn(n - 1))
        b += im*convert(Vector{elty}, randn(n))
    end

    B = SymTridiagonal(a, b)
    fB = (elty <: Complex ? complex128:float64)(full(B))

    for op in (+, -, *)
        @test_approx_eq full(op(A, B)) op(fA, fB)
    end
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

    A = Tridiagonal(a, b, c)
    fA = (elty <: Complex ? complex128:float64)(full(A))

    debug && println("Simple unary functions")
    for func in (det, inv)
        @test_approx_eq_eps func(A) func(fA) n^2*sqrt(eps(relty))
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
    fB = (elty <: Complex ? complex128:float64)(full(B))

    for op in (+, -, *)
        @test_approx_eq full(op(A, B)) op(fA, fB)
    end
end
