# This file is a part of Julia. License is MIT: http://julialang.org/license

debug = false

using Base.Test

# basic tridiagonal operations
n = 5

srand(123)

d = 1 .+ rand(n)
dl = -rand(n-1)
du = -rand(n-1)
v = randn(n)
B = randn(n,2)

for elty in (Float32, Float64, Complex64, Complex128, Int)
    if elty == Int
        srand(61516384)
        d = rand(1:100, n)
        dl = -rand(0:10, n-1)
        du = -rand(0:10, n-1)
        v = rand(1:100, n)
        B = rand(1:100, n, 2)
    else
        d = convert(Vector{elty}, d)
        dl = convert(Vector{elty}, dl)
        du = convert(Vector{elty}, du)
        v = convert(Vector{elty}, v)
        B = convert(Matrix{elty}, B)
    end
    ε = eps(abs2(float(one(elty))))
    T = Tridiagonal(dl, d, du)
    Ts = SymTridiagonal(d, dl)
    @test_throws ArgumentError size(Ts,0)
    @test size(Ts,3) == 1
    @test size(T, 1) == n
    @test size(T) == (n, n)
    F = diagm(d)
    for i = 1:n-1
        F[i,i+1] = du[i]
        F[i+1,i] = dl[i]
    end
    @test full(T) == F

    # elementary operations on tridiagonals
    @test conj(T) == Tridiagonal(conj(dl), conj(d), conj(du))
    @test transpose(T) == Tridiagonal(du, d, dl)
    @test ctranspose(T) == Tridiagonal(conj(du), conj(d), conj(dl))

    @test abs(T) == Tridiagonal(abs(dl),abs(d),abs(du))
    @test real(T) == Tridiagonal(real(dl),real(d),real(du))
    @test imag(T) == Tridiagonal(imag(dl),imag(d),imag(du))
    @test abs(Ts) == SymTridiagonal(abs(d),abs(dl))
    @test real(Ts) == SymTridiagonal(real(d),real(dl))
    @test imag(Ts) == SymTridiagonal(imag(d),imag(dl))

    # test interconversion of Tridiagonal and SymTridiagonal
    @test Tridiagonal(dl, d, dl) == SymTridiagonal(d, dl)
    @test SymTridiagonal(d, dl) == Tridiagonal(dl, d, dl)
    @test Tridiagonal(dl, d, du) + Tridiagonal(du, d, dl) == SymTridiagonal(2d, dl+du)
    @test SymTridiagonal(d, dl) + Tridiagonal(dl, d, du) == Tridiagonal(dl + dl, d+d, dl+du)
    @test convert(SymTridiagonal,Tridiagonal(Ts)) == Ts
    @test full(convert(SymTridiagonal{Complex64},Tridiagonal(Ts))) == convert(Matrix{Complex64},full(Ts))

    # tridiagonal linear algebra
    @test_approx_eq T*v F*v
    invFv = F\v
    @test_approx_eq T\v invFv
    # @test_approx_eq Base.solve(T,v) invFv
    # @test_approx_eq Base.solve(T, B) F\B
    Tlu = factorize(T)
    x = Tlu\v
    @test_approx_eq x invFv
    @test_approx_eq det(T) det(F)

    @test_approx_eq T * Base.LinAlg.UnitUpperTriangular(eye(n)) F*eye(n)
    @test_approx_eq T * Base.LinAlg.UnitLowerTriangular(eye(n)) F*eye(n)
    @test_approx_eq T * UpperTriangular(eye(n)) F*eye(n)
    @test_approx_eq T * LowerTriangular(eye(n)) F*eye(n)

    # symmetric tridiagonal
    if elty <: Real
        Ts = SymTridiagonal(d, dl)
        Fs = full(Ts)
        invFsv = Fs\v
        Tldlt = factorize(Ts)
        x = Tldlt\v
        @test_approx_eq x invFsv
        @test_approx_eq full(full(Tldlt)) Fs
        @test_throws DimensionMismatch Tldlt\rand(elty,n+1)
        @test size(Tldlt) == size(Ts)
        if elty <: AbstractFloat
            @test typeof(convert(Base.LinAlg.LDLt{Float32},Tldlt)) == Base.LinAlg.LDLt{Float32,SymTridiagonal{elty}}
        end
    end

    # eigenvalues/eigenvectors of symmetric tridiagonal
    if elty === Float32 || elty === Float64
        DT, VT = eig(Ts)
        D, Vecs = eig(Fs)
        @test_approx_eq DT D
        @test_approx_eq abs(VT'Vecs) eye(elty, n)
        @test eigvecs(Ts) == eigvecs(Fs)
        #call to LAPACK.stein here
        Test.test_approx_eq_modphase(eigvecs(Ts,eigvals(Ts)),eigvecs(Fs))
    end

    # Test det(A::Matrix)
    # In the long run, these tests should step through Strang's
    #  axiomatic definition of determinants.
    # If all axioms are satisfied and all the composition rules work,
    #  all determinants will be correct except for floating point errors.

    # The determinant of the identity matrix should always be 1.
    for i = 1:10
        A = eye(elty, i)
        @test_approx_eq det(A) one(elty)
    end

    # The determinant of a Householder reflection matrix should always be -1.
    for i = 1:10
        A = eye(elty, 10)
        A[i, i] = -one(elty)
        @test_approx_eq det(A) -one(elty)
    end

    # The determinant of a rotation matrix should always be 1.
    if elty != Int
        for theta = convert(Vector{elty}, pi ./ [1:4;])
            R = [cos(theta) -sin(theta);
                 sin(theta) cos(theta)]
            @test_approx_eq convert(elty, det(R)) one(elty)
        end

    # issue #1490
    @test_approx_eq_eps det(ones(elty, 3,3)) zero(elty) 3*eps(real(one(elty)))

    @test det(SymTridiagonal(elty[],elty[])) == one(elty)

    #tril/triu
    @test_throws ArgumentError tril!(SymTridiagonal(d,dl),n+1)
    @test_throws ArgumentError tril!(Tridiagonal(dl,d,du),n+1)
    @test tril(SymTridiagonal(d,dl))    == Tridiagonal(dl,d,zeros(dl))
    @test tril(SymTridiagonal(d,dl),1)  == Tridiagonal(dl,d,dl)
    @test tril(SymTridiagonal(d,dl),-1) == Tridiagonal(dl,zeros(d),zeros(dl))
    @test tril(SymTridiagonal(d,dl),-2) == Tridiagonal(zeros(dl),zeros(d),zeros(dl))
    @test tril(Tridiagonal(dl,d,du))    == Tridiagonal(dl,d,zeros(du))
    @test tril(Tridiagonal(dl,d,du),1)  == Tridiagonal(dl,d,du)
    @test tril(Tridiagonal(dl,d,du),-1) == Tridiagonal(dl,zeros(d),zeros(du))
    @test tril(Tridiagonal(dl,d,du),-2) == Tridiagonal(zeros(dl),zeros(d),zeros(du))

    @test_throws ArgumentError triu!(SymTridiagonal(d,dl),n+1)
    @test_throws ArgumentError triu!(Tridiagonal(dl,d,du),n+1)
    @test triu(SymTridiagonal(d,dl))    == Tridiagonal(zeros(dl),d,dl)
    @test triu(SymTridiagonal(d,dl),-1) == Tridiagonal(dl,d,dl)
    @test triu(SymTridiagonal(d,dl),1)  == Tridiagonal(zeros(dl),zeros(d),dl)
    @test triu(SymTridiagonal(d,dl),2)  == Tridiagonal(zeros(dl),zeros(d),zeros(dl))
    @test triu(Tridiagonal(dl,d,du))    == Tridiagonal(zeros(dl),d,du)
    @test triu(Tridiagonal(dl,d,du),-1) == Tridiagonal(dl,d,du)
    @test triu(Tridiagonal(dl,d,du),1)  == Tridiagonal(zeros(dl),zeros(d),du)
    @test triu(Tridiagonal(dl,d,du),2)  == Tridiagonal(zeros(dl),zeros(d),zeros(du))

    @test !istril(SymTridiagonal(d,dl))
    @test !istriu(SymTridiagonal(d,dl))
    @test istriu(Tridiagonal(zeros(dl),d,du))
    @test istril(Tridiagonal(dl,d,zeros(du)))
    end
end

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
        @test_throws ArgumentError SymTridiagonal(rand(n,n))

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
            @test isa(round(Int,A), SymTridiagonal)
            @test trunc(Int,A) == trunc(Int,fA)
            @test isa(trunc(Int,A), SymTridiagonal)
            @test ceil(Int,A) == ceil(Int,fA)
            @test isa(ceil(Int,A), SymTridiagonal)
            @test floor(Int,A) == floor(Int,fA)
            @test isa(floor(Int,A), SymTridiagonal)
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
            @test isa(round(Int,A), Tridiagonal)
            @test trunc(Int,A) == trunc(Int,fA)
            @test isa(trunc(Int,A), Tridiagonal)
            @test ceil(Int,A) == ceil(Int,fA)
            @test isa(ceil(Int,A), Tridiagonal)
            @test floor(Int,A) == floor(Int,fA)
            @test isa(floor(Int,A), Tridiagonal)
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

# Issue 12068
SymTridiagonal([1, 2], [0])^3 == [1 0; 0 8]

#test convert for SymTridiagonal
@test convert(SymTridiagonal{Float64},SymTridiagonal(ones(Float32,5),ones(Float32,4))) == SymTridiagonal(ones(Float64,5),ones(Float64,4))
@test convert(AbstractMatrix{Float64},SymTridiagonal(ones(Float32,5),ones(Float32,4))) == SymTridiagonal(ones(Float64,5),ones(Float64,4))
