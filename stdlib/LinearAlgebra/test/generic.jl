# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestGeneric

using Test, LinearAlgebra, Random

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")

isdefined(Main, :Quaternions) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "Quaternions.jl"))
using .Main.Quaternions

isdefined(Main, :OffsetArrays) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "OffsetArrays.jl"))
using .Main.OffsetArrays


Random.seed!(123)

n = 5 # should be odd

@testset for elty in (Int, Rational{BigInt}, Float32, Float64, BigFloat, Complex{Float32}, Complex{Float64}, Complex{BigFloat})
    # In the long run, these tests should step through Strang's
    #  axiomatic definition of determinants.
    # If all axioms are satisfied and all the composition rules work,
    #  all determinants will be correct except for floating point errors.
    if elty != Rational{BigInt}
        @testset "det(A::Matrix)" begin
            # The determinant of the identity matrix should always be 1.
            for i = 1:10
                A = Matrix{elty}(I, i, i)
                @test det(A) ≈ one(elty)
            end

            # The determinant of a Householder reflection matrix should always be -1.
            for i = 1:10
                A = Matrix{elty}(I, 10, 10)
                A[i, i] = -one(elty)
                @test det(A) ≈ -one(elty)
            end

            # The determinant of a rotation matrix should always be 1.
            if elty != Int
                for theta = convert(Vector{elty}, pi ./ [1:4;])
                    R = [cos(theta) -sin(theta);
                         sin(theta) cos(theta)]
                    @test convert(elty, det(R)) ≈ one(elty)
                end
            end
        end
    end
    if elty <: Int
        A = rand(-n:n, n, n) + 10I
    elseif elty <: Rational
        A = Rational{BigInt}[rand(-n:n)/rand(1:n) for i = 1:n, j = 1:n] + 10I
    elseif elty <: Real
        A = convert(Matrix{elty}, randn(n,n)) + 10I
    else
        A = convert(Matrix{elty}, complex.(randn(n,n), randn(n,n)))
    end

    @testset "logdet and logabsdet" begin
        @test logdet(A[1,1]) == log(det(A[1,1]))
        @test logdet(A) ≈ log(det(A))
        @test logabsdet(A)[1] ≈ log(abs(det(A)))
        @test logabsdet(Matrix{elty}(-I, n, n))[2] == -1
        infinity = convert(float(elty), Inf)
        @test logabsdet(zeros(elty, n, n)) == (-infinity, zero(elty))
        if elty <: Real
            @test logabsdet(A)[2] == sign(det(A))
            @test_throws DomainError logdet(Matrix{elty}(-I, n, n))
        else
            @test logabsdet(A)[2] ≈ sign(det(A))
        end
    end
end

@testset "diag" begin
    A = Matrix(1.0I, 4, 4)
    @test diag(A) == fill(1, 4)
    @test diag(view(A, 1:3, 1:3)) == fill(1, 3)
    @test diag(view(A, 1:2, 1:2)) == fill(1, 2)
    @test_throws ArgumentError diag(rand(10))
end

@testset "generic axpy" begin
    x = ['a','b','c','d','e']
    y = ['a','b','c','d','e']
    α, β = 'f', 'g'
    @test_throws DimensionMismatch LinearAlgebra.axpy!(α,x,['g'])
    @test_throws DimensionMismatch LinearAlgebra.axpby!(α,x,β,['g'])
    @test_throws BoundsError LinearAlgebra.axpy!(α,x,Vector(-1:5),y,Vector(1:7))
    @test_throws BoundsError LinearAlgebra.axpy!(α,x,Vector(1:7),y,Vector(-1:5))
    @test_throws BoundsError LinearAlgebra.axpy!(α,x,Vector(1:7),y,Vector(1:7))
    @test_throws DimensionMismatch LinearAlgebra.axpy!(α,x,Vector(1:3),y,Vector(1:5))
end

@test !issymmetric(fill(1,5,3))
@test !ishermitian(fill(1,5,3))
@test (x = fill(1,3); cross(x,x) == zeros(3))
@test_throws DimensionMismatch cross(fill(1,3), fill(1,4))
@test_throws DimensionMismatch cross(fill(1,2), fill(1,3))

@test tr(Bidiagonal(fill(1,5),fill(0,4),:U)) == 5


@testset "array and subarray" begin
    aa = reshape([1.:6;], (2,3))
    for a in (aa, view(aa, 1:2, 1:2))
        am, an = size(a)
        @testset "Scaling with rmul! and lmul" begin
            @test rmul!(copy(a), 5.) == a*5
            @test lmul!(5., copy(a)) == a*5
            b = randn(2048)
            subB = view(b, :, :)
            @test rmul!(copy(b), 5.) == b*5
            @test rmul!(copy(subB), 5.) == subB*5
            @test lmul!(Diagonal([1.; 2.]), copy(a)) == a.*[1; 2]
            @test lmul!(Diagonal([1; 2]), copy(a)) == a.*[1; 2]
            @test rmul!(copy(a), Diagonal(1.:an)) == a.*Vector(1:an)'
            @test rmul!(copy(a), Diagonal(1:an)) == a.*Vector(1:an)'
            @test_throws DimensionMismatch lmul!(Diagonal(Vector{Float64}(undef,am+1)), a)
            @test_throws DimensionMismatch rmul!(a, Diagonal(Vector{Float64}(undef,an+1)))
        end

        @testset "Scaling with rdiv! and ldiv!" begin
            @test rdiv!(copy(a), 5.) == a/5
            @test ldiv!(5., copy(a)) == a/5
            @test ldiv!(zero(a), 5., copy(a)) == a/5
        end

        @testset "Scaling with 3-argument mul!" begin
            @test mul!(similar(a), 5., a) == a*5
            @test mul!(similar(a), a, 5.) == a*5
            @test mul!(similar(a), Diagonal([1.; 2.]), a) == a.*[1; 2]
            @test mul!(similar(a), Diagonal([1; 2]), a)   == a.*[1; 2]
            @test_throws DimensionMismatch mul!(similar(a), Diagonal(Vector{Float64}(undef, am+1)), a)
            @test_throws DimensionMismatch mul!(Matrix{Float64}(undef, 3, 2), a, Diagonal(Vector{Float64}(undef, an+1)))
            @test_throws DimensionMismatch mul!(similar(a), a, Diagonal(Vector{Float64}(undef, an+1)))
            @test mul!(similar(a), a, Diagonal(1.:an)) == a.*Vector(1:an)'
            @test mul!(similar(a), a, Diagonal(1:an))  == a.*Vector(1:an)'
        end

        @testset "Scaling with 5-argument mul!" begin
            @test mul!(copy(a), 5., a, 10, 100) == a*150
            @test mul!(copy(a), a, 5., 10, 100) == a*150
            @test mul!(copy(a), Diagonal([1.; 2.]), a, 10, 100) == 10a.*[1; 2] .+ 100a
            @test mul!(copy(a), Diagonal([1; 2]), a, 10, 100)   == 10a.*[1; 2] .+ 100a
            @test mul!(copy(a), a, Diagonal(1.:an), 10, 100) == 10a.*Vector(1:an)' .+ 100a
            @test mul!(copy(a), a, Diagonal(1:an), 10, 100)  == 10a.*Vector(1:an)' .+ 100a
        end
    end
end

@testset "scale real matrix by complex type" begin
    @test_throws InexactError rmul!([1.0], 2.0im)
    @test isequal([1.0] * 2.0im,             Complex{Float64}[2.0im])
    @test isequal(2.0im * [1.0],             Complex{Float64}[2.0im])
    @test isequal(Float32[1.0] * 2.0f0im,    Complex{Float32}[2.0im])
    @test isequal(Float32[1.0] * 2.0im,      Complex{Float64}[2.0im])
    @test isequal(Float64[1.0] * 2.0f0im,    Complex{Float64}[2.0im])
    @test isequal(Float32[1.0] * big(2.0)im, Complex{BigFloat}[2.0im])
    @test isequal(Float64[1.0] * big(2.0)im, Complex{BigFloat}[2.0im])
    @test isequal(BigFloat[1.0] * 2.0im,     Complex{BigFloat}[2.0im])
    @test isequal(BigFloat[1.0] * 2.0f0im,   Complex{BigFloat}[2.0im])
end
@testset "* and mul! for non-commutative scaling" begin
    q = Quaternion(0.44567, 0.755871, 0.882548, 0.423612)
    qmat = [Quaternion(0.015007, 0.355067, 0.418645, 0.318373)]
    @test lmul!(q, copy(qmat)) != rmul!(copy(qmat), q)
    @test q*qmat ≉ qmat*q
    @test conj(q*qmat) ≈ conj(qmat)*conj(q)
    @test q * (q \ qmat) ≈ qmat ≈ (qmat / q) * q
    @test q\qmat ≉ qmat/q
    alpha = Quaternion(rand(4)...)
    beta = Quaternion(0, 0, 0, 0)
    @test mul!(copy(qmat), qmat, q, alpha, beta) ≈ qmat * q * alpha
    @test mul!(copy(qmat), q, qmat, alpha, beta) ≈ q * qmat * alpha
end
@testset "ops on Numbers" begin
    @testset for elty in [Float32,Float64,ComplexF32,ComplexF64]
        a = rand(elty)
        @test tr(a)            == a
        @test rank(zero(elty)) == 0
        @test rank(one(elty))  == 1
        @test !isfinite(cond(zero(elty)))
        @test cond(a)          == one(elty)
        @test cond(a,1)        == one(elty)
        @test issymmetric(a)
        @test ishermitian(one(elty))
        @test det(a) == a
        @test norm(a) == abs(a)
        @test norm(a, 0) == 1
    end

    @test !issymmetric(NaN16)
    @test !issymmetric(NaN32)
    @test !issymmetric(NaN)
    @test norm(NaN)    === NaN
    @test norm(NaN, 0) === NaN
end

@test rank(fill(0, 0, 0)) == 0
@test rank([1.0 0.0; 0.0 0.9],0.95) == 1
@test rank([1.0 0.0; 0.0 0.9],rtol=0.95) == 1
@test rank([1.0 0.0; 0.0 0.9],atol=0.95) == 1
@test rank([1.0 0.0; 0.0 0.9],atol=0.95,rtol=0.95)==1
@test qr(big.([0 1; 0 0])).R == [0 1; 0 0]

@test norm([2.4e-322, 4.4e-323]) ≈ 2.47e-322
@test norm([2.4e-322, 4.4e-323], 3) ≈ 2.4e-322
@test_throws ArgumentError opnorm(Matrix{Float64}(undef,5,5),5)

@testset "generic norm for arrays of arrays" begin
    x = Vector{Int}[[1,2], [3,4]]
    @test @inferred(norm(x)) ≈ sqrt(30)
    @test norm(x, 0) == length(x)
    @test norm(x, 1) ≈ 5+sqrt(5)
    @test norm(x, 3) ≈ cbrt(5^3  +sqrt(5)^3)
end

@testset "rotate! and reflect!" begin
    x = rand(ComplexF64, 10)
    y = rand(ComplexF64, 10)
    c = rand(Float64)
    s = rand(ComplexF64)

    x2 = copy(x)
    y2 = copy(y)
    rotate!(x, y, c, s)
    @test x ≈ c*x2 + s*y2
    @test y ≈ -conj(s)*x2 + c*y2

    x3 = copy(x)
    y3 = copy(y)
    reflect!(x, y, c, s)
    @test x ≈ c*x3 + s*y3
    @test y ≈ conj(s)*x3 - c*y3
end

@testset "LinearAlgebra.axp(b)y! for element type without commutative multiplication" begin
    α = [1 2; 3 4]
    β = [5 6; 7 8]
    x = fill([ 9 10; 11 12], 3)
    y = fill([13 14; 15 16], 3)
    axpy = LinearAlgebra.axpy!(α, x, deepcopy(y))
    axpby = LinearAlgebra.axpby!(α, x, β, deepcopy(y))
    @test axpy == x .* [α] .+ y
    @test axpy != [α] .* x .+ y
    @test axpby == x .* [α] .+ y .* [β]
    @test axpby != [α] .* x .+ [β] .* y
end

@testset "LinearAlgebra.axpy! for x and y of different dimensions" begin
    α = 5
    x = 2:5
    y = fill(1, 2, 4)
    rx = [1 4]
    ry = [2 8]
    @test LinearAlgebra.axpy!(α, x, rx, y, ry) == [1 1 1 1; 11 1 1 26]
end
@testset "norm and normalize!" begin
    vr = [3.0, 4.0]
    for Tr in (Float32, Float64)
        for T in (Tr, Complex{Tr})
            v = convert(Vector{T}, vr)
            @test norm(v) == 5.0
            w = normalize(v)
            @test norm(w - [0.6, 0.8], Inf) < eps(Tr)
            @test norm(w) == 1.0
            @test norm(normalize!(copy(v)) - w, Inf) < eps(Tr)
            @test isempty(normalize!(T[]))
        end
    end
end

@testset "normalize for multidimensional arrays" begin

    for arr in (
        fill(10.0, ()),  # 0 dim
        [1.0],           # 1 dim
        [1.0 2.0 3.0; 4.0 5.0 6.0], # 2-dim
        rand(1,2,3),                # higher dims
        rand(1,2,3,4),
        OffsetArray([-1,0], (-2,))  # no index 1
    )
        @test normalize(arr) == normalize!(copy(arr))
        @test size(normalize(arr)) == size(arr)
        @test axes(normalize(arr)) == axes(arr)
        @test vec(normalize(arr)) == normalize(vec(arr))
    end

    @test typeof(normalize([1 2 3; 4 5 6])) == Array{Float64,2}
end

@testset "Issue #30466" begin
    @test norm([typemin(Int), typemin(Int)], Inf) == -float(typemin(Int))
    @test norm([typemin(Int), typemin(Int)], 1) == -2float(typemin(Int))
end

@testset "potential overflow in normalize!" begin
    δ = inv(prevfloat(typemax(Float64)))
    v = [δ, -δ]

    @test norm(v) === 7.866824069956793e-309
    w = normalize(v)
    @test w ≈ [1/√2, -1/√2]
    @test norm(w) === 1.0
    @test norm(normalize!(v) - w, Inf) < eps()
end

@testset "normalize with Infs. Issue 29681." begin
    @test all(isequal.(normalize([1, -1, Inf]),
                       [0.0, -0.0, NaN]))
    @test all(isequal.(normalize([complex(1), complex(0, -1), complex(Inf, -Inf)]),
                       [0.0 + 0.0im, 0.0 - 0.0im, NaN + NaN*im]))
end

@testset "Issue 14657" begin
    @test det([true false; false true]) == det(Matrix(1I, 2, 2))
end

@test_throws ArgumentError LinearAlgebra.char_uplo(:Z)

@testset "Issue 17650" begin
    @test [0.01311489462160816, Inf] ≈ [0.013114894621608135, Inf]
end

@testset "Issue 19035" begin
    @test LinearAlgebra.promote_leaf_eltypes([1, 2, [3.0, 4.0]]) == Float64
    @test LinearAlgebra.promote_leaf_eltypes([[1,2, [3,4]], 5.0, [6im, [7.0, 8.0]]]) == ComplexF64
    @test [1, 2, 3] ≈ [1, 2, 3]
    @test [[1, 2], [3, 4]] ≈ [[1, 2], [3, 4]]
    @test [[1, 2], [3, 4]] ≈ [[1.0-eps(), 2.0+eps()], [3.0+2eps(), 4.0-1e8eps()]]
    @test [[1, 2], [3, 4]] ≉ [[1.0-eps(), 2.0+eps()], [3.0+2eps(), 4.0-1e9eps()]]
    @test [[1,2, [3,4]], 5.0, [6im, [7.0, 8.0]]] ≈ [[1,2, [3,4]], 5.0, [6im, [7.0, 8.0]]]
end

# Minimal modulo number type - but not subtyping Number
struct ModInt{n}
    k
    ModInt{n}(k) where {n} = new(mod(k,n))
    ModInt{n}(k::ModInt{n}) where {n} = k
end
Base.:+(a::ModInt{n}, b::ModInt{n}) where {n} = ModInt{n}(a.k + b.k)
Base.:-(a::ModInt{n}, b::ModInt{n}) where {n} = ModInt{n}(a.k - b.k)
Base.:*(a::ModInt{n}, b::ModInt{n}) where {n} = ModInt{n}(a.k * b.k)
Base.:-(a::ModInt{n}) where {n} = ModInt{n}(-a.k)
Base.inv(a::ModInt{n}) where {n} = ModInt{n}(invmod(a.k, n))
Base.:/(a::ModInt{n}, b::ModInt{n}) where {n} = a*inv(b)

Base.zero(::Type{ModInt{n}}) where {n} = ModInt{n}(0)
Base.zero(::ModInt{n}) where {n} = ModInt{n}(0)
Base.one(::Type{ModInt{n}}) where {n} = ModInt{n}(1)
Base.one(::ModInt{n}) where {n} = ModInt{n}(1)
Base.conj(a::ModInt{n}) where {n} = a
Base.adjoint(a::ModInt{n}) where {n} = ModInt{n}(conj(a))
Base.transpose(a::ModInt{n}) where {n} = a  # see Issue 20978
LinearAlgebra.Adjoint(a::ModInt{n}) where {n} = adjoint(a)
LinearAlgebra.Transpose(a::ModInt{n}) where {n} = transpose(a)

@testset "Issue 22042" begin
    A = [ModInt{2}(1) ModInt{2}(0); ModInt{2}(1) ModInt{2}(1)]
    b = [ModInt{2}(1), ModInt{2}(0)]

    @test A*(lu(A, Val(false))\b) == b

    # Needed for pivoting:
    Base.abs(a::ModInt{n}) where {n} = a
    LinearAlgebra.norm(a::ModInt{n}) where {n} = a

    Base.:<(a::ModInt{n}, b::ModInt{n}) where {n} = a.k < b.k

    @test A*(lu(A, Val(true))\b) == b
end

@testset "Issue 18742" begin
    @test_throws DimensionMismatch ones(4,5)/zeros(3,6)
    @test_throws DimensionMismatch ones(4,5)\zeros(3,6)
end
@testset "fallback throws properly for AbstractArrays with dimension > 2" begin
    @test_throws ErrorException adjoint(rand(2,2,2,2))
    @test_throws ErrorException transpose(rand(2,2,2,2))
end

@testset "generic functions for checking whether matrices have banded structure" begin
    using LinearAlgebra: isbanded
    pentadiag = [1 2 3; 4 5 6; 7 8 9]
    tridiag   = [1 2 0; 4 5 6; 0 8 9]
    ubidiag   = [1 2 0; 0 5 6; 0 0 9]
    lbidiag   = [1 0 0; 4 5 0; 0 8 9]
    adiag     = [1 0 0; 0 5 0; 0 0 9]
    @testset "istriu" begin
        @test !istriu(pentadiag)
        @test istriu(pentadiag, -2)
        @test !istriu(tridiag)
        @test istriu(tridiag, -1)
        @test istriu(ubidiag)
        @test !istriu(ubidiag, 1)
        @test !istriu(lbidiag)
        @test istriu(lbidiag, -1)
        @test istriu(adiag)
    end
    @testset "istril" begin
        @test !istril(pentadiag)
        @test istril(pentadiag, 2)
        @test !istril(tridiag)
        @test istril(tridiag, 1)
        @test !istril(ubidiag)
        @test istril(ubidiag, 1)
        @test istril(lbidiag)
        @test !istril(lbidiag, -1)
        @test istril(adiag)
    end
    @testset "isbanded" begin
        @test isbanded(pentadiag, -2, 2)
        @test !isbanded(pentadiag, -1, 2)
        @test !isbanded(pentadiag, -2, 1)
        @test isbanded(tridiag, -1, 1)
        @test !isbanded(tridiag, 0, 1)
        @test !isbanded(tridiag, -1, 0)
        @test isbanded(ubidiag, 0, 1)
        @test !isbanded(ubidiag, 1, 1)
        @test !isbanded(ubidiag, 0, 0)
        @test isbanded(lbidiag, -1, 0)
        @test !isbanded(lbidiag, 0, 0)
        @test !isbanded(lbidiag, -1, -1)
        @test isbanded(adiag, 0, 0)
        @test !isbanded(adiag, -1, -1)
        @test !isbanded(adiag, 1, 1)
    end
    @testset "isdiag" begin
        @test !isdiag(tridiag)
        @test !isdiag(ubidiag)
        @test !isdiag(lbidiag)
        @test isdiag(adiag)
    end
end

@testset "missing values" begin
    @test ismissing(norm(missing))
end

@testset "peakflops" begin
    @test LinearAlgebra.peakflops() > 0
end

@testset "NaN handling: Issue 28972" begin
    @test all(isnan, rmul!([NaN], 0.0))
    @test all(isnan, rmul!(Any[NaN], 0.0))
    @test all(isnan, lmul!(0.0, [NaN]))
    @test all(isnan, lmul!(0.0, Any[NaN]))

    @test all(!isnan, rmul!([NaN], false))
    @test all(!isnan, rmul!(Any[NaN], false))
    @test all(!isnan, lmul!(false, [NaN]))
    @test all(!isnan, lmul!(false, Any[NaN]))
end

@testset "generalized dot #32739" begin
    for elty in (Int, Float32, Float64, BigFloat, Complex{Float32}, Complex{Float64}, Complex{BigFloat})
        n = 10
        if elty <: Int
            A = rand(-n:n, n, n)
            x = rand(-n:n, n)
            y = rand(-n:n, n)
        elseif elty <: Real
            A = convert(Matrix{elty}, randn(n,n))
            x = rand(elty, n)
            y = rand(elty, n)
        else
            A = convert(Matrix{elty}, complex.(randn(n,n), randn(n,n)))
            x = rand(elty, n)
            y = rand(elty, n)
        end
        @test dot(x, A, y) ≈ dot(A'x, y) ≈ *(x', A, y) ≈ (x'A)*y
        @test dot(x, A', y) ≈ dot(A*x, y) ≈ *(x', A', y) ≈ (x'A')*y
        elty <: Real && @test dot(x, transpose(A), y) ≈ dot(x, transpose(A)*y) ≈ *(x', transpose(A), y) ≈ (x'*transpose(A))*y
        B = reshape([A], 1, 1)
        x = [x]
        y = [y]
        @test dot(x, B, y) ≈ dot(B'x, y)
        @test dot(x, B', y) ≈ dot(B*x, y)
        elty <: Real && @test dot(x, transpose(B), y) ≈ dot(x, transpose(B)*y)
    end
end

@testset "condskeel #34512" begin
    A = rand(3, 3)
    @test condskeel(A) ≈ condskeel(A, [8,8,8])
end

end # module TestGeneric
