# This file is a part of Julia. License is MIT: https://julialang.org/license

import Base: -, *, /, \
using Test

# A custom Quaternion type with minimal defined interface and methods.
# Used to test scale and scale! methods to show non-commutativity.
struct Quaternion{T<:Real} <: Number
    s::T
    v1::T
    v2::T
    v3::T
end
Quaternion(s::Real, v1::Real, v2::Real, v3::Real) = Quaternion(promote(s, v1, v2, v3)...)
Base.abs2(q::Quaternion) = q.s*q.s + q.v1*q.v1 + q.v2*q.v2 + q.v3*q.v3
Base.abs(q::Quaternion) = sqrt(abs2(q))
Base.real(::Type{Quaternion{T}}) where {T} = T
Base.conj(q::Quaternion) = Quaternion(q.s, -q.v1, -q.v2, -q.v3)
Base.isfinite(q::Quaternion) = isfinite(q.s) & isfinite(q.v1) & isfinite(q.v2) & isfinite(q.v3)

(-)(ql::Quaternion, qr::Quaternion) =
    Quaternion(ql.s - qr.s, ql.v1 - qr.v1, ql.v2 - qr.v2, ql.v3 - qr.v3)
(*)(q::Quaternion, w::Quaternion) = Quaternion(q.s*w.s - q.v1*w.v1 - q.v2*w.v2 - q.v3*w.v3,
                                               q.s*w.v1 + q.v1*w.s + q.v2*w.v3 - q.v3*w.v2,
                                               q.s*w.v2 - q.v1*w.v3 + q.v2*w.s + q.v3*w.v1,
                                               q.s*w.v3 + q.v1*w.v2 - q.v2*w.v1 + q.v3*w.s)
(*)(q::Quaternion, r::Real) = Quaternion(q.s*r, q.v1*r, q.v2*r, q.v3*r)
(*)(q::Quaternion, b::Bool) = b * q # remove method ambiguity
(/)(q::Quaternion, w::Quaternion) = q * conj(w) * (1.0 / abs2(w))
(\)(q::Quaternion, w::Quaternion) = conj(q) * w * (1.0 / abs2(q))

srand(123)

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
                A = eye(elty, i)
                @test det(A) ≈ one(elty)
            end

            # The determinant of a Householder reflection matrix should always be -1.
            for i = 1:10
                A = eye(elty, 10)
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
        @test logabsdet(convert(Matrix{elty}, -eye(n)))[2] == -1
        if elty <: Real
            @test logabsdet(A)[2] == sign(det(A))
            @test_throws DomainError logdet(convert(Matrix{elty}, -eye(n)))
        else
            @test logabsdet(A)[2] ≈ sign(det(A))
        end
    end
end

@testset "diff" begin
    # test diff, throw ArgumentError for invalid dimension argument
    X = [3  9   5;
         7  4   2;
         2  1  10]
    @test diff(X,1) == [4  -5 -3; -5  -3  8]
    @test diff(X,2) == [6 -4; -3 -2; -1 9]
    @test diff(view(X, 1:2, 1:2),1) == [4 -5]
    @test diff(view(X, 1:2, 1:2),2) == reshape([6; -3], (2,1))
    @test diff(view(X, 2:3, 2:3),1) == [-3 8]
    @test diff(view(X, 2:3, 2:3),2) == reshape([-2; 9], (2,1))
    @test_throws ArgumentError diff(X,3)
    @test_throws ArgumentError diff(X,-1)
end

@testset "linrange" begin
    # make sure unequal input arrays throw an error
    x = [2; 5; 6]
    y = [3; 7; 10; 10]
    @test_throws DimensionMismatch linreg(x, y)
    x = [2 5 6]
    y = [3; 7; 10]
    @test_throws MethodError linreg(x, y)

    # check (UnitRange, Array)
    x = 1:12
    y = [5.5; 6.3; 7.6; 8.8; 10.9; 11.79; 13.48; 15.02; 17.77; 20.81; 22.0; 22.99]
    @test [linreg(x,y)...] ≈ [2.5559090909090867, 1.6960139860139862]
    @test [linreg(view(x,1:6),view(y,1:6))...] ≈ [3.8366666666666642,1.3271428571428574]

    # check (LinSpace, UnitRange)
    x = linspace(1.0, 12.0, 100)
    y = -100:-1
    @test [linreg(x, y)...] ≈ [-109.0, 9.0]

    # check (UnitRange, UnitRange)
    x = 1:12
    y = 12:-1:1
    @test [linreg(x, y)...] ≈ [13.0, -1.0]

    # check (LinSpace, LinSpace)
    x = linspace(-5, 10, 100)
    y = linspace(50, 200, 100)
    @test [linreg(x, y)...] ≈ [100.0, 10.0]

    # check (Array, Array)
    # Anscombe's quartet (https://en.wikipedia.org/wiki/Anscombe%27s_quartet)
    x123 = [10.0; 8.0; 13.0; 9.0; 11.0; 14.0; 6.0; 4.0; 12.0; 7.0; 5.0]
    y1 = [8.04; 6.95; 7.58; 8.81; 8.33; 9.96; 7.24; 4.26; 10.84; 4.82; 5.68]
    @test [linreg(x123,y1)...] ≈ [3.0,0.5] atol=15e-5

    y2 = [9.14; 8.14; 8.74; 8.77; 9.26; 8.10; 6.12; 3.10; 9.13; 7.26; 4.74]
    @test [linreg(x123,y2)...] ≈ [3.0,0.5] atol=10e-3

    y3 = [7.46; 6.77; 12.74; 7.11; 7.81; 8.84; 6.08; 5.39; 8.15; 6.42; 5.73]
    @test [linreg(x123,y3)...] ≈ [3.0,0.5] atol=10e-3

    x4 = [8.0; 8.0; 8.0; 8.0; 8.0; 8.0; 8.0; 19.0; 8.0; 8.0; 8.0]
    y4 = [6.58; 5.76; 7.71; 8.84; 8.47; 7.04; 5.25; 12.50; 5.56; 7.91; 6.89]
    @test [linreg(x4,y4)...] ≈ [3.0,0.5] atol=10e-3
end

@testset "diag" begin
    A = eye(4)
    @test diag(A) == ones(4)
    @test diag(view(A, 1:3, 1:3)) == ones(3)
    @test diag(view(A, 1:2, 1:2)) == ones(2)
    @test_throws ArgumentError diag(rand(10))
end

@testset "generic axpy" begin
    x = ['a','b','c','d','e']
    y = ['a','b','c','d','e']
    α, β = 'f', 'g'
    @test_throws DimensionMismatch Base.LinAlg.axpy!(α,x,['g'])
    @test_throws DimensionMismatch Base.LinAlg.axpby!(α,x,β,['g'])
    @test_throws BoundsError Base.LinAlg.axpy!(α,x,collect(-1:5),y,collect(1:7))
    @test_throws BoundsError Base.LinAlg.axpy!(α,x,collect(1:7),y,collect(-1:5))
    @test_throws BoundsError Base.LinAlg.axpy!(α,x,collect(1:7),y,collect(1:7))
    @test_throws DimensionMismatch Base.LinAlg.axpy!(α,x,collect(1:3),y,collect(1:5))
end

@test !issymmetric(ones(5,3))
@test !ishermitian(ones(5,3))
@test cross(ones(3),ones(3)) == zeros(3)

@test trace(Bidiagonal(ones(5),zeros(4),:U)) == 5


@testset "array and subarray" begin
    aa = reshape([1.:6;], (2,3))
    for a in (aa, view(aa, 1:2, 1:2))
        @testset "2-argument version of scale!" begin
            @test scale!(copy(a), 5.) == a*5
            @test scale!(5., copy(a)) == a*5
            b = randn(Base.LinAlg.SCAL_CUTOFF) # make sure we try BLAS path
            subB = view(b, :, :)
            @test scale!(copy(b), 5.) == b*5
            @test scale!(copy(subB), 5.) == subB*5
            @test scale!([1.; 2.], copy(a)) == a.*[1; 2]
            @test scale!([1; 2], copy(a)) == a.*[1; 2]
            @test_throws DimensionMismatch scale!(ones(3), a)
            if isa(a, Array)
                @test scale!(copy(a), [1.; 2.; 3.]) == a.*[1 2 3]
                @test scale!(copy(a), [1; 2; 3]) == a.*[1 2 3]
                @test_throws DimensionMismatch scale!(a, ones(2))
            else
                @test scale!(copy(a), [1.; 2.]) == a.*[1 2]
                @test scale!(copy(a), [1; 2]) == a.*[1 2]
                @test_throws DimensionMismatch scale!(a, ones(3))
            end
        end

        @testset "3-argument version of scale!" begin
            @test scale!(similar(a), 5., a) == a*5
            @test scale!(similar(a), a, 5.) == a*5
            @test scale!(similar(a), [1.; 2.], a) == a.*[1; 2]
            @test scale!(similar(a), [1; 2], a) == a.*[1; 2]
            @test_throws DimensionMismatch scale!(similar(a), ones(3), a)
            @test_throws DimensionMismatch scale!(Array{Float64}(3, 2), a, ones(3))

            if isa(a, Array)
                @test scale!(similar(a), a, [1.; 2.; 3.]) == a.*[1 2 3]
                @test scale!(similar(a), a, [1; 2; 3]) == a.*[1 2 3]
                @test_throws DimensionMismatch scale!(similar(a), a, ones(2))
            else
                @test scale!(similar(a), a, [1.; 2.]) == a.*[1 2]
                @test scale!(similar(a), a, [1; 2]) == a.*[1 2]
                @test_throws DimensionMismatch scale!(similar(a), a, ones(3))
            end
        end
    end
end

@testset "scale real matrix by complex type" begin
    @test_throws InexactError scale!([1.0], 2.0im)
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
@testset "scale and scale! for non-commutative multiplication" begin
    q = Quaternion(0.44567, 0.755871, 0.882548, 0.423612)
    qmat = [Quaternion(0.015007, 0.355067, 0.418645, 0.318373)]
    @test scale!(q, copy(qmat)) != scale!(copy(qmat), q)
    ## Test * because it doesn't dispatch to scale!
    @test q*qmat ≉ qmat*q
    @test conj(q*qmat) ≈ conj(qmat)*conj(q)
    @test q * (q \ qmat) ≈ qmat ≈ (qmat / q) * q
    @test q\qmat ≉ qmat/q
end
@testset "ops on Numbers" begin
    @testset for elty in [Float32,Float64,Complex64,Complex128]
        a = rand(elty)
        @test trace(a)         == a
        @test rank(zero(elty)) == 0
        @test rank(one(elty))  == 1
        @test !isfinite(cond(zero(elty)))
        @test cond(a)          == one(elty)
        @test cond(a,1)        == one(elty)
        @test issymmetric(a)
        @test ishermitian(one(elty))
        @test det(a) == a
    end

    @test !issymmetric(NaN16)
    @test !issymmetric(NaN32)
    @test !issymmetric(NaN)
end

@test rank([1.0 0.0; 0.0 0.9],0.95) == 1
@test qr(big.([0 1; 0 0]))[2] == [0 1; 0 0]

@test norm([2.4e-322, 4.4e-323]) ≈ 2.47e-322
@test norm([2.4e-322, 4.4e-323], 3) ≈ 2.4e-322
@test_throws ArgumentError norm(ones(5,5),5)

@testset "generic vecnorm for arrays of arrays" begin
    x = Vector{Int}[[1,2], [3,4]]
    @test @inferred(norm(x)) ≈ sqrt(30)
    @test norm(x, 0) == length(x)
    @test norm(x, 1) ≈ sqrt(5) + 5
    @test norm(x, 3) ≈ cbrt(sqrt(125)+125)
end

@testset "LinAlg.axp(b)y! for element type without commutative multiplication" begin
    α = [1 2; 3 4]
    β = [5 6; 7 8]
    x = fill([ 9 10; 11 12], 3)
    y = fill([13 14; 15 16], 3)
    axpy = LinAlg.axpy!(α, x, deepcopy(y))
    axpby = LinAlg.axpby!(α, x, β, deepcopy(y))
    @test axpy == x .* [α] .+ y
    @test axpy != [α] .* x .+ y
    @test axpby == x .* [α] .+ y .* [β]
    @test axpby != [α] .* x .+ [β] .* y
end

@testset "LinAlg.axpy! for x and y of different dimensions" begin
    α = 5
    x = 2:5
    y = ones(Int, 2, 4)
    rx = [1 4]
    ry = [2 8]
    @test LinAlg.axpy!(α, x, rx, y, ry) == [1 1 1 1; 11 1 1 26]
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

@testset "potential overflow in normalize!" begin
    δ = inv(prevfloat(typemax(Float64)))
    v = [δ, -δ]

    @test norm(v) === 7.866824069956793e-309
    w = normalize(v)
    @test w ≈ [1/√2, -1/√2]
    @test norm(w) === 1.0
    @test norm(normalize!(v) - w, Inf) < eps()
end

@testset "Issue 14657" begin
    @test det([true false; false true]) == det(eye(Int, 2))
end

@test_throws ArgumentError Base.LinAlg.char_uplo(:Z)

@testset "Issue 17650" begin
    @test [0.01311489462160816, Inf] ≈ [0.013114894621608135, Inf]
end

@testset "Issue 19035" begin
    @test Base.LinAlg.promote_leaf_eltypes([1, 2, [3.0, 4.0]]) == Float64
    @test Base.LinAlg.promote_leaf_eltypes([[1,2, [3,4]], 5.0, [6im, [7.0, 8.0]]]) == Complex128
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
Base.transpose(a::ModInt{n}) where {n} = a  # see Issue 20978

@testset "Issue 22042" begin
    A = [ModInt{2}(1) ModInt{2}(0); ModInt{2}(1) ModInt{2}(1)]
    b = [ModInt{2}(1), ModInt{2}(0)]

    @test A*(lufact(A, Val(false))\b) == b

    # Needed for pivoting:
    Base.abs(a::ModInt{n}) where {n} = a
    Base.:<(a::ModInt{n}, b::ModInt{n}) where {n} = a.k < b.k

    @test A*(lufact(A, Val(true))\b) == b
end

@testset "fallback throws properly for AbstractArrays with dimension > 2" begin
    @test_throws ErrorException adjoint(rand(2,2,2,2))
    @test_throws ErrorException transpose(rand(2,2,2,2))
end
