# This file is a part of Julia. License is MIT: http://julialang.org/license

import Base: *
using Base.Test

# A custom Quaternion type with minimal defined interface and methods.
# Used to test scale and scale! methods to show non-commutativity.
immutable Quaternion{T<:Real} <: Number
    s::T
    v1::T
    v2::T
    v3::T
    norm::Bool
end
Quaternion(s::Real, v1::Real, v2::Real, v3::Real, n::Bool = false) =
    Quaternion( promote(s, v1, v2, v3)..., n)
Quaternion(a::Vector) = Quaternion(0, a[1], a[2], a[3])
(*)(q::Quaternion, w::Quaternion) = Quaternion(q.s*w.s - q.v1*w.v1 - q.v2*w.v2 - q.v3*w.v3,
                                               q.s*w.v1 + q.v1*w.s + q.v2*w.v3 - q.v3*w.v2,
                                               q.s*w.v2 - q.v1*w.v3 + q.v2*w.s + q.v3*w.v1,
                                               q.s*w.v3 + q.v1*w.v2 - q.v2*w.v1 + q.v3*w.s,
                                               q.norm && w.norm)

debug = false

srand(123)

n = 5 # should be odd

for elty in (Int, Rational{BigInt}, Float32, Float64, BigFloat, Complex{Float32}, Complex{Float64}, Complex{BigFloat})
    if elty <: Int
        A = rand(-n:n, n, n) + 10I
    elseif elty <: Rational
        A = Rational{BigInt}[rand(-n:n)/rand(1:n) for i = 1:n, j = 1:n] + 10I
    elseif elty <: Real
        A = convert(Matrix{elty}, randn(n,n)) + 10I
    else
        A = convert(Matrix{elty}, complex(randn(n,n), randn(n,n)))
    end

    debug && println("element type: $elty")

    @test_approx_eq logdet(A) log(det(A))
    @test_approx_eq logabsdet(A)[1] log(abs(det(A)))
    @test logabsdet(convert(Matrix{elty}, -eye(n)))[2] == -1
    if elty <: Real
        @test logabsdet(A)[2] == sign(det(A))
        @test_throws DomainError logdet(convert(Matrix{elty}, -eye(n)))
    else
        @test logabsdet(A)[2] ≈ sign(det(A))
    end
end

# test diff, throw ArgumentError for invalid dimension argument
let X = [3  9   5;
         7  4   2;
         2  1  10]
    @test diff(X,1) == [4  -5 -3; -5  -3  8]
    @test diff(X,2) == [6 -4; -3 -2; -1 9]
    @test diff(sub(X, 1:2, 1:2),1) == [4 -5]
    @test diff(sub(X, 1:2, 1:2),2) == reshape([6; -3], (2,1))
    @test diff(sub(X, 2:3, 2:3),1) == [-3 8]
    @test diff(sub(X, 2:3, 2:3),2) == reshape([-2; 9], (2,1))
    @test_throws ArgumentError diff(X,3)
    @test_throws ArgumentError diff(X,-1)
end

x = float([1:12;])
y = [5.5; 6.3; 7.6; 8.8; 10.9; 11.79; 13.48; 15.02; 17.77; 20.81; 22.0; 22.99]
@test_approx_eq linreg(x,y) [2.5559090909090867, 1.6960139860139862]
@test_approx_eq linreg(sub(x,1:6),sub(y,1:6)) [3.8366666666666642,1.3271428571428574]

# test diag
let A = eye(4)
    @test diag(A) == ones(4)
    @test diag(sub(A, 1:3, 1:3)) == ones(3)
    @test diag(sub(A, 1:2, 1:2)) == ones(2)
end

# test generic axpy
x = ['a','b','c','d','e']
y = ['a','b','c','d','e']
α = 'f'
@test_throws DimensionMismatch Base.LinAlg.axpy!(α,x,['g'])
@test_throws BoundsError Base.LinAlg.axpy!(α,x,collect(-1:5),y,collect(1:7))
@test_throws BoundsError Base.LinAlg.axpy!(α,x,collect(1:7),y,collect(-1:5))
@test_throws BoundsError Base.LinAlg.axpy!(α,x,collect(1:7),y,collect(1:7))
@test_throws DimensionMismatch Base.LinAlg.axpy!(α,x,collect(1:3),y,collect(1:5))

@test_throws ArgumentError diag(rand(10))
@test !issymmetric(ones(5,3))
@test !ishermitian(ones(5,3))
@test cross(ones(3),ones(3)) == zeros(3)

@test trace(Bidiagonal(ones(5),zeros(4),true)) == 5


# array and subarray tests
let aa = reshape([1.:6;], (2,3))
    for atype in ("Array", "SubArray")
        if atype == "Array"
            a = aa
        else
            a = sub(aa, 1:2, 1:2)
        end

        # 2-argument version of scale!
        @test scale!(copy(a), 5.) == a*5
        @test scale!(5., copy(a)) == a*5
        b = randn(Base.LinAlg.SCAL_CUTOFF) # make sure we try BLAS path
        subB = sub(b, :, :)
        @test scale!(copy(b), 5.) == b*5
        @test scale!(copy(subB), 5.) == subB*5
        @test scale!([1.; 2.], copy(a)) == a.*[1; 2]
        @test scale!([1; 2], copy(a)) == a.*[1; 2]
        @test_throws DimensionMismatch scale!(ones(3), a)

        if atype == "Array"
            @test scale!(copy(a), [1.; 2.; 3.]) == a.*[1 2 3]
            @test scale!(copy(a), [1; 2; 3]) == a.*[1 2 3]
            @test_throws DimensionMismatch scale!(a, ones(2))
        else
            @test scale!(copy(a), [1.; 2.]) == a.*[1 2]
            @test scale!(copy(a), [1; 2]) == a.*[1 2]
            @test_throws DimensionMismatch scale!(a, ones(3))
        end

        # 3-argument version of scale!
        @test scale!(similar(a), 5., a) == a*5
        @test scale!(similar(a), a, 5.) == a*5
        @test scale!(similar(a), [1.; 2.], a) == a.*[1; 2]
        @test scale!(similar(a), [1; 2], a) == a.*[1; 2]
        @test_throws DimensionMismatch scale!(similar(a), ones(3), a)
        @test_throws DimensionMismatch scale!(Array(Float64, 3, 2), a, ones(3))

        if atype == "Array"
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

# scale real matrix by complex type
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

# test scale and scale! for non-commutative multiplication
q = Quaternion([0.44567, 0.755871, 0.882548, 0.423612])
qmat = []
push!(qmat, Quaternion([0.015007, 0.355067, 0.418645, 0.318373]))
@test scale!(q, copy(qmat)) != scale!(copy(qmat), q)

# test ops on Numbers
for elty in [Float32,Float64,Complex64,Complex128]
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

@test rank([1.0 0.0; 0.0 0.9],0.95) == 1
@test qr(big([0 1; 0 0]))[2] == [0 1; 0 0]

@test norm([2.4e-322, 4.4e-323]) ≈ 2.47e-322
@test norm([2.4e-322, 4.4e-323], 3) ≈ 2.4e-322
@test_throws ArgumentError norm(ones(5,5),5)

# test generic vecnorm for arrays of arrays
let x = Vector{Int}[[1,2], [3,4]]
    @test norm(x) ≈ sqrt(30)
    @test norm(x, 1) ≈ sqrt(5) + 5
    @test norm(x, 3) ≈ cbrt(sqrt(125)+125)
end

# test that LinAlg.axpy! works for element type without commutative multiplication
let
    α = ones(Int, 2, 2)
    x = fill([1 0; 1 1], 3)
    y = fill(zeros(Int, 2, 2), 3)
    @test LinAlg.axpy!(α, x, deepcopy(y)) == x .* Matrix{Int}[α]
    @test LinAlg.axpy!(α, x, deepcopy(y)) != Matrix{Int}[α] .* x
end

# test that LinAlg.axpy! works for x and y of different dimensions
let
    α = 5
    x = 2:5
    y = ones(Int, 2, 4)
    rx = [1 4]
    ry = [2 8]
    @test LinAlg.axpy!(α, x, rx, y, ry) == [1 1 1 1; 11 1 1 26]
end

let
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

#Test potential overflow in normalize!
let
    δ = inv(prevfloat(typemax(Float64)))
    v = [δ, -δ]

    @test norm(v) === 7.866824069956793e-309
    w = normalize(v)
    @test w ≈ [1/√2, -1/√2]
    @test norm(w) === 1.0
    @test norm(normalize!(v) - w, Inf) < eps()
end

# Issue 14657
@test det([true false; false true]) == det(eye(Int, 2))

@test_throws ArgumentError Base.LinAlg.char_uplo(:Z)
