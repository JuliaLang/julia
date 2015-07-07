# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

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
    if elty <: Real
        @test_approx_eq logabsdet(A)[1] log(abs(det(A)))
        @test logabsdet(A)[2] == sign(abs(det(A)))
        @test_throws DomainError logdet(convert(Matrix{elty}, -eye(n)))
        @test logabsdet(convert(Matrix{elty}, -eye(n)))[2] == -1
    end
end