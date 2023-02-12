# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestEigen

using Test, LinearAlgebra, Random
using LinearAlgebra: BlasComplex, BlasFloat, BlasReal, QRPivoted, UtiAUi!

n = 10

# Split n into 2 parts for tests needing two matrices
n1 = div(n, 2)
n2 = 2*n1

Random.seed!(12343219)

areal = randn(n,n)/2
aimg  = randn(n,n)/2

@testset for eltya in (Float32, Float64, ComplexF32, ComplexF64, Int)
    aa = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
    asym = aa' + aa                  # symmetric indefinite
    apd  = aa' * aa                 # symmetric positive-definite
    for (a, asym, apd) in ((aa, asym, apd),
                           (view(aa, 1:n, 1:n),
                            view(asym, 1:n, 1:n),
                            view(apd, 1:n, 1:n)))
        ε = εa = eps(abs(float(one(eltya))))

        α = rand(eltya)
        β = rand(eltya)
        eab = eigen(α,β)
        @test eab.values == eigvals(fill(α,1,1),fill(β,1,1))
        @test eab.vectors == eigvecs(fill(α,1,1),fill(β,1,1))

        @testset "non-symmetric eigen decomposition" begin
            d, v = eigen(a)
            for i in 1:size(a,2)
                @test a*v[:,i] ≈ d[i]*v[:,i]
            end
            f = eigen(a)
            @test det(a) ≈ det(f)
            @test inv(a) ≈ inv(f)
            @test isposdef(a) == isposdef(f)
            @test eigvals(f) === f.values
            @test eigvecs(f) === f.vectors
            @test Array(f) ≈ a

            for T in (Tridiagonal(a), Hermitian(Tridiagonal(a)))
                f = eigen(T)
                d, v = f
                for i in 1:size(a,2)
                    @test T*v[:,i] ≈ d[i]*v[:,i]
                end
                @test det(T) ≈ det(f)
                @test inv(T) ≈ inv(f)
            end

            num_fact = eigen(one(eltya))
            @test num_fact.values[1] == one(eltya)
            h = asym
            @test minimum(eigvals(h)) ≈ eigmin(h)
            @test maximum(eigvals(h)) ≈ eigmax(h)
            @test_throws DomainError eigmin(a - a')
            @test_throws DomainError eigmax(a - a')
        end
        @testset "symmetric generalized eigenproblem" begin
            if isa(a, Array)
                asym_sg = asym[1:n1, 1:n1]
                a_sg = a[:,n1+1:n2]
            else
                asym_sg = view(asym, 1:n1, 1:n1)
                a_sg = view(a, 1:n, n1+1:n2)
            end
            ASG2 = a_sg'a_sg
            f = eigen(asym_sg, ASG2)
            @test asym_sg*f.vectors ≈ (ASG2*f.vectors) * Diagonal(f.values)
            @test f.values ≈ eigvals(asym_sg, ASG2)
            @test prod(f.values) ≈ prod(eigvals(asym_sg/(ASG2))) atol=200ε
            @test eigvecs(asym_sg, ASG2) == f.vectors
            @test eigvals(f) === f.values
            @test eigvecs(f) === f.vectors
            @test_throws ErrorException f.Z

            d,v = eigen(asym_sg, ASG2)
            @test d == f.values
            @test v == f.vectors

            # solver for in-place U' \ A / U (#14896)
            if !(eltya <: Integer)
                for atyp in (eltya <: Real ? (Symmetric, Hermitian) : (Hermitian,))
                    for utyp in (UpperTriangular, Diagonal), uplo in (:L, :U)
                        A = atyp(asym_sg, uplo)
                        U = utyp(ASG2)
                        @test UtiAUi!(copy(A), U) ≈ U' \ A / U
                    end
                end
            end

            # matrices of different types (#14896)
            D = Diagonal(ASG2)
            for uplo in (:L, :U)
                if eltya <: Real
                    fs = eigen(Symmetric(asym_sg, uplo), ASG2)
                    @test fs.values ≈ f.values
                    @test abs.(fs.vectors) ≈ abs.(f.vectors)  # may change sign
                    gs = eigen(Symmetric(asym_sg, uplo), D)
                    @test Symmetric(asym_sg, uplo)*gs.vectors ≈ (D*gs.vectors) * Diagonal(gs.values)
                end
                fh = eigen(Hermitian(asym_sg, uplo), ASG2)
                @test fh.values ≈ f.values
                @test abs.(fh.vectors) ≈ abs.(f.vectors)  # may change sign
                gh = eigen(Hermitian(asym_sg, uplo), D)
                @test Hermitian(asym_sg, uplo)*gh.vectors ≈ (D*gh.vectors) * Diagonal(gh.values)
                gd = eigen(Matrix(Hermitian(ASG2, uplo)), D)
                @test Hermitian(ASG2, uplo) * gd.vectors ≈ D * gd.vectors * Diagonal(gd.values)
                gd = eigen(Hermitian(Tridiagonal(ASG2), uplo), D)
                @test Hermitian(Tridiagonal(ASG2), uplo) * gd.vectors ≈ D * gd.vectors * Diagonal(gd.values)
            end
            gd = eigen(D, D)
            @test all(≈(1), gd.values)
            @test D * gd.vectors ≈ D * gd.vectors * Diagonal(gd.values)
            gd = eigen(Matrix(D), D)
            @test D * gd.vectors ≈ D * gd.vectors * Diagonal(gd.values)
            gd = eigen(D, Matrix(D))
            @test D * gd.vectors ≈ D * gd.vectors * Diagonal(gd.values)
            gd = eigen(Tridiagonal(ASG2), Matrix(D))
            @test Tridiagonal(ASG2) * gd.vectors ≈ D * gd.vectors * Diagonal(gd.values)
        end
        @testset "Non-symmetric generalized eigenproblem" begin
            if isa(a, Array)
                a1_nsg = a[1:n1, 1:n1]
                a2_nsg = a[n1+1:n2, n1+1:n2]
            else
                a1_nsg = view(a, 1:n1, 1:n1)
                a2_nsg = view(a, n1+1:n2, n1+1:n2)
            end
            sortfunc = x -> real(x) + imag(x)
            f = eigen(a1_nsg, a2_nsg; sortby = sortfunc)
            @test a1_nsg*f.vectors ≈ (a2_nsg*f.vectors) * Diagonal(f.values)
            @test f.values ≈ eigvals(a1_nsg, a2_nsg; sortby = sortfunc)
            @test prod(f.values) ≈ prod(eigvals(a1_nsg/a2_nsg, sortby = sortfunc)) atol=50000ε
            @test eigvecs(a1_nsg, a2_nsg; sortby = sortfunc) == f.vectors
            @test_throws ErrorException f.Z

            g = eigen(a1_nsg, Diagonal(1:n1))
            @test a1_nsg*g.vectors ≈ (Diagonal(1:n1)*g.vectors) * Diagonal(g.values)

            d,v = eigen(a1_nsg, a2_nsg; sortby = sortfunc)
            @test d == f.values
            @test v == f.vectors
        end
    end
end

@testset "eigenvalue computations with NaNs" begin
    for eltya in (NaN16, NaN32, NaN)
        @test_throws(ArgumentError, eigen(fill(eltya, 1, 1)))
        @test_throws(ArgumentError, eigen(fill(eltya, 2, 2)))
        test_matrix = rand(typeof(eltya),3,3)
        test_matrix[1,3] = eltya
        @test_throws(ArgumentError, eigen(test_matrix))
        @test_throws(ArgumentError, eigvals(test_matrix))
        @test_throws(ArgumentError, eigvecs(test_matrix))
        @test_throws(ArgumentError, eigen(Symmetric(test_matrix)))
        @test_throws(ArgumentError, eigvals(Symmetric(test_matrix)))
        @test_throws(ArgumentError, eigvecs(Symmetric(test_matrix)))
        @test_throws(ArgumentError, eigen(Hermitian(test_matrix)))
        @test_throws(ArgumentError, eigvals(Hermitian(test_matrix)))
        @test_throws(ArgumentError, eigvecs(Hermitian(test_matrix)))
        @test_throws(ArgumentError, eigen(Hermitian(complex.(test_matrix))))
        @test_throws(ArgumentError, eigvals(Hermitian(complex.(test_matrix))))
        @test_throws(ArgumentError, eigvecs(Hermitian(complex.(test_matrix))))
        @test eigen(Symmetric(test_matrix, :L)) isa Eigen
        @test eigen(Hermitian(test_matrix, :L)) isa Eigen
    end
end

# test a matrix larger than 140-by-140 for #14174
let aa = rand(200, 200)
    for a in (aa, view(aa, 1:n, 1:n))
        f = eigen(a)
        @test a ≈ f.vectors * Diagonal(f.values) / f.vectors
    end
end

@testset "rational promotion: issue #24935" begin
    A = [1//2 0//1; 0//1 2//3]
    for λ in (eigvals(A), @inferred(eigvals(Symmetric(A))))
        @test λ isa Vector{Float64}
        @test λ ≈ [0.5, 2/3]
    end
end

@testset "text/plain (REPL) printing of Eigen and GeneralizedEigen" begin
    A, B = randn(5,5), randn(5,5)
    e    = eigen(A)
    ge   = eigen(A, B)
    valsstring = sprint((t, s) -> show(t, "text/plain", s), e.values)
    vecsstring = sprint((t, s) -> show(t, "text/plain", s), e.vectors)
    factstring = sprint((t, s) -> show(t, "text/plain", s), e)
    @test factstring == "$(summary(e))\nvalues:\n$valsstring\nvectors:\n$vecsstring"
end

@testset "eigen of an Adjoint" begin
    Random.seed!(4)
    A = randn(3,3)
    @test eigvals(A') == eigvals(copy(A'))
    @test eigen(A')   == eigen(copy(A'))
    @test eigmin(A') == eigmin(copy(A'))
    @test eigmax(A') == eigmax(copy(A'))
end

@testset "equality of eigen factorizations" begin
    A = randn(3, 3)
    @test eigen(A) == eigen(A)
    @test hash(eigen(A)) == hash(eigen(A))
    @test isequal(eigen(A), eigen(A))
end

@testset "Float16" begin
    A = Float16[4. 12. -16.; 12. 37. -43.; -16. -43. 98.]
    B = eigen(A)
    B32 = eigen(Float32.(A))
    C = Float16[3 -2; 4 -1]
    D = eigen(C)
    D32 = eigen(Float32.(C))
    F = eigen(complex(C))
    F32 = eigen(complex(Float32.(C)))
    @test B isa Eigen{Float16, Float16, Matrix{Float16}, Vector{Float16}}
    @test B.values isa Vector{Float16}
    @test B.vectors isa Matrix{Float16}
    @test B.values ≈ B32.values
    @test B.vectors ≈ B32.vectors
    @test D isa Eigen{ComplexF16, ComplexF16, Matrix{ComplexF16}, Vector{ComplexF16}}
    @test D.values isa Vector{ComplexF16}
    @test D.vectors isa Matrix{ComplexF16}
    @test D.values ≈ D32.values
    @test D.vectors ≈ D32.vectors
    @test F isa Eigen{ComplexF16, ComplexF16, Matrix{ComplexF16}, Vector{ComplexF16}}
    @test F.values isa Vector{ComplexF16}
    @test F.vectors isa Matrix{ComplexF16}
    @test F.values ≈ F32.values
    @test F.vectors ≈ F32.vectors
end

import LinearAlgebra.sintheta

# return permutation p with b[p] ≈ a
function matchperm(a, b)
    n = length(a)
    p = collect(1:n)
    for i = 1:n
        s = abs(a[i] - b[p[i]])
        k = i
        for j = i+1:n
            cij = abs(a[i] - b[p[j]])
            if cij < s
                k = j
                s = cij
            end
        end
        if k != i
            p[k], p[i] = p[i], p[k]
        end
    end
    p
end
function permby!(lead, v, a...)
    p = matchperm(lead, v)
    v = v[p]
    x = getindex.(a, :, Ref(p))
    v, x...
end

normcol(vl, vr) = vl ./ dot.(eachcol(vl), eachcol(vr))'
@testset "left vectors and errors - real $elty" for elty in (Float64, Float32, Float16)
    A = elty[58 9 2; 186 383 96; -912 -1551 -388]
    val = elty[1, 2, 50]
    vec = [1 17 1; -51 -834 -2; 201 3277 5]

    vecl = inv(vec)'
    vec = elty.(vec)
    vecl = elty.(vecl)
    epsi = eps(elty) * 500

    gval, vr, vl, eerrbd, verrbd = eigen(A, scale=true, left=true, eerror=true, verror=true)
    p = matchperm(val, gval)
    vl1 = normcol(vl, vr)

    @test norm(vl1' * vr - I) <= norm(vl1) * epsi
    @test norm(A * vr - vr * Diagonal(gval)) <= norm(A) * epsi
    @test norm(vl' * A - Diagonal(gval) * vl') <= norm(A) * epsi

    val, vec, vecl = permby!(gval, val, vec, vecl)
    @test all(abs.(gval - val) .<= eerrbd)
    @test all(sintheta.(eachcol(vr), eachcol(vec)) .<= verrbd)
    @test all(sintheta.(eachcol(vl), eachcol(vecl)) .<= verrbd)
end

@testset "left vectors and errors - complex $elty" for elty in (Float64, Float32, Float16)
    celty = Complex{elty}
    D = [1 1 0 0; -1 1 0 0; 0 0 1 0; 0 0 0 2]
    U = [1 1 0 0; 0 1 1 0; 0 0 1 1; 0 0 0 1]
    A = elty.(float(U * D / U))
    val = celty[1 - im, 1 + im, 1, 2]
    vec = celty[2 2 0 0; 1-im 1+im 1 0; 0 0 1 1; 0 0 0 1]
    vecl = celty[1+im 1-im 0 0; -2im 2im 0 0; 2im -2im 1 0; -2im 2im -1 1]
    epsi = eps(elty) * 20

    gval, vr, vl, eerrbd, verrbd = eigen(A, scale=true, left=true, eerror=true, verror=true)
    vl1 = normcol(vl, vr)

    @test norm(vl1' * vr - I) <= norm(vl1) * epsi
    @test norm(A * vr - vr * Diagonal(gval)) <= norm(A) * epsi
    @test norm(vl' * A - Diagonal(gval) * vl') <= norm(A) * epsi

    val, vec, vecl = permby!(gval, val, vec, vecl)
    @test all(abs.(gval - val) .<= eerrbd * 10)
    @test all(sintheta.(eachcol(vr), eachcol(vec)) .<= verrbd * 2)
    @test all(sintheta.(eachcol(vl), eachcol(vecl)) .<= verrbd * 15)
end

@testset "left vectors and errors - $elty" for elty in (ComplexF64, ComplexF32, ComplexF16)
    vec = [1+0im  2+0im   3+0im
         0+1im  2+0im   3+0im
        -1+0im  2+0im  -3+0im]
    val = [im, 1, 50]
    A = elty.(float(vec * Diagonal(val) / vec))
    vecl = inv(vec)'
    vec = elty.(vec)
    vecl = elty.(vecl)
    epsi = eps(real(elty)) * 5

    gval, vr, vl, eerrbd, verrbd = eigen(A, scale=true, left=true, eerror=true, verror=true)
    vl1 = normcol(vl, vr)

    @test norm(vl1' * vr - I) <= norm(vl1) * epsi
    @test norm(A * vr - vr * Diagonal(gval)) <= norm(A) * epsi
    @test norm(vl' * A - Diagonal(gval) * vl') <= norm(A) * epsi

    val, vec, vecl = permby!(gval, val, vec, vecl)
    @test all(abs.(gval - val) .<= eerrbd * 10)
    @test all(sintheta.(eachcol(vr), eachcol(vec)) .<= verrbd * 10)
    @test all(sintheta.(eachcol(vl), eachcol(vecl)) .<= verrbd * 15)
end

@testset "generalized left vectors - real $elty" for elty in (Float64, Float32, Float16)
    A = elty[-132   -88    84    104
             -158.4 -79.2  76.8  129.6
              129.6  81.6 -79.2 -100.8
              160    84   -80   -132]
    B = elty[-60  -50   40    50
             -69  -46.4 38    58.2
              58.8 46  -37.6 -48
              70   50  -40   -60]
    val = elty[1, 2, 3, 4]
    vec = [1 -2 -2 -2; 3 -1 -2 -3; 3 -1 -1 -6; 1 -2 -4 -1]

    vecl = inv(B * vec)'
    vec = elty.(vec)
    vecl = elty.(vecl)
    epsi = eps(elty) * 20
    normab = hypot(norm(A), norm(B))

    gval, vr, vl = eigen(A, B, left=true)
    vl1 = normcol(vl, B * vr)

    @test norm(vl1' * B * vr - I) <= norm(B) * epsi
    @test norm(A * vr - B * vr * Diagonal(gval)) <= normab * epsi
    @test norm(vl' * A - Diagonal(gval) * vl' * B) <= normab * epsi

    val, vec, vecl = permby!(gval, val, vec, vecl)
    @test all(abs.(gval - val) .<= epsi * normab)
    @test all(sintheta.(eachcol(vr), eachcol(vec)) .<= epsi * 100)
    @test all(sintheta.(eachcol(vl), eachcol(vecl)) .<= epsi * 100)
end

@testset "generalized left vectors - complex $elty" for elty in (Float64, Float32, Float16)
    celty = Complex{elty}
    D = [1 1 0 0; -1 1 0 0; 0 0 1 0; 0 0 0 2]
    U = [1 1 0 0; 0 1 1 0; 0 0 1 1; 0 0 0 1]
    A = elty.(float(U * D / U))
    B = elty.(float(U * Diagonal([1,1,3,3]) / U))
    val = celty[1/3, 2/3, 1-im, 1+im]
    vec = celty[0 0 2 2; 1 0 1-im 1+im; 1 1 0 0; 0 1 0 0]
    vecl = celty[0 0 -1+im -1-im; 0 0 2 2; 1 0 -2 -2; -1 1 2 2]

    epsi = eps(elty) * 20
    normab = hypot(norm(A), norm(B))

    gval, vr, vl = eigen(A, B, left=true)
    vl1 = normcol(vl, B * vr)

    @test norm(vl1' * B * vr - I) <= norm(B) * epsi
    @test norm(A * vr - B * vr * Diagonal(gval)) <= normab * epsi
    @test norm(vl' * A - Diagonal(gval) * vl' * B) <= normab * epsi

    val, vec, vecl = permby!(gval, val, vec, vecl)
    @test all(abs.(gval - val) .<= epsi * normab)
    @test all(sintheta.(eachcol(vr), eachcol(vec)) .<= epsi * 100)
    @test all(sintheta.(eachcol(vl), eachcol(vecl)) .<= epsi * 100)
end

@testset "generalized left vectors - $elty" for elty in (ComplexF64, ComplexF32, ComplexF16)
    D = [1 1 0 0; -1 1 0 0; 0 0 1 0; 0 0 0 2]
    U = [1 1 0 0; 0 1 1 0; 0 0 1 1; 0 0 0 1]
    A = elty.(float(U * D / U))
    B = elty.(float(U * Diagonal([1,1,3,3]) / U))
    A += B .* im
    val = elty[1/3, 2/3, 1-im, 1+im] .+ im
    vec = elty[0 0 2 2; 1 0 1-im 1+im; 1 1 0 0; 0 1 0 0]
    vecl = elty[0 0 -1+im -1-im; 0 0 2 2; 1 0 -2 -2; -1 1 2 2]

    epsi = eps(real(elty)) * 20
    normab = hypot(norm(A), norm(B))

    gval, vr, vl = eigen(A, B, left=true)
    vl1 = normcol(vl, B * vr)

    @test norm(vl1' * B * vr - I) <= norm(B) * epsi
    @test norm(A * vr - B * vr * Diagonal(gval)) <= normab * epsi
    @test norm(vl' * A - Diagonal(gval) * vl' * B) <= normab * epsi

    val, vec, vecl = permby!(gval, val, vec, vecl)
    @test all(abs.(gval - val) .<= epsi * normab)
    @test all(sintheta.(eachcol(vr), eachcol(vec)) .<= epsi * 100)
    @test all(sintheta.(eachcol(vl), eachcol(vecl)) .<= epsi * 100)
end

end # module TestEigen
