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
                @test a*v[:,i] ≈ d[i]*v[:,i] # fragile
            end
            f = eigen(a)
            @test det(a) ≈ det(f)
            @test inv(a) ≈ inv(f)
            @test isposdef(a) == isposdef(f)
            @test eigvals(f) === f.values
            @test eigvecs(f) === f.vectors
            @test Array(f) ≈ a

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
            f = eigen(asym_sg, a_sg'a_sg)
            @test asym_sg*f.vectors ≈ (a_sg'a_sg*f.vectors) * Diagonal(f.values)
            @test f.values ≈ eigvals(asym_sg, a_sg'a_sg)
            @test prod(f.values) ≈ prod(eigvals(asym_sg/(a_sg'a_sg))) atol=200ε
            @test eigvecs(asym_sg, a_sg'a_sg) == f.vectors
            @test eigvals(f) === f.values
            @test eigvecs(f) === f.vectors
            @test_throws ErrorException f.Z

            d,v = eigen(asym_sg, a_sg'a_sg)
            @test d == f.values
            @test v == f.vectors

            # solver for in-place U' \ A / U (#14896)
            if !(eltya <: Integer)
                for atyp in (eltya <: Real ? (Symmetric, Hermitian) : (Hermitian,))
                    for utyp in (UpperTriangular, Diagonal)
                        A = atyp(asym_sg)
                        U = utyp(a_sg'a_sg)
                        @test UtiAUi!(copy(A), U) ≈ U' \ A / U
                    end
                end
            end

            # matrices of different types (#14896)
            if eltya <: Real
                fs = eigen(Symmetric(asym_sg), a_sg'a_sg)
                @test fs.values ≈ f.values
                @test abs.(fs.vectors) ≈ abs.(f.vectors)  # may change sign
                gs = eigen(Symmetric(asym_sg), Diagonal(a_sg'a_sg))
                @test Symmetric(asym_sg)*gs.vectors ≈ (Diagonal(a_sg'a_sg)*gs.vectors) * Diagonal(gs.values)
            end
            fh = eigen(Hermitian(asym_sg), a_sg'a_sg)
            @test fh.values ≈ f.values
            @test abs.(fh.vectors) ≈ abs.(f.vectors)  # may change sign
            gh = eigen(Hermitian(asym_sg), Diagonal(a_sg'a_sg))
            @test Hermitian(asym_sg)*gh.vectors ≈ (Diagonal(a_sg'a_sg)*gh.vectors) * Diagonal(gh.values)
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
            @test a1_nsg*f.vectors ≈ (a2_nsg*f.vectors) * Diagonal(f.values) # fragile
            @test f.values ≈ eigvals(a1_nsg, a2_nsg; sortby = sortfunc)
            # next test is fragile
            @test prod(f.values) ≈ prod(eigvals(a1_nsg/a2_nsg, sortby = sortfunc)) atol=50000ε
            @test eigvecs(a1_nsg, a2_nsg; sortby = sortfunc) == f.vectors
            @test_throws ErrorException f.Z

            d,v = eigen(a1_nsg, a2_nsg; sortby = sortfunc)
            @test d == f.values
            @test v == f.vectors
        end
    end
end

breal = triu(ones(n,n)) + Diagonal(1:n)
bimg = eps(Float32(1)) * randn(n,n)
creal = Float64[0.5 0 0 0; 0 1 1 0; 0 0 1 0; 0 0 0 2] + eps(Float32(1)) * randn(4,4)
cimg = eps(Float32(1)) * randn(4,4)
dreal = Diagonal(1.0:1.0:Float64(n)) + eps(Float32(1)) * randn(n,n)
@testset "$eltya extensions" for eltya in (Float32, Float64, ComplexF32, ComplexF64)
    bb = convert(Matrix{eltya}, eltya <: Complex ? complex.(breal, bimg) : breal)
    cc = convert(Matrix{eltya}, eltya <: Complex ? complex.(creal, cimg) : creal)
    dd = convert(Matrix{eltya}, eltya <: Complex ? complex.(dreal, bimg) : dreal)
    @testset "conditions and lvectors" begin
        ed = eigen(dd, lvectors=true, valscond=true, vecscond=true)
        ec = eigen(cc, lvectors=true, valscond=true, vecscond=true)
        eb = eigen(bb, lvectors=true, valscond=true, vecscond=true)
        # allow for small normalization errors
        @test maximum(eb.rconde) <= 1 + sqrt(eps(real(eltya)))
        # note no such guarantee for rcondv
        @test minimum(ec.rconde) < 0.01
        @test minimum(ec.rcondv) < 0.01
        @test minimum(ed.rconde) > 0.1
        @test minimum(ed.rcondv) > 0.1
        @test ed.vectorsl' * dd ≈ Diagonal(ed.values) * ed.vectorsl'
        @test eb.vectorsl' * bb ≈ Diagonal(eb.values) * eb.vectorsl'
        @test ec.vectorsl' * cc ≈ Diagonal(ec.values) * ec.vectorsl'
    end
    @testset "eigen of Bidiagonal" begin
        ff = Bidiagonal(diag(bb,0), diag(bb,1), :U)
        ef = eigen(ff)
        @test ef.unitary == false
        @test norm(inv(ef) * ff - I) < sqrt(eps(real(eltya)))
    end
    @testset "eigen of Triangular" begin
        ff = UpperTriangular(bb)
        ef = eigen(ff)
        @test ef.unitary == false
        @test norm(inv(ef) * ff - I) < sqrt(eps(real(eltya)))
        ff = LowerTriangular(copy(bb'))
        ef = eigen(ff)
        @test ef.unitary == false
        @test norm(inv(ef) * ff - I) < sqrt(eps(real(eltya)))
    end
end

@testset "eigenvalue computations with NaNs" begin
    for eltya in (NaN16, NaN32, NaN)
        @test_throws(ArgumentError, eigen(fill(eltya, 1, 1)))
        @test_throws(ArgumentError, eigen(fill(eltya, 2, 2)))
        test_matrix = rand(typeof(eltya),3,3)
        test_matrix[1,3] = eltya
        @test_throws(ArgumentError, eigen(test_matrix))
        @test_throws(ArgumentError, eigen(Symmetric(test_matrix)))
        @test_throws(ArgumentError, eigen(Hermitian(test_matrix)))
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
    A = randn(3,3)
    @test eigvals(A') == eigvals(copy(A'))
    @test eigen(A')   == eigen(copy(A'))
    A = A + A'
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
    @test B isa Eigen{Float16, Float16, Matrix{Float16}, Vector{Float16}, Vector{Float16}}
    @test B.values isa Vector{Float16}
    @test B.vectors isa Matrix{Float16}
    @test B.vectorsl isa Matrix{Float16}
    @test B.values ≈ B32.values
    @test B.vectors ≈ B32.vectors
    @test B.vectorsl ≈ B32.vectorsl
    @test D isa Eigen{ComplexF16, ComplexF16, Matrix{ComplexF16}, Vector{ComplexF16}, Vector{Float16}}
    @test D.values isa Vector{ComplexF16}
    @test D.vectors isa Matrix{ComplexF16}
    @test D.vectorsl isa Matrix{ComplexF16}
    @test D.values ≈ D32.values
    @test D.vectors ≈ D32.vectors
    @test D.vectorsl ≈ D32.vectorsl
    @test F isa Eigen{ComplexF16, ComplexF16, Matrix{ComplexF16}, Vector{ComplexF16}, Vector{Float16}}
    @test F.values isa Vector{ComplexF16}
    @test F.vectors isa Matrix{ComplexF16}
    @test F.vectorsl isa Matrix{ComplexF16}
    @test F.values ≈ F32.values
    @test F.vectors ≈ F32.vectors
    @test F.vectorsl ≈ F32.vectorsl
end

end # module TestEigen
