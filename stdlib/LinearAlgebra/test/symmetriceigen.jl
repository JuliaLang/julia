# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestSymmetricEigen

using Test, LinearAlgebra

@testset "chol-eigen-eigvals" begin
    ## Cholesky decomposition based

    # eigenvalue sorting
    sf = x->(imag(x),real(x))

    ## Real valued
    A = Float64[1 1 0 0; 1 2 1 0; 0 1 3 1; 0 0 1 4]
    H = (A+A')/2
    B = Float64[2 1 4 3; 0 3 1 3; 3 1 0 0; 0 1 3 1]
    BH = (B+B')/2
    # PD matrix
    BPD = B*B'
    # eigen
    C = cholesky(BPD)
    e,v = eigen(A, C; sortby=sf)
    @test A*v ≈ BPD*v*Diagonal(e)
    # eigvals
    @test eigvals(A, BPD; sortby=sf) ≈ eigvals(A, C; sortby=sf)

    ## Complex valued
    A =  [1.0+im 1.0+1.0im 0 0; 1.0+1.0im 2.0+3.0im 1.0+1.0im 0; 0 1.0+2.0im 3.0+4.0im 1.0+5.0im; 0 0 1.0+1.0im 4.0+4.0im]
    AH = (A+A')/2
    B =  [2.0+2.0im 1.0+1.0im 4.0+4.0im 3.0+3.0im; 0 3.0+2.0im 1.0+1.0im 3.0+4.0im; 3.0+3.0im 1.0+4.0im 0 0; 0 1.0+2.0im 3.0+1.0im 1.0+1.0im]
    BH = (B+B')/2
    # PD matrix
    BPD = B*B'
    # eigen
    C = cholesky(BPD)
    e,v = eigen(A, C; sortby=sf)
    @test A*v ≈ BPD*v*Diagonal(e)
    # eigvals
    @test eigvals(A, BPD; sortby=sf) ≈ eigvals(A, C; sortby=sf)
end

@testset "issue #49533" begin
    # eigenvalue sorting
    sf = x->(imag(x),real(x))

    ## Real valued
    A = Float64[1 1 0 0; 1 2 1 0; 0 1 3 1; 0 0 1 4]
    B = Matrix(Diagonal(Float64[1:4;]))
    # eigen
    e0,v0 = eigen(A, B)
    e1,v1 = eigen(A, Symmetric(B))
    e2,v2 = eigen(Symmetric(A), B)
    e3,v3 = eigen(Symmetric(A), Symmetric(B))
    @test e0 ≈ e1 && v0 ≈ v1
    @test e0 ≈ e2 && v0 ≈ v2
    @test e0 ≈ e3 && v0 ≈ v3
    # eigvals
    @test eigvals(A, B) ≈ eigvals(A, Symmetric(B))
    @test eigvals(A, B) ≈ eigvals(Symmetric(A), B)
    @test eigvals(A, B) ≈ eigvals(Symmetric(A), Symmetric(B))

    ## Complex valued
    A =  [1.0+im 1.0+1.0im 0 0; 1.0+1.0im 2.0+3.0im 1.0+1.0im 0; 0 1.0+2.0im 3.0+4.0im 1.0+5.0im; 0 0 1.0+1.0im 4.0+4.0im]
    AH = A'A
    B =  [2.0+2.0im 1.0+1.0im 4.0+4.0im 3.0+3.0im; 0 3.0+2.0im 1.0+1.0im 3.0+4.0im; 3.0+3.0im 1.0+4.0im 0 0; 0 1.0+2.0im 3.0+1.0im 1.0+1.0im]
    BH = B'B
    # eigen
    e1,v1 = eigen(A, Hermitian(BH))
    @test A*v1 ≈ Hermitian(BH)*v1*Diagonal(e1)
    e2,v2 = eigen(Hermitian(AH), B)
    @test Hermitian(AH)*v2 ≈ B*v2*Diagonal(e2)
    e3,v3 = eigen(Hermitian(AH), Hermitian(BH))
    @test Hermitian(AH)*v3 ≈ Hermitian(BH)*v3*Diagonal(e3)
    # eigvals
    @test eigvals(A, BH; sortby=sf) ≈ eigvals(A, Hermitian(BH); sortby=sf)
    @test eigvals(AH, B; sortby=sf) ≈ eigvals(Hermitian(AH), B; sortby=sf)
    @test eigvals(AH, BH; sortby=sf) ≈ eigvals(Hermitian(AH), Hermitian(BH); sortby=sf)
end

@testset "bk-lu-eigen-eigvals" begin
    # Bunchkaufman decomposition based

    # eigenvalue sorting
    sf = x->(imag(x),real(x))

    # Real-valued random matrix
    N = 10
    A = randn(N,N)
    B = randn(N,N)
    BH = (B+B')/2
    # eigen
    e0 = eigvals(A,BH; sortby=sf)
    e,v = eigen(A,bunchkaufman(Hermitian(BH,:L)); sortby=sf)
    @test e0 ≈ e
    @test A*v ≈ BH*v*Diagonal(e)
    e,v = eigen(A,bunchkaufman(Hermitian(BH,:U)); sortby=sf)
    @test e0 ≈ e
    @test A*v ≈ BH*v*Diagonal(e)
    e,v = eigen(A,lu(Hermitian(BH,:L)); sortby=sf)
    @test e0 ≈ e
    @test A*v ≈ BH*v*Diagonal(e)
    e,v = eigen(A,lu(Hermitian(BH,:U)); sortby=sf)
    @test e0 ≈ e
    @test A*v ≈ BH*v*Diagonal(e)
    # eigvals
    e0 = eigvals(A,BH; sortby=sf)
    el = eigvals(A,bunchkaufman(Hermitian(BH,:L)); sortby=sf)
    eu = eigvals(A,bunchkaufman(Hermitian(BH,:U)); sortby=sf)
    @test e0 ≈ el
    @test e0 ≈ eu
    el = eigvals(A,lu(Hermitian(BH,:L)); sortby=sf)
    eu = eigvals(A,lu(Hermitian(BH,:U)); sortby=sf)
    @test e0 ≈ el
    @test e0 ≈ eu

    # Complex-valued random matrix
    N = 10
    A = complex.(randn(N,N),randn(N,N))
    B = complex.(randn(N,N),randn(N,N))
    BH = (B+B')/2
    # eigen
    e0 = eigvals(A,BH; sortby=sf)
    e,v = eigen(A,bunchkaufman(Hermitian(BH,:L)); sortby=sf)
    @test e0 ≈ e
    @test A*v ≈ BH*v*Diagonal(e)
    e,v = eigen(A,bunchkaufman(Hermitian(BH,:U)); sortby=sf)
    @test e0 ≈ e
    @test A*v ≈ BH*v*Diagonal(e)
    e,v = eigen(A,lu(Hermitian(BH,:L)); sortby=sf)
    @test e0 ≈ e
    @test A*v ≈ BH*v*Diagonal(e)
    e,v = eigen(A,lu(Hermitian(BH,:U)); sortby=sf)
    @test e0 ≈ e
    @test A*v ≈ BH*v*Diagonal(e)
    # eigvals
    e0 = eigvals(A,BH; sortby=sf)
    el = eigvals(A,bunchkaufman(Hermitian(BH,:L)); sortby=sf)
    eu = eigvals(A,bunchkaufman(Hermitian(BH,:U)); sortby=sf)
    @test e0 ≈ el
    @test e0 ≈ eu
    el = eigvals(A,lu(Hermitian(BH,:L)); sortby=sf)
    eu = eigvals(A,lu(Hermitian(BH,:U)); sortby=sf)
    @test e0 ≈ el
    @test e0 ≈ eu
end

@testset "Hermitian tridiagonal eigen with Complex{Int} elements (#52801)" begin
    dv, ev = fill(complex(2), 4), fill(3-4im, 3)
    HT = Hermitian(Tridiagonal(ev, dv, ev))
    λ, V = eigen(HT)
    @test HT * V ≈ V * Diagonal(λ)
end

end # module TestSymmetricEigen
