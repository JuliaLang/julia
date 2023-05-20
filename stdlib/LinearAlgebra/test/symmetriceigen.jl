# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestSymmetricEigen

using Test, LinearAlgebra, Random

Random.seed!(555)

@testset "bk-eigenvals" begin
    N = 10
    A = complex.(randn(N,N),randn(N,N))
    B = complex.(randn(N,N),randn(N,N))
    BH = (B+B')/2 ; 

    sf = x->(real(x),imag(x))
    e0 = eigvals(A,BH; sortby=sf)
    el = eigvals(A,bunchkaufman(Hermitian(BH,:L)); sortby=sf)
    eu = eigvals(A,bunchkaufman(Hermitian(BH,:U)); sortby=sf)
    @test e0 ≈ el
    @test e0 ≈ eu
end

@testset "chol-eigen-eigvals" begin
    ## Cholesky decomposition based

    ## Complex valued
    A =  [1.0+im 1.0+1.0im 0 0; 1.0+1.0im 2.0+3.0im 1.0+1.0im 0; 0 1.0+2.0im 3.0+4.0im 1.0+5.0im; 0 0 1.0+1.0im 4.0+4.0im] ;
    AH = (A+A')/2 ;
    B =  [2.0+2.0im 1.0+1.0im 4.0+4.0im 3.0+3.0im; 0 3.0+2.0im 1.0+1.0im 3.0+4.0im; 3.0+3.0im 1.0+4.0im 0 0; 0 1.0+2.0im 3.0+1.0im 1.0+1.0im];
    BH = (B+B')/2 ; 

    # eigenvalue sorting
    sf = x->(real(x),imag(x))

    # PD matrix
    BPD = B*B' ;
    # eigen
    C = cholesky(BPD) ;
    e,v = eigen(A, C; sortby=sf)
    @test A*v ≈ BPD*v*Diagonal(e) ;
    # eigvals
    @test eigvals(A, BPD; sortby=sf) ≈ eigvals(A, C; sortby=sf)  
end

@testset "issue #49533" begin
    ## Real valued
    A = Float64[1 1 0 0; 1 2 1 0; 0 1 3 1; 0 0 1 4] ;
    B = Matrix(Diagonal(Float64[1:4;])) ;
    # eigen
    e0,v0 = eigen(A, B)
    e1,v1 = eigen(A, Symmetric(B))
    e2,v2 = eigen(Symmetric(A), B)
    @test e0 ≈ e1 && v0 ≈ v1
    @test e0 ≈ e2 && v0 ≈ v2
    # eigvals
    @test eigvals(A, B) ≈ eigvals(A, Symmetric(B))
    @test eigvals(A, B) ≈ eigvals(Symmetric(A), B)
    
    ## Complex valued
    A =  [1.0+im 1.0+1.0im 0 0; 1.0+1.0im 2.0+3.0im 1.0+1.0im 0; 0 1.0+2.0im 3.0+4.0im 1.0+5.0im; 0 0 1.0+1.0im 4.0+4.0im] ;
    AH = (A+A')/2 ;
    B =  [2.0+2.0im 1.0+1.0im 4.0+4.0im 3.0+3.0im; 0 3.0+2.0im 1.0+1.0im 3.0+4.0im; 3.0+3.0im 1.0+4.0im 0 0; 0 1.0+2.0im 3.0+1.0im 1.0+1.0im];
    BH = (B+B')/2 ; 
    # eigen
    sf = x->(real(x),imag(x))
    e1,v1 = eigen(A, Hermitian(BH))
    e2,v2 = eigen(Hermitian(AH), B)
    @test A*v1 ≈ Hermitian(BH)*v1*Diagonal(e1)
    @test Hermitian(AH)*v2 ≈ B*v2*Diagonal(e2)
    # eigvals
    @test eigvals(A, BH; sortby=sf) ≈ eigvals(A, Hermitian(BH); sortby=sf)
    @test eigvals(AH, B; sortby=sf) ≈ eigvals(Hermitian(AH), B; sortby=sf)    
end

end # module TestSymmetricEigen
