# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestBLAS

using Test, LinearAlgebra, Random
using LinearAlgebra: BlasReal, BlasComplex

Random.seed!(100)
## BLAS tests - testing the interface code to BLAS routines
@testset for elty in [Float32, Float64, ComplexF32, ComplexF64]

    @testset "syr2k!" begin
        U = randn(5,2)
        V = randn(5,2)
        if elty == ComplexF32 || elty == ComplexF64
            U = complex.(U, U)
            V = complex.(V, V)
        end
        U = convert(Array{elty, 2}, U)
        V = convert(Array{elty, 2}, V)
        @test tril(LinearAlgebra.BLAS.syr2k('L','N',U,V)) ≈ tril(U*transpose(V) + V*transpose(U))
        @test triu(LinearAlgebra.BLAS.syr2k('U','N',U,V)) ≈ triu(U*transpose(V) + V*transpose(U))
        @test tril(LinearAlgebra.BLAS.syr2k('L','T',U,V)) ≈ tril(transpose(U)*V + transpose(V)*U)
        @test triu(LinearAlgebra.BLAS.syr2k('U','T',U,V)) ≈ triu(transpose(U)*V + transpose(V)*U)
    end

    if elty in (ComplexF32, ComplexF64)
        @testset "her2k!" begin
            U = randn(5,2)
            V = randn(5,2)
            U = complex.(U, U)
            V = complex.(V, V)
            U = convert(Array{elty, 2}, U)
            V = convert(Array{elty, 2}, V)
            @test tril(LinearAlgebra.BLAS.her2k('L','N',U,V)) ≈ tril(U*V' + V*U')
            @test triu(LinearAlgebra.BLAS.her2k('U','N',U,V)) ≈ triu(U*V' + V*U')
            @test tril(LinearAlgebra.BLAS.her2k('L','C',U,V)) ≈ tril(U'*V + V'*U)
            @test triu(LinearAlgebra.BLAS.her2k('U','C',U,V)) ≈ triu(U'*V + V'*U)
        end
    end

    o4 = fill(elty(1), 4)
    z4 = zeros(elty, 4)

    I4 = Matrix{elty}(I, 4, 4)
    I43 = Matrix{elty}(I, 4, 3)
    L4 = tril(fill(elty(1), 4,4))
    U4 = triu(fill(elty(1), 4,4))
    Z4 = zeros(elty, (4,4))

    elm1 = convert(elty, -1)
    el2 = convert(elty, 2)
    v14 = convert(Vector{elty}, [1:4;])
    v41 = convert(Vector{elty}, [4:-1:1;])

    let n = 10
        @testset "dot products" begin
            if elty <: Real
                x1 = convert(Vector{elty}, randn(n))
                x2 = convert(Vector{elty}, randn(n))
                @test BLAS.dot(x1,x2) ≈ sum(x1.*x2)
                @test_throws DimensionMismatch BLAS.dot(x1,rand(elty, n + 1))
            else
                z1 = convert(Vector{elty}, complex.(randn(n),randn(n)))
                z2 = convert(Vector{elty}, complex.(randn(n),randn(n)))
                @test BLAS.dotc(z1,z2) ≈ sum(conj(z1).*z2)
                @test BLAS.dotu(z1,z2) ≈ sum(z1.*z2)
                @test_throws DimensionMismatch BLAS.dotc(z1,rand(elty, n + 1))
                @test_throws DimensionMismatch BLAS.dotu(z1,rand(elty, n + 1))
            end
        end
        @testset "iamax" begin
            if elty <: Real
                x = convert(Vector{elty}, randn(n))
                @test BLAS.iamax(x) == argmax(abs.(x))
            else
                z = convert(Vector{elty}, complex.(randn(n),randn(n)))
                @test BLAS.iamax(z) == argmax(map(x -> abs(real(x)) + abs(imag(x)), z))
            end
        end
        @testset "rot!" begin
            if elty <: Real
                x = convert(Vector{elty}, randn(n))
                y = convert(Vector{elty}, randn(n))
                c = rand(elty)
                s = rand(elty)
                x2 = copy(x)
                y2 = copy(y)
                BLAS.rot!(n, x, 1, y, 1, c, s)
                @test x ≈ c*x2 + s*y2
                @test y ≈ -s*x2 + c*y2
            else
                x = convert(Vector{elty}, complex.(randn(n),rand(n)))
                y = convert(Vector{elty}, complex.(randn(n),rand(n)))
                cty = (elty == ComplexF32) ? Float32 : Float64
                c = rand(cty)
                for sty in [cty, elty]
                    s = rand(sty)
                    x2 = copy(x)
                    y2 = copy(y)
                    BLAS.rot!(n, x, 1, y, 1, c, s)
                    @test x ≈ c*x2 + s*y2
                    @test y ≈ -conj(s)*x2 + c*y2
                end
            end
        end
        @testset "axp(b)y" begin
            if elty <: Real
                x1 = convert(Vector{elty}, randn(n))
                x2 = convert(Vector{elty}, randn(n))
                α  = rand(elty)
                β  = rand(elty)
                @test BLAS.axpy!(α,copy(x1),copy(x2)) ≈ α*x1 + x2
                @test BLAS.axpby!(α,copy(x1),β,copy(x2)) ≈ α*x1 + β*x2
                @test_throws DimensionMismatch BLAS.axpy!(α, copy(x1), rand(elty, n + 1))
                @test_throws DimensionMismatch BLAS.axpby!(α, copy(x1), β, rand(elty, n + 1))
                @test_throws DimensionMismatch BLAS.axpy!(α, copy(x1), 1:div(n,2), copy(x2), 1:n)
                @test_throws ArgumentError BLAS.axpy!(α, copy(x1), 0:div(n,2), copy(x2), 1:(div(n, 2) + 1))
                @test_throws ArgumentError BLAS.axpy!(α, copy(x1), 1:div(n,2), copy(x2), 0:(div(n, 2) - 1))
                @test BLAS.axpy!(α,copy(x1),1:n,copy(x2),1:n) ≈ x2 + α*x1
            else
                z1 = convert(Vector{elty}, complex.(randn(n), randn(n)))
                z2 = convert(Vector{elty}, complex.(randn(n), randn(n)))
                α  = rand(elty)
                @test BLAS.axpy!(α, copy(z1), copy(z2)) ≈ z2 + α * z1
                @test_throws DimensionMismatch BLAS.axpy!(α, copy(z1), rand(elty, n + 1))
                @test_throws DimensionMismatch BLAS.axpy!(α, copy(z1), 1:div(n, 2), copy(z2), 1:(div(n, 2) + 1))
                @test_throws ArgumentError BLAS.axpy!(α, copy(z1), 0:div(n,2), copy(z2), 1:(div(n, 2) + 1))
                @test_throws ArgumentError BLAS.axpy!(α, copy(z1), 1:div(n,2), copy(z2), 0:(div(n, 2) - 1))
                @test BLAS.axpy!(α,copy(z1),1:n,copy(z2),1:n) ≈ z2 + α*z1
            end
        end
        @testset "nrm2, iamax, and asum for StridedVectors" begin
            a = rand(elty,n)
            b = view(a,2:2:n,1)
            @test BLAS.nrm2(b) ≈ norm(b)
            if elty <: Real
                @test BLAS.asum(b) ≈ sum(abs.(b))
                @test BLAS.iamax(b) ≈ argmax(abs.(b))
            else
                @test BLAS.asum(b) ≈ sum(abs.(real(b))) + sum(abs.(imag(b)))
                @test BLAS.iamax(b) == argmax(map(x -> abs(real(x)) + abs(imag(x)), b))
            end
        end
        # scal
        α = rand(elty)
        a = rand(elty,n)
        @test BLAS.scal(n,α,a,1) ≈ α * a

        @testset "trsv" begin
            A = triu(rand(elty,n,n))
            @testset "Vector and SubVector" for x in (rand(elty, n), view(rand(elty,2n),1:2:2n))
                @test A\x ≈ BLAS.trsv('U','N','N',A,x)
                @test_throws DimensionMismatch BLAS.trsv('U','N','N',A,Vector{elty}(undef,n+1))
            end
        end
        @testset "ger, her, syr" for x in (rand(elty, n), view(rand(elty,2n), 1:2:2n)),
            y in (rand(elty,n), view(rand(elty,3n), 1:3:3n))

            A = rand(elty,n,n)
            α = rand(elty)

            @test BLAS.ger!(α,x,y,copy(A)) ≈ A + α*x*y'
            @test_throws DimensionMismatch BLAS.ger!(α,Vector{elty}(undef,n+1),y,copy(A))

            A = rand(elty,n,n)
            A = A + transpose(A)
            @test issymmetric(A)
            @test triu(BLAS.syr!('U',α,x,copy(A))) ≈ triu(A + α*x*transpose(x))
            @test_throws DimensionMismatch BLAS.syr!('U',α,Vector{elty}(undef,n+1),copy(A))

            if elty <: Complex
                A = rand(elty,n,n)
                A = A + A'
                α = real(α)
                @test triu(BLAS.her!('U',α,x,copy(A))) ≈ triu(A + α*x*x')
                @test_throws DimensionMismatch BLAS.her!('U',α,Vector{elty}(undef,n+1),copy(A))
            end
        end
        @testset "copy" begin
            x1 = convert(Vector{elty}, randn(n))
            x2 = convert(Vector{elty}, randn(n))
            BLAS.copyto!(x2, 1:n, x1, 1:n)
            @test x2 == x1
            @test_throws DimensionMismatch BLAS.copyto!(x2, 1:n, x1, 1:(n - 1))
            @test_throws ArgumentError BLAS.copyto!(x1, 0:div(n, 2), x2, 1:(div(n, 2) + 1))
            @test_throws ArgumentError BLAS.copyto!(x1, 1:(div(n, 2) + 1), x2, 0:div(n, 2))
        end
        # trmv
        A = triu(rand(elty,n,n))
        x = rand(elty,n)
        @test BLAS.trmv('U','N','N',A,x) ≈ A*x
        @testset "symmetric/Hermitian multiplication" begin
            x = rand(elty,n)
            A = rand(elty,n,n)
            Aherm = A + A'
            Asymm = A + transpose(A)
            @testset "symv and hemv" begin
                @test BLAS.symv('U',Asymm,x) ≈ Asymm*x
                offsizevec, offsizemat = Array{elty}.(undef,(n+1, (n,n+1)))
                @test_throws DimensionMismatch BLAS.symv!('U',one(elty),Asymm,x,one(elty),offsizevec)
                @test_throws DimensionMismatch BLAS.symv('U',offsizemat,x)
                if elty <: BlasComplex
                    @test BLAS.hemv('U',Aherm,x) ≈ Aherm*x
                    @test_throws DimensionMismatch BLAS.hemv('U',offsizemat,x)
                    @test_throws DimensionMismatch BLAS.hemv!('U',one(elty),Aherm,x,one(elty),offsizevec)
                end
            end

            @testset "symm error throwing" begin
                Cnn, Cnm, Cmn = Matrix{elty}.(undef,((n,n), (n,n-1), (n-1,n)))
                @test_throws DimensionMismatch BLAS.symm('L','U',Cnm,Cnn)
                @test_throws DimensionMismatch BLAS.symm('R','U',Cmn,Cnn)
                @test_throws DimensionMismatch BLAS.symm!('L','U',one(elty),Asymm,Cnn,one(elty),Cmn)
                @test_throws DimensionMismatch BLAS.symm!('L','U',one(elty),Asymm,Cnn,one(elty),Cnm)
                if elty <: BlasComplex
                    @test_throws DimensionMismatch BLAS.hemm('L','U',Cnm,Cnn)
                    @test_throws DimensionMismatch BLAS.hemm('R','U',Cmn,Cnn)
                    @test_throws DimensionMismatch BLAS.hemm!('L','U',one(elty),Aherm,Cnn,one(elty),Cmn)
                    @test_throws DimensionMismatch BLAS.hemm!('L','U',one(elty),Aherm,Cnn,one(elty),Cnm)
                end
            end
        end
        @testset "trmm error throwing" begin
            Cnn, Cmn, Cnm = Matrix{elty}.(undef,((n,n), (n+1,n), (n,n+1)))
            @test_throws DimensionMismatch BLAS.trmm('L','U','N','N',one(elty),triu(Cnn),Cmn)
            @test_throws DimensionMismatch BLAS.trmm('R','U','N','N',one(elty),triu(Cnn),Cnm)
        end

        # hpmv!
        if elty in (ComplexF32, ComplexF64)
            @testset "hpmv!" begin
                # Both matrix dimensions n coincide, as we have Hermitian matrices.
                # Define the inputs and outputs of hpmv!, y = α*A*x+β*y
                α = rand(elty)
                M = rand(elty, n, n)
                AL = Hermitian(M, :L)
                AU = Hermitian(M, :U)
                x = rand(elty, n)
                β = rand(elty)
                y = rand(elty, n)

                y_result_julia_lower = α*AL*x + β*y

                # Create lower triangular packing of AL
                AP = typeof(AL[1,1])[]
                for j in 1:n
                    for i in j:n
                        push!(AP, AL[i,j])
                    end
                end

                y_result_blas_lower = copy(y)
                BLAS.hpmv!('L', α, AP, x, β, y_result_blas_lower)
                @test y_result_julia_lower ≈ y_result_blas_lower

                y_result_julia_upper = α*AU*x + β*y

                # Create upper triangular packing of AU
                AP = typeof(AU[1,1])[]
                for j in 1:n
                    for i in 1:j
                        push!(AP, AU[i,j])
                    end
                end

                y_result_blas_upper = copy(y)
                BLAS.hpmv!('U', α, AP, x, β, y_result_blas_upper)
                @test y_result_julia_upper ≈ y_result_blas_upper
            end
        end

        # spmv!
        if elty in (Float32, Float64)
            @testset "spmv!" begin
                # Both matrix dimensions n coincide, as we have symmetric matrices.
                # Define the inputs and outputs of spmv!, y = α*A*x+β*y
                α = rand(elty)
                M = rand(elty, n, n)
                AL = Symmetric(M, :L)
                AU = Symmetric(M, :U)
                x = rand(elty, n)
                β = rand(elty)
                y = rand(elty, n)

                y_result_julia_lower = α*AL*x + β*y

                # Create lower triangular packing of AL
                AP = typeof(M[1,1])[]
                for j in 1:n
                    for i in j:n
                        push!(AP, AL[i,j])
                    end
                end

                y_result_blas_lower = copy(y)
                BLAS.spmv!('L', α, AP, x, β, y_result_blas_lower)
                @test y_result_julia_lower ≈ y_result_blas_lower


                y_result_julia_upper = α*AU*x + β*y

                # Create upper triangular packing of AU
                AP = typeof(M[1,1])[]
                for j in 1:n
                    for i in 1:j
                        push!(AP, AU[i,j])
                    end
                end

                y_result_blas_upper = copy(y)
                BLAS.spmv!('U', α, AP, x, β, y_result_blas_upper)
                @test y_result_julia_upper ≈ y_result_blas_upper
            end
        end

        #trsm
        A = triu(rand(elty,n,n))
        B = rand(elty,(n,n))
        @test BLAS.trsm('L','U','N','N',one(elty),A,B) ≈ A\B

        #will work for SymTridiagonal,Tridiagonal,Bidiagonal!
        @testset "banded matrix mv" begin
            @testset "gbmv" begin
                TD  = Tridiagonal(rand(elty,n-1),rand(elty,n),rand(elty,n-1))
                x   = rand(elty,n)
                #put TD into the BLAS format!
                fTD = zeros(elty,3,n)
                fTD[1,2:n] = TD.du
                fTD[2,:] = TD.d
                fTD[3,1:n-1] = TD.dl
                @test BLAS.gbmv('N',n,1,1,fTD,x) ≈ TD*x
            end
            #will work for SymTridiagonal only!
            @testset "sbmv" begin
                if elty <: BlasReal
                    ST  = SymTridiagonal(rand(elty,n),rand(elty,n-1))
                    x   = rand(elty,n)
                    #put TD into the BLAS format!
                    fST = zeros(elty,2,n)
                    fST[1,2:n] = ST.ev
                    fST[2,:] = ST.dv
                    @test BLAS.sbmv('U',1,fST,x) ≈ ST*x
                else
                    dv = real(rand(elty,n))
                    ev = rand(elty,n-1)
                    bH = zeros(elty,2,n)
                    bH[1,2:n] = ev
                    bH[2,:] = dv
                    fullH = diagm(0 => dv, -1 => conj(ev), 1 => ev)
                    @test BLAS.hbmv('U',1,bH,x) ≈ fullH*x
                end
            end
        end
    end

    @testset "gemv" begin
        @test all(BLAS.gemv('N', I4, o4) .== o4)
        @test all(BLAS.gemv('T', I4, o4) .== o4)
        @test all(BLAS.gemv('N', el2, I4, o4) .== el2 * o4)
        @test all(BLAS.gemv('T', el2, I4, o4) .== el2 * o4)
        @test_throws DimensionMismatch BLAS.gemv('N',I43,o4)
        o4cp = copy(o4)
        @test_throws DimensionMismatch BLAS.gemv!('T',one(elty),I43,o4,one(elty),o4cp)
        @test_throws DimensionMismatch BLAS.gemv!('C',one(elty),I43,o4,one(elty),o4cp)
        @test all(BLAS.gemv!('N', one(elty), I4, o4, elm1, o4cp) .== z4)
        @test all(o4cp .== z4)
        o4cp[:] = o4
        @test all(BLAS.gemv!('T', one(elty), I4, o4, elm1, o4cp) .== z4)
        @test all(o4cp .== z4)
        @test all(BLAS.gemv('N', U4, o4) .== v41)
        @test all(BLAS.gemv('N', U4, o4) .== v41)
    end
    @testset "gemm" begin
        @test all(BLAS.gemm('N', 'N', I4, I4) .== I4)
        @test all(BLAS.gemm('N', 'T', I4, I4) .== I4)
        @test all(BLAS.gemm('T', 'N', I4, I4) .== I4)
        @test all(BLAS.gemm('T', 'T', I4, I4) .== I4)
        @test all(BLAS.gemm('N', 'N', el2, I4, I4) .== el2 * I4)
        @test all(BLAS.gemm('N', 'T', el2, I4, I4) .== el2 * I4)
        @test all(BLAS.gemm('T', 'N', el2, I4, I4) .== el2 * I4)
        @test all(LinearAlgebra.BLAS.gemm('T', 'T', el2, I4, I4) .== el2 * I4)
        I4cp = copy(I4)
        @test all(BLAS.gemm!('N', 'N', one(elty), I4, I4, elm1, I4cp) .== Z4)
        @test all(I4cp .== Z4)
        I4cp[:] = I4
        @test all(BLAS.gemm!('N', 'T', one(elty), I4, I4, elm1, I4cp) .== Z4)
        @test all(I4cp .== Z4)
        I4cp[:] = I4
        @test all(BLAS.gemm!('T', 'N', one(elty), I4, I4, elm1, I4cp) .== Z4)
        @test all(I4cp .== Z4)
        I4cp[:] = I4
        @test all(BLAS.gemm!('T', 'T', one(elty), I4, I4, elm1, I4cp) .== Z4)
        @test all(I4cp .== Z4)
        @test all(BLAS.gemm('N', 'N', I4, U4) .== U4)
        @test all(BLAS.gemm('N', 'T', I4, U4) .== L4)
        @test_throws DimensionMismatch BLAS.gemm!('N','N', one(elty), I4, I4, elm1, Matrix{elty}(I, 5, 5))
        @test_throws DimensionMismatch BLAS.gemm!('N','N', one(elty), I43, I4, elm1, I4)
        @test_throws DimensionMismatch BLAS.gemm!('T','N', one(elty), I43, I4, elm1, I43)
        @test_throws DimensionMismatch BLAS.gemm!('N','T', one(elty), I43, I43, elm1, I43)
        @test_throws DimensionMismatch BLAS.gemm!('T','T', one(elty), I43, I43, elm1, Matrix{elty}(I, 3, 4))
    end
    @testset "gemm compared to (sy)(he)rk" begin
        if eltype(elm1) <: Complex
            @test all(triu(BLAS.herk('U', 'N', U4)) .== triu(BLAS.gemm('N', 'T', U4, U4)))
            @test all(tril(BLAS.herk('L', 'N', U4)) .== tril(BLAS.gemm('N', 'T', U4, U4)))
            @test all(triu(BLAS.herk('U', 'N', L4)) .== triu(BLAS.gemm('N', 'T', L4, L4)))
            @test all(tril(BLAS.herk('L', 'N', L4)) .== tril(BLAS.gemm('N', 'T', L4, L4)))
            @test all(triu(BLAS.herk('U', 'C', U4)) .== triu(BLAS.gemm('T', 'N', U4, U4)))
            @test all(tril(BLAS.herk('L', 'C', U4)) .== tril(BLAS.gemm('T', 'N', U4, U4)))
            @test all(triu(BLAS.herk('U', 'C', L4)) .== triu(BLAS.gemm('T', 'N', L4, L4)))
            @test all(tril(BLAS.herk('L', 'C', L4)) .== tril(BLAS.gemm('T', 'N', L4, L4)))
            ans = similar(L4)
            @test all(tril(BLAS.herk('L','C', L4)) .== tril(BLAS.herk!('L', 'C', real(one(elty)), L4, real(zero(elty)), ans)))
            @test all(LinearAlgebra.copytri!(ans, 'L') .== LinearAlgebra.BLAS.gemm('T', 'N', L4, L4))
            @test_throws DimensionMismatch BLAS.herk!('L','N',real(one(elty)),Matrix{elty}(I, 5, 5),real(one(elty)), Matrix{elty}(I, 6, 6))
        else
            @test all(triu(BLAS.syrk('U', 'N', U4)) .== triu(BLAS.gemm('N', 'T', U4, U4)))
            @test all(tril(BLAS.syrk('L', 'N', U4)) .== tril(BLAS.gemm('N', 'T', U4, U4)))
            @test all(triu(BLAS.syrk('U', 'N', L4)) .== triu(BLAS.gemm('N', 'T', L4, L4)))
            @test all(tril(BLAS.syrk('L', 'N', L4)) .== tril(BLAS.gemm('N', 'T', L4, L4)))
            @test all(triu(BLAS.syrk('U', 'T', U4)) .== triu(BLAS.gemm('T', 'N', U4, U4)))
            @test all(tril(BLAS.syrk('L', 'T', U4)) .== tril(BLAS.gemm('T', 'N', U4, U4)))
            @test all(triu(BLAS.syrk('U', 'T', L4)) .== triu(BLAS.gemm('T', 'N', L4, L4)))
            @test all(tril(BLAS.syrk('L', 'T', L4)) .== tril(BLAS.gemm('T', 'N', L4, L4)))
            ans = similar(L4)
            @test all(tril(BLAS.syrk('L','T', L4)) .== tril(BLAS.syrk!('L', 'T', one(elty), L4, zero(elty), ans)))
            @test all(LinearAlgebra.copytri!(ans, 'L') .== BLAS.gemm('T', 'N', L4, L4))
            @test_throws DimensionMismatch BLAS.syrk!('L','N',one(elty), Matrix{elty}(I, 5, 5),one(elty), Matrix{elty}(I, 6, 6))
        end
    end
end

@testset "syr for eltype $elty" for elty in (Float32, Float64, Complex{Float32}, Complex{Float64})
    A = rand(elty, 5, 5)
    @test triu(A[1,:] * transpose(A[1,:])) ≈ BLAS.syr!('U', one(elty), A[1,:], zeros(elty, 5, 5))
    @test tril(A[1,:] * transpose(A[1,:])) ≈ BLAS.syr!('L', one(elty), A[1,:], zeros(elty, 5, 5))
    @test triu(A[1,:] * transpose(A[1,:])) ≈ BLAS.syr!('U', one(elty), view(A, 1, :), zeros(elty, 5, 5))
    @test tril(A[1,:] * transpose(A[1,:])) ≈ BLAS.syr!('L', one(elty), view(A, 1, :), zeros(elty, 5, 5))
end

@testset "her for eltype $elty" for elty in (Complex{Float32}, Complex{Float64})
    A = rand(elty, 5, 5)
    @test triu(A[1,:] * A[1,:]') ≈ BLAS.her!('U', one(real(elty)), A[1,:], zeros(elty, 5, 5))
    @test tril(A[1,:] * A[1,:]') ≈ BLAS.her!('L', one(real(elty)), A[1,:], zeros(elty, 5, 5))
    @test triu(A[1,:] * A[1,:]') ≈ BLAS.her!('U', one(real(elty)), view(A, 1, :), zeros(elty, 5, 5))
    @test tril(A[1,:] * A[1,:]') ≈ BLAS.her!('L', one(real(elty)), view(A, 1, :), zeros(elty, 5, 5))
end

struct WrappedArray{T,N} <: AbstractArray{T,N}
    A::Array{T,N}
end

Base.size(A::WrappedArray) = size(A.A)
Base.getindex(A::WrappedArray, i::Int) = A.A[i]
Base.getindex(A::WrappedArray{T, N}, I::Vararg{Int, N}) where {T, N} = A.A[I...]
Base.setindex!(A::WrappedArray, v, i::Int) = setindex!(A.A, v, i)
Base.setindex!(A::WrappedArray{T, N}, v, I::Vararg{Int, N}) where {T, N} = setindex!(A.A, v, I...)
Base.unsafe_convert(::Type{Ptr{T}}, A::WrappedArray{T}) where T = Base.unsafe_convert(Ptr{T}, A.A)

Base.stride(A::WrappedArray, i::Int) = stride(A.A, i)

@testset "strided interface blas" begin
    for elty in (Float32, Float64, ComplexF32, ComplexF64)
    # Level 1
        x = WrappedArray(elty[1, 2, 3, 4])
        y = WrappedArray(elty[5, 6, 7, 8])
        BLAS.blascopy!(2, x, 1, y, 2)
        @test y == WrappedArray(elty[1, 6, 2, 8])
        BLAS.scal!(2, elty(2), x, 1)
        @test x == WrappedArray(elty[2, 4, 3, 4])
        @test BLAS.nrm2(1, x, 2) == elty(2)
        @test BLAS.nrm2(x) == BLAS.nrm2(x.A)
        BLAS.asum(x) == elty(13)
        BLAS.axpy!(4, elty(2), x, 1, y, 1)
        @test y == WrappedArray(elty[5, 14, 8, 16])
        BLAS.axpby!(elty(2), x, elty(3), y)
        @test y == WrappedArray(elty[19, 50, 30, 56])
        @test BLAS.iamax(x) == 2
    # Level 2
        A = WrappedArray(elty[1 2; 3 4])
        x = WrappedArray(elty[1, 2])
        y = WrappedArray(elty[3, 4])
        @test BLAS.gemv!('N', elty(2), A, x, elty(1), y) isa WrappedArray{elty,1}
        @test y == WrappedArray(elty[13, 26])
        @test BLAS.gbmv!('N', 2, 1, 0, elty(2), A, x, elty(1), y) isa WrappedArray{elty,1}
        @test y == WrappedArray(elty[15, 40])
        @test BLAS.symv!('U', elty(2), A, x, elty(1), y) isa WrappedArray{elty,1}
        @test y == WrappedArray(elty[25, 60])
        @test BLAS.trmv!('U', 'N', 'N', A, y) isa WrappedArray{elty,1}
        @test y == WrappedArray(elty[145, 240])
        @test BLAS.trsv!('U', 'N', 'N', A, y) isa WrappedArray{elty,1}
        @test y == WrappedArray(elty[25,60])
        @test BLAS.ger!(elty(2), x, y, A) isa WrappedArray{elty,2}
        @test A == WrappedArray(elty[51 122; 103 244])
        @test BLAS.syr!('L', elty(2), x, A) isa WrappedArray{elty,2}
        @test A == WrappedArray(elty[53 122; 107 252])
    # Level 3
        A = WrappedArray(elty[1 2; 3 4])
        B = WrappedArray(elty[5 6; 7 8])
        C = WrappedArray(elty[9 10; 11 12])
        BLAS.gemm!('N', 'N', elty(2), A, B, elty(1), C) isa WrappedArray{elty,2}
        @test C == WrappedArray([47 54; 97 112])
        BLAS.symm!('L', 'U', elty(2), A, B, elty(1), C) isa WrappedArray{elty,2}
        @test C == WrappedArray([85 98; 173 200])
        BLAS.syrk!('U', 'N', elty(2), A, elty(1), C) isa WrappedArray{elty,2}
        @test C == WrappedArray([95 120; 173 250])
        BLAS.syr2k!('U', 'N', elty(2), A, B, elty(1), C) isa WrappedArray{elty,2}
        @test C == WrappedArray([163 244; 173 462])
        BLAS.trmm!('L', 'U', 'N', 'N', elty(2), A, B) isa WrappedArray{elty,2}
        @test B == WrappedArray([38 44; 56 64])
        BLAS.trsm!('L', 'U', 'N', 'N', elty(2), A, B) isa WrappedArray{elty,2}
        @test B == WrappedArray([20 24; 28 32])
    end
    for elty in (Float32, Float64)
    # Level 1
        x = WrappedArray(elty[1, 2, 3, 4])
        y = WrappedArray(elty[5, 6, 7, 8])
        @test BLAS.dot(2, x, 1, y, 2) == elty(19)
    # Level 2
        A = WrappedArray(elty[1 2; 3 4])
        x = WrappedArray(elty[1, 2])
        y = WrappedArray(elty[3, 4])
        BLAS.sbmv!('U', 1, elty(2), A, x, elty(1), y) isa WrappedArray{elty,1}
        @test y == WrappedArray(elty[17,24])
    end
    for elty in (ComplexF32, ComplexF64)
    # Level 1
        x = WrappedArray(elty[1+im, 2+2im, 3+3im, 4+im])
        y = WrappedArray(elty[5-im, 6-2im, 7-3im, 8-im])
        @test BLAS.dotc(2, x, 1, y, 2) == elty(12-26im)
        @test BLAS.dotu(2, x, 1, y, 2) == elty(26+12im)
    # Level 2
        A = WrappedArray(elty[1+im 2+2im; 3+3im 4+4im])
        x = WrappedArray(elty[1+im, 2+2im])
        y = WrappedArray(elty[5-im, 6-2im])
        @test BLAS.hemv!('U', elty(2), A, x, elty(1), y) isa WrappedArray{elty,1}
        @test y == WrappedArray(elty[7+17im, 30+14im])
        BLAS.hbmv!('U', 1, elty(2), A, x, elty(1), y) isa WrappedArray{elty,1}
        @test y == WrappedArray(elty[13+39im, 54+30im])
        @test BLAS.her!('L', real(elty(2)), x, A) isa WrappedArray{elty,2}
        @test A == WrappedArray(elty[5 2+2im; 11+3im 20])
    # Level 3
        A = WrappedArray(elty[1+im 2+2im; 3+3im 4+4im])
        B = WrappedArray(elty[1+im 2+2im; 3+3im 4+4im])
        C = WrappedArray(elty[1+im 2+2im; 3+3im 4+4im])
        @test BLAS.hemm!('L', 'U', elty(2), A, B, elty(1), C) isa WrappedArray{elty,2}
        @test C == WrappedArray([3+27im 6+38im; 35+27im 52+36im])
        @test BLAS.herk!('U', 'N', real(elty(2)), A, real(elty(1)), C) isa WrappedArray{elty,2}
        @test C == WrappedArray([23 50+38im; 35+27im 152])
        @test BLAS.her2k!('U', 'N', elty(2), A, B, real(elty(1)), C) isa WrappedArray{elty,2}
        @test C == WrappedArray([63 138+38im; 35+27im 352])
    end
end

@testset "get_set_num_threads" begin
    default = BLAS.get_num_threads()
    @test default isa Integer
    @test default > 0
    new=rand(1:Sys.CPU_THREADS)
    BLAS.set_num_threads(new)
    @test BLAS.get_num_threads() == new
    BLAS.set_num_threads(default)
    @test BLAS.get_num_threads() == default
end

end # module TestBLAS
