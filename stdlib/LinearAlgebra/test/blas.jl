# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestBLAS

using Test, LinearAlgebra, Random
using LinearAlgebra: BlasReal, BlasComplex
fabs(x::Real) = abs(x)
fabs(x::Complex) = abs(real(x)) + abs(imag(x))

# help function to build packed storage
function pack(A, uplo)
    AP = eltype(A)[]
    n = size(A, 1)
    for j in 1:n, i in (uplo==:L ? (j:n) : (1:j))
        push!(AP, A[i,j])
    end
    return AP
end

@testset "vec_pointer_stride" begin
    a = zeros(4,4,4)
    @test BLAS.asum(view(a,1:2:4,:,:)) == 0 # vector like
    @test_throws ArgumentError BLAS.asum(view(a,1:3:4,:,:)) # non-vector like
end
Random.seed!(100)
## BLAS tests - testing the interface code to BLAS routines
@testset for elty in [Float32, Float64, ComplexF32, ComplexF64]

    @testset "syr2k!" begin
        U = randn(elty, 5, 2)
        V = randn(elty, 5, 2)
        @test tril(LinearAlgebra.BLAS.syr2k('L','N',U,V)) ≈ tril(U*transpose(V) + V*transpose(U))
        @test triu(LinearAlgebra.BLAS.syr2k('U','N',U,V)) ≈ triu(U*transpose(V) + V*transpose(U))
        @test tril(LinearAlgebra.BLAS.syr2k('L','T',U,V)) ≈ tril(transpose(U)*V + transpose(V)*U)
        @test triu(LinearAlgebra.BLAS.syr2k('U','T',U,V)) ≈ triu(transpose(U)*V + transpose(V)*U)
    end

    if elty in (ComplexF32, ComplexF64)
        @testset "her2k!" begin
            U = randn(elty, 5, 2)
            V = randn(elty, 5, 2)
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

    elm1 = elty(-1)
    el2 = elty(2)
    v14 = elty[1:4;]
    v41 = elty[4:-1:1;]

    let n = 10
        @testset "dot products" begin
            if elty <: Real
                x1 = randn(elty, n)
                x2 = randn(elty, n)
                @test BLAS.dot(x1,x2) ≈ sum(x1.*x2)
                @test_throws DimensionMismatch BLAS.dot(x1,rand(elty, n + 1))
            else
                z1 = randn(elty, n)
                z2 = randn(elty, n)
                @test BLAS.dotc(z1,z2) ≈ sum(conj(z1).*z2)
                @test BLAS.dotu(z1,z2) ≈ sum(z1.*z2)
                @test_throws DimensionMismatch BLAS.dotc(z1,rand(elty, n + 1))
                @test_throws DimensionMismatch BLAS.dotu(z1,rand(elty, n + 1))
            end
        end
        @testset "iamax" begin
            x = randn(elty, n)
            @test BLAS.iamax(x) == findmax(fabs, x)[2]
        end
        @testset "rot!" begin
            x = randn(elty, n)
            y = randn(elty, n)
            c = rand(real(elty))
            for sty in unique!([real(elty), elty])
                s = rand(sty)
                x2 = copy(x)
                y2 = copy(y)
                BLAS.rot!(n, x, 1, y, 1, c, s)
                @test x ≈ c*x2 + s*y2
                @test y ≈ -conj(s)*x2 + c*y2
            end
        end
        @testset "axp(b)y" begin
            x1 = randn(elty, n)
            x2 = randn(elty, n)
            α  = rand(elty)
            β  = rand(elty)
            for X1 in (x1, view(x1,n:-1:1)), X2 in (x2, view(x2, n:-1:1))
                @test BLAS.axpy!(α,deepcopy(X1),deepcopy(X2)) ≈ α*X1 + X2
                @test BLAS.axpby!(α,deepcopy(X1),β,deepcopy(X2)) ≈ α*X1 + β*X2
            end
            for ind1 in (1:n, n:-1:1), ind2 in (1:n, n:-1:1)
                @test BLAS.axpy!(α,copy(x1),ind1,copy(x2),ind2) ≈ x2 + α*(ind1 == ind2 ? x1 : reverse(x1))
            end
            @test_throws DimensionMismatch BLAS.axpy!(α, copy(x1), rand(elty, n + 1))
            @test_throws DimensionMismatch BLAS.axpby!(α, copy(x1), β, rand(elty, n + 1))
            @test_throws DimensionMismatch BLAS.axpy!(α, copy(x1), 1:div(n,2), copy(x2), 1:n)
            @test_throws ArgumentError BLAS.axpy!(α, copy(x1), 0:div(n,2), copy(x2), 1:(div(n, 2) + 1))
            @test_throws ArgumentError BLAS.axpy!(α, copy(x1), 1:div(n,2), copy(x2), 0:(div(n, 2) - 1))
        end
        @testset "nrm2, iamax, and asum for StridedVectors" begin
            a = rand(elty,n)
            for ind in (2:2:n, n:-2:2)
                b = view(a, ind, 1)
                @test BLAS.nrm2(b) ≈ sqrt(sum(abs2, b))
                @test BLAS.asum(b) ≈ sum(fabs, b)
                @test BLAS.iamax(b) == findmax(fabs, b)[2] * (step(ind) >= 0)
            end
        end
        @testset "scal" begin
            α = rand(elty)
            a = rand(elty,n)
            @test BLAS.scal(n,α,a,1) ≈ α * a
            for v in (a, view(a, n:-1:1))
                @test BLAS.scal!(α, deepcopy(v)) ≈ α * v
            end
        end

        @testset "ger, her, syr" for x in (rand(elty, n), view(rand(elty,2n), 1:2:2n), view(rand(elty,n), n:-1:1)),
            y in (rand(elty,n), view(rand(elty,3n), 1:3:3n), view(rand(elty,2n), 2n:-2:2))

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
            x1 = randn(elty, n)
            x2 = randn(elty, n)
            for ind1 in (1:n, n:-1:1), ind2 in (1:n, n:-1:1)
                @test x2 === BLAS.copyto!(x2, ind1, x1, ind2) == (ind1 == ind2 ? x1 : reverse(x1))
            end
            @test_throws DimensionMismatch BLAS.copyto!(x2, 1:n, x1, 1:(n - 1))
            @test_throws ArgumentError BLAS.copyto!(x1, 0:div(n, 2), x2, 1:(div(n, 2) + 1))
            @test_throws ArgumentError BLAS.copyto!(x1, 1:(div(n, 2) + 1), x2, 0:div(n, 2))
        end
        @testset "trmv and trsv" begin
            A = rand(elty,n,n)
            x = rand(elty,n)
            xerr = Vector{elty}(undef,n+1)
            for uplo in ('U', 'L'), diag in ('U','N'), trans in ('N', 'T', 'C')
                Wrapper = if uplo == 'U'
                    diag == 'U' ? UnitUpperTriangular : UpperTriangular
                else
                    diag == 'U' ? UnitLowerTriangular : LowerTriangular
                end
                fun = trans == 'N' ? identity : trans == 'T' ? transpose : adjoint
                fullA = collect(fun(Wrapper(A)))
                @testset "trmv" begin
                    @test BLAS.trmv(uplo,trans,diag,A,x) ≈ fullA * x
                    @test_throws DimensionMismatch BLAS.trmv(uplo,trans,diag,A,xerr)
                    for xx in (x, view(x, n:-1:1))
                        @test BLAS.trmv!(uplo,trans,diag,A,deepcopy(xx)) ≈ fullA * xx
                    end
                end
                @testset "trsv" begin
                    @test BLAS.trsv(uplo,trans,diag,A,x) ≈ fullA \ x
                    @test_throws DimensionMismatch BLAS.trsv(uplo,trans,diag,A,xerr)
                    for xx in (x, view(x, n:-1:1))
                        @test BLAS.trsv!(uplo,trans,diag,A,deepcopy(xx)) ≈ fullA \ xx
                    end
                end
            end
        end
        @testset "symmetric/Hermitian multiplication" begin
            x = rand(elty,n)
            A = rand(elty,n,n)
            y = rand(elty, n)
            α = randn(elty)
            β = randn(elty)
            Aherm = A + A'
            Asymm = A + transpose(A)
            offsizevec, offsizemat = Array{elty}.(undef,(n+1, (n,n+1)))
            @testset "symv and hemv" for uplo in ('U', 'L')
                @test BLAS.symv(uplo,Asymm,x) ≈ Asymm*x
                for xx in (x, view(x, n:-1:1)), yy in (y, view(y, n:-1:1))
                    @test BLAS.symv!(uplo,α,Asymm,xx,β,deepcopy(yy)) ≈ α * Asymm * xx + β * yy
                end
                @test_throws DimensionMismatch BLAS.symv!(uplo,α,Asymm,x,β,offsizevec)
                @test_throws DimensionMismatch BLAS.symv(uplo,offsizemat,x)
                if elty <: BlasComplex
                    @test BLAS.hemv(uplo,Aherm,x) ≈ Aherm*x
                    for xx in (x, view(x, n:-1:1)), yy in (y, view(y, n:-1:1))
                        @test BLAS.hemv!(uplo,α,Aherm,xx,β,deepcopy(yy)) ≈ α * Aherm * xx + β * yy
                    end
                    @test_throws DimensionMismatch BLAS.hemv(uplo,offsizemat,x)
                    @test_throws DimensionMismatch BLAS.hemv!(uplo,one(elty),Aherm,x,one(elty),offsizevec)
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
                A = rand(elty, n, n)
                x = rand(elty, n)
                β = rand(elty)
                y = rand(elty, n)
                for uplo in (:L, :U)
                    Cuplo = String(uplo)[1]
                    AH = Hermitian(A, uplo)
                    # Create lower/upper triangular packing of AL
                    AP = pack(AH, uplo)
                    for xx in (x, view(x,n:-1:1)), yy in (y, view(y,n:-1:1))
                        @test BLAS.hpmv!(Cuplo, α, AP, xx, β, deepcopy(yy)) ≈ α*AH*xx + β*yy
                    end
                    AP′ = view(zeros(elty, n*(n+1)),1:2:n*(n+1))
                    @test_throws ErrorException BLAS.hpmv!(Cuplo, α, AP′, x, β, y)
                    AP′ = view(AP, 1:length(AP′) - 1)
                    @test_throws DimensionMismatch BLAS.hpmv!(Cuplo, α, AP′, x, β, y)
                    @test_throws DimensionMismatch BLAS.hpmv!(Cuplo, α, AP′, x, β, view(y,1:n-1))
                end
            end
        end

        # spmv!
        if elty in (Float32, Float64)
            @testset "spmv!" begin
                # Both matrix dimensions n coincide, as we have symmetric matrices.
                # Define the inputs and outputs of spmv!, y = α*A*x+β*y
                α = rand(elty)
                A = rand(elty, n, n)
                x = rand(elty, n)
                β = rand(elty)
                y = rand(elty, n)
                for uplo in (:L, :U)
                    Cuplo = String(uplo)[1]
                    AS = Symmetric(A, uplo)
                    # Create lower/upper triangular packing of AL
                    AP = pack(AS, uplo)
                    for xx in (x, view(x,n:-1:1)), yy in (y, view(y,n:-1:1))
                        @test BLAS.spmv!(Cuplo, α, AP, xx, β, deepcopy(yy)) ≈ α*AS*xx + β*yy
                    end
                    AP′ = view(zeros(elty, n*(n+1)),1:2:n*(n+1))
                    @test_throws ErrorException BLAS.spmv!(Cuplo, α, AP′, x, β, y)
                    AP′ = view(AP, 1:length(AP′) - 1)
                    @test_throws DimensionMismatch BLAS.spmv!(Cuplo, α, AP′, x, β, y)
                    @test_throws DimensionMismatch BLAS.spmv!(Cuplo, α, AP′, x, β, view(y,1:n-1))
                end
            end
        end

        # spr!
        if elty in (Float32, Float64)
            @testset "spr! $elty" begin
                α = rand(elty)
                M = rand(elty, n, n)
                AL = Symmetric(M, :L)
                AU = Symmetric(M, :U)
                for x in (rand(elty, n), view(rand(elty, n), n:-1:1))
                    ALP_result_julia_lower = pack(α*x*x' + AL, :L)
                    ALP_result_blas_lower = pack(AL, :L)
                    BLAS.spr!('L', α, x, ALP_result_blas_lower)
                    @test ALP_result_julia_lower ≈ ALP_result_blas_lower
                    ALP_result_blas_lower = append!(pack(AL, :L), ones(elty, 10))
                    BLAS.spr!('L', α, x, ALP_result_blas_lower)
                    @test ALP_result_julia_lower ≈ ALP_result_blas_lower[1:end-10]
                    ALP_result_blas_lower = reshape(pack(AL, :L), 1, length(ALP_result_julia_lower), 1)
                    BLAS.spr!('L', α, x, ALP_result_blas_lower)
                    @test ALP_result_julia_lower ≈ vec(ALP_result_blas_lower)

                    AUP_result_julia_upper = pack(α*x*x' + AU, :U)
                    AUP_result_blas_upper = pack(AU, :U)
                    BLAS.spr!('U', α, x, AUP_result_blas_upper)
                    @test AUP_result_julia_upper ≈ AUP_result_blas_upper
                    AUP_result_blas_upper = append!(pack(AU, :U), ones(elty, 10))
                    BLAS.spr!('U', α, x, AUP_result_blas_upper)
                    @test AUP_result_julia_upper ≈ AUP_result_blas_upper[1:end-10]
                    AUP_result_blas_upper = reshape(pack(AU, :U), 1, length(AUP_result_julia_upper), 1)
                    BLAS.spr!('U', α, x, AUP_result_blas_upper)
                    @test AUP_result_julia_upper ≈ vec(AUP_result_blas_upper)
                end
            end
        end

        #trsm
        A = triu(rand(elty,n,n))
        B = rand(elty,(n,n))
        @test BLAS.trsm('L','U','N','N',one(elty),A,B) ≈ A\B

        #will work for SymTridiagonal,Tridiagonal,Bidiagonal!
        @testset "banded matrix mv" begin
            @testset "gbmv" begin
                TD = Tridiagonal(rand(elty,n-1),rand(elty,n),rand(elty,n-1))
                x  = rand(elty, n)
                #put TD into the BLAS format!
                fTD = zeros(elty,3,n)
                fTD[1,2:n] = TD.du
                fTD[2,:] = TD.d
                fTD[3,1:n-1] = TD.dl
                @test BLAS.gbmv('N',n,1,1,fTD,x) ≈ TD*x
                y = rand(elty, n)
                α = randn(elty)
                β = randn(elty)
                for xx in (x, view(x, n:-1:1)), yy in (y, view(y, n:-1:1))
                    @test BLAS.gbmv!('N',n,1,1,α,fTD,xx,β,deepcopy(yy)) ≈ α * TD * xx + β * yy
                end
            end
            #will work for SymTridiagonal only!
            @testset "sbmv and hbmv" begin
                x = rand(elty,n)
                if elty <: BlasReal
                    ST  = SymTridiagonal(rand(elty,n),rand(elty,n-1))
                    #put TD into the BLAS format!
                    fST = zeros(elty,2,n)
                    fST[1,2:n] = ST.ev
                    fST[2,:] = ST.dv
                    @test BLAS.sbmv('U',1,fST,x) ≈ ST*x
                    y = rand(elty, n)
                    α = randn(elty)
                    β = randn(elty)
                    for xx in (x, view(x, n:-1:1)), yy in (y, view(y, n:-1:1))
                        @test BLAS.sbmv!('U',1,α,fST,xx,β,deepcopy(yy)) ≈ α * ST * xx + β * yy
                    end
                else
                    dv = rand(real(elty),n)
                    ev = rand(elty,n-1)
                    bH = zeros(elty,2,n)
                    bH[1,2:n] = ev
                    bH[2,:] = dv
                    fullH = diagm(0 => dv, -1 => conj(ev), 1 => ev)
                    @test BLAS.hbmv('U',1,bH,x) ≈ fullH*x
                    y = rand(elty, n)
                    α = randn(elty)
                    β = randn(elty)
                    for xx in (x, view(x, n:-1:1)), yy in (y, view(y, n:-1:1))
                        @test BLAS.hbmv!('U',1,α,bH,xx,β,deepcopy(yy)) ≈ α * fullH * xx + β * yy
                    end
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
        @testset "non-standard strides" begin
            A = rand(elty, 3, 4)
            x = rand(elty, 5)
            for y = (view(ones(elty, 5), 1:2:5), view(ones(elty, 7), 6:-2:2))
                ycopy = copy(y)
                @test BLAS.gemv!('N', elty(2), view(A, :, 2:2:4), view(x, 1:3:4), elty(3), y) ≈ 2*A[:,2:2:4]*x[1:3:4] + 3*ycopy
                ycopy = copy(y)
                @test BLAS.gemv!('N', elty(2), view(A, :, 4:-2:2), view(x, 1:3:4), elty(3), y) ≈ 2*A[:,4:-2:2]*x[1:3:4] + 3*ycopy
                ycopy = copy(y)
                @test BLAS.gemv!('N', elty(2), view(A, :, 2:2:4), view(x, 4:-3:1), elty(3), y) ≈ 2*A[:,2:2:4]*x[4:-3:1] + 3*ycopy
                ycopy = copy(y)
                @test BLAS.gemv!('N', elty(2), view(A, :, 4:-2:2), view(x, 4:-3:1), elty(3), y) ≈ 2*A[:,4:-2:2]*x[4:-3:1] + 3*ycopy
                ycopy = copy(y)
                @test BLAS.gemv!('N', elty(2), view(A, :, StepRangeLen(1,0,1)), view(x, 1:1), elty(3), y) ≈ 2*A[:,1:1]*x[1:1] + 3*ycopy # stride(A,2) == 0
            end
            @test BLAS.gemv!('N', elty(1), zeros(elty, 0, 5), zeros(elty, 5), elty(1), zeros(elty, 0)) == elty[] # empty matrix, stride(A,2) == 0
            @test BLAS.gemv('N', elty(-1), view(A, 2:3, 1:2:3), view(x, 2:-1:1)) ≈ -1*A[2:3,1:2:3]*x[2:-1:1]
            @test BLAS.gemv('N', view(A, 2:3, 3:-2:1), view(x, 1:2:3)) ≈ A[2:3,3:-2:1]*x[1:2:3]
            for (trans, f) = (('T',transpose), ('C',adjoint))
                for y = (view(ones(elty, 3), 1:2:3), view(ones(elty, 5), 4:-2:2))
                    ycopy = copy(y)
                    @test BLAS.gemv!(trans, elty(2), view(A, :, 2:2:4), view(x, 1:2:5), elty(3), y) ≈ 2*f(A[:,2:2:4])*x[1:2:5] + 3*ycopy
                    ycopy = copy(y)
                    @test BLAS.gemv!(trans, elty(2), view(A, :, 4:-2:2), view(x, 1:2:5), elty(3), y) ≈ 2*f(A[:,4:-2:2])*x[1:2:5] + 3*ycopy
                    ycopy = copy(y)
                    @test BLAS.gemv!(trans, elty(2), view(A, :, 2:2:4), view(x, 5:-2:1), elty(3), y) ≈ 2*f(A[:,2:2:4])*x[5:-2:1] + 3*ycopy
                    ycopy = copy(y)
                    @test BLAS.gemv!(trans, elty(2), view(A, :, 4:-2:2), view(x, 5:-2:1), elty(3), y) ≈ 2*f(A[:,4:-2:2])*x[5:-2:1] + 3*ycopy
                end
                @test BLAS.gemv!(trans, elty(2), view(A, :, StepRangeLen(1,0,1)), view(x, 1:2:5), elty(3), elty[1]) ≈ 2*f(A[:,1:1])*x[1:2:5] + elty[3] # stride(A,2) == 0
            end
            for trans = ('N', 'T', 'C')
                @test_throws ErrorException BLAS.gemv(trans, view(A, 1:2:3, 1:2), view(x, 1:2)) # stride(A,1) must be 1
            end
        end
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

@testset "syr for eltype $elty" for elty in (Float32, Float64, ComplexF32, ComplexF64)
    A = rand(elty, 5, 5)
    @test triu(A[1,:] * transpose(A[1,:])) ≈ BLAS.syr!('U', one(elty), A[1,:], zeros(elty, 5, 5))
    @test tril(A[1,:] * transpose(A[1,:])) ≈ BLAS.syr!('L', one(elty), A[1,:], zeros(elty, 5, 5))
    @test triu(A[1,:] * transpose(A[1,:])) ≈ BLAS.syr!('U', one(elty), view(A, 1, :), zeros(elty, 5, 5))
    @test tril(A[1,:] * transpose(A[1,:])) ≈ BLAS.syr!('L', one(elty), view(A, 1, :), zeros(elty, 5, 5))
end

@testset "her for eltype $elty" for elty in (ComplexF32, ComplexF64)
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

Base.strides(A::WrappedArray) = strides(A.A)
Base.elsize(::Type{WrappedArray{T,N}}) where {T,N} = Base.elsize(Array{T,N})

@testset "strided interface adjtrans" begin
    x = WrappedArray([1, 2, 3, 4])
    @test stride(x,1) == 1
    @test stride(x,2) == stride(x,3) == 4
    @test strides(x') == strides(transpose(x)) == (4,1)
    @test pointer(x') == pointer(transpose(x)) == pointer(x)
    @test_throws BoundsError stride(x,0)

    A = WrappedArray([1 2; 3 4; 5 6])
    @test stride(A,1) == 1
    @test stride(A,2) == 3
    @test stride(A,3) == stride(A,4) >= 6
    @test strides(A') == strides(transpose(A)) == (3,1)
    @test pointer(A') == pointer(transpose(A)) == pointer(A)
    @test_throws BoundsError stride(A,0)

    y = WrappedArray([1+im, 2, 3, 4])
    @test strides(transpose(y)) == (4,1)
    @test pointer(transpose(y)) == pointer(y)
    @test_throws MethodError strides(y')
    @test_throws ErrorException pointer(y')

    B = WrappedArray([1+im 2; 3 4; 5 6])
    @test strides(transpose(B)) == (3,1)
    @test pointer(transpose(B)) == pointer(B)
    @test_throws MethodError strides(B')
    @test_throws ErrorException pointer(B')

    @test_throws MethodError stride(1:5,0)
    @test_throws MethodError stride(1:5,1)
    @test_throws MethodError stride(1:5,2)
    @test_throws MethodError strides(transpose(1:5))
    @test_throws MethodError strides((1:5)')
    @test_throws ErrorException pointer(transpose(1:5))
    @test_throws ErrorException pointer((1:5)')
end

@testset "strided interface blas" begin
    for elty in (Float32, Float64, ComplexF32, ComplexF64)
    # Level 1
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

        M = fill(elty(1.0), 3, 3)
        @test BLAS.scal!(elty(2), view(M,:,2)) === view(M,:,2)
        @test BLAS.scal!(elty(3), view(M,3,:)) === view(M,3,:)
        @test M == elty[1. 2. 1.; 1. 2. 1.; 3. 6. 3.]
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
    # Level 2
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
    @test default isa Int
    @test default > 0
    BLAS.set_num_threads(1)
    @test BLAS.get_num_threads() === 1
    BLAS.set_num_threads(default)
    @test BLAS.get_num_threads() === default
end

@testset "test for 0-strides" for elty in (Float32, Float64, ComplexF32, ComplexF64)
    A = randn(elty, 10, 10);
    a = view([randn(elty)], 1 .+ 0(1:10))
    b = view([randn(elty)], 1 .+ 0(1:10))
    α, β = randn(elty), randn(elty)
    @testset "dot/dotc/dotu" begin
        if elty <: Real
            @test BLAS.dot(a,b) ≈ sum(a.*b)
        else
            @test BLAS.dotc(a,b) ≈ sum(conj(a).*b)
            @test BLAS.dotu(a,b) ≈ sum(a.*b)
        end
    end
    @testset "axp(b)y!" begin
        @test BLAS.axpy!(α,a,copy(b)) ≈ α*a + b
        @test BLAS.axpby!(α,a,β,copy(b)) ≈ α*a + β*b
        @test_throws "dest" BLAS.axpy!(α,a,b)
        @test_throws "dest" BLAS.axpby!(α,a,β,b)
    end
    @test BLAS.iamax(a) == 0
    @test_throws "dest" BLAS.scal!(b[1], a)
    @testset "nrm2/asum" begin # OpenBLAS allways return 0.0
        @test_throws "input" BLAS.nrm2(a)
        @test_throws "input" BLAS.asum(a)
    end
    # All level2 reject 0-stride array.
    @testset "gemv!" begin
        @test_throws "input" BLAS.gemv!('N', true, A, a, false, copy(b))
        @test_throws "dest" BLAS.gemv!('N', true, A, copy(a), false, b)
    end
end

end # module TestBLAS
