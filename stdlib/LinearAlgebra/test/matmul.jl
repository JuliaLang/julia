# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestMatmul

using Test, LinearAlgebra, Random
using LinearAlgebra: mul!

## Test Julia fallbacks to BLAS routines

@testset "matrices with zero dimensions" begin
    for (dimsA, dimsB, dimsC) in (
            ((0,5), (5,3), (0,3)),
            ((3,5), (5,0), (3,0)),
            ((3,0), (0,4), (3,4)),
            ((0,5), (5,0), (0,0)),
            ((0,0), (0,4), (0,4)),
            ((3,0), (0,0), (3,0)),
            ((0,0), (0,0), (0,0)) )
        @test Matrix{Float64}(undef, dimsA) * Matrix{Float64}(undef, dimsB) == zeros(dimsC)
    end
    @test Matrix{Float64}(undef, 5, 0) |> t -> t't == zeros(0,0)
    @test Matrix{Float64}(undef, 5, 0) |> t -> t*t' == zeros(5,5)
    @test Matrix{ComplexF64}(undef, 5, 0) |> t -> t't == zeros(0,0)
    @test Matrix{ComplexF64}(undef, 5, 0) |> t -> t*t' == zeros(5,5)
end
@testset "2x2 matmul" begin
    AA = [1 2; 3 4]
    BB = [5 6; 7 8]
    AAi = AA+(0.5*im).*BB
    BBi = BB+(2.5*im).*AA[[2,1],[2,1]]
    for A in (copy(AA), view(AA, 1:2, 1:2)), B in (copy(BB), view(BB, 1:2, 1:2))
        @test A*B == [19 22; 43 50]
        @test *(transpose(A), B) == [26 30; 38 44]
        @test *(A, transpose(B)) == [17 23; 39 53]
        @test *(transpose(A), transpose(B)) == [23 31; 34 46]
    end
    for Ai in (copy(AAi), view(AAi, 1:2, 1:2)), Bi in (copy(BBi), view(BBi, 1:2, 1:2))
        @test Ai*Bi == [-21+53.5im -4.25+51.5im; -12+95.5im 13.75+85.5im]
        @test *(adjoint(Ai), Bi) == [68.5-12im 57.5-28im; 88-3im 76.5-25im]
        @test *(Ai, adjoint(Bi)) == [64.5+5.5im 43+31.5im; 104-18.5im 80.5+31.5im]
        @test *(adjoint(Ai), adjoint(Bi)) == [-28.25-66im 9.75-58im; -26-89im 21-73im]
        @test_throws DimensionMismatch [1 2; 0 0; 0 0] * [1 2]
    end
    @test_throws DimensionMismatch mul!(Matrix{Float64}(undef,3,3), AA, BB)
end
@testset "3x3 matmul" begin
    AA = [1 2 3; 4 5 6; 7 8 9].-5
    BB = [1 0 5; 6 -10 3; 2 -4 -1]
    AAi = AA+(0.5*im).*BB
    BBi = BB+(2.5*im).*AA[[2,1,3],[2,3,1]]
    for A in (copy(AA), view(AA, 1:3, 1:3)), B in (copy(BB), view(BB, 1:3, 1:3))
        @test A*B == [-26 38 -27; 1 -4 -6; 28 -46 15]
        @test *(adjoint(A), B) == [-6 2 -25; 3 -12 -18; 12 -26 -11]
        @test *(A, adjoint(B)) == [-14 0 6; 4 -3 -3; 22 -6 -12]
        @test *(adjoint(A), adjoint(B)) == [6 -8 -6; 12 -9 -9; 18 -10 -12]
    end
    for Ai in (copy(AAi), view(AAi, 1:3, 1:3)), Bi in (copy(BBi), view(BBi, 1:3, 1:3))
        @test Ai*Bi == [-44.75+13im 11.75-25im -38.25+30im; -47.75-16.5im -51.5+51.5im -56+6im; 16.75-4.5im -53.5+52im -15.5im]
        @test *(adjoint(Ai), Bi) == [-21+2im -1.75+49im -51.25+19.5im; 25.5+56.5im -7-35.5im 22+35.5im; -3+12im -32.25+43im -34.75-2.5im]
        @test *(Ai, adjoint(Bi)) == [-20.25+15.5im -28.75-54.5im 22.25+68.5im; -12.25+13im -15.5+75im -23+27im; 18.25+im 1.5+94.5im -27-54.5im]
        @test *(adjoint(Ai), adjoint(Bi)) == [1+2im 20.75+9im -44.75+42im; 19.5+17.5im -54-36.5im 51-14.5im; 13+7.5im 11.25+31.5im -43.25-14.5im]
        @test_throws DimensionMismatch [1 2 3; 0 0 0; 0 0 0] * [1 2 3]
    end
    @test_throws DimensionMismatch mul!(Matrix{Float64}(undef,4,4), AA, BB)
end

# Generic AbstractArrays
module MyArray15367
    using Test, Random

    struct MyArray{T,N} <: AbstractArray{T,N}
        data::Array{T,N}
    end

    Base.size(A::MyArray) = size(A.data)
    Base.getindex(A::MyArray, indices...) = A.data[indices...]

    A = MyArray(rand(4,5))
    b = rand(5)
    @test A*b ≈ A.data*b
end

@testset "Generic integer matrix multiplication" begin
    AA = [1 2 3; 4 5 6] .- 3
    BB = [2 -2; 3 -5; -4 7]
    for A in (copy(AA), view(AA, 1:2, 1:3)), B in (copy(BB), view(BB, 1:3, 1:2))
        @test A*B == [-7 9; -4 9]
        @test *(transpose(A), transpose(B)) == [-6 -11 15; -6 -13 18; -6 -15 21]
    end
    AA = fill(1, 2, 100)
    BB = fill(1, 100, 3)
    for A in (copy(AA), view(AA, 1:2, 1:100)), B in (copy(BB), view(BB, 1:100, 1:3))
        @test A*B == [100 100 100; 100 100 100]
    end
    AA = rand(1:20, 5, 5) .- 10
    BB = rand(1:20, 5, 5) .- 10
    CC = Matrix{Int}(undef, size(AA, 1), size(BB, 2))
    for A in (copy(AA), view(AA, 1:5, 1:5)), B in (copy(BB), view(BB, 1:5, 1:5)), C in (copy(CC), view(CC, 1:5, 1:5))
        @test *(transpose(A), B) == A'*B
        @test *(A, transpose(B)) == A*B'
        # Preallocated
        @test mul!(C, A, B) == A*B
        @test mul!(C, transpose(A), B) == A'*B
        @test mul!(C, A, transpose(B)) == A*B'
        @test mul!(C, transpose(A), transpose(B)) == A'*B'
        @test LinearAlgebra.mul!(C, adjoint(A), transpose(B)) == A'*transpose(B)

        #test DimensionMismatch for generic_matmatmul
        @test_throws DimensionMismatch LinearAlgebra.mul!(C, adjoint(A), transpose(fill(1,4,4)))
        @test_throws DimensionMismatch LinearAlgebra.mul!(C, adjoint(fill(1,4,4)), transpose(B))
    end
    vv = [1,2]
    CC = Matrix{Int}(undef, 2, 2)
    for v in (copy(vv), view(vv, 1:2)), C in (copy(CC), view(CC, 1:2, 1:2))
        @test @inferred(mul!(C, v, adjoint(v))) == [1 2; 2 4]
    end
end

@testset "generic_matvecmul" begin
    AA = rand(5,5)
    BB = rand(5)
    for A in (copy(AA), view(AA, 1:5, 1:5)), B in (copy(BB), view(BB, 1:5))
        @test_throws DimensionMismatch LinearAlgebra.generic_matvecmul!(zeros(6),'N',A,B)
        @test_throws DimensionMismatch LinearAlgebra.generic_matvecmul!(B,'N',A,zeros(6))
    end
    vv = [1,2,3]
    CC = Matrix{Int}(undef, 3, 3)
    for v in (copy(vv), view(vv, 1:3)), C in (copy(CC), view(CC, 1:3, 1:3))
        @test mul!(C, v, transpose(v)) == v*v'
    end
    vvf = map(Float64,vv)
    CC = Matrix{Float64}(undef, 3, 3)
    for vf in (copy(vvf), view(vvf, 1:3)), C in (copy(CC), view(CC, 1:3, 1:3))
        @test mul!(C, vf, transpose(vf)) == vf*vf'
    end
end

@testset "fallbacks & such for BlasFloats" begin
    AA = rand(Float64,6,6)
    BB = rand(Float64,6,6)
    CC = zeros(Float64,6,6)
    for A in (copy(AA), view(AA, 1:6, 1:6)), B in (copy(BB), view(BB, 1:6, 1:6)), C in (copy(CC), view(CC, 1:6, 1:6))
        @test LinearAlgebra.mul!(C, transpose(A), transpose(B)) == transpose(A)*transpose(B)
        @test LinearAlgebra.mul!(C, A, adjoint(B)) == A*transpose(B)
        @test LinearAlgebra.mul!(C, adjoint(A), B) == transpose(A)*B
    end
end

@testset "matrix algebra with subarrays of floats (stride != 1)" begin
    A = reshape(map(Float64,1:20),5,4)
    Aref = A[1:2:end,1:2:end]
    Asub = view(A, 1:2:5, 1:2:4)
    b = [1.2,-2.5]
    @test (Aref*b) == (Asub*b)
    @test *(transpose(Asub), Asub) == *(transpose(Aref), Aref)
    @test *(Asub, transpose(Asub)) == *(Aref, transpose(Aref))
    Ai = A .+ im
    Aref = Ai[1:2:end,1:2:end]
    Asub = view(Ai, 1:2:5, 1:2:4)
    @test *(adjoint(Asub), Asub) == *(adjoint(Aref), Aref)
    @test *(Asub, adjoint(Asub)) == *(Aref, adjoint(Aref))
end

@testset "issue #15286" begin
    A = reshape(map(Float64, 1:20), 5, 4)
    C = zeros(8, 8)
    sC = view(C, 1:2:8, 1:2:8)
    B = reshape(map(Float64,-9:10),5,4)
    @test mul!(sC, transpose(A), A) == A'*A
    @test mul!(sC, transpose(A), B) == A'*B

    Aim = A .- im
    C = zeros(ComplexF64,8,8)
    sC = view(C, 1:2:8, 1:2:8)
    B = reshape(map(Float64,-9:10),5,4) .+ im
    @test mul!(sC, adjoint(Aim), Aim) == Aim'*Aim
    @test mul!(sC, adjoint(Aim), B) == Aim'*B
end

@testset "syrk & herk" begin
    AA = reshape(1:1503, 501, 3).-750.0
    res = Float64[135228751 9979252 -115270247; 9979252 10481254 10983256; -115270247 10983256 137236759]
    for A in (copy(AA), view(AA, 1:501, 1:3))
        @test *(transpose(A), A) == res
        @test *(adjoint(A), transpose(copy(A'))) == res
    end
    cutoff = 501
    A = reshape(1:6*cutoff,2*cutoff,3).-(6*cutoff)/2
    Asub = view(A, 1:2:2*cutoff, 1:3)
    Aref = A[1:2:2*cutoff, 1:3]
    @test *(transpose(Asub), Asub) == *(transpose(Aref), Aref)
    Ai = A .- im
    Asub = view(Ai, 1:2:2*cutoff, 1:3)
    Aref = Ai[1:2:2*cutoff, 1:3]
    @test *(adjoint(Asub), Asub) == *(adjoint(Aref), Aref)

    A5x5, A6x5 = Matrix{Float64}.(undef, ((5, 5), (6, 5)))
    @test_throws DimensionMismatch LinearAlgebra.syrk_wrapper!(A5x5,'N',A6x5)
    @test_throws DimensionMismatch LinearAlgebra.herk_wrapper!(A5x5,'N',A6x5)
end

@testset "matmul for types w/o sizeof (issue #1282)" begin
    AA = fill(complex(1,1), 10, 10)
    for A in (copy(AA), view(AA, 1:10, 1:10))
        A2 = A^2
        @test A2[1,1] == 20im
    end
end

@testset "mul! (scaling)" begin
    A5x5, b5, C5x6 = Array{Float64}.(undef,((5,5), 5, (5,6)))
    for A in (A5x5, view(A5x5, :, :)), b in (b5,  view(b5, :)), C in (C5x6, view(C5x6, :, :))
        @test_throws DimensionMismatch mul!(A, Diagonal(b), C)
    end
end

# issue #6450
@test dot(Any[1.0,2.0], Any[3.5,4.5]) === 12.5

@testset "dot" for elty in (Float32, Float64, ComplexF32, ComplexF64)
    x = convert(Vector{elty},[1.0, 2.0, 3.0])
    y = convert(Vector{elty},[3.5, 4.5, 5.5])
    @test_throws DimensionMismatch dot(x, 1:2, y, 1:3)
    @test_throws BoundsError dot(x, 1:4, y, 1:4)
    @test_throws BoundsError dot(x, 1:3, y, 2:4)
    @test dot(x, 1:2, y, 1:2) == convert(elty, 12.5)
    @test transpose(x)*y == convert(elty, 29.0)
    X = convert(Matrix{elty},[1.0 2.0; 3.0 4.0])
    Y = convert(Matrix{elty},[1.5 2.5; 3.5 4.5])
    @test dot(X, Y) == convert(elty, 35.0)
    Z = convert(Vector{Matrix{elty}},[reshape(1:4, 2, 2), fill(1, 2, 2)])
    @test dot(Z, Z) == convert(elty, 34.0)
end

dot1(x,y) = invoke(dot, Tuple{Any,Any}, x,y)
dot2(x,y) = invoke(dot, Tuple{AbstractArray,AbstractArray}, x,y)
@testset "generic dot" begin
    AA = [1+2im 3+4im; 5+6im 7+8im]
    BB = [2+7im 4+1im; 3+8im 6+5im]
    for A in (copy(AA), view(AA, 1:2, 1:2)), B in (copy(BB), view(BB, 1:2, 1:2))
        @test dot(A,B) == dot(vec(A),vec(B)) == dot1(A,B) == dot2(A,B) == dot(float.(A),float.(B))
        @test dot(Int[], Int[]) == 0 == dot1(Int[], Int[]) == dot2(Int[], Int[])
        @test_throws MethodError dot(Any[], Any[])
        @test_throws MethodError dot1(Any[], Any[])
        @test_throws MethodError dot2(Any[], Any[])
        for n1 = 0:2, n2 = 0:2, d in (dot, dot1, dot2)
            if n1 != n2
                @test_throws DimensionMismatch d(1:n1, 1:n2)
            else
                @test d(1:n1, 1:n2) ≈ norm(1:n1)^2
            end
        end
    end
end

@testset "Issue 11978" begin
    A = Matrix{Matrix{Float64}}(undef, 2, 2)
    A[1,1] = Matrix(1.0I, 3, 3)
    A[2,2] = Matrix(1.0I, 2, 2)
    A[1,2] = Matrix(1.0I, 3, 2)
    A[2,1] = Matrix(1.0I, 2, 3)
    b = Vector{Vector{Float64}}(undef, 2)
    b[1] = fill(1., 3)
    b[2] = fill(1., 2)
    @test A*b == Vector{Float64}[[2,2,1], [2,2]]
end

@test_throws ArgumentError LinearAlgebra.copytri!(Matrix{Float64}(undef,10,10),'Z')

@testset "gemv! and gemm_wrapper for $elty" for elty in [Float32,Float64,ComplexF64,ComplexF32]
    A10x10, x10, x11 = Array{elty}.(undef, ((10,10), 10, 11))
    @test_throws DimensionMismatch LinearAlgebra.gemv!(x10,'N',A10x10,x11)
    @test_throws DimensionMismatch LinearAlgebra.gemv!(x11,'N',A10x10,x10)
    @test LinearAlgebra.gemv!(elty[], 'N', Matrix{elty}(undef,0,0), elty[]) == elty[]
    @test LinearAlgebra.gemv!(x10, 'N', Matrix{elty}(undef,10,0), elty[]) == zeros(elty,10)

    I0x0 = Matrix{elty}(I, 0, 0)
    I10x10 = Matrix{elty}(I, 10, 10)
    I10x11 = Matrix{elty}(I, 10, 11)
    @test LinearAlgebra.gemm_wrapper('N','N', I10x10, I10x10) == I10x10
    @test_throws DimensionMismatch LinearAlgebra.gemm_wrapper!(I10x10,'N','N', I10x11, I10x10)
    @test_throws DimensionMismatch LinearAlgebra.gemm_wrapper!(I10x10,'N','N', I0x0, I0x0)

    A = rand(elty,3,3)
    @test LinearAlgebra.matmul3x3('T','N',A, Matrix{elty}(I, 3, 3)) == transpose(A)
end

@testset "#13593, #13488" begin
    aa = rand(3,3)
    bb = rand(3,3)
    for a in (copy(aa), view(aa, 1:3, 1:3)), b in (copy(bb), view(bb, 1:3, 1:3))
        @test_throws ArgumentError mul!(a, a, b)
        @test_throws ArgumentError mul!(a, b, a)
        @test_throws ArgumentError mul!(a, a, a)
    end
end

# Number types that lack conversion to the destination type
struct RootInt
    i::Int
end
import Base: *, adjoint, transpose
import LinearAlgebra: Adjoint, Transpose
(*)(x::RootInt, y::RootInt) = x.i*y.i
adjoint(x::RootInt) = x
transpose(x::RootInt) = x
Adjoint(x::RootInt) = x
Transpose(x::RootInt) = x
# TODO once Adjoint/Transpose constructors call adjoint/transpose recursively
# rather than Adjoint/Transpose, the additional definitions should become unnecessary

@test Base.promote_op(*, RootInt, RootInt) === Int

@testset "#14293" begin
    a = [RootInt(3)]
    C = [0]
    mul!(C, a, transpose(a))
    @test C[1] == 9
    a = [RootInt(2),RootInt(10)]
    @test a*adjoint(a) == [4 20; 20 100]
    A = [RootInt(3) RootInt(5)]
    @test A*a == [56]
end

function test_mul(C, A, B)
    mul!(C, A, B)
    @test Array(A) * Array(B) ≈ C
    @test A*B ≈ C
end

@testset "mul! vs * for special types" begin
    eltypes = [Float32, Float64, Int64]
    for k in [3, 4, 10]
        T = rand(eltypes)
        bi1 = Bidiagonal(rand(T, k), rand(T, k-1), rand([:U, :L]))
        bi2 = Bidiagonal(rand(T, k), rand(T, k-1), rand([:U, :L]))
        tri1 = Tridiagonal(rand(T,k-1), rand(T, k), rand(T, k-1))
        tri2 = Tridiagonal(rand(T,k-1), rand(T, k), rand(T, k-1))
        stri1 = SymTridiagonal(rand(T, k), rand(T, k-1))
        stri2 = SymTridiagonal(rand(T, k), rand(T, k-1))
        C = rand(T, k, k)
        specialmatrices = (bi1, bi2, tri1, tri2, stri1, stri2)
        for A in specialmatrices
            B = specialmatrices[rand(1:length(specialmatrices))]
            test_mul(C, A, B)
        end
        for S in specialmatrices
            l = rand(1:6)
            B = randn(k, l)
            C = randn(k, l)
            test_mul(C, S, B)
            A = randn(l, k)
            C = randn(l, k)
            test_mul(C, A, S)
        end
    end
    for T in eltypes
        A = Bidiagonal(rand(T, 2), rand(T, 1), rand([:U, :L]))
        B = Bidiagonal(rand(T, 2), rand(T, 1), rand([:U, :L]))
        C = randn(2,2)
        test_mul(C, A, B)
        B = randn(2, 9)
        C = randn(2, 9)
        test_mul(C, A, B)
    end
    let
        tri44 = Tridiagonal(randn(3), randn(4), randn(3))
        tri33 = Tridiagonal(randn(2), randn(3), randn(2))
        full43 = randn(4, 3)
        full24 = randn(2, 4)
        full33 = randn(3, 3)
        full44 = randn(4, 4)
        @test_throws DimensionMismatch mul!(full43, tri44, tri33)
        @test_throws DimensionMismatch mul!(full44, tri44, tri33)
        @test_throws DimensionMismatch mul!(full44, tri44, full43)
        @test_throws DimensionMismatch mul!(full43, tri33, full43)
        @test_throws DimensionMismatch mul!(full43, full43, tri44)
    end
end

# #18218
module TestPR18218
    using Test
    import Base.*, Base.+, Base.zero
    struct TypeA
        x::Int
    end
    Base.convert(::Type{TypeA}, x::Int) = TypeA(x)
    struct TypeB
        x::Int
    end
    struct TypeC
        x::Int
    end
    Base.convert(::Type{TypeC}, x::Int) = TypeC(x)
    zero(c::TypeC) = TypeC(0)
    zero(::Type{TypeC}) = TypeC(0)
    (*)(x::Int, a::TypeA) = TypeB(x*a.x)
    (*)(a::TypeA, x::Int) = TypeB(a.x*x)
    (+)(a::Union{TypeB,TypeC}, b::Union{TypeB,TypeC}) = TypeC(a.x+b.x)
    A = TypeA[1 2; 3 4]
    b = [1, 2]
    d = A * b
    @test typeof(d) == Vector{TypeC}
    @test d == TypeC[5, 11]
end

@testset "VecOrMat of Vectors" begin
    X   = rand(ComplexF64, 3, 3)
    Xv1 = [X[:,j] for i in 1:1, j in 1:3]
    Xv2 = [transpose(X[i,:]) for i in 1:3]
    Xv3 = [transpose(X[i,:]) for i in 1:3, j in 1:1]

    XX   = X*X
    XtX  = transpose(X)*X
    XcX  = X'*X
    XXt  = X*transpose(X)
    XtXt = transpose(XX)
    XcXt = X'*transpose(X)
    XXc  = X*X'
    XtXc = transpose(X)*X'
    XcXc = X'*X'

    @test (Xv1*Xv2)[1] ≈ XX
    @test (Xv1*Xv3)[1] ≈ XX
    @test  transpose(Xv1)*Xv1     ≈ XtX
    @test  transpose(Xv2)*Xv2     ≈ XtX
    @test (transpose(Xv3)*Xv3)[1] ≈ XtX
    @test  Xv1'*Xv1     ≈ XcX
    @test Xv2'*Xv2 ≈ norm(Xv2)^2
    @test (Xv3'*Xv3)[1] ≈ XcX
    @test (Xv1*transpose(Xv1))[1] ≈ XXt
    @test  Xv2*transpose(Xv2)     ≈ XXt
    @test  Xv3*transpose(Xv3)     ≈ XXt
    @test transpose(Xv1)*transpose(Xv2) ≈ XtXt
    @test transpose(Xv1)*transpose(Xv3) ≈ XtXt
    @test  Xv1'*transpose(Xv2) ≈ XcXt
    @test  Xv1'*transpose(Xv3) ≈ XcXt
    @test (Xv1*Xv1')[1] ≈ XXc
    @test  Xv2*Xv2'     ≈ XXc
    @test  Xv3*Xv3'     ≈ XXc
    @test transpose(Xv1)*Xv2' ≈ XtXc
    @test transpose(Xv1)*Xv3' ≈ XtXc
    @test Xv1'*Xv2' ≈ XcXc
    @test Xv1'*Xv3' ≈ XcXc
end

@testset "method ambiguity" begin
    # Ambiguity test is run inside a clean process.
    # https://github.com/JuliaLang/julia/issues/28804
    script = joinpath(@__DIR__, "ambiguous_exec.jl")
    cmd = `$(Base.julia_cmd()) --startup-file=no $script`
    @test success(pipeline(cmd; stdout=stdout, stderr=stderr))
end

end # module TestMatmul
