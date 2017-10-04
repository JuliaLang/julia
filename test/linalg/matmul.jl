# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

## Test Julia fallbacks to BLAS routines

# matrices with zero dimensions
@test ones(0,5)*ones(5,3) == zeros(0,3)
@test ones(3,5)*ones(5,0) == zeros(3,0)
@test ones(3,0)*ones(0,4) == zeros(3,4)
@test ones(0,5)*ones(5,0) == zeros(0,0)
@test ones(0,0)*ones(0,4) == zeros(0,4)
@test ones(3,0)*ones(0,0) == zeros(3,0)
@test ones(0,0)*ones(0,0) == zeros(0,0)
@test Array{Float64}(5, 0) |> t -> t't == zeros(0,0)
@test Array{Float64}(5, 0) |> t -> t*t' == zeros(5,5)
@test Array{Complex128}(5, 0) |> t -> t't == zeros(0,0)
@test Array{Complex128}(5, 0) |> t -> t*t' == zeros(5,5)

# 2x2
let
    AA = [1 2; 3 4]
    BB = [5 6; 7 8]
    AAi = AA+(0.5*im).*BB
    BBi = BB+(2.5*im).*AA[[2,1],[2,1]]
    for A in (copy(AA), view(AA, 1:2, 1:2)), B in (copy(BB), view(BB, 1:2, 1:2))
        @test A*B == [19 22; 43 50]
        @test At_mul_B(A, B) == [26 30; 38 44]
        @test A_mul_Bt(A, B) == [17 23; 39 53]
        @test At_mul_Bt(A, B) == [23 31; 34 46]
    end
    for Ai in (copy(AAi), view(AAi, 1:2, 1:2)), Bi in (copy(BBi), view(BBi, 1:2, 1:2))
        @test Ai*Bi == [-21+53.5im -4.25+51.5im; -12+95.5im 13.75+85.5im]
        @test Ac_mul_B(Ai, Bi) == [68.5-12im 57.5-28im; 88-3im 76.5-25im]
        @test A_mul_Bc(Ai, Bi) == [64.5+5.5im 43+31.5im; 104-18.5im 80.5+31.5im]
        @test Ac_mul_Bc(Ai, Bi) == [-28.25-66im 9.75-58im; -26-89im 21-73im]
        @test_throws DimensionMismatch [1 2; 0 0; 0 0] * [1 2]
    end
    CC = ones(3, 3)
    @test_throws DimensionMismatch A_mul_B!(CC, AA, BB)
end
# 3x3
let
    AA = [1 2 3; 4 5 6; 7 8 9].-5
    BB = [1 0 5; 6 -10 3; 2 -4 -1]
    AAi = AA+(0.5*im).*BB
    BBi = BB+(2.5*im).*AA[[2,1,3],[2,3,1]]
    for A in (copy(AA), view(AA, 1:3, 1:3)), B in (copy(BB), view(BB, 1:3, 1:3))
        @test A*B == [-26 38 -27; 1 -4 -6; 28 -46 15]
        @test Ac_mul_B(A, B) == [-6 2 -25; 3 -12 -18; 12 -26 -11]
        @test A_mul_Bc(A, B) == [-14 0 6; 4 -3 -3; 22 -6 -12]
        @test Ac_mul_Bc(A, B) == [6 -8 -6; 12 -9 -9; 18 -10 -12]
    end
    for Ai in (copy(AAi), view(AAi, 1:3, 1:3)), Bi in (copy(BBi), view(BBi, 1:3, 1:3))
        @test Ai*Bi == [-44.75+13im 11.75-25im -38.25+30im; -47.75-16.5im -51.5+51.5im -56+6im; 16.75-4.5im -53.5+52im -15.5im]
        @test Ac_mul_B(Ai, Bi) == [-21+2im -1.75+49im -51.25+19.5im; 25.5+56.5im -7-35.5im 22+35.5im; -3+12im -32.25+43im -34.75-2.5im]
        @test A_mul_Bc(Ai, Bi) == [-20.25+15.5im -28.75-54.5im 22.25+68.5im; -12.25+13im -15.5+75im -23+27im; 18.25+im 1.5+94.5im -27-54.5im]
        @test Ac_mul_Bc(Ai, Bi) == [1+2im 20.75+9im -44.75+42im; 19.5+17.5im -54-36.5im 51-14.5im; 13+7.5im 11.25+31.5im -43.25-14.5im]
        @test_throws DimensionMismatch [1 2 3; 0 0 0; 0 0 0] * [1 2 3]
    end
    CC = ones(4, 4)
    @test_throws DimensionMismatch A_mul_B!(CC, AA, BB)
end
# Generic integer matrix multiplication
# Generic AbstractArrays
module MyArray15367
    using Test
    struct MyArray{T,N} <: AbstractArray{T,N}
        data::Array{T,N}
    end

    Base.size(A::MyArray) = size(A.data)
    Base.getindex(A::MyArray, indexes...) = A.data[indexes...]

    A = MyArray(rand(4,5))
    b = rand(5)
    @test A*b ≈ A.data*b
end

let
    AA = [1 2 3; 4 5 6] .- 3
    BB = [2 -2; 3 -5; -4 7]
    for A in (copy(AA), view(AA, 1:2, 1:3)), B in (copy(BB), view(BB, 1:3, 1:2))
        @test A*B == [-7 9; -4 9]
        @test At_mul_Bt(A, B) == [-6 -11 15; -6 -13 18; -6 -15 21]
    end
    AA = ones(Int, 2, 100)
    BB = ones(Int, 100, 3)
    for A in (copy(AA), view(AA, 1:2, 1:100)), B in (copy(BB), view(BB, 1:100, 1:3))
        @test A*B == [100 100 100; 100 100 100]
    end
    AA = rand(1:20, 5, 5) .- 10
    BB = rand(1:20, 5, 5) .- 10
    CC = Array{Int}(size(AA, 1), size(BB, 2))
    for A in (copy(AA), view(AA, 1:5, 1:5)), B in (copy(BB), view(BB, 1:5, 1:5)), C in (copy(CC), view(CC, 1:5, 1:5))
        @test At_mul_B(A, B) == A'*B
        @test A_mul_Bt(A, B) == A*B'
        # Preallocated
        @test A_mul_B!(C, A, B) == A*B
        @test At_mul_B!(C, A, B) == A'*B
        @test A_mul_Bt!(C, A, B) == A*B'
        @test At_mul_Bt!(C, A, B) == A'*B'
        @test Base.LinAlg.Ac_mul_Bt!(C, A, B) == A'*B.'

        #test DimensionMismatch for generic_matmatmul
        @test_throws DimensionMismatch Base.LinAlg.Ac_mul_Bt!(C,A,ones(Int,4,4))
        @test_throws DimensionMismatch Base.LinAlg.Ac_mul_Bt!(C,ones(Int,4,4),B)
    end
    vv = [1,2]
    CC = Array{Int}(2, 2)
    for v in (copy(vv), view(vv, 1:2)), C in (copy(CC), view(CC, 1:2, 1:2))
        @test @inferred(A_mul_Bc!(C, v, v)) == [1 2; 2 4]
    end
end

#and for generic_matvecmul
let
    AA = rand(5,5)
    BB = rand(5)
    for A in (copy(AA), view(AA, 1:5, 1:5)), B in (copy(BB), view(BB, 1:5))
        @test_throws DimensionMismatch Base.LinAlg.generic_matvecmul!(zeros(6),'N',A,B)
        @test_throws DimensionMismatch Base.LinAlg.generic_matvecmul!(B,'N',A,zeros(6))
    end
    vv = [1,2,3]
    CC = Array{Int}(3, 3)
    for v in (copy(vv), view(vv, 1:3)), C in (copy(CC), view(CC, 1:3, 1:3))
        @test A_mul_Bt!(C, v, v) == v*v'
    end
    vvf = map(Float64,vv)
    CC = Array{Float64}(3, 3)
    for vf in (copy(vvf), view(vvf, 1:3)), C in (copy(CC), view(CC, 1:3, 1:3))
        @test A_mul_Bt!(C, vf, vf) == vf*vf'
    end
end

# fallbacks & such for BlasFloats
let
    AA = rand(Float64,6,6)
    BB = rand(Float64,6,6)
    CC = zeros(Float64,6,6)
    for A in (copy(AA), view(AA, 1:6, 1:6)), B in (copy(BB), view(BB, 1:6, 1:6)), C in (copy(CC), view(CC, 1:6, 1:6))
        @test Base.LinAlg.At_mul_Bt!(C,A,B) == A.'*B.'
        @test Base.LinAlg.A_mul_Bc!(C,A,B) == A*B.'
        @test Base.LinAlg.Ac_mul_B!(C,A,B) == A.'*B
    end
end

# matrix algebra with subarrays of floats (stride != 1)
let
    A = reshape(map(Float64,1:20),5,4)
    Aref = A[1:2:end,1:2:end]
    Asub = view(A, 1:2:5, 1:2:4)
    b = [1.2,-2.5]
    @test (Aref*b) == (Asub*b)
    @test At_mul_B(Asub, Asub) == At_mul_B(Aref, Aref)
    @test A_mul_Bt(Asub, Asub) == A_mul_Bt(Aref, Aref)
    Ai = A .+ im
    Aref = Ai[1:2:end,1:2:end]
    Asub = view(Ai, 1:2:5, 1:2:4)
    @test Ac_mul_B(Asub, Asub) == Ac_mul_B(Aref, Aref)
    @test A_mul_Bc(Asub, Asub) == A_mul_Bc(Aref, Aref)
end

# issue #15286
let A = reshape(map(Float64, 1:20), 5, 4), C = zeros(8, 8), sC = view(C, 1:2:8, 1:2:8), B = reshape(map(Float64,-9:10),5,4)
    @test At_mul_B!(sC, A, A) == A'*A
    @test At_mul_B!(sC, A, B) == A'*B

    Aim = A .- im
    C = zeros(Complex128,8,8)
    sC = view(C, 1:2:8, 1:2:8)
    B = reshape(map(Float64,-9:10),5,4) .+ im
    @test Ac_mul_B!(sC, Aim, Aim) == Aim'*Aim
    @test Ac_mul_B!(sC, Aim, B) == Aim'*B
end

# syrk & herk
let
    AA = reshape(1:1503, 501, 3).-750.0
    res = Float64[135228751 9979252 -115270247; 9979252 10481254 10983256; -115270247 10983256 137236759]
    for A in (copy(AA), view(AA, 1:501, 1:3))
        @test At_mul_B(A, A) == res
        @test A_mul_Bt(A',A') == res
    end
    cutoff = 501
    A = reshape(1:6*cutoff,2*cutoff,3).-(6*cutoff)/2
    Asub = view(A, 1:2:2*cutoff, 1:3)
    Aref = A[1:2:2*cutoff, 1:3]
    @test At_mul_B(Asub, Asub) == At_mul_B(Aref, Aref)
    Ai = A .- im
    Asub = view(Ai, 1:2:2*cutoff, 1:3)
    Aref = Ai[1:2:2*cutoff, 1:3]
    @test Ac_mul_B(Asub, Asub) == Ac_mul_B(Aref, Aref)

    @test_throws DimensionMismatch Base.LinAlg.syrk_wrapper!(zeros(5,5),'N',ones(6,5))
    @test_throws DimensionMismatch Base.LinAlg.herk_wrapper!(zeros(5,5),'N',ones(6,5))
end

# matmul for types w/o sizeof (issue #1282)
let
    AA = fill(complex(1,1), 10, 10)
    for A in (copy(AA), view(AA, 1:10, 1:10))
        A2 = A^2
        @test A2[1,1] == 20im
    end
end

let
    AA = zeros(5, 5)
    BB = ones(5)
    CC = rand(5, 6)
    for A in (copy(AA), view(AA, 1:5, 1:5)), B in (copy(BB), view(BB, 1:5)), C in (copy(CC), view(CC, 1:5, 1:6))
        @test_throws DimensionMismatch scale!(A, B, C)
    end
end

# issue #6450
@test dot(Any[1.0,2.0], Any[3.5,4.5]) === 12.5

@testset "dot" for elty in (Float32, Float64, Complex64, Complex128)
    x = convert(Vector{elty},[1.0, 2.0, 3.0])
    y = convert(Vector{elty},[3.5, 4.5, 5.5])
    @test_throws DimensionMismatch dot(x, 1:2, y, 1:3)
    @test_throws BoundsError dot(x, 1:4, y, 1:4)
    @test_throws BoundsError dot(x, 1:3, y, 2:4)
    @test dot(x, 1:2,y, 1:2) == convert(elty, 12.5)
    @test x.'*y == convert(elty, 29.0)
    @test_throws MethodError dot(rand(elty, 2, 2), randn(elty, 2, 2))
    X = convert(Vector{Matrix{elty}},[reshape(1:4, 2, 2), ones(2, 2)])
    res = convert(Matrix{elty}, [7.0 13.0; 13.0 27.0])
    @test dot(X, X) == res
end

vecdot_(x,y) = invoke(vecdot, Tuple{Any,Any}, x,y) # generic vecdot
let AA = [1+2im 3+4im; 5+6im 7+8im], BB = [2+7im 4+1im; 3+8im 6+5im]
    for A in (copy(AA), view(AA, 1:2, 1:2)), B in (copy(BB), view(BB, 1:2, 1:2))
        @test vecdot(A,B) == dot(vec(A),vec(B)) == vecdot_(A,B) == vecdot(float.(A),float.(B))
        @test vecdot(Int[], Int[]) == 0 == vecdot_(Int[], Int[])
        @test_throws MethodError vecdot(Any[], Any[])
        @test_throws MethodError vecdot_(Any[], Any[])
        for n1 = 0:2, n2 = 0:2, d in (vecdot, vecdot_)
            if n1 != n2
                @test_throws DimensionMismatch d(1:n1, 1:n2)
            else
                @test d(1:n1, 1:n2) ≈ vecnorm(1:n1)^2
            end
        end
    end
end

# Issue 11978
let
    A = Array{Matrix{Float64}}(2, 2)
    A[1,1] = eye(3)
    A[1,2] = eye(3,2)
    A[2,1] = eye(2,3)
    A[2,2] = eye(2)
    b = Array{Vector{Float64}}(2)
    b[1] = ones(3)
    b[2] = ones(2)
    @test A*b == Vector{Float64}[[2,2,1], [2,2]]
end

@test_throws ArgumentError Base.LinAlg.copytri!(ones(10,10),'Z')

for elty in [Float32,Float64,Complex128,Complex64]
    @test_throws DimensionMismatch Base.LinAlg.gemv!(ones(elty,10),'N',rand(elty,10,10),ones(elty,11))
    @test_throws DimensionMismatch Base.LinAlg.gemv!(ones(elty,11),'N',rand(elty,10,10),ones(elty,10))
    @test Base.LinAlg.gemv!(ones(elty,0),'N',rand(elty,0,0),rand(elty,0)) == ones(elty,0)
    @test Base.LinAlg.gemv!(ones(elty,10), 'N',ones(elty,10,0),ones(elty,0)) == zeros(elty,10)

    @test Base.LinAlg.gemm_wrapper('N','N',eye(elty,10,10),eye(elty,10,10)) == eye(elty,10,10)
    @test_throws DimensionMismatch Base.LinAlg.gemm_wrapper!(eye(elty,10,10),'N','N',eye(elty,10,11),eye(elty,10,10))
    @test_throws DimensionMismatch Base.LinAlg.gemm_wrapper!(eye(elty,10,10),'N','N',eye(elty,0,0),eye(elty,0,0))

    A = rand(elty,3,3)
    @test Base.LinAlg.matmul3x3('T','N',A,eye(elty,3)) == A.'
end

# 13593, #13488
let
    aa = rand(3,3)
    bb = rand(3,3)
    for a in (copy(aa), view(aa, 1:3, 1:3)), b in (copy(bb), view(bb, 1:3, 1:3))
        @test_throws ArgumentError A_mul_B!(a, a, b)
        @test_throws ArgumentError A_mul_B!(a, b, a)
        @test_throws ArgumentError A_mul_B!(a, a, a)
    end
end

# Number types that lack conversion to the destination type (#14293)
struct RootInt
    i::Int
end
import Base: *, transpose
(*)(x::RootInt, y::RootInt) = x.i*y.i
transpose(x::RootInt) = x
@test Base.promote_op(*, RootInt, RootInt) === Int

let
    a = [RootInt(3)]
    C = [0]
    A_mul_Bt!(C, a, a)
    @test C[1] == 9
    a = [RootInt(2),RootInt(10)]
    @test a*a' == [4 20; 20 100]
    A = [RootInt(3) RootInt(5)]
    @test A*a == [56]
end

function test_mul(C, A, B)
    A_mul_B!(C, A, B)
    @test Array(A) * Array(B) ≈ C
    @test A*B ≈ C
end

let
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
        @test_throws DimensionMismatch A_mul_B!(full43, tri44, tri33)
        @test_throws DimensionMismatch A_mul_B!(full44, tri44, tri33)
        @test_throws DimensionMismatch A_mul_B!(full44, tri44, full43)
        @test_throws DimensionMismatch A_mul_B!(full43, tri33, full43)
        @test_throws DimensionMismatch A_mul_B!(full43, full43, tri44)
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
