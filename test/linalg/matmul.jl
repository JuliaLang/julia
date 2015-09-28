# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

## Test Julia fallbacks to BLAS routines

# matrices with zero dimensions
for i = 1:10
    @test ones(0,5)*ones(5,3) == zeros(0,3)
    @test ones(3,5)*ones(5,0) == zeros(3,0)
    @test ones(3,0)*ones(0,4) == zeros(3,4)
    @test ones(0,5)*ones(5,0) == zeros(0,0)
    @test ones(0,0)*ones(0,4) == zeros(0,4)
    @test ones(3,0)*ones(0,0) == zeros(3,0)
    @test ones(0,0)*ones(0,0) == zeros(0,0)
    @test Array(Float64, 5, 0) |> t -> t't == zeros(0,0)
    @test Array(Float64, 5, 0) |> t -> t*t' == zeros(5,5)
    @test Array(Complex128, 5, 0) |> t -> t't == zeros(0,0)
    @test Array(Complex128, 5, 0) |> t -> t*t' == zeros(5,5)
end

# 2x2
A = [1 2; 3 4]
B = [5 6; 7 8]
@test A*B == [19 22; 43 50]
@test At_mul_B(A, B) == [26 30; 38 44]
@test A_mul_Bt(A, B) == [17 23; 39 53]
@test At_mul_Bt(A, B) == [23 31; 34 46]
Ai = A+(0.5*im).*B
Bi = B+(2.5*im).*A[[2,1],[2,1]]
@test Ai*Bi == [-21+53.5im -4.25+51.5im; -12+95.5im 13.75+85.5im]
@test Ac_mul_B(Ai, Bi) == [68.5-12im 57.5-28im; 88-3im 76.5-25im]
@test A_mul_Bc(Ai, Bi) == [64.5+5.5im 43+31.5im; 104-18.5im 80.5+31.5im]
@test Ac_mul_Bc(Ai, Bi) == [-28.25-66im 9.75-58im; -26-89im 21-73im]

# 3x3
A = [1 2 3; 4 5 6; 7 8 9].-5
B = [1 0 5; 6 -10 3; 2 -4 -1]
@test A*B == [-26 38 -27; 1 -4 -6; 28 -46 15]
@test Ac_mul_B(A, B) == [-6 2 -25; 3 -12 -18; 12 -26 -11]
@test A_mul_Bc(A, B) == [-14 0 6; 4 -3 -3; 22 -6 -12]
@test Ac_mul_Bc(A, B) == [6 -8 -6; 12 -9 -9; 18 -10 -12]
Ai = A+(0.5*im).*B
Bi = B+(2.5*im).*A[[2,1,3],[2,3,1]]
@test Ai*Bi == [-44.75+13im 11.75-25im -38.25+30im; -47.75-16.5im -51.5+51.5im -56+6im; 16.75-4.5im -53.5+52im -15.5im]
@test Ac_mul_B(Ai, Bi) == [-21+2im -1.75+49im -51.25+19.5im; 25.5+56.5im -7-35.5im 22+35.5im; -3+12im -32.25+43im -34.75-2.5im]
@test A_mul_Bc(Ai, Bi) == [-20.25+15.5im -28.75-54.5im 22.25+68.5im; -12.25+13im -15.5+75im -23+27im; 18.25+im 1.5+94.5im -27-54.5im]
@test Ac_mul_Bc(Ai, Bi) == [1+2im 20.75+9im -44.75+42im; 19.5+17.5im -54-36.5im 51-14.5im; 13+7.5im 11.25+31.5im -43.25-14.5im]

# Generic integer matrix multiplication
A = [1 2 3; 4 5 6] .- 3
B = [2 -2; 3 -5; -4 7]
@test A*B == [-7 9; -4 9]
@test At_mul_Bt(A, B) == [-6 -11 15; -6 -13 18; -6 -15 21]
A = ones(Int, 2, 100)
B = ones(Int, 100, 3)
@test A*B == [100 100 100; 100 100 100]
A = rand(1:20, 5, 5) .- 10
B = rand(1:20, 5, 5) .- 10
@test At_mul_B(A, B) == A'*B
@test A_mul_Bt(A, B) == A*B'

# Preallocated
C = Array(Int, size(A, 1), size(B, 2))
@test A_mul_B!(C, A, B) == A*B
@test At_mul_B!(C, A, B) == A'*B
@test A_mul_Bt!(C, A, B) == A*B'
@test At_mul_Bt!(C, A, B) == A'*B'
@test Base.LinAlg.Ac_mul_Bt!(C, A, B) == A'*B.'

#test DimensionMismatch for generic_matmatmul
@test_throws DimensionMismatch Base.LinAlg.Ac_mul_Bt!(C,A,ones(Int,4,4))
@test_throws DimensionMismatch Base.LinAlg.Ac_mul_Bt!(C,ones(Int,4,4),B)

#and for generic_matvecmul
A = rand(5,5)
B = rand(5)
@test_throws DimensionMismatch Base.LinAlg.generic_matvecmul!(zeros(6),'N',A,B)
@test_throws DimensionMismatch Base.LinAlg.generic_matvecmul!(B,'N',A,zeros(6))

v = [1,2,3]
C = Array(Int, 3, 3)
@test A_mul_Bt!(C, v, v) == v*v'
vf = map(Float64,v)
C = Array(Float64, 3, 3)
@test A_mul_Bt!(C, v, v) == v*v'

# fallbacks & such for BlasFloats
A = rand(Float64,6,6)
B = rand(Float64,6,6)
C = zeros(Float64,6,6)
@test Base.LinAlg.At_mul_Bt!(C,A,B) == A.'*B.'
@test Base.LinAlg.A_mul_Bc!(C,A,B) == A*B.'
@test Base.LinAlg.Ac_mul_B!(C,A,B) == A.'*B

# matrix algebra with subarrays of floats (stride != 1)
A = reshape(map(Float64,1:20),5,4)
Aref = A[1:2:end,1:2:end]
Asub = sub(A, 1:2:5, 1:2:4)
b = [1.2,-2.5]
@test (Aref*b) == (Asub*b)
@test At_mul_B(Asub, Asub) == At_mul_B(Aref, Aref)
@test A_mul_Bt(Asub, Asub) == A_mul_Bt(Aref, Aref)
Ai = A .+ im
Aref = Ai[1:2:end,1:2:end]
Asub = sub(Ai, 1:2:5, 1:2:4)
@test Ac_mul_B(Asub, Asub) == Ac_mul_B(Aref, Aref)
@test A_mul_Bc(Asub, Asub) == A_mul_Bc(Aref, Aref)

# syrk & herk
A = reshape(1:1503, 501, 3).-750.0
res = Float64[135228751 9979252 -115270247; 9979252 10481254 10983256; -115270247 10983256 137236759]
@test At_mul_B(A, A) == res
@test A_mul_Bt(A',A') == res
cutoff = 501
A = reshape(1:6*cutoff,2*cutoff,3).-(6*cutoff)/2
Asub = sub(A, 1:2:2*cutoff, 1:3)
Aref = A[1:2:2*cutoff, 1:3]
@test At_mul_B(Asub, Asub) == At_mul_B(Aref, Aref)
Ai = A .- im
Asub = sub(Ai, 1:2:2*cutoff, 1:3)
Aref = Ai[1:2:2*cutoff, 1:3]
@test Ac_mul_B(Asub, Asub) == Ac_mul_B(Aref, Aref)

@test_throws DimensionMismatch Base.LinAlg.syrk_wrapper!(zeros(5,5),'N',ones(6,5))
@test_throws DimensionMismatch Base.LinAlg.herk_wrapper!(zeros(5,5),'N',ones(6,5))

# matmul for types w/o sizeof (issue #1282)
A = Array(Complex{Int},10,10)
A[:] = complex(1,1)
A2 = A^2
@test A2[1,1] == 20im

@test_throws DimensionMismatch scale!(zeros(5,5),ones(5),rand(5,6))

# issue #6450
@test dot(Any[1.0,2.0], Any[3.5,4.5]) === 12.5

for elty in (Float32,Float64,Complex64,Complex128)
    x = convert(Vector{elty},[1.0,2.0,3.0])
    y = convert(Vector{elty},[3.5,4.5,5.5])
    @test_throws DimensionMismatch dot(x, 1:2, y, 1:3)
    @test_throws BoundsError dot(x, 1:4, y, 1:4)
    @test_throws BoundsError dot(x, 1:3, y, 2:4)
    @test dot(x,1:2,y,1:2) == convert(elty,12.5)
    @test x.'*y == convert(elty, 29.0)
end

vecdot_(x,y) = invoke(vecdot, (Any,Any), x,y) # generic vecdot
let A = [1+2im 3+4im; 5+6im 7+8im], B = [2+7im 4+1im; 3+8im 6+5im]
    @test vecdot(A,B) == dot(vec(A),vec(B)) == vecdot_(A,B) == vecdot(float(A),float(B))
    @test vecdot(Int[], Int[]) == 0 == vecdot_(Int[], Int[])
    @test_throws MethodError vecdot(Any[], Any[])
    @test_throws MethodError vecdot_(Any[], Any[])
    for n1 = 0:2, n2 = 0:2, d in (vecdot, vecdot_)
        if n1 != n2
            @test_throws DimensionMismatch d(1:n1, 1:n2)
        else
            @test_approx_eq d(1:n1, 1:n2) vecnorm(1:n1)^2
        end
    end
end

# Issue 11978
A = Array(Matrix{Float64}, 2, 2)
A[1,1] = eye(3)
A[1,2] = eye(3,2)
A[2,1] = eye(2,3)
A[2,2] = eye(2)
b = Array(Vector{Float64}, 2)
b[1] = ones(3)
b[2] = ones(2)
@test A*b == Vector{Float64}[[2,2,1], [2,2]]

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
a = rand(3,3)
b = rand(3,3)
@test_throws ArgumentError A_mul_B!(a, a, b)
@test_throws ArgumentError A_mul_B!(a, b, a)
@test_throws ArgumentError A_mul_B!(a, a, a)
