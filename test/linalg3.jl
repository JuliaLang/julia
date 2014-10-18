## Least squares solutions
a = [ones(20) 1:20 1:20]
b = reshape(eye(8, 5), 20, 2)
for elty in (Float32, Float64, Complex64, Complex128)
    a = convert(Matrix{elty}, a)
    b = convert(Matrix{elty}, b)

    # Vector rhs
    x = a[:,1:2]\b[:,1]
    @test_approx_eq ((a[:,1:2]*x-b[:,1])'*(a[:,1:2]*x-b[:,1]))[1] convert(elty, 2.546616541353384)

    # Matrix rhs
    x = a[:,1:2]\b
    @test_approx_eq det((a[:,1:2]*x-b)'*(a[:,1:2]*x-b)) convert(elty, 4.437969924812031)

    # Rank deficient
    x = a\b
    @test_approx_eq det((a*x-b)'*(a*x-b)) convert(elty, 4.437969924812031)

    # Underdetermined minimum norm
    x = convert(Matrix{elty}, [1 0 0; 0 1 -1]) \ convert(Vector{elty}, [1,1])
    @test_approx_eq x convert(Vector{elty}, [1, 0.5, -0.5])

    # symmetric, positive definite
    @test_approx_eq inv(convert(Matrix{elty}, [6. 2; 2 1])) convert(Matrix{elty}, [0.5 -1; -1 3])

    # symmetric, indefinite
    @test_approx_eq inv(convert(Matrix{elty}, [1. 2; 2 1])) convert(Matrix{elty}, [-1. 2; 2 -1]/3)
end

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
v = [1,2,3]
C = Array(Int, 3, 3)
@test A_mul_Bt!(C, v, v) == v*v'
vf = float64(v)
C = Array(Float64, 3, 3)
@test A_mul_Bt!(C, v, v) == v*v'

# matrix algebra with subarrays of floats (stride != 1)
A = reshape(float64(1:20),5,4)
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
res = float64([135228751 9979252 -115270247; 9979252 10481254 10983256; -115270247 10983256 137236759])
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

# Matrix exponential
for elty in (Float32, Float64, Complex64, Complex128)
        A1  = convert(Matrix{elty}, [4 2 0; 1 4 1; 1 1 4])
        eA1 = convert(Matrix{elty}, [147.866622446369 127.781085523181  127.781085523182;
        183.765138646367 183.765138646366  163.679601723179;
        71.797032399996  91.8825693231832 111.968106246371]')
        @test_approx_eq expm(A1) eA1

        A2  = convert(Matrix{elty},
            [29.87942128909879    0.7815750847907159 -2.289519314033932;
            0.7815750847907159 25.72656945571064    8.680737820540137;
            -2.289519314033932   8.680737820540137  34.39400925519054])
        eA2 = convert(Matrix{elty},
            [  5496313853692458.0 -18231880972009236.0 -30475770808580460.0;
             -18231880972009252.0  60605228702221920.0 101291842930249760.0;
             -30475770808580480.0 101291842930249728.0 169294411240851968.0])
        @test_approx_eq expm(A2) eA2

        A3  = convert(Matrix{elty}, [-131 19 18;-390 56 54;-387 57 52])
        eA3 = convert(Matrix{elty}, [-1.50964415879218 -5.6325707998812  -4.934938326092;
        0.367879439109187 1.47151775849686  1.10363831732856;
        0.135335281175235 0.406005843524598 0.541341126763207]')
        @test_approx_eq expm(A3) eA3

        # issue 5116
        A4  = [0 10 0 0; -1 0 0 0; 0 0 0 0; -2 0 0 0]
        eA4 = [-0.999786072879326  -0.065407069689389   0.0   0.0
                0.006540706968939  -0.999786072879326   0.0   0.0
                0.0                 0.0                 1.0   0.0
                0.013081413937878  -3.999572145758650   0.0   1.0]
        @test_approx_eq expm(A4) eA4

        # issue 5116
        A5  = [ 0. 0. 0. 0. ; 0. 0. -im 0.; 0. im 0. 0.; 0. 0. 0. 0.]
        eA5 = [ 1.0+0.0im   0.0+0.0im                 0.0+0.0im                0.0+0.0im
                0.0+0.0im   1.543080634815244+0.0im   0.0-1.175201193643801im  0.0+0.0im
                0.0+0.0im   0.0+1.175201193643801im   1.543080634815243+0.0im  0.0+0.0im
                0.0+0.0im   0.0+0.0im                 0.0+0.0im                1.0+0.0im]
        @test_approx_eq expm(A5) eA5

        # Hessenberg
        @test_approx_eq hessfact(A1)[:H] convert(Matrix{elty},
                        [4.000000000000000  -1.414213562373094  -1.414213562373095
                        -1.414213562373095   4.999999999999996  -0.000000000000000
                                         0  -0.000000000000002   3.000000000000000])
end

# Hermitian matrix exponential
A1 = randn(4,4) + im*randn(4,4)
A2 = A1 + A1'
@test_approx_eq expm(A2) expm(Hermitian(A2))

# matmul for types w/o sizeof (issue #1282)
A = Array(Complex{Int},10,10)
A[:] = complex(1,1)
A2 = A^2
@test A2[1,1] == 20im

# test sparse matrix norms
Ac = sprandn(10,10,.1) + im* sprandn(10,10,.1)
Ar = sprandn(10,10,.1)
Ai = int(ceil(Ar*100))
@test_approx_eq norm(Ac,1)     norm(full(Ac),1)
@test_approx_eq norm(Ac,Inf)   norm(full(Ac),Inf)
@test_approx_eq vecnorm(Ac)    vecnorm(full(Ac))
@test_approx_eq norm(Ar,1)     norm(full(Ar),1)
@test_approx_eq norm(Ar,Inf)   norm(full(Ar),Inf)
@test_approx_eq vecnorm(Ar)    vecnorm(full(Ar))
@test_approx_eq norm(Ai,1)     norm(full(Ai),1)
@test_approx_eq norm(Ai,Inf)   norm(full(Ai),Inf)
@test_approx_eq vecnorm(Ai)    vecnorm(full(Ai))

# scale real matrix by complex type
@test_throws ErrorException scale!([1.0], 2.0im)
@test isequal(scale([1.0], 2.0im),             Complex{Float64}[2.0im])
@test isequal(scale(Float32[1.0], 2.0f0im),    Complex{Float32}[2.0im])
@test isequal(scale(Float32[1.0], 2.0im),      Complex{Float64}[2.0im])
@test isequal(scale(Float64[1.0], 2.0f0im),    Complex{Float64}[2.0im])
@test isequal(scale(Float32[1.0], big(2.0)im), Complex{BigFloat}[2.0im])
@test isequal(scale(Float64[1.0], big(2.0)im), Complex{BigFloat}[2.0im])
@test isequal(scale(BigFloat[1.0], 2.0im),     Complex{BigFloat}[2.0im])
@test isequal(scale(BigFloat[1.0], 2.0f0im),   Complex{BigFloat}[2.0im])

# issue #6450
@test dot(Any[1.0,2.0], Any[3.5,4.5]) === 12.5

# issue #7181
A = [ 1  5  9
      2  6 10
      3  7 11
      4  8 12 ]
@test_throws BoundsError diag(A, -5)
@test diag(A,-4) == []
@test diag(A,-3) == [4]
@test diag(A,-2) == [3,8]
@test diag(A,-1) == [2,7,12]
@test diag(A, 0) == [1,6,11]
@test diag(A, 1) == [5,10]
@test diag(A, 2) == [9]
@test diag(A, 3) == []
@test_throws BoundsError diag(A, 4)

@test diag(zeros(0,0)) == []
@test_throws BoundsError diag(zeros(0,0),1)
@test_throws BoundsError diag(zeros(0,0),-1)

@test diag(zeros(1,0)) == []
@test diag(zeros(1,0),-1) == []
@test_throws BoundsError diag(zeros(1,0),1)
@test_throws BoundsError diag(zeros(1,0),-2)

@test diag(zeros(0,1)) == []
@test diag(zeros(0,1),1) == []
@test_throws BoundsError diag(zeros(0,1),-1)
@test_throws BoundsError diag(zeros(0,1),2)
