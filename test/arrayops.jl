#Array test

## basics

@test length([1, 2, 3]) == 3
@test countnz([1, 2, 3]) == 3

a = ones(4)
b = a+a
@test b[1]==2. && b[2]==2. && b[3]==2. && b[4]==2.

@test length((1,)) == 1
@test length((1,2)) == 2

@test isequal(1.+[1,2,3], [2,3,4])
@test isequal([1,2,3].+1, [2,3,4])
@test isequal(1.-[1,2,3], [0,-1,-2])
@test isequal([1,2,3].-1, [0,1,2])

@test isequal(5*[1,2,3], [5,10,15])
@test isequal([1,2,3]*5, [5,10,15])
@test isequal(1./[1,2,5], [1.0,0.5,0.2])
@test isequal([1,2,3]/5, [0.2,0.4,0.6])

@test isequal(1.<<[1,2,5], [2,4,32])
@test isequal(128.>>[1,2,5], [64,32,4])
@test isequal(2.>>1, 1)
@test isequal(1.<<1, 2)
@test isequal([1,2,5].<<[1,2,5], [2,8,160])
@test isequal([10,20,50].>>[1,2,5], [5,5,1])


a = ones(2,2)
a[1,1] = 1
a[1,2] = 2
a[2,1] = 3
a[2,2] = 4
b = a'
@test a[1,1] == 1. && a[1,2] == 2. && a[2,1] == 3. && a[2,2] == 4.
@test b[1,1] == 1. && b[2,1] == 2. && b[1,2] == 3. && b[2,2] == 4.

a = Array(Float64, 2, 2, 2, 2, 2)
a[1,1,1,1,1] = 10
a[1,2,1,1,2] = 20
a[1,1,2,2,1] = 30

@test a[1,1,1,1,1] == 10
@test a[1,2,1,1,2] == 20
@test a[1,1,2,2,1] == 30

b = reshape(a, (32,))
@test b[1]  == 10
@test b[19] == 20
@test b[13] == 30

b = rand(32)
a = reshape(b, (2, 2, 2, 2, 2))
@test ndims(a) == 5
@test a[2,1,2,2,1] == b[14]
@test a[2,2,2,2,2] == b[end]

a = rand(1, 1, 8, 8, 1)
@test @inferred(squeeze(a, 1)) == @inferred(squeeze(a, (1,))) == reshape(a, (1, 8, 8, 1))
@test @inferred(squeeze(a, (1, 5))) == squeeze(a, (5, 1)) == reshape(a, (1, 8, 8))
@test @inferred(squeeze(a, (1, 2, 5))) == squeeze(a, (5, 2, 1)) == reshape(a, (8, 8))
@test_throws ErrorException squeeze(a, 0)
@test_throws ErrorException squeeze(a, (1, 1))
@test_throws ErrorException squeeze(a, (1, 2, 1))
@test_throws ErrorException squeeze(a, (1, 1, 2))
@test_throws ErrorException squeeze(a, 3)
@test_throws ErrorException squeeze(a, 4)
@test_throws ErrorException squeeze(a, 6)

sz = (5,8,7)
A = reshape(1:prod(sz),sz...)
@test A[2:6] == [2:6]
tmp = A[1:3,2,2:4]
@test tmp == cat(3,46:48,86:88,126:128)
@test A[:,7:-3:1,5] == [191 176 161; 192 177 162; 193 178 163; 194 179 164; 195 180 165]
tmp = A[:,3:9]
@test tmp == reshape(11:45,5,7)
rng = (2,2:3,2:2:5)
tmp = zeros(Int,map(maximum,rng)...)
tmp[rng...] = A[rng...]
@test  tmp == cat(3,zeros(Int,2,3),[0 0 0; 0 47 52],zeros(Int,2,3),[0 0 0; 0 127 132])

@test cat([1,2],1,2,3.,4.,5.) == diagm([1,2,3.,4.,5.])
blk = [1 2;3 4]
tmp = cat([1,3],blk,blk)
@test tmp[1:2,1:2,1] == blk
@test tmp[1:2,1:2,2] == zero(blk)
@test tmp[3:4,1:2,1] == zero(blk)
@test tmp[3:4,1:2,2] == blk

x = rand(2,2)
b = x[1,:]
@test isequal(size(b), (1, 2))
b = x[:,1]
@test isequal(size(b), (2,))

x = rand(5,5)
b = x[2:3,2]
@test b[1] == x[2,2] && b[2] == x[3,2]

B = zeros(4,5)
B[:,3] = 1:4
@test B == [0 0 1 0 0; 0 0 2 0 0; 0 0 3 0 0; 0 0 4 0 0]
B[2,:] = 11:15
@test B == [0 0 1 0 0; 11 12 13 14 15; 0 0 3 0 0; 0 0 4 0 0]
B[[3,1],[2,4]] = [21 22; 23 24]
@test B == [0 23 1 24 0; 11 12 13 14 15; 0 21 3 22 0; 0 0 4 0 0]
B[4,[2,3]] = 7
@test B == [0 23 1 24 0; 11 12 13 14 15; 0 21 3 22 0; 0 7 7 0 0]

@test isequal(reshape(1:27, 3, 3, 3)[1,:], [1  4  7  10  13  16  19  22  25])

a = [3, 5, -7, 6]
b = [4, 6, 2, -7, 1]
ind = findin(a, b)
@test ind == [3,4]

rt = Base.return_types(setindex!, (Array{Int32, 3}, UInt8, Vector{Int}, Float64, UnitRange{Int}))
@test length(rt) == 1 && rt[1] == Array{Int32, 3}

# get
let
    A = reshape(1:24, 3, 8)
    x = get(A, 32, -12)
    @test x == -12
    x = get(A, 14, -12)
    @test x == 14
    x = get(A, (2,4), -12)
    @test x == 11
    x = get(A, (4,4), -12)
    @test x == -12
    X = get(A, -5:5, NaN32)
    @test eltype(X) == Float32
    @test isnan(X) == [trues(6),falses(5)]
    @test X[7:11] == [1:5]
    X = get(A, (2:4, 9:-2:-13), 0)
    Xv = zeros(Int, 3, 12)
    Xv[1:2, 2:5] = A[2:3, 7:-2:1]
    @test X == Xv
    X2 = get(A, Vector{Int}[[2:4], [9:-2:-13]], 0)
    @test X == X2
end

## arrays as dequeues
l = Any[1]
push!(l,2,3,8)
@test l[1]==1 && l[2]==2 && l[3]==3 && l[4]==8
v = pop!(l)
@test v == 8
v = pop!(l)
@test v == 3
@test length(l)==2
unshift!(l,4,7,5)
@test l[1]==4 && l[2]==7 && l[3]==5 && l[4]==1 && l[5]==2
v = shift!(l)
@test v == 4
@test length(l)==4

v = [3, 7, 6]
@test_throws BoundsError insert!(v, 0, 5)
for i = 1:4
    vc = copy(v)
    @test insert!(vc, i, 5) === vc
    @test vc == [v[1:(i-1)], 5, v[i:end]]
end
@test_throws BoundsError insert!(v, 5, 5)

# concatenation
@test isequal([ones(2,2)  2*ones(2,1)], [1. 1 2; 1 1 2])
@test isequal([ones(2,2), 2*ones(1,2)], [1. 1; 1 1; 2 2])

# typed array literals
X = Float64[1 2 3]
Y = [1. 2. 3.]
@test size(X) == size(Y)
for i = 1:3 @test X[i] === Y[i] end
X = Float64[1;2;3]
Y = [1.,2.,3.]
@test size(X) == size(Y)
for i = 1:3 @test X[i] === Y[i] end
X = Float64[1 2 3; 4 5 6]
Y = [1. 2. 3.; 4. 5. 6.]
@test size(X) == size(Y)
for i = 1:length(X) @test X[i] === Y[i] end

# "end"
X = [ i+2j for i=1:5, j=1:5 ]
@test X[end,end] == 15
@test X[end]     == 15  # linear index
@test X[2,  end] == 12
@test X[end,  2] == 9
@test X[end-1,2] == 8
Y = [2, 1, 4, 3]
@test X[Y[end],1] == 5
@test X[end,Y[end]] == 11

## find, findfirst ##
a = [0,1,2,3,0,1,2,3]
@test find(a) == [2,3,4,6,7,8]
@test find(a.==2) == [3,7]
@test find(isodd,a) == [2,4,6,8]
@test findfirst(a) == 2
@test findfirst(a.==0) == 1
@test findfirst(a.==5) == 0
@test findfirst([1,2,4,1,2,3,4], 3) == 6
@test findfirst(isodd, [2,4,6,3,9,2,0]) == 4
@test findfirst(isodd, [2,4,6,2,0]) == 0


## findn ##

b = findn(ones(2,2,2,2))
@test (length(b[1]) == 16)
@test (length(b[2]) == 16)
@test (length(b[3]) == 16)
@test (length(b[4]) == 16)

#hand made case
a = ([2,1,2],[1,2,2],[2,2,2])
z = zeros(2,2,2)
for i = 1:3
    z[a[1][i],a[2][i],a[3][i]] = 10
end
@test isequal(a,findn(z))

#argmin argmax
@test indmax([10,12,9,11]) == 2
@test indmin([10,12,9,11]) == 3
@test findmin([NaN,3.2,1.8]) == (1.8,3)
@test findmax([NaN,3.2,1.8]) == (3.2,2)
@test findmin([NaN,3.2,1.8,NaN]) == (1.8,3)
@test findmax([NaN,3.2,1.8,NaN]) == (3.2,2)
@test findmin([3.2,1.8,NaN,2.0]) == (1.8,2)
@test findmax([3.2,1.8,NaN,2.0]) == (3.2,1)

## permutedims ##

#keeps the num of dim
p = randperm(5)
q = randperm(5)
a = rand(p...)
b = permutedims(a,q)
@test isequal(size(b), tuple(p[q]...))

#hand made case
y = zeros(1,2,3)
for i = 1:6
    y[i]=i
end

z = zeros(3,1,2)
for i = 1:3
    z[i] = i*2-1
    z[i+3] = i*2
end

#permutes correctly
@test isequal(z,permutedims(y,[3,1,2]))
@test isequal(z,permutedims(y,(3,1,2)))

# of a subarray
a = rand(5,5)
s = sub(a,2:3,2:3)
p = permutedims(s, [2,1])
@test p[1,1]==a[2,2] && p[1,2]==a[3,2]
@test p[2,1]==a[2,3] && p[2,2]==a[3,3]

## ipermutedims ##

tensors = Any[rand(1,2,3,4),rand(2,2,2,2),rand(5,6,5,6),rand(1,1,1,1)]
for i = tensors
    perm = randperm(4)
    @test isequal(i,ipermutedims(permutedims(i,perm),perm))
    @test isequal(i,permutedims(ipermutedims(i,perm),perm))
end

## unique across dim ##

# All rows and columns unique
A = ones(10, 10)
A[diagind(A)] = shuffle!([1:10])
@test unique(A, 1) == A
@test unique(A, 2) == A

# 10 repeats of each row
B = A[shuffle!(repmat(1:10, 10)), :]
C = unique(B, 1)
@test sortrows(C) == sortrows(A)
@test unique(B, 2) == B
@test unique(B.', 2).' == C

# Along third dimension
D = cat(3, B, B)
@test unique(D, 1) == cat(3, C, C)
@test unique(D, 3) == cat(3, B)

# With hash collisions
immutable HashCollision
    x::Float64
end
Base.hash(::HashCollision, h::UInt) = h
@test map(x->x.x, unique(map(HashCollision, B), 1)) == C

## large matrices transpose ##

for i = 1 : 3
    a = rand(200, 300)
    @test isequal(a', permutedims(a, [2, 1]))
end

begin
    local A, A1, A2, A3, v, v2, cv, cv2, c, R, T
    A = ones(Int,2,3,4)
    A1 = reshape(repmat([1,2],1,12),2,3,4)
    A2 = reshape(repmat([1 2 3],2,4),2,3,4)
    A3 = reshape(repmat([1 2 3 4],6,1),2,3,4)
    @test isequal(cumsum(A),A1)
    @test isequal(cumsum(A,1),A1)
    @test isequal(cumsum(A,2),A2)
    @test isequal(cumsum(A,3),A3)

    R = repeat([1, 2], inner = [1], outer = [1])
    @test R == [1, 2]
    R = repeat([1, 2], inner = [2], outer = [1])
    @test R == [1, 1, 2, 2]
    R = repeat([1, 2], inner = [1], outer = [2])
    @test R == [1, 2, 1, 2]
    R = repeat([1, 2], inner = [2], outer = [2])
    @test R == [1, 1, 2, 2, 1, 1, 2, 2]
    R = repeat([1, 2], inner = [1, 1], outer = [1, 1])
    @test R == [1, 2]''
    R = repeat([1, 2], inner = [2, 1], outer = [1, 1])
    @test R == [1, 1, 2, 2]''
    R = repeat([1, 2], inner = [1, 2], outer = [1, 1])
    @test R == [1 1; 2 2]
    R = repeat([1, 2], inner = [1, 1], outer = [2, 1])
    @test R == [1, 2, 1, 2]''
    R = repeat([1, 2], inner = [1, 1], outer = [1, 2])
    @test R == [1 1; 2 2]

    R = repeat([1 2;
                3 4], inner = [1, 1], outer = [1, 1])
    @test R == [1 2;
                  3 4]
    R = repeat([1 2;
                3 4], inner = [1, 1], outer = [2, 1])
    @test R == [1 2;
                  3 4;
                  1 2;
                  3 4]
    R = repeat([1 2;
                3 4], inner = [1, 1], outer = [1, 2])
    @test R == [1 2 1 2;
                  3 4 3 4]
    R = repeat([1 2;
                3 4], inner = [1, 1], outer = [2, 2])
    @test R == [1 2 1 2;
                  3 4 3 4;
                  1 2 1 2;
                  3 4 3 4]
    R = repeat([1 2;
                3 4], inner = [2, 1], outer = [1, 1])
    @test R == [1 2;
                  1 2;
                  3 4;
                  3 4]
    R = repeat([1 2;
                3 4], inner = [2, 1], outer = [2, 1])
    @test R == [1 2;
                  1 2;
                  3 4;
                  3 4;
                  1 2;
                  1 2;
                  3 4;
                  3 4]
    R = repeat([1 2;
                3 4], inner = [2, 1], outer = [1, 2])
    @test R == [1 2 1 2;
                  1 2 1 2;
                  3 4 3 4;
                  3 4 3 4;]
    R = repeat([1 2;
                3 4], inner = [2, 1], outer = [2, 2])
    @test R == [1 2 1 2;
                  1 2 1 2;
                  3 4 3 4;
                  3 4 3 4;
                  1 2 1 2;
                  1 2 1 2;
                  3 4 3 4;
                  3 4 3 4]
    R = repeat([1 2;
                3 4], inner = [1, 2], outer = [1, 1])
    @test R == [1 1 2 2;
                  3 3 4 4]
    R = repeat([1 2;
                3 4], inner = [1, 2], outer = [2, 1])
    @test R == [1 1 2 2;
                  3 3 4 4;
                  1 1 2 2;
                  3 3 4 4]
    R = repeat([1 2;
                3 4], inner = [1, 2], outer = [1, 2])
    @test R == [1 1 2 2 1 1 2 2;
                  3 3 4 4 3 3 4 4]
    R = repeat([1 2;
                3 4], inner = [1, 2], outer = [2, 2])
    @test R == [1 1 2 2 1 1 2 2;
                  3 3 4 4 3 3 4 4;
                  1 1 2 2 1 1 2 2;
                  3 3 4 4 3 3 4 4]
    R = repeat([1 2;
                3 4], inner = [2, 2], outer = [1, 1])
    @test R == [1 1 2 2;
                  1 1 2 2;
                  3 3 4 4;
                  3 3 4 4]
    R = repeat([1 2;
                3 4], inner = [2, 2], outer = [2, 1])
    @test R == [1 1 2 2;
                  1 1 2 2;
                  3 3 4 4;
                  3 3 4 4;
                  1 1 2 2;
                  1 1 2 2;
                  3 3 4 4;
                  3 3 4 4]
    R = repeat([1 2;
                3 4], inner = [2, 2], outer = [1, 2])
    @test R == [1 1 2 2 1 1 2 2;
                  1 1 2 2 1 1 2 2;
                  3 3 4 4 3 3 4 4;
                  3 3 4 4 3 3 4 4]
    R = repeat([1 2;
                3 4], inner = [2, 2], outer = [2, 2])
    @test R == [1 1 2 2 1 1 2 2;
                  1 1 2 2 1 1 2 2;
                  3 3 4 4 3 3 4 4;
                  3 3 4 4 3 3 4 4;
                  1 1 2 2 1 1 2 2;
                  1 1 2 2 1 1 2 2;
                  3 3 4 4 3 3 4 4;
                  3 3 4 4 3 3 4 4]

    A = reshape([1:8], 2, 2, 2)
    R = repeat(A, inner = [1, 1, 2], outer = [1, 1, 1])
    T = reshape([1:4, 1:4, 5:8, 5:8], 2, 2, 4)
    @test R == T
    A = Array(Int, 2, 2, 2)
    A[:, :, 1] = [1 2;
                  3 4]
    A[:, :, 2] = [5 6;
                  7 8]
    R = repeat(A, inner = [2, 2, 2], outer = [2, 2, 2])
    @test R[1, 1, 1] == 1
    @test R[2, 2, 2] == 1
    @test R[3, 3, 3] == 8
    @test R[4, 4, 4] == 8
    @test R[5, 5, 5] == 1
    @test R[6, 6, 6] == 1
    @test R[7, 7, 7] == 8
    @test R[8, 8, 8] == 8

    A = rand(4,4)
    for s in Any[A[1:2:4, 1:2:4], sub(A, 1:2:4, 1:2:4)]
        c = cumsum(s, 1)
        @test c[1,1] == A[1,1]
        @test c[2,1] == A[1,1]+A[3,1]
        @test c[1,2] == A[1,3]
        @test c[2,2] == A[1,3]+A[3,3]

        c = cumsum(s, 2)
        @test c[1,1] == A[1,1]
        @test c[2,1] == A[3,1]
        @test c[1,2] == A[1,1]+A[1,3]
        @test c[2,2] == A[3,1]+A[3,3]
    end

    v   = [1,1e100,1,-1e100]*1000
    v2  = [1,-1e100,1,1e100]*1000

    cv  = [1,1e100,1e100,2]*1000
    cv2 = [1,-1e100,-1e100,2]*1000

    @test isequal(cumsum_kbn(v), cv)
    @test isequal(cumsum_kbn(v2), cv2)

    A = [v reverse(v) v2 reverse(v2)]

    c = cumsum_kbn(A, 1)

    @test isequal(c[:,1], cv)
    @test isequal(c[:,3], cv2)
    @test isequal(c[4,:], [2.0 2.0 2.0 2.0]*1000)

    c = cumsum_kbn(A, 2)

    @test isequal(c[1,:], cv2')
    @test isequal(c[3,:], cv')
    @test isequal(c[:,4], [2.0,2.0,2.0,2.0]*1000)

end

@test (1:5)[[true,false,true,false,true]] == [1,3,5]
@test [1:5][[true,false,true,false,true]] == [1,3,5]
@test_throws BoundsError (1:5)[[true,false,true,false]]
@test_throws BoundsError (1:5)[[true,false,true,false,true,false]]
@test_throws BoundsError [1:5][[true,false,true,false]]
@test_throws BoundsError [1:5][[true,false,true,false,true,false]]
a = [1:5]
a[[true,false,true,false,true]] = 6
@test a == [6,2,6,4,6]
a[[true,false,true,false,true]] = [7,8,9]
@test a == [7,2,8,4,9]
@test_throws DimensionMismatch (a[[true,false,true,false,true]] = [7,8,9,10])
A = reshape(1:15, 3, 5)
@test A[[true, false, true], [false, false, true, true, false]] == [7 10; 9 12]
@test_throws BoundsError A[[true, false], [false, false, true, true, false]]
@test_throws BoundsError A[[true, false, true], [false, true, true, false]]
@test_throws BoundsError A[[true, false, true, true], [false, false, true, true, false]]
@test_throws BoundsError A[[true, false, true], [false, false, true, true, false, true]]
A = ones(Int, 3, 5)
@test_throws DimensionMismatch A[2,[true, false, true, true, false]] = 2:5
A[2,[true, false, true, true, false]] = 2:4
@test A == [1 1 1 1 1; 2 1 3 4 1; 1 1 1 1 1]
@test_throws DimensionMismatch A[[true,false,true], 5] = [19]
@test_throws DimensionMismatch A[[true,false,true], 5] = 19:21
A[[true,false,true], 5] = 7
@test A == [1 1 1 1 7; 2 1 3 4 1; 1 1 1 1 7]

B = cat(3, 1, 2, 3)
@test B[:,:,[true, false, true]] == reshape([1,3], 1, 1, 2)  # issue #5454

# issue #2342
@test isequal(cumsum([1 2 3]), [1 2 3])

# set-like operations
@test isequal(union([1,2,3], [4,3,4]), [1,2,3,4])
@test isequal(union(['e','c','a'], ['b','a','d']), ['e','c','a','b','d'])
@test isequal(union([1,2,3], [4,3], [5]), [1,2,3,4,5])
@test isequal(union([1,2,3]), [1,2,3])
@test isequal(union([1,2,3], Int64[]), Int64[1,2,3])
@test isequal(union([1,2,3], Float64[]), Float64[1.0,2,3])
@test isequal(union(Int64[], [1,2,3]), Int64[1,2,3])
@test isequal(union(Int64[]), Int64[])
@test isequal(intersect([1,2,3], [4,3,4]), [3])
@test isequal(intersect(['e','c','a'], ['b','a','d']), ['a'])
@test isequal(intersect([1,2,3], [3,1], [2,1,3]), [1,3])
@test isequal(intersect([1,2,3]), [1,2,3])
@test isequal(intersect([1,2,3], Int64[]), Int64[])
@test isequal(intersect([1,2,3], Float64[]), Float64[])
@test isequal(intersect(Int64[], [1,2,3]), Int64[])
@test isequal(intersect(Int64[]), Int64[])
@test isequal(setdiff([1,2,3,4], [2,5,4]), [1,3])
@test isequal(setdiff([1,2,3,4], [7,8,9]), [1,2,3,4])
@test isequal(setdiff([1,2,3,4], Int64[]), Int64[1,2,3,4])
@test isequal(setdiff([1,2,3,4], [1,2,3,4,5]), Int64[])
@test isequal(symdiff([1,2,3], [4,3,4]), [1,2,4])
@test isequal(symdiff(['e','c','a'], ['b','a','d']), ['e','c','b','d'])
@test isequal(symdiff([1,2,3], [4,3], [5]), [1,2,4,5])
@test isequal(symdiff([1,2,3,4,5], [1,2,3], [3,4]), [3,5])
@test isequal(symdiff([1,2,3]), [1,2,3])
@test isequal(symdiff([1,2,3], Int64[]), Int64[1,2,3])
@test isequal(symdiff([1,2,3], Float64[]), Float64[1.0,2,3])
@test isequal(symdiff(Int64[], [1,2,3]), Int64[1,2,3])
@test isequal(symdiff(Int64[]), Int64[])

# mapslices
begin
    local a,h,i
    a = rand(5,5)
    h = mapslices(v -> hist(v,0:0.1:1)[2], a, 1)
    H = mapslices(v -> hist(v,0:0.1:1)[2], a, 2)
    s = mapslices(sort, a, [1])
    S = mapslices(sort, a, [2])
    for i = 1:5
        @test h[:,i] == hist(a[:,i],0:0.1:1)[2]
        @test vec(H[i,:]) == hist(vec(a[i,:]),0:0.1:1)[2]
        @test s[:,i] == sort(a[:,i])
        @test vec(S[i,:]) == sort(vec(a[i,:]))
    end

    # issue #3613
    b = mapslices(sum, ones(2,3,4), [1,2])
    @test size(b) === (1,1,4)
    @test all(b.==6)

    # issue #5141
    ## Update Removed the version that removes the dimensions when dims==1:ndims(A)
    c1 = mapslices(x-> maximum(-x), a, [])
    @test c1 == -a

    # other types than Number
    @test mapslices(prod,["1" "2"; "3" "4"],1) == ["13" "24"]
    @test mapslices(prod,["1"],1) == ["1"]

    # issue #5177

    c = ones(2,3,4)
    m1 = mapslices(x-> ones(2,3), c, [1,2])
    m2 = mapslices(x-> ones(2,4), c, [1,3])
    m3 = mapslices(x-> ones(3,4), c, [2,3])
    @test size(m1) == size(m2) == size(m3) == size(c)

    n1 = mapslices(x-> ones(6), c, [1,2])
    n2 = mapslices(x-> ones(6), c, [1,3])
    n3 = mapslices(x-> ones(6), c, [2,3])
    n1a = mapslices(x-> ones(1,6), c, [1,2])
    n2a = mapslices(x-> ones(1,6), c, [1,3])
    n3a = mapslices(x-> ones(1,6), c, [2,3])
    @test size(n1a) == (1,6,4) && size(n2a) == (1,3,6)  && size(n3a) == (2,1,6)
    @test size(n1) == (6,1,4) && size(n2) == (6,3,1)  && size(n3) == (2,6,1)
end


# single multidimensional index
let
    a = rand(6,6)
    I = [1 4 5; 4 2 6; 5 6 3]
    a2 = a[I]
    @test size(a2) == size(I)
    for i = 1:length(a2)
        @test a2[i] == a[I[i]]
    end
    a = [1,3,5]
    b = [1 3]
    a[b] = 8
    @test a == [8,3,8]
end

# assigning an array into itself
a = [1,3,5]
b = [3,1,2]
a[b] = a
@test a == [3,5,1]

# lexicographic comparison
@test lexcmp([1.0], [1]) == 0
@test lexcmp([1], [1.0]) == 0
@test lexcmp([1, 1], [1, 1]) == 0
@test lexcmp([1, 1], [2, 1]) == -1
@test lexcmp([2, 1], [1, 1]) == 1
@test lexcmp([1, 1], [1, 2]) == -1
@test lexcmp([1, 2], [1, 1]) == 1
@test lexcmp([1], [1, 1]) == -1
@test lexcmp([1, 1], [1]) == 1

# sort on arrays
begin
    local a = rand(3,3)

    asr = sortrows(a)
    @test lexless(asr[1,:],asr[2,:])
    @test lexless(asr[2,:],asr[3,:])

    asc = sortcols(a)
    @test lexless(asc[:,1],asc[:,2])
    @test lexless(asc[:,2],asc[:,3])

    asr = sortrows(a, rev=true)
    @test lexless(asr[2,:],asr[1,:])
    @test lexless(asr[3,:],asr[2,:])

    asc = sortcols(a, rev=true)
    @test lexless(asc[:,2],asc[:,1])
    @test lexless(asc[:,3],asc[:,2])

    as = sort(a, 1)
    @test issorted(as[:,1])
    @test issorted(as[:,2])
    @test issorted(as[:,3])

    as = sort(a, 2)
    @test issorted(as[1,:])
    @test issorted(as[2,:])
    @test issorted(as[3,:])
end

# fill
@test fill!(Array(Float64,1),-0.0)[1] === -0.0
A = ones(3,3)
S = sub(A, 2, 1:3)
fill!(S, 2)
S = sub(A, 1:2, 3)
fill!(S, 3)
@test A == [1 1 3; 2 2 3; 1 1 1]
rt = Base.return_types(fill!, (Array{Int32, 3}, UInt8))
@test length(rt) == 1 && rt[1] == Array{Int32, 3}

# splice!
for idx in Any[1, 2, 5, 9, 10, 1:0, 2:1, 1:1, 2:2, 1:2, 2:4, 9:8, 10:9, 9:9, 10:10,
               8:9, 9:10, 6:9, 7:10]
    for repl in Any[[], [11], [11,22], [11,22,33,44,55]]
        a = [1:10]; acopy = copy(a)
        @test splice!(a, idx, repl) == acopy[idx]
        @test a == [acopy[1:(first(idx)-1)], repl, acopy[(last(idx)+1):end]]
    end
end

# deleteat!
for idx in Any[1, 2, 5, 9, 10, 1:0, 2:1, 1:1, 2:2, 1:2, 2:4, 9:8, 10:9, 9:9, 10:10,
               8:9, 9:10, 6:9, 7:10]
    a = [1:10]; acopy = copy(a)
    @test deleteat!(a, idx) == [acopy[1:(first(idx)-1)], acopy[(last(idx)+1):end]]
end
a = [1:10]
@test deleteat!(a, [1,3,5,7:10...]) == [2,4,6]


# comprehensions
X = [ i+2j for i=1:5, j=1:5 ]
@test X[2,3] == 8
@test X[4,5] == 14
@test isequal(ones(2,3) * ones(2,3)', [3. 3.; 3. 3.])
@test isequal([ [1,2] for i=1:2, : ], [1 2; 1 2])
# where element type is a Union. try to confuse type inference.
foo32_64(x) = (x<2) ? int32(x) : int64(x)
boo32_64() = [ foo32_64(i) for i=1:2 ]
let a36 = boo32_64()
    @test a36[1]==1 && a36[2]==2
end
@test isequal([1,2,3], [a for (a,b) in enumerate(2:4)])
@test isequal([2,3,4], [b for (a,b) in enumerate(2:4)])

@test_throws DomainError (10.^[-1])[1] == 0.1
@test (10.^[-1.])[1] == 0.1

# reverse
@test reverse([2,3,1]) == [1,3,2]
@test reverse([1:10],1,4) == [4,3,2,1,5,6,7,8,9,10]
@test reverse([1:10],3,6) == [1,2,6,5,4,3,7,8,9,10]
@test reverse([1:10],6,10) == [1,2,3,4,5,10,9,8,7,6]
@test reverse(1:10,1,4) == [4,3,2,1,5,6,7,8,9,10]
@test reverse(1:10,3,6) == [1,2,6,5,4,3,7,8,9,10]
@test reverse(1:10,6,10) == [1,2,3,4,5,10,9,8,7,6]
@test reverse!([1:10],1,4) == [4,3,2,1,5,6,7,8,9,10]
@test reverse!([1:10],3,6) == [1,2,6,5,4,3,7,8,9,10]
@test reverse!([1:10],6,10) == [1,2,3,4,5,10,9,8,7,6]

# flipdim
@test isequal(flipdim([2,3,1], 1), [1,3,2])
@test isequal(flipdim([2,3,1], 2), [2,3,1])
@test isequal(flipdim([2 3 1], 1), [2 3 1])
@test isequal(flipdim([2 3 1], 2), [1 3 2])
@test_throws ErrorException flipdim([2,3,1], -1)
@test isequal(flipdim(1:10, 1), 10:-1:1)
@test isequal(flipdim(1:10, 2), 1:10)
@test_throws ErrorException flipdim(1:10, -1)
@test isequal(flipdim(Array(Int,0,0),1), Array(Int,0,0))  # issue #5872

# issue 4228
A = [[i i; i i] for i=1:2]
@test cumsum(A) == Any[[1 1; 1 1], [3 3; 3 3]]
@test cumprod(A) == Any[[1 1; 1 1], [4 4; 4 4]]

# PR #4627
A = [1,2]
@test append!(A, A) == [1,2,1,2]
@test prepend!(A, A) == [1,2,1,2,1,2,1,2]

# cases where shared arrays can/can't be grown
A = [1 3;2 4]
B = reshape(A, 4)
@test push!(B,5) == [1,2,3,4,5]
@test pop!(B) == 5
C = reshape(B, 1, 4)
@test_throws MethodError push!(C, 5)

A = [NaN]; B = [NaN]
@test !(A==A)
@test isequal(A,A)
@test A===A
@test !(A==B)
@test isequal(A,B)
@test A!==B

# complete testsuite for reducedim

# Inferred types
Nmax = 3 # TODO: go up to CARTESIAN_DIMS+2 (currently this exposes problems)
for N = 1:Nmax
    #indexing with (UnitRange, UnitRange, UnitRange)
    args = ntuple(N, d->UnitRange{Int})
    @test Base.return_types(getindex, tuple(Array{Float32, N}, args...)) == [Array{Float32, N}]
    @test Base.return_types(getindex, tuple(BitArray{N}, args...)) == Any[BitArray{N}]
    @test Base.return_types(setindex!, tuple(Array{Float32, N}, Array{Int, 1}, args...)) == [Array{Float32, N}]
    # Indexing with (UnitRange, UnitRange, Float64)
    args = ntuple(N, d->d<N ? UnitRange{Int} : Float64)
    N > 1 && @test Base.return_types(getindex, tuple(Array{Float32, N}, args...)) == [Array{Float32, N-1}]
    N > 1 && @test Base.return_types(getindex, tuple(BitArray{N}, args...)) == [BitArray{N-1}]
    N > 1 && @test Base.return_types(setindex!, tuple(Array{Float32, N}, Array{Int, 1}, args...)) == [Array{Float32, N}]
end

# issue #6645 (32-bit)
let
    x = Float64[]
    for i=1:5; push!(x, 1.0); end
    @test dot(zeros(5),x) == 0.0
end

# issue #6977
@test size([]') == (1,0)

# issue #6996
@test Any[ 1 2; 3 4 ]' == Any[ 1 2; 3 4 ].'

# map with promotion (issue #6541)
@test map(join, ["z", "я"]) == ["z", "я"]

# Handle block matrices
A = [randn(2,2) for i = 1:2, j = 1:2]
@test issym(A.'A)
A = [complex(randn(2,2), randn(2,2)) for i = 1:2, j = 1:2]
@test ishermitian(A'A)

# issue #7197
function i7197()
    S = [1 2 3; 4 5 6; 7 8 9]
    ind2sub(size(S), 5)
end
@test i7197() == (2,2)

# PR #9256
function pr9256()
    m = [1 2 3; 4 5 6; 7 8 9]
    ind2sub(m, 6)
end
@test pr9256() == (3,2)

# PR #8622 and general indexin test
function pr8622()
    x=[1,3,5,7]
    y=[5,4,3]
    return indexin(x,y)
end
@test pr8622() == [0,3,1,0]

# commit b718cbc72e90, getindex(::Number, ::Real)
b718cbc = 5
@test b718cbc[1.0] == 5
@test_throws InexactError b718cbc[1.1]

#6828 - size of specific dimensions
a = Array(Float64, 10)
@test size(a) == (10,)
@test size(a, 1) == 10
@test size(a,2,1) == (1,10)
a = Array(Float64, 2,3)
@test size(a) == (2,3)
@test size(a,4,3,2,1) == (1,1,3,2)
@test size(a,1,2) == (2,3)
a = Array(Float64, 9,8,7,6,5,4,3,2,1)
@test size(a,1,1) == (9,9)
@test size(a,4) == 6
@test size(a,9,8,7,6,5,4,3,2,19,8,7,6,5,4,3,2,1) == (1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,9)

# Multidimensional iterators
for a in ([1:5], reshape([2]))
    counter = 0
    for I in eachindex(a)
        counter += 1
    end
    @test counter == length(a)
    counter = 0
    for aa in a
        counter += 1
    end
    @test counter == length(a)
end

function mdsum(A)
    s = 0.0
    for a in A
        s += a
    end
    s
end

function mdsum2(A)
    s = 0.0
    @inbounds for I in eachindex(A)
        s += A[I]
    end
    s
end

a = [1:5]
@test isa(Base.linearindexing(a), Base.LinearFast)
b = sub(a, :)
@test isa(Base.linearindexing(b), Base.IteratorsMD.LinearFast)
aa = fill(99, 10)
aa[1:2:9] = a
shp = [5]
for i = 1:10
    A = reshape(a, tuple(shp...))
    @test mdsum(A) == 15
    @test mdsum2(A) == 15
    AA = reshape(aa, tuple(2, shp...))
    B = sub(AA, 1:1, ntuple(i, i->Colon())...)
    @test isa(Base.linearindexing(B), Base.IteratorsMD.LinearSlow)
    @test mdsum(B) == 15
    @test mdsum2(B) == 15
    unshift!(shp, 1)
end

a = [1:10]
shp = [2,5]
for i = 2:10
    A = reshape(a, tuple(shp...))
    @test mdsum(A) == 55
    @test mdsum2(A) == 55
    B = sub(A, ntuple(i, i->Colon())...)
    @test mdsum(B) == 55
    @test mdsum2(B) == 55
    insert!(shp, 2, 1)
end

a = reshape([2])
@test mdsum(a) == 2
@test mdsum2(a) == 2

a = ones(0,5)
b = sub(a, :, :)
@test mdsum(b) == 0
a = ones(5,0)
b = sub(a, :, :)
@test mdsum(b) == 0

I1 = CartesianIndex((2,3,0))
I2 = CartesianIndex((-1,5,2))
@test I1 + I2 == CartesianIndex((1,8,2))
@test I2 + I1 == CartesianIndex((1,8,2))
@test I1 - I2 == CartesianIndex((3,-2,-2))
@test I2 - I1 == CartesianIndex((-3,2,2))

@test min(CartesianIndex((2,3)), CartesianIndex((5,2))) == CartesianIndex((2,2))
@test max(CartesianIndex((2,3)), CartesianIndex((5,2))) == CartesianIndex((5,3))
