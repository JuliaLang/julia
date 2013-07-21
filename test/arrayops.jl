#Array test

## basics

a = ones(4)
b = a+a
@test b[1]==2. && b[2]==2. && b[3]==2. && b[4]==2.

@test length((1,)) == 1
@test length((1,2)) == 2

@test isequal(1+[1,2,3], [2,3,4])
@test isequal([1,2,3]+1, [2,3,4])
@test isequal(1-[1,2,3], [0,-1,-2])
@test isequal([1,2,3]-1, [0,1,2])

@test isequal(5*[1,2,3], [5,10,15])
@test isequal([1,2,3]*5, [5,10,15])
@test isequal(1/[1,2,5], [1.0,0.5,0.2])
@test isequal([1,2,3]/5, [0.2,0.4,0.6])

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

sz = (5,8,7)
A = reshape(1:prod(sz),sz...)
tmp = A[2:6]
@test tmp == [2:6]
tmp = A[1:3,2,2:4]
@test tmp == cat(3,46:48,86:88,126:128)
tmp = A[:,7:-3:1,5]
@test tmp == [191 176 161; 192 177 162; 193 178 163; 194 179 164; 195 180 165]
tmp = A[:,3:9]
@test tmp == reshape(11:45,5,7)
rng = (2,2:3,2:2:5)
tmp = zeros(Int,map(max,rng)...)
tmp[rng...] = A[rng...]
@test  tmp == cat(3,zeros(Int,2,3),[0 0 0; 0 47 52],zeros(Int,2,3),[0 0 0; 0 127 132])

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

# sub
A = reshape(1:120, 3, 5, 8)
sA = sub(A, 2, 1:5, 1:8)
@test Base.parentdims(sA) == 1:3
@test size(sA) == (1, 5, 8)
@test_throws sA[2, 1:8]
@test sA[1, 2, 1:8][:] == 5:15:120
sA[2:5:end] = -1
@test all(sA[2:5:end] .== -1)
@test all(A[5:15:120] .== -1)
@test strides(sA) == (1,3,15)
sA = sub(A, 1:3, 1:5, 5)
@test Base.parentdims(sA) == 1:2
sA[1:3,1:5] = -2
@test all(A[:,:,5] .== -2)
sA[:] = -3
@test all(A[:,:,5] .== -3)
@test strides(sA) == (1,3)
sA = sub(A, 1:2:3, 1:3:5, 1:2:8)
@test Base.parentdims(sA) == 1:3
@test strides(sA) == (2,9,30)

# slice
A = reshape(1:120, 3, 5, 8)
sA = slice(A, 2, 1:5, 1:8)
@test Base.parentdims(sA) == 2:3
@test size(sA) == (5, 8)
@test strides(sA) == (3,15)
@test sA[2, 1:8][:] == 5:15:120
@test sA[:,1] == 2:3:14
@test sA[2:5:end] == 5:15:120
sA[2:5:end] = -1
@test all(sA[2:5:end] .== -1)
@test all(A[5:15:120] .== -1)
sA = slice(A, 1:3, 1:5, 5)
@test Base.parentdims(sA) == 1:2
@test size(sA) == (3,5)
@test strides(sA) == (1,3)
sA = slice(A, 1:2:3, 3, 1:2:8)
@test Base.parentdims(sA) == [1,3]
@test size(sA) == (2,4)
@test strides(sA) == (2,30)

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
    X = get(A, -5:5, nan(Float32))
    @test eltype(X) == Float32
    @test isnan(X) == [trues(6),falses(5)]
    @test X[7:11] == 1:5
    X = get(A, (2:4, 9:-2:-13), 0)
    Xv = zeros(Int, 3, 12)
    Xv[1:2, 2:5] = A[2:3, 7:-2:1]
    @test X == Xv
    X2 = get(A, Vector{Int}[[2:4], [9:-2:-13]], 0)
    @test X == X2
end

## arrays as dequeues
l = {1,2,3}
push!(l,8)
@test l[1]==1 && l[2]==2 && l[3]==3 && l[4]==8
v = pop!(l)
@test v == 8
v = pop!(l)
@test v == 3
@test length(l)==2

# concatenation
@test isequal([ones(2,2)  2*ones(2,1)], [1 1 2; 1 1 2])
@test isequal([ones(2,2), 2*ones(1,2)], [1 1; 1 1; 2 2])

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
@assert indmax([10,12,9,11]) == 2
@assert indmin([10,12,9,11]) == 3
@assert findmin([NaN,3.2,1.8]) == (1.8,3)
@assert findmax([NaN,3.2,1.8]) == (3.2,2)
@assert findmin([NaN,3.2,1.8,NaN]) == (1.8,3)
@assert findmax([NaN,3.2,1.8,NaN]) == (3.2,2)
@assert findmin([3.2,1.8,NaN,2.0]) == (1.8,2)
@assert findmax([3.2,1.8,NaN,2.0]) == (3.2,1)

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
@test isequal(z,permutedims(y,(3,1,2)))

# of a subarray
a = rand(5,5)
s = sub(a,2:3,2:3)
p = permutedims(s, [2,1])
@test p[1,1]==a[2,2] && p[1,2]==a[3,2]
@test p[2,1]==a[2,3] && p[2,2]==a[3,3]

## ipermutedims ##

tensors = {rand(1,2,3,4),rand(2,2,2,2),rand(5,6,5,6),rand(1,1,1,1)}
for i = tensors
    perm = randperm(4)
    @test isequal(i,ipermutedims(permutedims(i,perm),perm))
    @test isequal(i,permutedims(ipermutedims(i,perm),perm))
end


## reduce ##

z = zeros(2,2,2,2)
for i=1:16
    z[i] = i
end

@test sum(z) == sum(z,(1,2,3,4))[1] == 136

v = cell(2,2,1,1)
v[1,1,1,1] = 28.0
v[1,2,1,1] = 36.0
v[2,1,1,1] = 32.0
v[2,2,1,1] = 40.0

@test isequal(v,sum(z,(3,4)))

## large matrices transpose ##

for i = 1 : 3
    a = rand(200, 300)

    @test isequal(a', permutedims(a, (2, 1)))
end

## cumsum, cummin, cummax

@assert isequal(cummin([1, 2, 5, -1, 3, -2]), [1, 1, 1, -1, -1, -2])
@assert isequal(cummax([1, 2, 5, -1, 3, -2]), [1, 2, 5, 5, 5, 5])

@assert isequal(cummax([1 0; 0 1], 1), [1 0; 1 1])
@assert isequal(cummax([1 0; 0 1], 2), [1 1; 0 1])
@assert isequal(cummin([1 0; 0 1], 1), [1 0; 0 0])
@assert isequal(cummin([1 0; 0 1], 2), [1 0; 0 0])

@test sum_kbn([1,1e100,1,-1e100]) == 2

begin
    local A, A1, A2, A3, v, v2, cv, cv2, c
    A = ones(Int,2,3,4)
    A1 = reshape(repmat([1,2],1,12),2,3,4)
    A2 = reshape(repmat([1 2 3],2,4),2,3,4)
    A3 = reshape(repmat([1 2 3 4],6,1),2,3,4)
    @test isequal(cumsum(A),A1)
    @test isequal(cumsum(A,1),A1)
    @test isequal(cumsum(A,2),A2)
    @test isequal(cumsum(A,3),A3)

    A = rand(4,4)
    for s in {A[1:2:4, 1:2:4], sub(A, 1:2:4, 1:2:4)}
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

# issue #2342
@test isequal(cumsum([1 2 3]), [1 2 3])

# set-like operations
@test isequal(union([1,2,3], [4,3,4]), [1,2,3,4])
@test isequal(union(['e','c','a'], ['b','a','d']), ['e','c','a','b','d'])
@test isequal(union([1,2,3], [4,3], [5]), [1,2,3,4,5])
@test isequal(union([1,2,3]), [1,2,3])
@test isequal(union([1,2,3], Int64[]), [1,2,3])
@test isequal(union([1,2,3], Float64[]), [1.0,2,3])
@test isequal(union(Int64[], [1,2,3]), [1,2,3])
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
@test isequal(setdiff([1,2,3,4], Int64[]), [1,2,3,4])
@test isequal(setdiff([1,2,3,4], [1,2,3,4,5]), Int64[])
@test isequal(symdiff([1,2,3], [4,3,4]), [1,2,4])
@test isequal(symdiff(['e','c','a'], ['b','a','d']), ['e','c','b','d'])
@test isequal(symdiff([1,2,3], [4,3], [5]), [1,2,4,5])
@test isequal(symdiff([1,2,3,4,5], [1,2,3], [3,4]), [3,5])
@test isequal(symdiff([1,2,3]), [1,2,3])
@test isequal(symdiff([1,2,3], Int64[]), [1,2,3])
@test isequal(symdiff([1,2,3], Float64[]), [1.0,2,3])
@test isequal(symdiff(Int64[], [1,2,3]), [1,2,3])
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

# sort on arrays
begin
    local a = rand(3,3)

    asr = sortrows(a)
    @test isless(asr[1,:],asr[2,:])
    @test isless(asr[2,:],asr[3,:])

    asc = sortcols(a)
    @test isless(asc[:,1],asc[:,2])
    @test isless(asc[:,2],asc[:,3])

    asr = sortrows(a, rev=true)
    @test isless(asr[2,:],asr[1,:])
    @test isless(asr[3,:],asr[2,:])

    asc = sortcols(a, rev=true)
    @test isless(asc[:,2],asc[:,1])
    @test isless(asc[:,3],asc[:,2])

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

# splice!
for idx in {1, 2, 5, 9, 10, 1:0, 2:1, 1:1, 2:2, 1:2, 2:4, 9:8, 10:9, 9:9, 10:10,
            8:9, 9:10, 6:9, 7:10}
    for repl in {[], [11], [11,22], [11,22,33,44,55]}
        a = [1:10]; acopy = copy(a)
        @test splice!(a, idx, repl) == acopy[idx]
        @test a == [acopy[1:(first(idx)-1)], repl, acopy[(last(idx)+1):end]]
    end
end

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

@test_throws (10.^[-1])[1] == 0.1
@test (10.^[-1.])[1] == 0.1
