#Array test

## basics

a = ones(4)
b = a+a
@assert b[1]==2. && b[2]==2. && b[3]==2. && b[4]==2.

@assert length((1,)) == 1
@assert length((1,2)) == 2

@assert isequal(1+[1,2,3], [2,3,4])
@assert isequal([1,2,3]+1, [2,3,4])
@assert isequal(1-[1,2,3], [0,-1,-2])
@assert isequal([1,2,3]-1, [0,1,2])

@assert isequal(5*[1,2,3], [5,10,15])
@assert isequal([1,2,3]*5, [5,10,15])
@assert isequal(1/[1,2,5], [1.0,0.5,0.2])
@assert isequal([1,2,3]/5, [0.2,0.4,0.6])

a = ones(2,2)
a[1,1] = 1
a[1,2] = 2
a[2,1] = 3
a[2,2] = 4
b = a'
assert(a[1,1] == 1. && a[1,2] == 2. &&
       a[2,1] == 3. && a[2,2] == 4.)
assert(b[1,1] == 1. && b[2,1] == 2. &&
       b[1,2] == 3. && b[2,2] == 4.)

a = Array(Float64, 2, 2, 2, 2, 2)
a[1,1,1,1,1] = 10
a[1,2,1,1,2] = 20
a[1,1,2,2,1] = 30

@assert a[1,1,1,1,1] == 10
@assert a[1,2,1,1,2] == 20
@assert a[1,1,2,2,1] == 30

b = reshape(a, (32,))
@assert b[1]  == 10
@assert b[19] == 20
@assert b[13] == 30

b = rand(32)
a = reshape(b, (2, 2, 2, 2, 2))
@assert ndims(a) == 5
@assert a[2,1,2,2,1] == b[14]
@assert a[2,2,2,2,2] == b[end]

sz = (5,8,7)
A = reshape(1:prod(sz),sz...)
tmp = A[2:6]
@assert tmp == [2:6]
tmp = A[1:3,2,2:4]
@assert tmp == cat(3,46:48,86:88,126:128)
tmp = A[:,7:-3:1,5]
@assert tmp == [191 176 161; 192 177 162; 193 178 163; 194 179 164; 195 180 165]
tmp = A[:,3:9]
@assert tmp == reshape(11:45,5,7)
rng = (2,2:3,2:2:5)
tmp = zeros(Int,map(max,rng)...)
tmp[rng...] = A[rng...]
@assert  tmp == cat(3,zeros(Int,2,3),[0 0 0; 0 47 52],zeros(Int,2,3),[0 0 0; 0 127 132])

x = rand(2,2)
b = x[1,:]
@assert isequal(size(b), (1, 2))
b = x[:,1]
@assert isequal(size(b), (2,))

x = rand(5,5)
b = x[2:3,2]
@assert b[1] == x[2,2] && b[2] == x[3,2]

B = zeros(4,5)
B[:,3] = 1:4
@assert B == [0 0 1 0 0; 0 0 2 0 0; 0 0 3 0 0; 0 0 4 0 0]
B[2,:] = 11:15
@assert B == [0 0 1 0 0; 11 12 13 14 15; 0 0 3 0 0; 0 0 4 0 0]
B[[3,1],[2,4]] = [21 22; 23 24]
@assert B == [0 23 1 24 0; 11 12 13 14 15; 0 21 3 22 0; 0 0 4 0 0]
B[4,[2,3]] = 7
@assert B == [0 23 1 24 0; 11 12 13 14 15; 0 21 3 22 0; 0 7 7 0 0]

@assert isequal(reshape(1:27, 3, 3, 3)[1,:], [1  4  7  10  13  16  19  22  25])

## arrays as dequeues
l = {1,2,3}
push(l,8)
@assert l[1]==1 && l[2]==2 && l[3]==3 && l[4]==8
v = pop(l)
@assert v == 8
v = pop(l)
@assert v == 3
@assert length(l)==2

# concatenation
@assert isequal([ones(2,2)  2*ones(2,1)], [1 1 2; 1 1 2])
@assert isequal([ones(2,2), 2*ones(1,2)], [1 1; 1 1; 2 2])

# "end"
X = [ i+2j for i=1:5, j=1:5 ]
@assert X[end,end] == 15
@assert X[end]     == 15  # linear index
@assert X[2,  end] == 12
@assert X[end,  2] == 9
@assert X[end-1,2] == 8
Y = [2, 1, 4, 3]
@assert X[Y[end],1] == 5
@assert X[end,Y[end]] == 11

## find, findfirst ##
a = [0,1,2,3,0,1,2,3]
@assert find(a) == [2,3,4,6,7,8]
@assert find(a.==2) == [3,7]
@assert find(isodd,a) == [2,4,6,8]
@assert findfirst(a) == 2
@assert findfirst(a.==0) == 1
@assert findfirst(a.==5) == 0
@assert findfirst([1,2,4,1,2,3,4], 3) == 6
@assert findfirst(isodd, [2,4,6,3,9,2,0]) == 4
@assert findfirst(isodd, [2,4,6,2,0]) == 0


## findn ##

b = findn(ones(2,2,2,2))
@assert (length(b[1]) == 16)
@assert (length(b[2]) == 16)
@assert (length(b[3]) == 16)
@assert (length(b[4]) == 16)

#hand made case
a = ([2,1,2],[1,2,2],[2,2,2])
z = zeros(2,2,2)
for i = 1:3
    z[a[1][i],a[2][i],a[3][i]] = 10
end
@assert isequal(a,findn(z))


## permute ##

#keeps the num of dim
p = randperm(5)
q = randperm(5)
a = rand(p...)
b = permute(a,q)
@assert isequal(size(b), tuple(p[q]...))

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
@assert isequal(z,permute(y,(3,1,2))) 

# of a subarray
a = rand(5,5)
s = sub(a,2:3,2:3)
p = permute(s, [2,1])
@assert p[1,1]==a[2,2] && p[1,2]==a[3,2]
@assert p[2,1]==a[2,3] && p[2,2]==a[3,3]

## ipermute ##

tensors = {rand(1,2,3,4),rand(2,2,2,2),rand(5,6,5,6),rand(1,1,1,1)}
for i = tensors
    perm = randperm(4)
    @assert isequal(i,ipermute(permute(i,perm),perm))
    @assert isequal(i,permute(ipermute(i,perm),perm))
end


## reduce ##

z = zeros(2,2,2,2)
for i=1:16
    z[i] = i
end

@assert sum(z) == sum(z,(1,2,3,4))[1] == 136

v = cell(2,2,1,1)
v[1,1,1,1] = 28.0
v[1,2,1,1] = 36.0
v[2,1,1,1] = 32.0
v[2,2,1,1] = 40.0

@assert isequal(v,sum(z,(3,4)))

## large matrices transpose ##

for i = 1 : 5
    a = rand(200, 300)

    @assert isequal(a', permute(a, (2, 1)))
end

## basic darray ##

d = drand(10,10)
@assert isequal(d'', d)
@assert isequal(convert(Array,d), d)

## cumsum

begin
    local A, A1, A2, A3, v, v2, cv, cv2, c
    A = ones(Int,2,3,4)
    A1 = reshape(repmat([1,2],1,12),2,3,4)
    A2 = reshape(repmat([1 2 3],2,4),2,3,4)
    A3 = reshape(repmat([1 2 3 4],6,1),2,3,4)
    @assert isequal(cumsum(A),A1)
    @assert isequal(cumsum(A,1),A1)
    @assert isequal(cumsum(A,2),A2)
    @assert isequal(cumsum(A,3),A3)

    A = rand(4,4)
    for s in {A[1:2:4, 1:2:4], sub(A, 1:2:4, 1:2:4)}
        c = cumsum(s, 1)
        @assert c[1,1] == A[1,1]
        @assert c[2,1] == A[1,1]+A[3,1]
        @assert c[1,2] == A[1,3]
        @assert c[2,2] == A[1,3]+A[3,3]

        c = cumsum(s, 2)
        @assert c[1,1] == A[1,1]
        @assert c[2,1] == A[3,1]
        @assert c[1,2] == A[1,1]+A[1,3]
        @assert c[2,2] == A[3,1]+A[3,3]
    end

    v   = [1,1e100,1,-1e100]*1000
    v2  = [1,-1e100,1,1e100]*1000

    cv  = [1,1e100,1e100,2]*1000
    cv2 = [1,-1e100,-1e100,2]*1000

    @assert isequal(cumsum(v), cv)
    @assert isequal(cumsum(v2), cv2)

    A = [v reverse(v) v2 reverse(v2)]

    c = cumsum(A, 1)

    @assert isequal(c[:,1], cv)
    @assert isequal(c[:,3], cv2)
    @assert isequal(c[4,:], [2.0 2.0 2.0 2.0]*1000)

    c = cumsum(A, 2)

    @assert isequal(c[1,:], cv2')
    @assert isequal(c[3,:], cv')
    @assert isequal(c[:,4], [2.0,2.0,2.0,2.0]*1000)

end
