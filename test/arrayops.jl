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
tmp = A[1:3,2,2:4]
@assert all(tmp == cat(3,46:48,86:88,126:128))
tmp = A[:,7:-3:1,5]
@assert all(tmp == [191 176 161; 192 177 162; 193 178 163; 194 179 164; 195 180 165])
tmp = A[:,3:9]
@assert all(tmp == reshape(11:45,5,7))
rng = (2,2:3,2:2:5)
tmp = zeros(Int,map(max,rng)...)
tmp[rng...] = A[rng...]
@assert  all(tmp == cat(3,zeros(Int,2,3),[0 0 0; 0 47 52],zeros(Int,2,3),[0 0 0; 0 127 132]))

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
X = [ i+2j | i=1:5, j=1:5 ]
@assert X[end,end] == 15
@assert X[end]     == 15  # linear index
@assert X[2,  end] == 12
@assert X[end,  2] == 9
@assert X[end-1,2] == 8
Y = [2, 1, 4, 3]
@assert X[Y[end],1] == 5
@assert X[end,Y[end]] == 11


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
@assert all(d'' == d)
@assert all(convert(Array,d)==d)
