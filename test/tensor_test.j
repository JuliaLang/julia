#Array test

## find ##

b = find(ones(2,2,2,2))
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
@assert isequal(a,find(z))


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

@assert sum(z) == sum(z,(1,2,3,4)) == 136

v = cell(2,2,1,1)
v[1,1,1,1] = 28.0
v[1,2,1,1] = 36.0
v[2,1,1,1] = 32.0
v[2,2,1,1] = 40.0

@assert isequal(v,sum(z,(3,4)))


println("Done. No errors!")
