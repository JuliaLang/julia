@assert rand() != rand()

# ranges
@assert size(10:1:0) == (0,)
@assert length(1:.2:2) == 6
@assert length(Range(1.,.2,2.)) == 6
@assert length(2:-.2:1) == 6
@assert length(Range(2.,-.2,1.)) == 6
@assert length(2:.2:1) == 0
@assert length(Range(2.,.2,1.)) == 0

# comprehensions
X = [ i+2j | i=1:5, j=1:5 ]
@assert X[2,3] == 8
@assert X[4,5] == 14
@assert ones(2,3) * ones(2,3)' == [3. 3.; 3. 3.]
@assert [ [1,2] | i=1:2, : ] == [1 2; 1 2]
# where element type is a Union. try to confuse type inference.
foo32_64(x) = (x<2) ? int32(x) : int64(x)
boo32_64() = [ foo32_64(i) | i=1:2 ]
let a36 = boo32_64()
    @assert a36[1]==1 && a36[2]==2
end

# blas, lapack
n = 10
a = rand(n,n)
asym = a+a'+n*eye(n)
b = rand(n)
r = chol(asym)
@assert sum(r'*r - asym) < 1e-8
(l,u,p) = lu(a)
@assert sum(l[p,:]*u - a) < 1e-8
(q,r,p) = qr(a)
@assert sum(q*r[:,p] - a) < 1e-8
(d,v) = eig(asym)
@assert sum(asym*v[:,1]-d[1]*v[:,1]) < 1e-8
(d,v) = eig(a)
@assert abs(sum(a*v[:,1]-d[1]*v[:,1])) < 1e-8
(u,s,vt) = svd(a)
@assert sum(u*diagm(s)*vt - a) < 1e-8
x = a \ b
@assert sum(a*x-b) < 1e-8
x = triu(a) \ b
@assert sum(triu(a)*x-b) < 1e-8
x = tril(a) \ b
@assert sum(tril(a)*x-b) < 1e-8

# arpack
if WORD_SIZE==64
    # TODO: hangs on 32-bit
    (d,v) = eigs(asym, 3)
    @assert sum(asym*v[:,1]-d[1]*v[:,1]) < 1e-8

    (d,v) = eigs(a,3)
    @assert abs(sum(a*v[:,2]-d[2]*v[:,2])) < 1e-8
end

# fft
a = rand(8) + im*rand(8)
@assert norm((1/length(a))*ifft(fft(a)) - a) < 1e-8

# hash table
h = HashTable()
for i=1:10000
    h[i] = i+1
end
for i=1:10000
    @assert (h[i] == i+1)
end
for i=1:2:10000
    del(h, i)
end
for i=1:2:10000
    h[i] = i+1
end
for i=1:10000
    @assert (h[i] == i+1)
end
for i=1:10000
    del(h, i)
end
@assert isempty(h)
h[77] = 100
@assert h[77]==100
for i=1:10000
    h[i] = i+1
end
for i=1:2:10000
    del(h, i)
end
for i=10001:20000
    h[i] = i+1
end
for i=2:2:10000
    @assert h[i]==i+1
end
for i=10000:20000
    @assert h[i]==i+1
end
