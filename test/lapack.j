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
