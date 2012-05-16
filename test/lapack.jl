# blas, lapack
n = 10
a = rand(n,n)
asym = a+a'+n*eye(n)
b = rand(n)
r = chol(asym)
@assert norm(r'*r - asym) < 1e-8
(l,u,p) = lu(a)
@assert norm(l[p,:]*u - a) < 1e-8
(q,r,p) = qr(a)
@assert norm(q*r[:,p] - a) < 1e-8
(d,v) = eig(asym)
@assert norm(asym*v[:,1]-d[1]*v[:,1]) < 1e-8
(d,v) = eig(a)
@assert norm(a*v[:,1]-d[1]*v[:,1]) < 1e-8
(u,s,vt) = svd(a)
@assert norm(u*diagm(s)*vt - a) < 1e-8
x = a \ b
@assert norm(a*x-b) < 1e-8
x = triu(a) \ b
@assert norm(triu(a)*x-b) < 1e-8
x = tril(a) \ b
@assert norm(tril(a)*x-b) < 1e-8
