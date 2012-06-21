# blas, lapack
Eps = sqrt(eps())

begin
local n
n = 10
a = rand(n,n)
asym = a+a'+n*eye(n)
b = rand(n)
r = chol(asym)                          # Cholesky decomposition
@assert norm(r'*r - asym) < Eps

l = chol!(copy(asym), "L")              # lower-triangular Cholesky decomposition
@assert norm(l*l' - asym) < Eps

(l,u,p) = lu(a)                         # LU decomposition
@assert norm(l*u - a[p,:]) < Eps
@assert norm(l[invperm(p),:]*u - a) < Eps

(q,r) = qr(a)                           # QR decomposition
@assert norm(q*r - a) < Eps

(q,r,p) = qrp(a)                        # pivoted QR decomposition
@assert norm(q*r - a[:,p]) < Eps
@assert norm(q*r[:,invperm(p)] - a) < Eps

(d,v) = eig(asym)                       # symmetric eigen-decomposition
@assert norm(asym*v[:,1]-d[1]*v[:,1]) < Eps
@assert norm(v*diagmm(d,v') - asym) < Eps

(d,v) = eig(a)
for i in 1:size(a,2) @assert norm(a*v[:,i] - d[i]*v[:,i]) < Eps end

(u,s,vt) = svd(a)                       # singular value decomposition
@assert norm(u*diagmm(s,vt) - a) < Eps

(u,s,vt) = sdd(a)                       # svd using divide-and-conquer
@assert norm(u*diagmm(s,vt) - a) < Eps

x = a \ b
@assert norm(a*x - b) < Eps

x = triu(a) \ b
@assert norm(triu(a)*x - b) < Eps

x = tril(a) \ b
@assert norm(tril(a)*x - b) < Eps

end
