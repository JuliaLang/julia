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

l = chol!(copy(asym), 'L')              # lower-triangular Cholesky decomposition
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

# symmetric, positive definite
@assert norm(inv([6. 2; 2 1]) - [0.5 -1; -1 3]) < Eps
# symmetric, negative definite
@assert norm(inv([1. 2; 2 1]) - [-1. 2; 2 -1]/3) < Eps

## matrix algebra with subarrays (tests lapack vs. julia fallback) ##
A = reshape(float64(1:16),4,4)
Aref = A[1:2:end,1:2:end]
Asub = sub(A, 1:2:4, 1:2:4)
b = [1.2,-2.5]
@assert (Aref*b) == (Asub*b)

## transpose-multiply ##
b = float64([1:4])
v = zeros(4)
vt = zeros(4)
A_mul_x(v, A, b)
At_mul_x(vt, A', b)
@assert v == vt

A = reshape(1:16, 4, 4)  # Int bypasses lapack, test fallback
b = [1:4]
v = zeros(Int,4)
vt = zeros(Int,4)
A_mul_x(v, A, b)
At_mul_x(vt, A', b)
@assert v == vt


end
