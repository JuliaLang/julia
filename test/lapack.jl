# blas, lapack
Eps = sqrt(eps())
n = 10
a = rand(n,n)
asym = a+a'+n*eye(n)
b = rand(n)
r = chol(asym)
@assert norm(r'*r - asym) < Eps
(l,u,p) = lu(a)
@assert norm(l*u - a[p,:]) < Eps
(q,r,s) = qr(a)
@assert norm(q*r - a[:,s]) < Eps
(d,v) = eig(asym)
@assert norm(asym*v[:,1]-d[1]*v[:,1]) < Eps
(d,v) = eig(a)
@assert norm(a*v[:,1]-d[1]*v[:,1]) < Eps
(U,s,vt) = svd(a)
@assert norm(U*diagm(s)*vt - a) < Eps
x = a \ b
@assert norm(a*x - b) < Eps
x = triu(a) \ b
@assert norm(triu(a)*x - b) < Eps
x = tril(a) \ b
@assert norm(tril(a)*x - b) < Eps

isPerm{T <: Integer}(p::Vector{T}) = all(int(1:length(p)) == int(sort(p)))

function invPerm{T <: Integer}(p::Vector{T})
    if !isPerm(p)
        error("p must be a permutation of 1:length(p)")
    end
    lp = length(p)
    ip = Array(Int, size(p))
    for i=1:lp; ip[p[i]] = i; end
    ip
end

@assert all(p == invPerm(invPerm(p)))
@assert norm(l[invPerm(p),:]*u - a) < 1e-8
