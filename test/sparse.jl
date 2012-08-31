cd("../extras") do
require("linalg_suitesparse.jl")

# check matrix operations
se33 = speye(3)
@assert isequal(se33 * se33, se33)

# check mixed sparse-dense matrix operations
do33 = ones(3)
@assert isequal(se33 \ do33, do33)

# check horiz concatenation
@assert all([se33 se33] == sparse([1, 2, 3, 1, 2, 3], [1, 2, 3, 4, 5, 6], ones(6)))

# check vert concatenation
@assert all([se33; se33] == sparse([1, 4, 2, 5, 3, 6], [1, 1, 2, 2, 3, 3], ones(6)))

# check h+v concatenation
se44 = speye(4)
sz42 = spzeros(4, 2)
sz41 = spzeros(4, 1)
sz34 = spzeros(3, 4)
se77 = speye(7)
@assert all([se44 sz42 sz41; sz34 se33] == se77)

# check concatenation promotion
sz41_f32 = spzeros(Float32, 4, 1)
se33_i32 = speye(Int32, 3, 3)
@assert all([se44 sz42 sz41_f32; sz34 se33_i32] == se77)

# check mixed sparse-dense concatenation
sz33 = spzeros(3)
de33 = eye(3)
@assert  all([se33 de33; sz33 se33] == full([se33 se33; sz33 se33 ]))

# check splicing + concatenation on
# random instances, with nested vcat
# (also side-checks sparse ref, which uses
# sparse multiplication)
for i = 1 : 10
    a = sprand(5, 4, 0.5)
    @assert all([a[1:2,1:2] a[1:2,3:4]; a[3:5,1] [a[3:4,2:4]; a[5,2:4]]] == a)
end

# check basic tridiagonal operations
n = 5
d = 1 + rand(n)
dl = -rand(n-1)
du = -rand(n-1)
T = Tridiagonal(dl, d, du)
@assert size(T, 1) == n
@assert size(T) == (n, n)
F = diagm(d)
for i = 1:n-1
    F[i,i+1] = du[i]
    F[i+1,i] = dl[i]
end
@assert full(T) == F

# tridiagonal linear algebra
Eps = sqrt(eps())
v = randn(n)
@assert T*v == F*v
invFv = F\v
@assert norm(T\v - invFv) < Eps
@assert norm(solve(T,v) - invFv) < Eps
B = randn(n,2)
@assert norm(solve(T, B) - F\B) < Eps
Tc = copy(T)
vc = copy(v)
Tlu, ipiv = _jl_lapack_gttrf(Tc)
x = _jl_lapack_gttrs('N', Tlu, ipiv, vc)
@assert norm(x - invFv) < Eps

# symmetric tridiagonal
Ts = Tridiagonal(dl, d, dl)
Fs = full(Ts)
Tsc = copy(Ts)
vc = copy(v)
invFsv = Fs\v
Tlu, x = _jl_lapack_ptsv(Tsc, vc)
@assert norm(x - invFsv) < Eps
Tsc = copy(Ts)
vc = copy(v)
Tlu = _jl_lapack_pttrf(Tsc)
x = _jl_lapack_pttrs(Tlu, vc)
@assert norm(x - invFsv) < Eps

# eigenvalues/eigenvectors of symmetric tridiagonal
DT, VT = eig(Ts)
D, V = eig(Fs)
@assert norm(DT - D) < Eps
@assert norm(VT - V) < Eps


# Woodbury
U = randn(n,2)
V = randn(2,n)
C = randn(2,2)
W = Woodbury(T, U, C, V)
F = full(W)

@assert norm(W*v - F*v) < Eps
@assert norm(W\v - F\v) < Eps

end # cd
