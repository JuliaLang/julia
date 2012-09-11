# blas, lapack
Eps = sqrt(eps())

begin
local n
n = 10
a = rand(n,n)
asym = a+a'+n*eye(n)
b = rand(n)
r = factors(chol(asym))                          # Cholesky decomposition
@assert norm(r'*r - asym) < Eps

l = factors(chol!(copy(asym), 'L'))              # lower-triangular Cholesky decomposition
@assert norm(l*l' - asym) < Eps

(l,u,p) = factors(lu(a))                         # LU decomposition
@assert norm(l*u - a[p,:]) < Eps
@assert norm(l[invperm(p),:]*u - a) < Eps

(q,r) = factors(qr(a))                           # QR decomposition
@assert norm(q*r - a) < Eps

(q,r,p) = factors(qrp(a))                        # pivoted QR decomposition
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

## Test Julia fallbacks to BLAS routines
## 2x2
A = [1 2; 3 4]
B = [5 6; 7 8]
@assert A*B == [19 22; 43 50]
@assert At_mul_B(A, B) == [26 30; 38 44]
@assert A_mul_Bt(A, B) == [17 23; 39 53]
@assert At_mul_Bt(A, B) == [23 31; 34 46]
Ai = A+(0.5*im).*B
Bi = B+(2.5*im).*A[[2,1],[2,1]]
@assert Ai*Bi == [-21+53.5im -4.25+51.5im; -12+95.5im 13.75+85.5im]
@assert Ac_mul_B(Ai, Bi) == [68.5-12im 57.5-28im; 88-3im 76.5-25im]
@assert A_mul_Bc(Ai, Bi) == [64.5+5.5im 43+31.5im; 104-18.5im 80.5+31.5im]
@assert Ac_mul_Bc(Ai, Bi) == [-28.25-66im 9.75-58im; -26-89im 21-73im]
# 3x3
A = [1 2 3; 4 5 6; 7 8 9]-5
B = [1 0 5; 6 -10 3; 2 -4 -1]
@assert A*B == [-26 38 -27; 1 -4 -6; 28 -46 15]
@assert Ac_mul_B(A, B) == [-6 2 -25; 3 -12 -18; 12 -26 -11]
@assert A_mul_Bc(A, B) == [-14 0 6; 4 -3 -3; 22 -6 -12]
@assert Ac_mul_Bc(A, B) == [6 -8 -6; 12 -9 -9; 18 -10 -12]
Ai = A+(0.5*im).*B
Bi = B+(2.5*im).*A[[2,1,3],[2,3,1]]
@assert Ai*Bi == [-44.75+13im 11.75-25im -38.25+30im; -47.75-16.5im -51.5+51.5im -56+6im; 16.75-4.5im -53.5+52im -15.5im]
@assert Ac_mul_B(Ai, Bi) == [-21+2im -1.75+49im -51.25+19.5im; 25.5+56.5im -7-35.5im 22+35.5im; -3+12im -32.25+43im -34.75-2.5im]
@assert A_mul_Bc(Ai, Bi) == [-20.25+15.5im -28.75-54.5im 22.25+68.5im; -12.25+13im -15.5+75im -23+27im; 18.25+im 1.5+94.5im -27-54.5im]
@assert Ac_mul_Bc(Ai, Bi) == [1+2im 20.75+9im -44.75+42im; 19.5+17.5im -54-36.5im 51-14.5im; 13+7.5im 11.25+31.5im -43.25-14.5im]
# Generic integer matrix multiplication
A = [1 2 3; 4 5 6] - 3
B = [2 -2; 3 -5; -4 7]
@assert A*B == [-7 9; -4 9]
@assert At_mul_Bt(A, B) == [-6 -11 15; -6 -13 18; -6 -15 21]
A = ones(Int, 2, 100)
B = ones(Int, 100, 3)
@assert A*B == [100 100 100; 100 100 100]
A = randi(20, 5, 5) - 10
B = randi(20, 5, 5) - 10
@assert At_mul_B(A, B) == A'*B
@assert A_mul_Bt(A, B) == A*B'
# Preallocated
C = Array(Int, size(A, 1), size(B, 2))
@assert A_mul_B(C, A, B) == A*B
@assert At_mul_B(C, A, B) == A'*B
@assert A_mul_Bt(C, A, B) == A*B'
@assert At_mul_Bt(C, A, B) == A'*B'
# matrix algebra with subarrays of floats (stride != 1)
A = reshape(float64(1:20),5,4)
Aref = A[1:2:end,1:2:end]
Asub = sub(A, 1:2:5, 1:2:4)
b = [1.2,-2.5]
@assert (Aref*b) == (Asub*b)
@assert At_mul_B(Asub, Asub) == At_mul_B(Aref, Aref)
@assert A_mul_Bt(Asub, Asub) == A_mul_Bt(Aref, Aref)
Ai = A + im
Aref = Ai[1:2:end,1:2:end]
Asub = sub(Ai, 1:2:5, 1:2:4)
@assert Ac_mul_B(Asub, Asub) == Ac_mul_B(Aref, Aref)
@assert A_mul_Bc(Asub, Asub) == A_mul_Bc(Aref, Aref)
# syrk & herk
A = reshape(1:1503, 501, 3)-750.0
res = float64([135228751 9979252 -115270247; 9979252 10481254 10983256; -115270247 10983256 137236759])
@assert At_mul_B(A, A) == res
@assert A_mul_Bt(A',A') == res
cutoff = 501
A = reshape(1:6*cutoff,2*cutoff,3)-(6*cutoff)/2
Asub = sub(A, 1:2:2*cutoff, 1:3)
Aref = A[1:2:2*cutoff, 1:3]
@assert At_mul_B(Asub, Asub) == At_mul_B(Aref, Aref)
Ai = A - im
Asub = sub(Ai, 1:2:2*cutoff, 1:3)
Aref = Ai[1:2:2*cutoff, 1:3]
@assert Ac_mul_B(Asub, Asub) == Ac_mul_B(Aref, Aref)
A1  = float([4 2 0; 1 4 1; 1 1 4])
eA1 = [147.866622446369 127.781085523181  127.781085523182;
       183.765138646367 183.765138646366  163.679601723179;
        71.797032399996  91.8825693231832 111.968106246371]'
@assert norm((expm(A1) - eA1) ./ eA1) < 1000*eps()
A2  = [29.87942128909879    0.7815750847907159 -2.289519314033932;
        0.7815750847907159 25.72656945571064    8.680737820540137;
       -2.289519314033932   8.680737820540137  34.39400925519054]
eA2 = [  5496313853692216. -18231880972008932. -30475770808579672.;
       -18231880972008928.  60605228702222480. 101291842930249776.;
       -30475770808579672. 101291842930249808. 169294411240850528]
@assert norm((expm(A2) - eA2) ./ eA2) < 1000*eps()
A3  = float([-131 19 18;-390 56 54;-387 57 52])
eA3 = [-1.50964415879218 -5.6325707998812  -4.934938326092;
        0.367879439109187 1.47151775849686  1.10363831732856;
        0.135335281175235 0.406005843524598 0.541341126763207]'
@assert norm((expm(A3) - eA3) ./ eA3) < 50000*eps()


# basic tridiagonal operations
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
@assert norm(T*v - F*v) < Eps
invFv = F\v
@assert norm(T\v - invFv) < Eps
@assert norm(solve(T,v) - invFv) < Eps
B = randn(n,2)
@assert norm(solve(T, B) - F\B) < Eps
Tlu = lu(T)
x = Tlu\v
@assert norm(x - invFv) < Eps

# symmetric tridiagonal
Ts = Tridiagonal(dl, d+1, dl)
Fs = full(Ts)
invFsv = Fs\v
Tldlt = ldlt(Ts)
x = Tldlt\v
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


end
