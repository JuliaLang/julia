# check sparse matrix construction
@test isequal(full(sparse(complex(ones(5,5),ones(5,5)))), complex(ones(5,5),ones(5,5)))

# check matrix operations
se33 = speye(3)
do33 = ones(3)
@test isequal(se33 * se33, se33)

# check sparse binary op
@test all(full(se33 + convert(SparseMatrixCSC{Float32,Int32}, se33)) == 2*eye(3))
@test all(full(se33 * convert(SparseMatrixCSC{Float32,Int32}, se33)) == eye(3))

# check horiz concatenation
@test all([se33 se33] == sparse([1, 2, 3, 1, 2, 3], [1, 2, 3, 4, 5, 6], ones(6)))

# check vert concatenation
@test all([se33; se33] == sparse([1, 4, 2, 5, 3, 6], [1, 1, 2, 2, 3, 3], ones(6)))

# check h+v concatenation
se44 = speye(4)
sz42 = spzeros(4, 2)
sz41 = spzeros(4, 1)
sz34 = spzeros(3, 4)
se77 = speye(7)
@test all([se44 sz42 sz41; sz34 se33] == se77)

# check blkdiag concatenation
@test all(blkdiag(se33, se33) == sparse([1, 2, 3, 4, 5, 6], [1, 2, 3, 4, 5, 6], ones(6)))

# check concatenation promotion
sz41_f32 = spzeros(Float32, 4, 1)
se33_i32 = speye(Int32, 3, 3)
@test all([se44 sz42 sz41_f32; sz34 se33_i32] == se77)

# check mixed sparse-dense concatenation
sz33 = spzeros(3, 3)
de33 = eye(3)
@test  all([se33 de33; sz33 se33] == full([se33 se33; sz33 se33 ]))

# check splicing + concatenation on
# random instances, with nested vcat
# also side-checks sparse ref
for i = 1 : 10
    a = sprand(5, 4, 0.5)
    @test all([a[1:2,1:2] a[1:2,3:4]; a[3:5,1] [a[3:4,2:4]; a[5,2:4]]] == a)
end

# sparse ref
a116 = reshape(1:16, 4, 4)
s116 = sparse(a116)
p = [4, 1, 2, 3, 2]
@test full(s116[p,:]) == a116[p,:]
@test full(s116[:,p]) == a116[:,p]
@test full(s116[p,p]) == a116[p,p]

# sparse assign
p = [4, 1, 3]
a116[p, p] = -1
s116[p, p] = -1
@test a116 == s116

p = [2, 1, 4]
a116[p, p] = reshape(1:9, 3, 3)
s116[p, p] = reshape(1:9, 3, 3)
@test a116 == s116

# matrix-vector multiplication (non-square)
for i = 1:5
    a = sprand(10, 5, 0.5)
    b = rand(5)
    @test maximum(abs(a*b - full(a)*b)) < 100*eps()
end

# complex matrix-vector multiplication and left-division
for i = 1:5
    a = speye(5) + 0.1*sprandn(5, 5, 0.2)
    b = randn(5,3) + im*randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    a = speye(5) + 0.1*sprandn(5, 5, 0.2) + 0.1*im*sprandn(5, 5, 0.2)
    b = randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    a = speye(5) + tril(0.1*sprandn(5, 5, 0.2))
    b = randn(5,3) + im*randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    a = speye(5) + tril(0.1*sprandn(5, 5, 0.2) + 0.1*im*sprandn(5, 5, 0.2))
    b = randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    a = speye(5) + triu(0.1*sprandn(5, 5, 0.2))
    b = randn(5,3) + im*randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    a = speye(5) + triu(0.1*sprandn(5, 5, 0.2) + 0.1*im*sprandn(5, 5, 0.2))
    b = randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    a = speye(5) + triu(0.1*sprandn(5, 5, 0.2))
    b = randn(5,3) + im*randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    a = spdiagm(randn(5)) + im*spdiagm(randn(5))
    b = randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    b = randn(5,3) + im*randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())
end

# matrix multiplication and kron
for i = 1:5
    a = sprand(10, 5, 0.7)
    b = sprand(5, 15, 0.3)
    @test maximum(abs(a*b - full(a)*full(b))) < 100*eps()
    @test maximum(abs(Base.LinAlg.spmatmul(a,b,sortindices=:sortcols) - full(a)*full(b))) < 100*eps()
    @test maximum(abs(Base.LinAlg.spmatmul(a,b,sortindices=:doubletranspose) - full(a)*full(b))) < 100*eps()
    @test full(kron(a,b)) == kron(full(a), full(b))
end

# reductions
@test sum(se33)[1] == 3.0
@test sum(se33, 1) == [1.0 1.0 1.0]
@test sum(se33, 2) == [1.0 1.0 1.0]'
@test prod(se33)[1] == 0.0
@test prod(se33, 1) == [0.0 0.0 0.0]
@test prod(se33, 2) == [0.0 0.0 0.0]'

# spdiagm
@test full(spdiagm((ones(2), ones(2)), (0, -1), 3, 3)) ==
                       [1.0  0.0  0.0; 1.0  1.0  0.0;  0.0  1.0  0.0]

# elimination tree
## upper triangle of the pattern test matrix from Figure 4.2 of
## "Direct Methods for Sparse Linear Systems" by Tim Davis, SIAM, 2006
rowval = int32([1,2,2,3,4,5,1,4,6,1,7,2,5,8,6,9,3,4,6,8,10,3,5,7,8,10,11])
colval = int32([1,2,3,3,4,5,6,6,6,7,7,8,8,8,9,9,10,10,10,10,10,11,11,11,11,11,11])
A = sparse(rowval, colval, ones(length(rowval)))
P,post = Base.LinAlg.etree(A, true)
@test P == int32([6,3,8,6,8,7,9,10,10,11,0])
@test post == int32([2,3,5,8,1,4,6,7,9,10,11])

# issue #4986, reinterpret
sfe22 = speye(Float64, 2)
mfe22 = eye(Float64, 2)
@test reinterpret(Int64, sfe22) == reinterpret(Int64, mfe22)

# issue #5190
@test_throws DimensionMismatch sparsevec([3,5,7],[0.1,0.0,3.2],4)

# issue #5169
@test nnz(sparse([1,1],[1,2],[0.0,-0.0])) == 0

# issue #5386
K,J,V = findnz(SparseMatrixCSC(2,1,[1,3],[1,2],[1.0,0.0]))
@test length(K) == length(J) == length(V) == 1

# https://groups.google.com/d/msg/julia-users/Yq4dh8NOWBQ/GU57L90FZ3EJ
A = speye(Bool, 5)
@test find(A) == find(x -> x == true, A) == find(full(A))

# issue #5437
@test nnz(sparse([1,2,3],[1,2,3],[0.0,1.0,2.0])) == 2

# issue #5824
@test sprand(4,5,0.5).^0 == sparse(ones(4,5))

# issue #5985
@test sprandbool(4, 5, 0.0) == sparse(zeros(Bool, 4, 5))
@test sprandbool(4, 5, 1.00) == sparse(ones(Bool, 4, 5))
sprb45nnzs = zeros(5)
for i=1:5
    sprb45 = sprandbool(4, 5, 0.5)
    @test length(sprb45) == 20
    sprb45nnzs[i] = sum(sprb45)[1]
end
@test 4 <= mean(sprb45nnzs) <= 16

# issue #5853, sparse diff
for i=1:2, a=Any[[1 2 3], [1 2 3]', eye(3)]
    @test all(diff(sparse(a),i) == diff(a,i))
end

# test for "access to undefined error" types that initially allocate elements as #undef
@test all(sparse(1:2, 1:2, Number[1,2])^2 == sparse(1:2, 1:2, [1,4]))
sd1 = diff(sparse([1,1,1], [1,2,3], Number[1,2,3]), 1)

# issue #6036
P = spzeros(Float64, 3, 3)
for i = 1:3
    P[i,i] = i
end

@test minimum(P) === 0.0
@test maximum(P) === 3.0
@test minimum(-P) === -3.0
@test maximum(-P) === 0.0

@test maximum(P, (1,)) == [1.0 2.0 3.0]
@test maximum(P, (2,)) == reshape([1.0,2.0,3.0],3,1)
@test maximum(P, (1,2)) == reshape([3.0],1,1)

@test maximum(sparse(-ones(3,3))) == -1
@test minimum(sparse(ones(3,3))) == 1

# Unary functions
a = sprand(5,15, 0.5)
afull = full(a)
for op in (:sin, :cos, :tan, :ceil, :floor, :abs, :abs2)
    @eval begin
        @test ($op)(afull) == full($(op)(a))
    end
end

for op in (:ceil, :floor)
    @eval begin
        @test ($op)(Int,afull) == full($(op)(Int,a))
    end
end


# getindex tests
ni = 23
nj = 32
a116 = reshape(1:(ni*nj), ni, nj)
s116 = sparse(a116)

ad116 = diagm(diag(a116))
sd116 = sparse(ad116)

for (aa116, ss116) in [(a116, s116), (ad116, sd116)]
    ij=11; i=3; j=2
    @test ss116[ij] == aa116[ij]
    @test ss116[(i,j)] == aa116[i,j]
    @test ss116[i,j] == aa116[i,j]
    @test ss116[i-1,j] == aa116[i-1,j]
    ss116[i,j] = 0
    @test ss116[i,j] == 0
    ss116 = sparse(aa116)

    # range indexing
    @test full(ss116[i,:]) == aa116[i,:]
    @test full(ss116[:,j]) == aa116[:,j]'' # sparse matrices/vectors always have ndims==2:
    @test full(ss116[i,1:2:end]) == aa116[i,1:2:end]
    @test full(ss116[1:2:end,j]) == aa116[1:2:end,j]''
    @test full(ss116[i,end:-2:1]) == aa116[i,end:-2:1]
    @test full(ss116[end:-2:1,j]) == aa116[end:-2:1,j]''
    # float-range indexing is not supported

    # sorted vector indexing
    @test full(ss116[i,[3:2:end-3]]) == aa116[i,[3:2:end-3]]
    @test full(ss116[[3:2:end-3],j]) == aa116[[3:2:end-3],j]''
    @test full(ss116[i,[end-3:-2:1]]) == aa116[i,[end-3:-2:1]]
    @test full(ss116[[end-3:-2:1],j]) == aa116[[end-3:-2:1],j]''

    # unsorted vector indexing with repetition
    p = [4, 1, 2, 3, 2, 6]
    @test full(ss116[p,:]) == aa116[p,:]
    @test full(ss116[:,p]) == aa116[:,p]
    @test full(ss116[p,p]) == aa116[p,p]

    # bool indexing
    li = randbool(size(aa116,1))
    lj = randbool(size(aa116,2))
    @test full(ss116[li,j]) == aa116[li,j]''
    @test full(ss116[li,:]) == aa116[li,:]
    @test full(ss116[i,lj]) == aa116[i,lj]
    @test full(ss116[:,lj]) == aa116[:,lj]
    @test full(ss116[li,lj]) == aa116[li,lj]
end

# workaround issue #7197: comment out let-block
#let S = SparseMatrixCSC(3, 3, UInt8[1,1,1,1], UInt8[], Int64[])
S1290 = SparseMatrixCSC(3, 3, UInt8[1,1,1,1], UInt8[], Int64[])
    S1290[1,1] = 1
    S1290[5] = 2
    S1290[end] = 3
    @test S1290[end] == (S1290[1] + S1290[2,2])
    @test 6 == sum(diag(S1290))
    @test (full(S1290)[[3,1],1])'' == full(S1290[[3,1],1])
# end


# setindex tests
let a = spzeros(Int, 10, 10)
    @test countnz(a) == 0
    a[1,:] = 1
    @test countnz(a) == 10
    @test a[1,:] == sparse(ones(Int,1,10))
    a[:,2] = 2
    @test countnz(a) == 19
    @test a[:,2] == 2*sparse(ones(Int,10,1))

    a[1,:] = 1:10
    @test a[1,:] == sparse([1:10]')
    a[:,2] = 1:10
    @test a[:,2] == sparse([1:10])
end

let A = spzeros(Int, 10, 20)
    A[1:5,1:10] = 10
    A[1:5,1:10] = 10
    @test countnz(A) == 50
    @test A[1:5,1:10] == 10 * ones(Int, 5, 10)
    A[6:10,11:20] = 0
    @test countnz(A) == 50
    A[6:10,11:20] = 20
    @test countnz(A) == 100
    @test A[6:10,11:20] == 20 * ones(Int, 5, 10)
    A[4:8,8:16] = 15
    @test countnz(A) == 121
    @test A[4:8,8:16] == 15 * ones(Int, 5, 9)
end

let ASZ = 1000, TSZ = 800
    A = sprand(ASZ, 2*ASZ, 0.0001)
    B = copy(A)
    nA = countnz(A)
    x = A[1:TSZ, 1:(2*TSZ)]
    nx = countnz(x)
    A[1:TSZ, 1:(2*TSZ)] = 0
    nB = countnz(A)
    @test nB == (nA - nx)
    A[1:TSZ, 1:(2*TSZ)] = x
    @test countnz(A) == nA
    @test A == B
    A[1:TSZ, 1:(2*TSZ)] = 10
    @test countnz(A) == nB + 2*TSZ*TSZ
    A[1:TSZ, 1:(2*TSZ)] = x
    @test countnz(A) == nA
    @test A == B
end

let A = speye(Int, 5), I=[1:10], X=reshape([trues(10), falses(15)],5,5)
    @test A[I] == A[X] == reshape([1,0,0,0,0,0,1,0,0,0], 10, 1)
    A[I] = [1:10]
    @test A[I] == A[X] == reshape([1:10], 10, 1)
end

let S = sprand(50, 30, 0.5, x->int(rand(x)*100)), I = sprandbool(50, 30, 0.2)
    FS = full(S)
    FI = full(I)
    @test sparse(FS[FI]) == S[I] == S[FI]
    @test sum(S[FI]) + sum(S[!FI]) == sum(S)

    sumS1 = sum(S)
    sumFI = sum(S[FI])
    S[FI] = 0
    @test sum(S[FI]) == 0
    sumS2 = sum(S)
    @test (sum(S) + sumFI) == sumS1

    S[FI] = 10
    @test sum(S) == sumS2 + 10*sum(FI)
    S[FI] = 0
    @test sum(S) == sumS2

    S[FI] = [1:sum(FI)]
    @test sum(S) == sumS2 + sum(1:sum(FI))
end

let S = sprand(50, 30, 0.5, x->int(rand(x)*100))
    N = length(S) >> 2
    I = randperm(N) .* 4
    J = randperm(N)
    sumS1 = sum(S)
    sumS2 = sum(S[I])
    S[I] = 0
    @test sum(S) == (sumS1 - sumS2)
    S[I] = J
    @test sum(S) == (sumS1 - sumS2 + sum(J))
end

#Issue 7507
@test (i7507=sparsevec(Dict{Int64, Float64}(), 10))==spzeros(10,1)

#Issue 7650
let S = spzeros(3, 3)
    @test size(reshape(S, 9, 1)) == (9,1)
end

let X = eye(5), M = rand(5,4), C = spzeros(3,3)
    SX = sparse(X); SM = sparse(M)
    VX = vec(X); VSX = vec(SX)
    VM = vec(M); VSM1 = vec(SM); VSM2 = sparsevec(M)
    VC = vec(C)
    @test reshape(VX, (25,1)) == VSX
    @test reshape(VM, (20,1)) == VSM1 == VSM2
    @test size(VC) == (9,1)
    @test nnz(VC) == 0
    @test nnz(VSX) == 5
end

#Issue 7677
let A = sprand(5,5,0.5,(n)->rand(Float64,n)), ACPY = copy(A)
    B = reshape(A,25,1)
    @test A == ACPY
    C = reinterpret(Int64, A, (25, 1))
    @test A == ACPY
    D = reinterpret(Int64, B)
    @test C == D
end

# indmax, indmin, findmax, findmin
let S = sprand(100,80, 0.5), A = full(S)
    @test indmax(S) == indmax(A)
    @test indmin(S) == indmin(A)
    @test findmin(S) == findmin(A)
    @test findmax(S) == findmax(A)
    for region in [(1,), (2,), (1,2)], m in [findmax, findmin]
        @test m(S, region) == m(A, region)
    end
end

let S = spzeros(10,8), A = full(S)
    @test indmax(S) == indmax(A) == 1
    @test indmin(S) == indmin(A) == 1
end

let A = Array(Int,0,0), S = sparse(A)
    iA = try indmax(A) end
    iS = try indmax(S) end
    @test iA === iS === nothing
    iA = try indmin(A) end
    iS = try indmin(S) end
    @test iA === iS === nothing
end

# issue #8225
@test_throws BoundsError sparse([0],[-1],[1.0],2,2)

# issue #8363
@test_throws BoundsError sparsevec(Dict(-1=>1,1=>2))

# issue #8976
@test conj(sparse([1im])) == sparse(conj([1im]))
@test conj!(sparse([1im])) == sparse(conj!([1im]))
