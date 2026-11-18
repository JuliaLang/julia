# Sparse and dense linear algebra that must stay statically dispatchable under `--trim`

using SparseArrays
using LinearAlgebra

# A = [1 0 4; 0 2 0; 0 0 3]
const SPARSE_A = sparse([1, 2, 3, 1], [1, 2, 3, 3], [1.0, 2.0, 3.0, 4.0])
const DENSE_A = Matrix(SPARSE_A)
const LINALG_X = [1.0, 2.0, 3.0]

function _test_sparse_constructors()
    A = sparse([1, 2, 3], [1, 2, 3], [1.0, 2.0, 3.0])
    B = spdiagm(0 => ones(3), 1 => ones(2))
    C = sparse([1, 1, 2], [1, 1, 2], [1.0, 2.0, 3.0], 2, 2, +)
    println(Core.stdout, "sparse constructors: ", nnz(A), " ", nnz(B), " ", nnz(C), " ", C[1, 1])
end

function _test_dense_constructors()
    A = Matrix(Diagonal([1.0, 2.0, 3.0]))
    B = Matrix(Bidiagonal(ones(3), ones(2), :U))
    C = Matrix{Float64}(I, 2, 2) + ones(2, 2)
    println(Core.stdout, "dense constructors: ", sum(A), " ", sum(B), " ", sum(C), " ", C[1, 1])
end

function _test_sparse_products()
    A, x = SPARSE_A, LINALG_X
    matvec = sum(A * x)
    y = zeros(3)
    mul!(y, A, x)
    matmat = nnz(A * A)
    kronecker = nnz(kron(A, A))
    println(Core.stdout, "sparse products: ", matvec, " ", sum(y), " ", matmat, " ", kronecker)
end

function _test_dense_products()
    A, x = DENSE_A, LINALG_X
    matvec = sum(A * x)
    y = zeros(3)
    mul!(y, A, x)
    matmat = sum(A * A)
    kronecker = sum(kron(A, A))
    println(Core.stdout, "dense products: ", matvec, " ", sum(y), " ", matmat, " ", kronecker)
end

function _test_sparse_structure()
    A = SPARSE_A
    transposed = nnz(sparse(transpose(A)))
    indexed = A[1, 3] + A[2, 2]
    B = copy(A)
    B[2, 1] = 5.0
    B[1, 1] = 0.0
    dropped = nnz(dropzeros!(B))
    permuted = nnz(permute(A, [3, 1, 2], [1, 2, 3]))
    println(Core.stdout, "sparse structure: ", transposed, " ", indexed, " ", dropped, " ", permuted)
end

function _test_dense_structure()
    A = DENSE_A
    transposed = sum(Matrix(transpose(A)) * A)
    indexed = A[1, 3] + A[2, 2]
    B = copy(A)
    B[2, 1] = 5.0
    B[1, 1] = 0.0
    upper = sum(triu(B))
    permuted = A[[3, 1, 2], [1, 2, 3]][1, 3]
    println(Core.stdout, "dense structure: ", transposed, " ", indexed, " ", upper, " ", permuted)
end

function _test_sparse_broadcast()
    A = SPARSE_A
    scaled = nnz(A .* 2.0) + nnz(A .+ A)
    densified = sum(A .+ 1.0)
    found = length(findnz(A)[1])
    concatenated = nnz([A A; A A])
    println(Core.stdout, "sparse broadcast: ", scaled, " ", densified, " ", found, " ", concatenated)
end

function _test_dense_broadcast()
    A = DENSE_A
    scaled = sum(A .* 2.0) + sum(A .+ A)
    shifted = sum(A .+ 1.0)
    found = count(!iszero, A)
    concatenated = sum([A A; A A])
    println(Core.stdout, "dense broadcast: ", scaled, " ", shifted, " ", found, " ", concatenated)
end

function _test_sparse_vectors()
    A = SPARSE_A
    v = sparsevec([1, 3], [2.0, 4.0], 5)
    dotted = dot(v, v .* 2.0)
    added = nnz(v + v)
    diagonal = sum(diag(A))
    densesum = sum(Matrix(A))
    println(Core.stdout, "sparse vectors: ", dotted, " ", added, " ", diagonal, " ", densesum)
end

function _test_dense_vectors()
    A = DENSE_A
    v = [2.0, 0.0, 4.0, 0.0, 0.0]
    dotted = dot(v, v .* 2.0)
    added = count(!iszero, v + v)
    diagonal = sum(diag(A))
    crossed = sum(cross(LINALG_X, [3.0, 2.0, 1.0]))
    println(Core.stdout, "dense vectors: ", dotted, " ", added, " ", diagonal, " ", crossed)
end

function _test_sparse_reductions()
    A = SPARSE_A
    total = sum(A)
    firstrow = sum(A, dims = 2)[1]
    trace = tr(A)
    frobenius = norm(A)
    println(Core.stdout, "sparse reductions: ", total, " ", firstrow, " ", trace, " ", frobenius)
end

function _test_dense_reductions()
    A = DENSE_A
    total = sum(A)
    firstrow = sum(A, dims = 2)[1]
    trace = tr(A)
    frobenius = norm(A)
    println(Core.stdout, "dense reductions: ", total, " ", firstrow, " ", trace, " ", frobenius)
end

function _test_sparse_nested_reductions()
    A = SPARSE_A
    onenorm = opnorm(A, 1)
    colnorm = maximum(norm(view(A, :, j)) for j in axes(A, 2))
    nested = sum(a -> sum(b -> a * b, nonzeros(A), init = 0.0), nonzeros(A), init = 0.0)
    println(Core.stdout, "sparse nested reductions: ", onenorm, " ", colnorm, " ", nested)
end

function _test_dense_nested_reductions()
    A = DENSE_A
    onenorm = opnorm(A, 1)
    colnorm = maximum(norm(view(A, :, j)) for j in axes(A, 2))
    nested = sum(a -> sum(b -> a * b, A, init = 0.0), A, init = 0.0)
    println(Core.stdout, "dense nested reductions: ", onenorm, " ", colnorm, " ", nested)
end

function _test_sparse_solves()
    A, x = SPARSE_A, LINALG_X
    triangular = round(sum(UpperTriangular(A) \ x), digits = 6)
    backslash = round(sum(A \ x), digits = 6)
    lusolve = round(sum(lu(A) \ x), digits = 6)
    println(Core.stdout, "sparse solves: ", triangular, " ", backslash, " ", lusolve)
end

function _test_dense_solves()
    A, x = DENSE_A, LINALG_X
    triangular = round(sum(UpperTriangular(A) \ x), digits = 6)
    backslash = round(sum(A \ x), digits = 6)
    lusolve = round(sum(lu(A) \ x), digits = 6)
    println(Core.stdout, "dense solves: ", triangular, " ", backslash, " ", lusolve)
end

function _test_sparse_factorizations()
    A, x = SPARSE_A, LINALG_X
    S = sparse([2.0 1.0; 1.0 2.0])
    b = [1.0, 1.0]
    cholsolve = round(sum(cholesky(S) \ b), digits = 6)
    ldltsolve = round(sum(ldlt(S) \ b), digits = 6)
    qrsolve = round(sum(qr(A) \ x), digits = 6)
    println(Core.stdout, "sparse factorizations: ", cholsolve, " ", ldltsolve, " ", qrsolve)
end

function _test_dense_factorizations()
    A, x = DENSE_A, LINALG_X
    S = [2.0 1.0; 1.0 2.0]
    b = [1.0, 1.0]
    cholsolve = round(sum(cholesky(S) \ b), digits = 6)
    bksolve = round(sum(bunchkaufman(S) \ b), digits = 6)
    qrsolve = round(sum(qr(A) \ x), digits = 6)
    println(Core.stdout, "dense factorizations: ", cholsolve, " ", bksolve, " ", qrsolve)
end

function _test_dense_decompositions()
    A = DENSE_A
    S = [2.0 1.0; 1.0 2.0]
    determinant = round(det(A), digits = 6)
    inverse = round(sum(inv(S)), digits = 6)
    singular = round(sum(svdvals(S)), digits = 6)
    println(Core.stdout, "dense decompositions: ", determinant, " ", inverse, " ", singular)
end

# TODO: needs LinearAlgebra to specialize on `sortby` and on the number of `diagm` pairs
function _test_dense_eigen()
    A = DENSE_A
    S = [2.0 1.0; 1.0 2.0]
    symvals = round(sum(eigvals(Symmetric(S))), digits = 6)
    symmax = round(maximum(eigen(Symmetric(S)).values), digits = 6)
    genvals = round(sum(real, eigvals(A)), digits = 6)
    sorted = eigen(Symmetric(S); sortby = x -> -x).values[1]
    banded = sum(diagm(0 => ones(3), 1 => ones(2)))
    println(Core.stdout, "dense eigen: ", symvals, " ", symmax, " ", genvals, " ", sorted, " ", banded)
end

function _test_linalg()
    _test_sparse_constructors()
    _test_dense_constructors()
    _test_sparse_products()
    _test_dense_products()
    _test_sparse_structure()
    _test_dense_structure()
    _test_sparse_broadcast()
    _test_dense_broadcast()
    _test_sparse_vectors()
    _test_dense_vectors()
    _test_sparse_reductions()
    _test_dense_reductions()
    _test_sparse_nested_reductions()
    _test_dense_nested_reductions()
    _test_sparse_solves()
    _test_dense_solves()
    _test_sparse_factorizations()
    _test_dense_factorizations()
    _test_dense_decompositions()
    # TODO: needs LinearAlgebra to specialize on `sortby` and on the number of `diagm` pairs
    # _test_dense_eigen()
end
