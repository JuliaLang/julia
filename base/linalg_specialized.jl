#### Specialized matrix types ####

# Some of these also have important routines defined in factorizations.jl,
# linalg_lapack.jl, etc.

#### Tridiagonal matrices ####
type Tridiagonal{T} <: AbstractMatrix{T}
    dl::Vector{T}   # sub-diagonal
    d::Vector{T}   # diagonal
    du::Vector{T}   # sup-diagonal
    dutmp::Vector{T}  # scratch space for vector RHS solver, sup-diagonal
    rhstmp::Vector{T}  # scratch space, rhs

    function Tridiagonal(N::Int)
        dutmp = Array(T, N-1)
        rhstmp = Array(T, N)
        new(dutmp, rhstmp, dutmp, dutmp, rhstmp)  # first three will be overwritten
    end
end
function Tridiagonal{T<:Number}(dl::Vector{T}, d::Vector{T}, du::Vector{T})
    N = length(d)
    if length(dl) != N-1 || length(du) != N-1
        error("The sub- and super-diagonals must have length N-1")
    end
    M = Tridiagonal{T}(N)
    M.dl = copy(dl)
    M.d = copy(d)
    M.du = copy(du)
    return M
end
function Tridiagonal{Tl<:Number, Td<:Number, Tu<:Number}(dl::Vector{Tl}, d::Vector{Td}, du::Vector{Tu})
    R = promote(Tl, Td, Tu)
    Tridiagonal(convert(Vector{R}, dl), convert(Vector{R}, d), convert(Vector{R}, du))
end
size(M::Tridiagonal) = (length(M.d), length(M.d))
function show(io, M::Tridiagonal)
    println(io, summary(M), ":")
    print(io, " sub: ")
    print_matrix(io, (M.dl)')
    print(io, "\ndiag: ")
    print_matrix(io, (M.d)')
    print(io, "\n sup: ")
    print_matrix(io, (M.du)')
end
full{T}(M::Tridiagonal{T}) = convert(Matrix{T}, M)
function convert{T}(::Type{Matrix{T}}, M::Tridiagonal{T})
    A = zeros(T, size(M))
    for i = 1:length(M.d)
        A[i,i] = M.d[i]
    end
    for i = 1:length(M.d)-1
        A[i+1,i] = M.dl[i]
        A[i,i+1] = M.du[i]
    end
    return A
end
function similar(M::Tridiagonal, T, dims::Dims)
    if length(dims) != 2 || dims[1] != dims[2]
        error("Tridiagonal matrices must be square")
    end
    return Tridiagonal{T}(dims[1])
end
copy(M::Tridiagonal) = Tridiagonal(M.dl, M.d, M.du)

# Operations on Tridiagonal matrices
round(M::Tridiagonal) = Tridiagonal(round(M.dl), round(M.d), round(M.du))
iround(M::Tridiagonal) = Tridiagonal(iround(M.dl), iround(M.d), iround(M.du))

## Solvers
# Allocation-free variants
# Note that solve is non-aliasing, so you can use the same array for
# input and output
function solve(x::AbstractArray, xrng::Ranges{Int}, M::Tridiagonal, rhs::AbstractArray, rhsrng::Ranges{Int})
    d = M.d
    N = length(d)
    if length(xrng) != N || length(rhsrng) != N
        error("dimension mismatch")
    end
    dl = M.dl
    du = M.du
    dutmp = M.dutmp
    rhstmp = M.rhstmp
    xstart = first(xrng)
    xstride = step(xrng)
    rhsstart = first(rhsrng)
    rhsstride = step(rhsrng)
    # Forward sweep
    denom = d[1]
    dulast = du[1] / denom
    dutmp[1] = dulast
    rhslast = rhs[rhsstart] / denom
    rhstmp[1] = rhslast
    irhs = rhsstart+rhsstride
    for i in 2:N-1
        dltmp = dl[i-1]
        denom = d[i] - dltmp*dulast
        dulast = du[i] / denom
        dutmp[i] = dulast
        rhslast = (rhs[irhs] - dltmp*rhslast)/denom
        rhstmp[i] = rhslast
        irhs += rhsstride
    end
    dltmp = dl[N-1]
    denom = d[N] - dltmp*dulast
    xlast = (rhs[irhs] - dltmp*rhslast)/denom
    # Backward sweep
    ix = xstart + (N-2)*xstride
    x[ix+xstride] = xlast
    for i in N-1:-1:1
        xlast = rhstmp[i] - dutmp[i]*xlast
        x[ix] = xlast
        ix -= xstride
    end
    return x
end
solve(x::StridedVector, M::Tridiagonal, rhs::StridedVector) = solve(x, 1:length(x), M, rhs, 1:length(rhs))
function solve(M::Tridiagonal, rhs::StridedVector)
    x = similar(rhs)
    solve(x, M, rhs)
end
function solve(X::StridedMatrix, M::Tridiagonal, B::StridedMatrix)
    if size(B, 1) != size(M, 1)
        error("dimension mismatch")
    end
    if size(X) != size(B)
        error("dimension mismatch in output")
    end
    m, n = size(B)
    r = 1:m
    for j = 1:n
        r.start = (j-1)*m+1
        solve(X, r, M, B, r)
    end
    return X
end
function solve(M::Tridiagonal, B::StridedMatrix)
    X = similar(B)
    solve(X, M, B)
end

# User-friendly solver
\(M::Tridiagonal, rhs::Union(StridedVector,StridedMatrix)) = solve(M, rhs)

# Tridiagonal multiplication
function mult(x::AbstractArray, xrng::Ranges{Int}, M::Tridiagonal, v::AbstractArray, vrng::Ranges{Int})
    dl = M.dl
    d = M.d
    du = M.du
    N = length(d)
    xi = first(xrng)
    xstride = step(xrng)
    vi = first(vrng)
    vstride = step(vrng)
    x[xi] = d[1]*v[vi] + du[1]*v[vi+vstride]
    xi += xstride
    for i = 2:N-1
        x[xi] = dl[i-1]*v[vi] + d[i]*v[vi+vstride] + du[i]*v[vi+2*vstride]
        xi += xstride
        vi += vstride
    end
    x[xi] = dl[N-1]*v[vi] + d[N]*v[vi+vstride]
    return x
end
mult(x::StridedVector, M::Tridiagonal, v::StridedVector) = mult(x, 1:length(x), M, v, 1:length(v))
function mult(X::StridedMatrix, M::Tridiagonal, B::StridedMatrix)
    if size(B, 1) != size(M, 1)
        error("dimension mismatch")
    end
    if size(X) != size(B)
        error("dimension mismatch in output")
    end
    m, n = size(B)
    r = 1:m
    for j = 1:n
        r.start = (j-1)*m+1
        mult(X, r, M, B, r)
    end
    return X
end
mult(X::StridedMatrix, M1::Tridiagonal, M2::Tridiagonal) = mult(X, M1, full(M2)) # OPTIMIZE ME (ideally, with banded matrices)
function *{T,S}(M::Tridiagonal{T}, B::StridedVecOrMat{S})
    X = Array(promote_type(T,S), size(B)...)
    mult(X, M, B)
end
*(A::Tridiagonal, B::Tridiagonal) = A*full(B)


#### Woodbury matrices ####
# This type provides support for the Woodbury matrix identity
type Woodbury{T} <: AbstractMatrix{T}
    A
    U::Matrix{T}
    C
    Cp
    V::Matrix{T}
    tmpN1::Vector{T}
    tmpN2::Vector{T}
    tmpk1::Vector{T}
    tmpk2::Vector{T}

    function Woodbury(A::AbstractMatrix{T}, U::Matrix{T}, C, V::Matrix{T})
        N = size(A, 1)
        k = size(U, 2)
        if size(A, 2) != N || size(U, 1) != N || size(V, 1) != k || size(V, 2) != N
            error("Sizes do not match")
        end
        if k > 1
            if size(C, 1) != k || size(C, 2) != k
                error("Size of C is incorrect")
            end
        end
        Cp = inv(inv(C) + V*(A\U))
        # temporary space for allocation-free solver
        tmpN1 = Array(T, N)
        tmpN2 = Array(T, N)
        tmpk1 = Array(T, k)
        tmpk2 = Array(T, k)
        # don't copy A, it could be huge
        new(A, copy(U), copy(C), Cp, copy(V), tmpN1, tmpN2, tmpk1, tmpk2)
    end
end
Woodbury{T}(A::AbstractMatrix{T}, U::Matrix{T}, C, V::Matrix{T}) = Woodbury{T}(A, U, C, V)
Woodbury{T}(A::AbstractMatrix{T}, U::Vector{T}, C, V::Matrix{T}) = Woodbury{T}(A, reshape(U, length(U), 1), C, V)

size(W::Woodbury) = size(W.A)
function show(io, W::Woodbury)
    println(io, summary(W), ":")
    print(io, "A: ", W.A)
    print(io, "\nU:\n")
    print_matrix(io, W.U)
    if isa(W.C, Matrix)
        print(io, "\nC:\n")
        print_matrix(io, W.C)
    else
        print(io, "\nC: ", W.C)
    end
    print(io, "\nV:\n")
    print_matrix(io, W.V)
end
full{T}(W::Woodbury{T}) = convert(Matrix{T}, W)
convert{T}(::Type{Matrix{T}}, W::Woodbury{T}) = full(W.A) + W.U*W.C*W.V
function similar(W::Woodbury, T, dims::Dims)
    if length(dims) != 2 || dims[1] != dims[2]
        error("Woodbury matrices must be square")
    end
    n = size(W, 1)
    k = size(W.U, 2)
    return Woodbury{T}(similar(W.A), Array(T, n, k), Array(T, k, k), Array(T, k, n))
end
copy(W::Woodbury) = Woodbury(W.A, W.U, W.C, W.V)

## Woodbury matrix routines ##

function *(W::Woodbury, B::StridedVecOrMat)
    return W.A*B + W.U*(W.C*(W.V*B))
end
function \(W::Woodbury, R::StridedVecOrMat)
    AinvR = W.A\R
    return AinvR - W.A\(W.U*(W.Cp*(W.V*AinvR)))
end
# Allocation-free solver for arbitrary strides (requires that W.A has a
# non-aliasing "solve" routine, e.g., is Tridiagonal)
function solve(x::AbstractArray, xrng::Ranges{Int}, W::Woodbury, rhs::AbstractArray, rhsrng::Ranges{Int})
    solve(W.tmpN1, 1:length(W.tmpN1), W.A, rhs, rhsrng)
    A_mul_B(W.tmpk1, W.V, W.tmpN1)
    A_mul_B(W.tmpk2, W.Cp, W.tmpk1)
    A_mul_B(W.tmpN2, W.U, W.tmpk2)
    solve(W.tmpN2, W.A, W.tmpN2)
    indx = first(xrng)
    xinc = step(xrng)
    for i = 1:length(W.tmpN2)
        x[indx] = W.tmpN1[i] - W.tmpN2[i]
        indx += xinc
    end
end
solve(x::AbstractVector, W::Woodbury, rhs::AbstractVector) = solve(x, 1:length(x), W, rhs, 1:length(rhs))
function solve(W::Woodbury, rhs::AbstractVector)
    x = similar(rhs)
    solve(x, W, rhs)
end
function solve(X::StridedMatrix, W::Woodbury, B::StridedMatrix)
    if size(B, 1) != size(W, 1)
        error("dimension mismatch")
    end
    if size(X) != size(B)
        error("dimension mismatch in output")
    end
    m, n = size(B)
    r = 1:m
    for j = 1:n
        r.start = (j-1)*m+1
        solve(X, r, W, B, r)
    end
    return X
end
function solve(W::Woodbury, B::StridedMatrix)
    X = similar(B)
    solve(X, W, B)
end
