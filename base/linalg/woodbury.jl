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

function show(io::IO, W::Woodbury)
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

function det(W::Woodbury)
    det(W.A)*det(W.C)/det(W.Cp)
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
