# This file is a part of Julia. License is MIT: https://julialang.org/license

## Diagonal matrices

struct Diagonal{T,V<:AbstractVector{T}} <: AbstractMatrix{T}
    diag::V

    function Diagonal{T,V}(diag) where {T,V<:AbstractVector{T}}
        require_one_based_indexing(diag)
        new{T,V}(diag)
    end
end
Diagonal(v::AbstractVector{T}) where {T} = Diagonal{T,typeof(v)}(v)
Diagonal{T}(v::AbstractVector) where {T} = Diagonal(convert(AbstractVector{T}, v)::AbstractVector{T})

"""
    Diagonal(A::AbstractMatrix)

Construct a matrix from the diagonal of `A`.

# Examples
```jldoctest
julia> A = [1 2 3; 4 5 6; 7 8 9]
3×3 Array{Int64,2}:
 1  2  3
 4  5  6
 7  8  9

julia> Diagonal(A)
3×3 Diagonal{Int64,Array{Int64,1}}:
 1  ⋅  ⋅
 ⋅  5  ⋅
 ⋅  ⋅  9
```
"""
Diagonal(A::AbstractMatrix) = Diagonal(diag(A))

"""
    Diagonal(V::AbstractVector)

Construct a matrix with `V` as its diagonal.

# Examples
```jldoctest
julia> V = [1, 2]
2-element Array{Int64,1}:
 1
 2

julia> Diagonal(V)
2×2 Diagonal{Int64,Array{Int64,1}}:
 1  ⋅
 ⋅  2
```
"""
Diagonal(V::AbstractVector)

Diagonal(D::Diagonal) = D
Diagonal{T}(D::Diagonal{T}) where {T} = D
Diagonal{T}(D::Diagonal) where {T} = Diagonal{T}(D.diag)

AbstractMatrix{T}(D::Diagonal) where {T} = Diagonal{T}(D)
Matrix(D::Diagonal) = diagm(0 => D.diag)
Array(D::Diagonal) = Matrix(D)

# For D<:Diagonal, similar(D[, neweltype]) should yield a Diagonal matrix.
# On the other hand, similar(D, [neweltype,] shape...) should yield a sparse matrix.
# The first method below effects the former, and the second the latter.
similar(D::Diagonal, ::Type{T}) where {T} = Diagonal(similar(D.diag, T))
# The method below is moved to SparseArrays for now
# similar(D::Diagonal, ::Type{T}, dims::Union{Dims{1},Dims{2}}) where {T} = spzeros(T, dims...)

copyto!(D1::Diagonal, D2::Diagonal) = (copyto!(D1.diag, D2.diag); D1)

size(D::Diagonal) = (length(D.diag),length(D.diag))

function size(D::Diagonal,d::Integer)
    if d<1
        throw(ArgumentError("dimension must be ≥ 1, got $d"))
    end
    return d<=2 ? length(D.diag) : 1
end

@inline function getindex(D::Diagonal, i::Int, j::Int)
    @boundscheck checkbounds(D, i, j)
    if i == j
        @inbounds r = D.diag[i]
    else
        r = diagzero(D, i, j)
    end
    r
end
diagzero(::Diagonal{T},i,j) where {T} = zero(T)
diagzero(D::Diagonal{Matrix{T}},i,j) where {T} = zeros(T, size(D.diag[i], 1), size(D.diag[j], 2))

function setindex!(D::Diagonal, v, i::Int, j::Int)
    @boundscheck checkbounds(D, i, j)
    if i == j
        @inbounds D.diag[i] = v
    elseif !iszero(v)
        throw(ArgumentError("cannot set off-diagonal entry ($i, $j) to a nonzero value ($v)"))
    end
    return v
end


## structured matrix methods ##
function Base.replace_in_print_matrix(A::Diagonal,i::Integer,j::Integer,s::AbstractString)
    i==j ? s : Base.replace_with_centered_mark(s)
end

parent(D::Diagonal) = D.diag

ishermitian(D::Diagonal{<:Real}) = true
ishermitian(D::Diagonal{<:Number}) = isreal(D.diag)
ishermitian(D::Diagonal) = all(ishermitian, D.diag)
issymmetric(D::Diagonal{<:Number}) = true
issymmetric(D::Diagonal) = all(issymmetric, D.diag)
isposdef(D::Diagonal) = all(isposdef, D.diag)

factorize(D::Diagonal) = D

real(D::Diagonal) = Diagonal(real(D.diag))
imag(D::Diagonal) = Diagonal(imag(D.diag))

iszero(D::Diagonal) = all(iszero, D.diag)
isone(D::Diagonal) = all(isone, D.diag)
isdiag(D::Diagonal) = all(isdiag, D.diag)
isdiag(D::Diagonal{<:Number}) = true
istriu(D::Diagonal) = true
istril(D::Diagonal) = true
function triu!(D::Diagonal,k::Integer=0)
    n = size(D,1)
    if !(-n + 1 <= k <= n + 1)
        throw(ArgumentError(string("the requested diagonal, $k, must be at least ",
            "$(-n + 1) and at most $(n + 1) in an $n-by-$n matrix")))
    elseif k > 0
        fill!(D.diag,0)
    end
    return D
end

function tril!(D::Diagonal,k::Integer=0)
    n = size(D,1)
    if !(-n - 1 <= k <= n - 1)
        throw(ArgumentError(string("the requested diagonal, $k, must be at least ",
            "$(-n - 1) and at most $(n - 1) in an $n-by-$n matrix")))
    elseif k < 0
        fill!(D.diag,0)
    end
    return D
end

(==)(Da::Diagonal, Db::Diagonal) = Da.diag == Db.diag
(-)(A::Diagonal) = Diagonal(-A.diag)
(+)(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag + Db.diag)
(-)(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag - Db.diag)

(*)(x::Number, D::Diagonal) = Diagonal(x * D.diag)
(*)(D::Diagonal, x::Number) = Diagonal(D.diag * x)
(/)(D::Diagonal, x::Number) = Diagonal(D.diag / x)
(*)(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag .* Db.diag)
(*)(D::Diagonal, V::AbstractVector) = D.diag .* V

(*)(A::AbstractTriangular, D::Diagonal) =
    rmul!(copyto!(similar(A, promote_op(*, eltype(A), eltype(D.diag))), A), D)
(*)(D::Diagonal, B::AbstractTriangular) =
    lmul!(D, copyto!(similar(B, promote_op(*, eltype(B), eltype(D.diag))), B))

(*)(A::AbstractMatrix, D::Diagonal) =
    rmul!(copyto!(similar(A, promote_op(*, eltype(A), eltype(D.diag)), size(A)), A), D)
(*)(D::Diagonal, A::AbstractMatrix) =
    lmul!(D, copyto!(similar(A, promote_op(*, eltype(A), eltype(D.diag)), size(A)), A))

function rmul!(A::AbstractMatrix, D::Diagonal)
    require_one_based_indexing(A)
    A .= A .* permutedims(D.diag)
    return A
end

function lmul!(D::Diagonal, B::AbstractMatrix)
    require_one_based_indexing(B)
    B .= D.diag .* B
    return B
end

rmul!(A::Union{LowerTriangular,UpperTriangular}, D::Diagonal) = typeof(A)(rmul!(A.data, D))
function rmul!(A::UnitLowerTriangular, D::Diagonal)
    rmul!(A.data, D)
    for i = 1:size(A, 1)
        A.data[i,i] = D.diag[i]
    end
    LowerTriangular(A.data)
end
function rmul!(A::UnitUpperTriangular, D::Diagonal)
    rmul!(A.data, D)
    for i = 1:size(A, 1)
        A.data[i,i] = D.diag[i]
    end
    UpperTriangular(A.data)
end

function lmul!(D::Diagonal, B::UnitLowerTriangular)
    lmul!(D, B.data)
    for i = 1:size(B, 1)
        B.data[i,i] = D.diag[i]
    end
    LowerTriangular(B.data)
end
function lmul!(D::Diagonal, B::UnitUpperTriangular)
    lmul!(D, B.data)
    for i = 1:size(B, 1)
        B.data[i,i] = D.diag[i]
    end
    UpperTriangular(B.data)
end

*(D::Adjoint{<:Any,<:Diagonal}, B::Diagonal) = Diagonal(adjoint.(D.parent.diag) .* B.diag)
*(A::Adjoint{<:Any,<:AbstractTriangular}, D::Diagonal) =
    rmul!(copyto!(similar(A, promote_op(*, eltype(A), eltype(D.diag))), A), D)
function *(adjA::Adjoint{<:Any,<:AbstractMatrix}, D::Diagonal)
    A = adjA.parent
    Ac = similar(A, promote_op(*, eltype(A), eltype(D.diag)), (size(A, 2), size(A, 1)))
    adjoint!(Ac, A)
    rmul!(Ac, D)
end

*(D::Transpose{<:Any,<:Diagonal}, B::Diagonal) = Diagonal(transpose.(D.parent.diag) .* B.diag)
*(A::Transpose{<:Any,<:AbstractTriangular}, D::Diagonal) =
    rmul!(copyto!(similar(A, promote_op(*, eltype(A), eltype(D.diag))), A), D)
function *(transA::Transpose{<:Any,<:AbstractMatrix}, D::Diagonal)
    A = transA.parent
    At = similar(A, promote_op(*, eltype(A), eltype(D.diag)), (size(A, 2), size(A, 1)))
    transpose!(At, A)
    rmul!(At, D)
end

*(D::Diagonal, B::Adjoint{<:Any,<:Diagonal}) = Diagonal(D.diag .* adjoint.(B.parent.diag))
*(D::Diagonal, B::Adjoint{<:Any,<:AbstractTriangular}) =
    lmul!(D, copyto!(similar(B, promote_op(*, eltype(B), eltype(D.diag))), B))
*(D::Diagonal, adjQ::Adjoint{<:Any,<:Union{QRCompactWYQ,QRPackedQ}}) = (Q = adjQ.parent; rmul!(Array(D), adjoint(Q)))
function *(D::Diagonal, adjA::Adjoint{<:Any,<:AbstractMatrix})
    A = adjA.parent
    Ac = similar(A, promote_op(*, eltype(A), eltype(D.diag)), (size(A, 2), size(A, 1)))
    adjoint!(Ac, A)
    lmul!(D, Ac)
end

*(D::Diagonal, B::Transpose{<:Any,<:Diagonal}) = Diagonal(D.diag .* transpose.(B.parent.diag))
*(D::Diagonal, B::Transpose{<:Any,<:AbstractTriangular}) =
    lmul!(D, copyto!(similar(B, promote_op(*, eltype(B), eltype(D.diag))), B))
function *(D::Diagonal, transA::Transpose{<:Any,<:AbstractMatrix})
    A = transA.parent
    At = similar(A, promote_op(*, eltype(A), eltype(D.diag)), (size(A, 2), size(A, 1)))
    transpose!(At, A)
    lmul!(D, At)
end

*(D::Adjoint{<:Any,<:Diagonal}, B::Adjoint{<:Any,<:Diagonal}) =
    Diagonal(adjoint.(D.parent.diag) .* adjoint.(B.parent.diag))
*(D::Transpose{<:Any,<:Diagonal}, B::Transpose{<:Any,<:Diagonal}) =
    Diagonal(transpose.(D.parent.diag) .* transpose.(B.parent.diag))

rmul!(A::Diagonal, B::Diagonal) = Diagonal(A.diag .*= B.diag)
lmul!(A::Diagonal, B::Diagonal) = Diagonal(B.diag .= A.diag .* B.diag)

function lmul!(adjA::Adjoint{<:Any,<:Diagonal}, B::AbstractMatrix)
    A = adjA.parent
    return lmul!(adjoint(A), B)
end
function lmul!(transA::Transpose{<:Any,<:Diagonal}, B::AbstractMatrix)
    A = transA.parent
    return lmul!(transpose(A), B)
end

function rmul!(A::AbstractMatrix, adjB::Adjoint{<:Any,<:Diagonal})
    B = adjB.parent
    return rmul!(A, adjoint(B))
end
function rmul!(A::AbstractMatrix, transB::Transpose{<:Any,<:Diagonal})
    B = transB.parent
    return rmul!(A, transpose(B))
end

# Elements of `out` may not be defined (e.g., for `BigFloat`). To make
# `mul!(out, A, B)` work for such cases, `out .*ₛ beta` short-circuits
# `out * beta`.  Using `broadcasted` to avoid the multiplication
# inside this function.
function *ₛ end
Broadcast.broadcasted(::typeof(*ₛ), out, beta) =
    iszero(beta::Number) ? false : broadcasted(*, out, beta)

# Get ambiguous method if try to unify AbstractVector/AbstractMatrix here using AbstractVecOrMat
@inline mul!(out::AbstractVector, A::Diagonal, in::AbstractVector,
             alpha::Number, beta::Number) =
    out .= (A.diag .* in) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractVector, A::Adjoint{<:Any,<:Diagonal}, in::AbstractVector,
             alpha::Number, beta::Number) =
    out .= (adjoint.(A.parent.diag) .* in) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractVector, A::Transpose{<:Any,<:Diagonal}, in::AbstractVector,
             alpha::Number, beta::Number) =
    out .= (transpose.(A.parent.diag) .* in) .*ₛ alpha .+ out .*ₛ beta

@inline mul!(out::AbstractMatrix, A::Diagonal, in::StridedMatrix,
             alpha::Number, beta::Number) =
    out .= (A.diag .* in) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractMatrix, A::Adjoint{<:Any,<:Diagonal}, in::StridedMatrix,
             alpha::Number, beta::Number) =
    out .= (adjoint.(A.parent.diag) .* in) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractMatrix, A::Transpose{<:Any,<:Diagonal}, in::StridedMatrix,
             alpha::Number, beta::Number) =
    out .= (transpose.(A.parent.diag) .* in) .*ₛ alpha .+ out .*ₛ beta

@inline mul!(out::AbstractMatrix, A::Diagonal, in::Adjoint{<:Any,<:StridedMatrix},
             alpha::Number, beta::Number) =
    out .= (A.diag .* in) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractMatrix, A::Adjoint{<:Any,<:Diagonal}, in::Adjoint{<:Any,<:StridedMatrix},
             alpha::Number, beta::Number) =
    out .= (adjoint.(A.parent.diag) .* in) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractMatrix, A::Transpose{<:Any,<:Diagonal}, in::Adjoint{<:Any,<:StridedMatrix},
             alpha::Number, beta::Number) =
    out .= (transpose.(A.parent.diag) .* in) .*ₛ alpha .+ out .*ₛ beta

@inline mul!(out::AbstractMatrix, A::Diagonal, in::Transpose{<:Any,<:StridedMatrix},
             alpha::Number, beta::Number) =
    out .= (A.diag .* in) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractMatrix, A::Adjoint{<:Any,<:Diagonal}, in::Transpose{<:Any,<:StridedMatrix},
             alpha::Number, beta::Number) =
    out .= (adjoint.(A.parent.diag) .* in) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractMatrix, A::Transpose{<:Any,<:Diagonal}, in::Transpose{<:Any,<:StridedMatrix},
             alpha::Number, beta::Number) =
    out .= (transpose.(A.parent.diag) .* in) .*ₛ alpha .+ out .*ₛ beta

@inline mul!(out::AbstractMatrix, in::StridedMatrix, A::Diagonal,
             alpha::Number, beta::Number) =
    out .= (in .* permutedims(A.diag)) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractMatrix, in::StridedMatrix, A::Adjoint{<:Any,<:Diagonal},
             alpha::Number, beta::Number) =
    out .= (in .* adjoint(A.parent.diag)) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractMatrix, in::StridedMatrix, A::Transpose{<:Any,<:Diagonal},
             alpha::Number, beta::Number) =
    out .= (in .* transpose(A.parent.diag)) .*ₛ alpha .+ out .*ₛ beta

@inline mul!(out::AbstractMatrix, in::Adjoint{<:Any,<:StridedMatrix}, A::Diagonal,
             alpha::Number, beta::Number) =
    out .= (in .* permutedims(A.diag)) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractMatrix, in::Adjoint{<:Any,<:StridedMatrix}, A::Adjoint{<:Any,<:Diagonal},
             alpha::Number, beta::Number) =
    out .= (in .* adjoint(A.parent.diag)) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractMatrix, in::Adjoint{<:Any,<:StridedMatrix}, A::Transpose{<:Any,<:Diagonal},
             alpha::Number, beta::Number) =
    out .= (in .* transpose(A.parent.diag)) .*ₛ alpha .+ out .*ₛ beta

@inline mul!(out::AbstractMatrix, in::Transpose{<:Any,<:StridedMatrix}, A::Diagonal,
             alpha::Number, beta::Number) =
    out .= (in .* permutedims(A.diag)) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractMatrix, in::Transpose{<:Any,<:StridedMatrix}, A::Adjoint{<:Any,<:Diagonal},
             alpha::Number, beta::Number) =
    out .= (in .* adjoint(A.parent.diag)) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractMatrix, in::Transpose{<:Any,<:StridedMatrix}, A::Transpose{<:Any,<:Diagonal},
             alpha::Number, beta::Number) =
    out .= (in .* transpose(A.parent.diag)) .*ₛ alpha .+ out .*ₛ beta

# ambiguities with Symmetric/Hermitian
# RealHermSymComplex[Sym]/[Herm] only include Number; invariant to [c]transpose
*(A::Diagonal, transB::Transpose{<:Any,<:RealHermSymComplexSym}) = A * transB.parent
*(transA::Transpose{<:Any,<:RealHermSymComplexSym}, B::Diagonal) = transA.parent * B
*(A::Diagonal, adjB::Adjoint{<:Any,<:RealHermSymComplexHerm}) = A * adjB.parent
*(adjA::Adjoint{<:Any,<:RealHermSymComplexHerm}, B::Diagonal) = adjA.parent * B
*(transA::Transpose{<:Any,<:RealHermSymComplexSym}, transD::Transpose{<:Any,<:Diagonal}) = transA.parent * transD
*(transD::Transpose{<:Any,<:Diagonal}, transA::Transpose{<:Any,<:RealHermSymComplexSym}) = transD * transA.parent
*(adjA::Adjoint{<:Any,<:RealHermSymComplexHerm}, adjD::Adjoint{<:Any,<:Diagonal}) = adjA.parent * adjD
*(adjD::Adjoint{<:Any,<:Diagonal}, adjA::Adjoint{<:Any,<:RealHermSymComplexHerm}) = adjD * adjA.parent
mul!(C::AbstractMatrix, A::Adjoint{<:Any,<:Diagonal}, B::Adjoint{<:Any,<:RealHermSymComplexSym}) = C .= adjoint.(A.parent.diag) .* B
mul!(C::AbstractMatrix, A::Transpose{<:Any,<:Diagonal}, B::Transpose{<:Any,<:RealHermSymComplexHerm}) = C .= transpose.(A.parent.diag) .* B

@inline mul!(C::AbstractMatrix,
             A::Adjoint{<:Any,<:Diagonal}, B::Adjoint{<:Any,<:RealHermSym},
             alpha::Number, beta::Number) = mul!(C, A, B.parent, alpha, beta)
@inline mul!(C::AbstractMatrix,
             A::Adjoint{<:Any,<:Diagonal}, B::Adjoint{<:Any,<:RealHermSymComplexHerm},
             alpha::Number, beta::Number) = mul!(C, A, B.parent, alpha, beta)
@inline mul!(C::AbstractMatrix,
             A::Transpose{<:Any,<:Diagonal}, B::Transpose{<:Any,<:RealHermSym},
             alpha::Number, beta::Number) = mul!(C, A, B.parent, alpha, beta)
@inline mul!(C::AbstractMatrix,
             A::Transpose{<:Any,<:Diagonal}, B::Transpose{<:Any,<:RealHermSymComplexSym},
             alpha::Number, beta::Number) = mul!(C, A, B.parent, alpha, beta)

@inline mul!(C::AbstractMatrix,
             A::Adjoint{<:Any,<:Diagonal}, B::Adjoint{<:Any,<:RealHermSymComplexSym},
             alpha::Number, beta::Number) =
    C .= (adjoint.(A.parent.diag) .* B) .*ₛ alpha .+ C .*ₛ beta
@inline mul!(C::AbstractMatrix,
             A::Transpose{<:Any,<:Diagonal}, B::Transpose{<:Any,<:RealHermSymComplexHerm},
             alpha::Number, beta::Number) =
    C .= (transpose.(A.parent.diag) .* B) .*ₛ alpha .+ C .*ₛ beta

(/)(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag ./ Db.diag)

function ldiv!(D::Diagonal{T}, v::AbstractVector{T}) where {T}
    if length(v) != length(D.diag)
        throw(DimensionMismatch("diagonal matrix is $(length(D.diag)) by $(length(D.diag)) but right hand side has $(length(v)) rows"))
    end
    for i = 1:length(D.diag)
        d = D.diag[i]
        if iszero(d)
            throw(SingularException(i))
        end
        v[i] = d\v[i]
    end
    v
end
function ldiv!(D::Diagonal{T}, V::AbstractMatrix{T}) where {T}
    require_one_based_indexing(V)
    if size(V,1) != length(D.diag)
        throw(DimensionMismatch("diagonal matrix is $(length(D.diag)) by $(length(D.diag)) but right hand side has $(size(V,1)) rows"))
    end
    for i = 1:length(D.diag)
        d = D.diag[i]
        if iszero(d)
            throw(SingularException(i))
        end
        for j = 1:size(V,2)
            @inbounds V[i,j] = d\V[i,j]
        end
    end
    V
end
ldiv!(x::AbstractArray, A::Diagonal, b::AbstractArray) = (x .= A.diag .\ b)

ldiv!(adjD::Adjoint{<:Any,<:Diagonal{T}}, B::AbstractVecOrMat{T}) where {T} =
    (D = adjD.parent; ldiv!(conj(D), B))
ldiv!(transD::Transpose{<:Any,<:Diagonal{T}}, B::AbstractVecOrMat{T}) where {T} =
    (D = transD.parent; ldiv!(D, B))

function ldiv!(D::Diagonal, A::Union{LowerTriangular,UpperTriangular})
    broadcast!(\, parent(A), D.diag, parent(A))
    A
end

function rdiv!(A::AbstractMatrix{T}, D::Diagonal{T}) where {T}
    require_one_based_indexing(A)
    dd = D.diag
    m, n = size(A)
    if (k = length(dd)) ≠ n
        throw(DimensionMismatch("left hand side has $n columns but D is $k by $k"))
    end
    @inbounds for j in 1:n
        ddj = dd[j]
        if iszero(ddj)
            throw(SingularException(j))
        end
        for i in 1:m
            A[i, j] /= ddj
        end
    end
    A
end

function rdiv!(A::Union{LowerTriangular,UpperTriangular}, D::Diagonal)
    broadcast!(/, parent(A), parent(A), permutedims(D.diag))
    A
end

rdiv!(A::AbstractMatrix{T}, adjD::Adjoint{<:Any,<:Diagonal{T}}) where {T} =
    (D = adjD.parent; rdiv!(A, conj(D)))
rdiv!(A::AbstractMatrix{T}, transD::Transpose{<:Any,<:Diagonal{T}}) where {T} =
    (D = transD.parent; rdiv!(A, D))

(/)(A::Union{StridedMatrix, AbstractTriangular}, D::Diagonal) =
    rdiv!((typeof(oneunit(eltype(D))/oneunit(eltype(A)))).(A), D)

(\)(F::Factorization, D::Diagonal) =
    ldiv!(F, Matrix{typeof(oneunit(eltype(D))/oneunit(eltype(F)))}(D))
\(adjF::Adjoint{<:Any,<:Factorization}, D::Diagonal) =
    (F = adjF.parent; ldiv!(adjoint(F), Matrix{typeof(oneunit(eltype(D))/oneunit(eltype(F)))}(D)))
(\)(A::Union{QR,QRCompactWY,QRPivoted}, B::Diagonal) =
    invoke(\, Tuple{Union{QR,QRCompactWY,QRPivoted}, AbstractVecOrMat}, A, B)

function kron(A::Diagonal{T1}, B::Diagonal{T2}) where {T1<:Number, T2<:Number}
    valA = A.diag; nA = length(valA)
    valB = B.diag; nB = length(valB)
    valC = Vector{typeof(zero(T1)*zero(T2))}(undef,nA*nB)
    @inbounds for i = 1:nA, j = 1:nB
        valC[(i-1)*nB+j] = valA[i] * valB[j]
    end
    return Diagonal(valC)
end

function kron(A::Diagonal{T}, B::AbstractMatrix{S}) where {T<:Number, S<:Number}
    Base.require_one_based_indexing(B)
    (mA, nA) = size(A); (mB, nB) = size(B)
    R = zeros(Base.promote_op(*, T, S), mA * mB, nA * nB)
    m = 1
    for j = 1:nA
        A_jj = A[j,j]
        for k = 1:nB
            for l = 1:mB
                R[m] = A_jj * B[l,k]
                m += 1
            end
            m += (nA - 1) * mB
        end
        m += mB
    end
    return R
end

function kron(A::AbstractMatrix{T}, B::Diagonal{S}) where {T<:Number, S<:Number}
    require_one_based_indexing(A)
    (mA, nA) = size(A); (mB, nB) = size(B)
    R = zeros(promote_op(*, T, S), mA * mB, nA * nB)
    m = 1
    for j = 1:nA
        for l = 1:mB
            Bll = B[l,l]
            for k = 1:mA
                R[m] = A[k,j] * Bll
                m += nB
            end
            m += 1
        end
        m -= nB
    end
    return R
end

conj(D::Diagonal) = Diagonal(conj(D.diag))
transpose(D::Diagonal{<:Number}) = D
transpose(D::Diagonal) = Diagonal(transpose.(D.diag))
adjoint(D::Diagonal{<:Number}) = conj(D)
adjoint(D::Diagonal) = Diagonal(adjoint.(D.diag))

function diag(D::Diagonal, k::Integer=0)
    # every branch call similar(..., ::Int) to make sure the
    # same vector type is returned independent of k
    if k == 0
        return copyto!(similar(D.diag, length(D.diag)), D.diag)
    elseif -size(D,1) <= k <= size(D,1)
        return fill!(similar(D.diag, size(D,1)-abs(k)), 0)
    else
        throw(ArgumentError(string("requested diagonal, $k, must be at least $(-size(D, 1)) ",
            "and at most $(size(D, 2)) for an $(size(D, 1))-by-$(size(D, 2)) matrix")))
    end
end
tr(D::Diagonal) = sum(tr, D.diag)
det(D::Diagonal) = prod(det, D.diag)
logdet(D::Diagonal{<:Real}) = sum(log, D.diag)
function logdet(D::Diagonal{<:Complex}) # make sure branch cut is correct
    z = sum(log, D.diag)
    complex(real(z), rem2pi(imag(z), RoundNearest))
end

# Matrix functions
for f in (:exp, :log, :sqrt,
          :cos, :sin, :tan, :csc, :sec, :cot,
          :cosh, :sinh, :tanh, :csch, :sech, :coth,
          :acos, :asin, :atan, :acsc, :asec, :acot,
          :acosh, :asinh, :atanh, :acsch, :asech, :acoth)
    @eval $f(D::Diagonal) = Diagonal($f.(D.diag))
end

#Linear solver
function ldiv!(D::Diagonal, B::StridedVecOrMat)
    m, n = size(B, 1), size(B, 2)
    if m != length(D.diag)
        throw(DimensionMismatch("diagonal matrix is $(length(D.diag)) by $(length(D.diag)) but right hand side has $m rows"))
    end
    (m == 0 || n == 0) && return B
    for j = 1:n
        for i = 1:m
            di = D.diag[i]
            if di == 0
                throw(SingularException(i))
            end
            B[i,j] = di \ B[i,j]
        end
    end
    return B
end
(\)(D::Diagonal, A::AbstractMatrix) =
    ldiv!(D, (typeof(oneunit(eltype(D))/oneunit(eltype(A)))).(A))

(\)(D::Diagonal, b::AbstractVector) = D.diag .\ b
(\)(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag .\ Db.diag)

function inv(D::Diagonal{T}) where T
    Di = similar(D.diag, typeof(inv(zero(T))))
    for i = 1:length(D.diag)
        if D.diag[i] == zero(T)
            throw(SingularException(i))
        end
        Di[i] = inv(D.diag[i])
    end
    Diagonal(Di)
end

function pinv(D::Diagonal{T}) where T
    Di = similar(D.diag, typeof(inv(zero(T))))
    for i = 1:length(D.diag)
        isfinite(inv(D.diag[i])) ? Di[i]=inv(D.diag[i]) : Di[i]=zero(T)
    end
    Diagonal(Di)
end
function pinv(D::Diagonal{T}, tol::Real) where T
    Di = similar(D.diag, typeof(inv(zero(T))))
    if( !isempty(D.diag) ) maxabsD = maximum(abs.(D.diag)) end
    for i = 1:length(D.diag)
        if( abs(D.diag[i]) > tol*maxabsD && isfinite(inv(D.diag[i])) )
            Di[i]=inv(D.diag[i])
        else
            Di[i]=zero(T)
        end
    end
    Diagonal(Di)
end

#Eigensystem
eigvals(D::Diagonal{<:Number}; permute::Bool=true, scale::Bool=true) = D.diag
eigvals(D::Diagonal; permute::Bool=true, scale::Bool=true) =
    [eigvals(x) for x in D.diag] #For block matrices, etc.
eigvecs(D::Diagonal) = Matrix{eltype(D)}(I, size(D))
function eigen(D::Diagonal; permute::Bool=true, scale::Bool=true, sortby::Union{Function,Nothing}=nothing)
    if any(!isfinite, D.diag)
        throw(ArgumentError("matrix contains Infs or NaNs"))
    end
    Eigen(sorteig!(eigvals(D), eigvecs(D), sortby)...)
end

#Singular system
svdvals(D::Diagonal{<:Number}) = sort!(abs.(D.diag), rev = true)
svdvals(D::Diagonal) = [svdvals(v) for v in D.diag]
function svd(D::Diagonal{<:Number})
    S   = abs.(D.diag)
    piv = sortperm(S, rev = true)
    U   = Diagonal(D.diag ./ S)
    Up  = hcat([U[:,i] for i = 1:length(D.diag)][piv]...)
    V   = Diagonal(fill!(similar(D.diag), one(eltype(D.diag))))
    Vp  = hcat([V[:,i] for i = 1:length(D.diag)][piv]...)
    return SVD(Up, S[piv], copy(Vp'))
end

# disambiguation methods: * of Diagonal and Adj/Trans AbsVec
*(x::Adjoint{<:Any,<:AbstractVector}, D::Diagonal) = Adjoint(map((t,s) -> t'*s, D.diag, parent(x)))
*(x::Transpose{<:Any,<:AbstractVector}, D::Diagonal) = Transpose(map((t,s) -> transpose(t)*s, D.diag, parent(x)))
*(x::Adjoint{<:Any,<:AbstractVector}, D::Diagonal, y::AbstractVector) =
    mapreduce(t -> t[1]*t[2]*t[3], +, zip(x, D.diag, y))
*(x::Transpose{<:Any,<:AbstractVector}, D::Diagonal, y::AbstractVector) =
    mapreduce(t -> t[1]*t[2]*t[3], +, zip(x, D.diag, y))
function dot(x::AbstractVector, D::Diagonal, y::AbstractVector)
    mapreduce(t -> dot(t[1], t[2], t[3]), +, zip(x, D.diag, y))
end

function cholesky!(A::Diagonal, ::Val{false} = Val(false); check::Bool = true)
    info = 0
    for (i, di) in enumerate(A.diag)
        if isreal(di) && real(di) > 0
            A.diag[i] = √di
        elseif check
            throw(PosDefException(i))
        else
            info = i
            break
        end
    end
    Cholesky(A, 'U', convert(BlasInt, info))
end

cholesky(A::Diagonal, ::Val{false} = Val(false); check::Bool = true) =
    cholesky!(cholcopy(A), Val(false); check = check)

function getproperty(C::Cholesky{<:Any,<:Diagonal}, d::Symbol)
    Cfactors = getfield(C, :factors)
    if d in (:U, :L, :UL)
        return Cfactors
    else
        return getfield(C, d)
    end
end

Base._sum(A::Diagonal, ::Colon) = sum(A.diag)

function logabsdet(A::Diagonal)
     mapreduce(x -> (log(abs(x)), sign(x)), ((d1, s1), (d2, s2)) -> (d1 + d2, s1 * s2),
               A.diag)
end
