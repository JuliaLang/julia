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
    Diagonal(V::AbstractVector)

Construct a matrix with `V` as its diagonal.

See also [`diag`](@ref), [`diagm`](@ref).

# Examples
```jldoctest
julia> Diagonal([1, 10, 100])
3×3 Diagonal{$Int, Vector{$Int}}:
 1   ⋅    ⋅
 ⋅  10    ⋅
 ⋅   ⋅  100

julia> diagm([7, 13])
2×2 Matrix{$Int}:
 7   0
 0  13
```
"""
Diagonal(V::AbstractVector)

"""
    Diagonal(A::AbstractMatrix)

Construct a matrix from the diagonal of `A`.

# Examples
```jldoctest
julia> A = permutedims(reshape(1:15, 5, 3))
3×5 Matrix{Int64}:
  1   2   3   4   5
  6   7   8   9  10
 11  12  13  14  15

julia> Diagonal(A)
3×3 Diagonal{$Int, Vector{$Int}}:
 1  ⋅   ⋅
 ⋅  7   ⋅
 ⋅  ⋅  13

julia> diag(A, 2)
3-element Vector{$Int}:
  3
  9
 15
```
"""
Diagonal(A::AbstractMatrix) = Diagonal(diag(A))

Diagonal(D::Diagonal) = D
Diagonal{T}(D::Diagonal{T}) where {T} = D
Diagonal{T}(D::Diagonal) where {T} = Diagonal{T}(D.diag)

AbstractMatrix{T}(D::Diagonal) where {T} = Diagonal{T}(D)
Matrix(D::Diagonal) = diagm(0 => D.diag)
Array(D::Diagonal) = Matrix(D)

"""
    Diagonal{T}(undef, n)

Construct an uninitialized `Diagonal{T}` of length `n`. See `undef`.
"""
Diagonal{T}(::UndefInitializer, n::Integer) where T = Diagonal(Vector{T}(undef, n))

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
diagzero(D::Diagonal{<:AbstractMatrix{T}},i,j) where {T} = zeros(T, size(D.diag[i], 1), size(D.diag[j], 2))

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
istriu(D::Diagonal, k::Integer=0) = k <= 0 || iszero(D.diag) ? true : false
istril(D::Diagonal, k::Integer=0) = k >= 0 || iszero(D.diag) ? true : false
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

for f in (:+, :-)
    @eval function $f(D::Diagonal, S::Symmetric)
        return Symmetric($f(D, S.data), sym_uplo(S.uplo))
    end
    @eval function $f(S::Symmetric, D::Diagonal)
        return Symmetric($f(S.data, D), sym_uplo(S.uplo))
    end
    @eval function $f(D::Diagonal{<:Real}, H::Hermitian)
        return Hermitian($f(D, H.data), sym_uplo(H.uplo))
    end
    @eval function $f(H::Hermitian, D::Diagonal{<:Real})
        return Hermitian($f(H.data, D), sym_uplo(H.uplo))
    end
end

(*)(x::Number, D::Diagonal) = Diagonal(x * D.diag)
(*)(D::Diagonal, x::Number) = Diagonal(D.diag * x)
(/)(D::Diagonal, x::Number) = Diagonal(D.diag / x)
(\)(x::Number, D::Diagonal) = Diagonal(x \ D.diag)
(^)(D::Diagonal, a::Number) = Diagonal(D.diag .^ a)
(^)(D::Diagonal, a::Real) = Diagonal(D.diag .^ a) # for disambiguation
(^)(D::Diagonal, a::Integer) = Diagonal(D.diag .^ a) # for disambiguation
Base.literal_pow(::typeof(^), D::Diagonal, valp::Val) =
    Diagonal(Base.literal_pow.(^, D.diag, valp)) # for speed
Base.literal_pow(::typeof(^), D::Diagonal, ::Val{-1}) = inv(D) # for disambiguation

function (*)(Da::Diagonal, Db::Diagonal)
    nDa, mDb = size(Da, 2), size(Db, 1)
    if nDa != mDb
        throw(DimensionMismatch("second dimension of Da, $nDa, does not match first dimension of Db, $mDb"))
    end
    return Diagonal(Da.diag .* Db.diag)
end

function (*)(D::Diagonal, V::AbstractVector)
    nD = size(D, 2)
    if nD != length(V)
        throw(DimensionMismatch("second dimension of D, $nD, does not match length of V, $(length(V))"))
    end
    return D.diag .* V
end

(*)(A::AbstractTriangular, D::Diagonal) =
    rmul!(copy_oftype(A, promote_op(*, eltype(A), eltype(D.diag))), D)
(*)(D::Diagonal, B::AbstractTriangular) =
    lmul!(D, copy_oftype(B, promote_op(*, eltype(B), eltype(D.diag))))

(*)(A::AbstractMatrix, D::Diagonal) =
    rmul!(copy_similar(A, promote_op(*, eltype(A), eltype(D.diag))), D)
(*)(D::Diagonal, A::AbstractMatrix) =
    lmul!(D, copy_similar(A, promote_op(*, eltype(A), eltype(D.diag))))

function rmul!(A::AbstractMatrix, D::Diagonal)
    require_one_based_indexing(A)
    nA, nD = size(A, 2), length(D.diag)
    if nA != nD
        throw(DimensionMismatch("second dimension of A, $nA, does not match the first of D, $nD"))
    end
    A .= A .* permutedims(D.diag)
    return A
end

function lmul!(D::Diagonal, B::AbstractVecOrMat)
    require_one_based_indexing(B)
    nB, nD = size(B, 1), length(D.diag)
    if nB != nD
        throw(DimensionMismatch("second dimension of D, $nD, does not match the first of B, $nB"))
    end
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

function *(adjA::Adjoint{<:Any,<:AbstractMatrix}, D::Diagonal)
    A = adjA.parent
    Ac = similar(A, promote_op(*, eltype(A), eltype(D.diag)), (size(A, 2), size(A, 1)))
    adjoint!(Ac, A)
    rmul!(Ac, D)
end

function *(transA::Transpose{<:Any,<:AbstractMatrix}, D::Diagonal)
    A = transA.parent
    At = similar(A, promote_op(*, eltype(A), eltype(D.diag)), (size(A, 2), size(A, 1)))
    transpose!(At, A)
    rmul!(At, D)
end

*(D::Diagonal, adjQ::Adjoint{<:Any,<:Union{QRCompactWYQ,QRPackedQ}}) =
    rmul!(Array{promote_type(eltype(D), eltype(adjQ))}(D), adjQ)

function *(D::Diagonal, adjA::Adjoint{<:Any,<:AbstractMatrix})
    A = adjA.parent
    Ac = similar(A, promote_op(*, eltype(A), eltype(D.diag)), (size(A, 2), size(A, 1)))
    adjoint!(Ac, A)
    lmul!(D, Ac)
end

function *(D::Diagonal, transA::Transpose{<:Any,<:AbstractMatrix})
    A = transA.parent
    At = similar(A, promote_op(*, eltype(A), eltype(D.diag)), (size(A, 2), size(A, 1)))
    transpose!(At, A)
    lmul!(D, At)
end

rmul!(A::Diagonal, B::Diagonal) = Diagonal(A.diag .*= B.diag)
lmul!(A::Diagonal, B::Diagonal) = Diagonal(B.diag .= A.diag .* B.diag)

# Get ambiguous method if try to unify AbstractVector/AbstractMatrix here using AbstractVecOrMat
@inline mul!(out::AbstractVector, A::Diagonal, in::AbstractVector, alpha::Number, beta::Number) =
    out .= (A.diag .* in) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractMatrix, A::Diagonal, in::AbstractMatrix, alpha::Number, beta::Number) =
    out .= (A.diag .* in) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractMatrix, A::Diagonal, in::Adjoint{<:Any,<:AbstractVecOrMat},
             alpha::Number, beta::Number) =
    out .= (A.diag .* in) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractMatrix, A::Diagonal, in::Transpose{<:Any,<:AbstractVecOrMat},
             alpha::Number, beta::Number) =
    out .= (A.diag .* in) .*ₛ alpha .+ out .*ₛ beta

@inline mul!(out::AbstractMatrix, in::AbstractMatrix, A::Diagonal, alpha::Number, beta::Number) =
    out .= (in .* permutedims(A.diag)) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractMatrix, in::Adjoint{<:Any,<:AbstractVecOrMat}, A::Diagonal,
             alpha::Number, beta::Number) =
    out .= (in .* permutedims(A.diag)) .*ₛ alpha .+ out .*ₛ beta
@inline mul!(out::AbstractMatrix, in::Transpose{<:Any,<:AbstractVecOrMat}, A::Diagonal,
             alpha::Number, beta::Number) =
    out .= (in .* permutedims(A.diag)) .*ₛ alpha .+ out .*ₛ beta

function mul!(C::AbstractMatrix, Da::Diagonal, Db::Diagonal, alpha::Number, beta::Number)
    mA = size(Da, 1)
    mB = size(Db, 1)
    mA == mB || throw(DimensionMismatch("A has dimensions ($mA,$mA) but B has dimensions ($mB,$mB)"))
    mC, nC = size(C)
    mC == nC == mA || throw(DimensionMismatch("output matrix has size: ($mC,$nC), but should have size ($mA,$mA)"))
    require_one_based_indexing(C)
    da = Da.diag
    db = Db.diag
    _rmul_or_fill!(C, beta)
    if iszero(beta)
        @inbounds @simd for i in 1:mA
            C[i,i] = Ref(da[i] * db[i]) .*ₛ alpha
        end
    else
        @inbounds @simd for i in 1:mA
            C[i,i] += Ref(da[i] * db[i]) .*ₛ alpha
        end
    end
    return C
end

(/)(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag ./ Db.diag)

ldiv!(x::AbstractArray, A::Diagonal, b::AbstractArray) = (x .= A.diag .\ b)

function ldiv!(D::Diagonal, A::Union{LowerTriangular,UpperTriangular})
    broadcast!(\, parent(A), D.diag, parent(A))
    A
end

function rdiv!(A::AbstractMatrix, D::Diagonal)
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

(/)(A::Union{StridedMatrix, AbstractTriangular}, D::Diagonal) =
    rdiv!((typeof(oneunit(eltype(D))/oneunit(eltype(A)))).(A), D)

@inline function kron!(C::AbstractMatrix, A::Diagonal, B::Diagonal)
    valA = A.diag; nA = length(valA)
    valB = B.diag; nB = length(valB)
    nC = checksquare(C)
    @boundscheck nC == nA*nB ||
        throw(DimensionMismatch("expect C to be a $(nA*nB)x$(nA*nB) matrix, got size $(nC)x$(nC)"))
    isempty(A) || isempty(B) || fill!(C, zero(A[1,1] * B[1,1]))
    @inbounds for i = 1:nA, j = 1:nB
        idx = (i-1)*nB+j
        C[idx, idx] = valA[i] * valB[j]
    end
    return C
end

kron(A::Diagonal{<:Number}, B::Diagonal{<:Number}) = Diagonal(kron(A.diag, B.diag))

@inline function kron!(C::AbstractMatrix, A::Diagonal, B::AbstractMatrix)
    Base.require_one_based_indexing(B)
    (mA, nA) = size(A)
    (mB, nB) = size(B)
    (mC, nC) = size(C)
    @boundscheck (mC, nC) == (mA * mB, nA * nB) ||
        throw(DimensionMismatch("expect C to be a $(mA * mB)x$(nA * nB) matrix, got size $(mC)x$(nC)"))
    isempty(A) || isempty(B) || fill!(C, zero(A[1,1] * B[1,1]))
    m = 1
    @inbounds for j = 1:nA
        A_jj = A[j,j]
        for k = 1:nB
            for l = 1:mB
                C[m] = A_jj * B[l,k]
                m += 1
            end
            m += (nA - 1) * mB
        end
        m += mB
    end
    return C
end

@inline function kron!(C::AbstractMatrix, A::AbstractMatrix, B::Diagonal)
    require_one_based_indexing(A)
    (mA, nA) = size(A)
    (mB, nB) = size(B)
    (mC, nC) = size(C)
    @boundscheck (mC, nC) == (mA * mB, nA * nB) ||
        throw(DimensionMismatch("expect C to be a $(mA * mB)x$(nA * nB) matrix, got size $(mC)x$(nC)"))
    isempty(A) || isempty(B) || fill!(C, zero(A[1,1] * B[1,1]))
    m = 1
    @inbounds for j = 1:nA
        for l = 1:mB
            Bll = B[l,l]
            for k = 1:mA
                C[m] = A[k,j] * Bll
                m += nB
            end
            m += 1
        end
        m -= nB
    end
    return C
end

conj(D::Diagonal) = Diagonal(conj(D.diag))
transpose(D::Diagonal{<:Number}) = D
transpose(D::Diagonal) = Diagonal(transpose.(D.diag))
adjoint(D::Diagonal{<:Number}) = conj(D)
adjoint(D::Diagonal) = Diagonal(adjoint.(D.diag))
Base.permutedims(D::Diagonal) = D
Base.permutedims(D::Diagonal, perm) = (Base.checkdims_perm(D, D, perm); D)

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
function logdet(D::Diagonal{<:Complex}) # make sure branch cut is correct
    z = sum(log, D.diag)
    complex(real(z), rem2pi(imag(z), RoundNearest))
end

# Matrix functions
for f in (:exp, :cis, :log, :sqrt,
          :cos, :sin, :tan, :csc, :sec, :cot,
          :cosh, :sinh, :tanh, :csch, :sech, :coth,
          :acos, :asin, :atan, :acsc, :asec, :acot,
          :acosh, :asinh, :atanh, :acsch, :asech, :acoth)
    @eval $f(D::Diagonal) = Diagonal($f.(D.diag))
end

(\)(D::Diagonal, A::AbstractMatrix) =
    ldiv!(D, (typeof(oneunit(eltype(D))/oneunit(eltype(A)))).(A))

(\)(D::Diagonal, b::AbstractVector) = D.diag .\ b
(\)(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag .\ Db.diag)

function ldiv!(D::Diagonal, B::AbstractVecOrMat)
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
*(x::Adjoint{<:Any,<:AbstractVector},   D::Diagonal, y::AbstractVector) = _mapreduce_prod(*, x, D, y)
*(x::Transpose{<:Any,<:AbstractVector}, D::Diagonal, y::AbstractVector) = _mapreduce_prod(*, x, D, y)
dot(x::AbstractVector, D::Diagonal, y::AbstractVector) = _mapreduce_prod(dot, x, D, y)

dot(A::Diagonal, B::Diagonal) = dot(A.diag, B.diag)
function dot(D::Diagonal, B::AbstractMatrix)
    size(D) == size(B) || throw(DimensionMismatch("Matrix sizes $(size(D)) and $(size(B)) differ"))
    return dot(D.diag, view(B, diagind(B)))
end

dot(A::AbstractMatrix, B::Diagonal) = conj(dot(B, A))

function _mapreduce_prod(f, x, D::Diagonal, y)
    if isempty(x) && isempty(D) && isempty(y)
        return zero(Base.promote_op(f, eltype(x), eltype(D), eltype(y)))
    else
        return mapreduce(t -> f(t[1], t[2], t[3]), +, zip(x, D.diag, y))
    end
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
function Base._sum(A::Diagonal, dims::Integer)
    res = Base.reducedim_initarray(A, dims, zero(eltype(A)))
    if dims <= 2
        for i = 1:length(A.diag)
            @inbounds res[i] = A.diag[i]
        end
    else
        for i = 1:length(A.diag)
            @inbounds res[i,i] = A.diag[i]
        end
    end
    res
end

function logabsdet(A::Diagonal)
     mapreduce(x -> (log(abs(x)), sign(x)), ((d1, s1), (d2, s2)) -> (d1 + d2, s1 * s2),
               A.diag)
end

function Base.muladd(A::Diagonal, B::Diagonal, z::Diagonal)
    Diagonal(A.diag .* B.diag .+ z.diag)
end
