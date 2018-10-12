# This file is a part of Julia. License is MIT: https://julialang.org/license

## Diagonal matrices

struct Diagonal{T,V<:AbstractVector{T}} <: AbstractMatrix{T}
    diag::V

    function Diagonal{T,V}(diag) where {T,V<:AbstractVector{T}}
        @assert !has_offset_axes(diag)
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
isposdef(D::Diagonal) = all(x -> x > 0, D.diag)

factorize(D::Diagonal) = D

real(D::Diagonal) = Diagonal(real(D.diag))
imag(D::Diagonal) = Diagonal(imag(D.diag))

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

(*)(A::AbstractTriangular, D::Diagonal) = rmul!(copy(A), D)
(*)(D::Diagonal, B::AbstractTriangular) = lmul!(D, copy(B))

(*)(A::AbstractMatrix, D::Diagonal) =
    rmul!(copyto!(similar(A, promote_op(*, eltype(A), eltype(D.diag)), size(A)), A), D)
(*)(D::Diagonal, A::AbstractMatrix) =
    lmul!(D, copyto!(similar(A, promote_op(*, eltype(A), eltype(D.diag)), size(A)), A))

function rmul!(A::AbstractMatrix, D::Diagonal)
    @assert !has_offset_axes(A)
    A .= A .* transpose(D.diag)
    return A
end

function lmul!(D::Diagonal, B::AbstractMatrix)
    @assert !has_offset_axes(B)
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
*(A::Adjoint{<:Any,<:AbstractTriangular}, D::Diagonal) = rmul!(copy(A), D)
function *(adjA::Adjoint{<:Any,<:AbstractMatrix}, D::Diagonal)
    A = adjA.parent
    Ac = similar(A, promote_op(*, eltype(A), eltype(D.diag)), (size(A, 2), size(A, 1)))
    adjoint!(Ac, A)
    rmul!(Ac, D)
end

*(D::Transpose{<:Any,<:Diagonal}, B::Diagonal) = Diagonal(transpose.(D.parent.diag) .* B.diag)
*(A::Transpose{<:Any,<:AbstractTriangular}, D::Diagonal) = rmul!(copy(A), D)
function *(transA::Transpose{<:Any,<:AbstractMatrix}, D::Diagonal)
    A = transA.parent
    At = similar(A, promote_op(*, eltype(A), eltype(D.diag)), (size(A, 2), size(A, 1)))
    transpose!(At, A)
    rmul!(At, D)
end

*(D::Diagonal, B::Adjoint{<:Any,<:Diagonal}) = Diagonal(D.diag .* adjoint.(B.parent.diag))
*(D::Diagonal, B::Adjoint{<:Any,<:AbstractTriangular}) = lmul!(D, collect(B))
*(D::Diagonal, adjQ::Adjoint{<:Any,<:Union{QRCompactWYQ,QRPackedQ}}) = (Q = adjQ.parent; rmul!(Array(D), adjoint(Q)))
function *(D::Diagonal, adjA::Adjoint{<:Any,<:AbstractMatrix})
    A = adjA.parent
    Ac = similar(A, promote_op(*, eltype(A), eltype(D.diag)), (size(A, 2), size(A, 1)))
    adjoint!(Ac, A)
    lmul!(D, Ac)
end

*(D::Diagonal, B::Transpose{<:Any,<:Diagonal}) = Diagonal(D.diag .* transpose.(B.parent.diag))
*(D::Diagonal, B::Transpose{<:Any,<:AbstractTriangular}) = lmul!(D, copy(B))
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
    return lmul!(conj(A.diag), B)
end
function lmul!(transA::Transpose{<:Any,<:Diagonal}, B::AbstractMatrix)
    A = transA.parent
    return lmul!(A.diag, B)
end

function rmul!(A::AbstractMatrix, adjB::Adjoint{<:Any,<:Diagonal})
    B = adjB.parent
    return rmul!(A, conj(B.diag))
end
function rmul!(A::AbstractMatrix, transB::Transpose{<:Any,<:Diagonal})
    B = transB.parent
    return rmul!(A, B.diag)
end

# Get ambiguous method if try to unify AbstractVector/AbstractMatrix here using AbstractVecOrMat
mul!(out::AbstractVector, A::Diagonal, in::AbstractVector) = out .= A.diag .* in
mul!(out::AbstractVector, A::Adjoint{<:Any,<:Diagonal}, in::AbstractVector) = out .= adjoint.(A.parent.diag) .* in
mul!(out::AbstractVector, A::Transpose{<:Any,<:Diagonal}, in::AbstractVector) = out .= transpose.(A.parent.diag) .* in

mul!(out::AbstractMatrix, A::Diagonal, in::StridedMatrix) = out .= A.diag .* in
mul!(out::AbstractMatrix, A::Adjoint{<:Any,<:Diagonal}, in::StridedMatrix) = out .= adjoint.(A.parent.diag) .* in
mul!(out::AbstractMatrix, A::Transpose{<:Any,<:Diagonal}, in::StridedMatrix) = out .= transpose.(A.parent.diag) .* in

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
mul!(C::AbstractMatrix, A::Adjoint{<:Any,<:Diagonal}, B::Adjoint{<:Any,<:RealHermSymComplexHerm}) = mul!(C, A, B.parent)
mul!(C::AbstractMatrix, A::Transpose{<:Any,<:Diagonal}, B::Transpose{<:Any,<:RealHermSymComplexSym}) = mul!(C, A, B.parent)
mul!(C::AbstractMatrix, A::Adjoint{<:Any,<:Diagonal}, B::Adjoint{<:Any,<:RealHermSymComplexSym}) = C .= adjoint.(A.parent.diag) .* B
mul!(C::AbstractMatrix, A::Transpose{<:Any,<:Diagonal}, B::Transpose{<:Any,<:RealHermSymComplexHerm}) = C .= transpose.(A.parent.diag) .* B


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
    @assert !has_offset_axes(V)
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


ldiv!(adjD::Adjoint{<:Any,<:Diagonal{T}}, B::AbstractVecOrMat{T}) where {T} =
    (D = adjD.parent; ldiv!(conj(D), B))
ldiv!(transD::Transpose{<:Any,<:Diagonal{T}}, B::AbstractVecOrMat{T}) where {T} =
    (D = transD.parent; ldiv!(D, B))

function ldiv!(D::Diagonal, A::Union{LowerTriangular,UpperTriangular})
    broadcast!(\, parent(A), D.diag, parent(A))
    A
end

function rdiv!(A::AbstractMatrix{T}, D::Diagonal{T}) where {T}
    @assert !has_offset_axes(A)
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
tr(D::Diagonal) = sum(D.diag)
det(D::Diagonal) = prod(D.diag)
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
            B[i,j] /= di
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
function eigen(D::Diagonal; permute::Bool=true, scale::Bool=true)
    if any(!isfinite, D.diag)
        throw(ArgumentError("matrix contains Infs or NaNs"))
    end
    Eigen(eigvals(D), eigvecs(D))
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
*(x::Adjoint{<:Any,<:AbstractVector}, D::Diagonal, y::AbstractVector) =
    mapreduce(t -> t[1]*t[2]*t[3], +, zip(x, D.diag, y))
*(x::Transpose{<:Any,<:AbstractVector}, D::Diagonal) = Transpose(map(*, D.diag, parent(x)))
*(x::Transpose{<:Any,<:AbstractVector}, D::Diagonal, y::AbstractVector) =
    mapreduce(t -> t[1]*t[2]*t[3], +, zip(x, D.diag, y))
# TODO: these methods will yield row matrices, rather than adjoint/transpose vectors

function cholesky!(A::Diagonal, ::Val{false} = Val(false); check::Bool = true)
    info = 0
    diagonal = A.diag
    for i in axes(diagonal, 1)
        d = diagonal[i]
        if !(d == 0 || (isreal(d) && d < 0))
            diagonal[i] = √d
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
