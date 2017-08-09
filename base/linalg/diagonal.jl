# This file is a part of Julia. License is MIT: https://julialang.org/license

## Diagonal matrices

struct Diagonal{T,V<:AbstractVector{T}} <: AbstractMatrix{T}
    diag::V
end
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
Diagonal(V::AbstractVector{T}) where {T} = Diagonal{T,typeof(V)}(V)
Diagonal{T}(V::AbstractVector{T}) where {T} = Diagonal{T,typeof(V)}(V)
Diagonal{T}(V::AbstractVector) where {T} = Diagonal{T}(convert(AbstractVector{T}, V))

convert(::Type{Diagonal{T}}, D::Diagonal{T}) where {T} = D
convert(::Type{Diagonal{T}}, D::Diagonal) where {T} = Diagonal{T}(convert(AbstractVector{T}, D.diag))
convert(::Type{AbstractMatrix{T}}, D::Diagonal) where {T} = convert(Diagonal{T}, D)
convert(::Type{Matrix}, D::Diagonal) = diagm(D.diag)
convert(::Type{Array}, D::Diagonal) = convert(Matrix, D)
full(D::Diagonal) = convert(Array, D)

function similar(D::Diagonal, ::Type{T}) where T
    return Diagonal{T}(similar(D.diag, T))
end

copy!(D1::Diagonal, D2::Diagonal) = (copy!(D1.diag, D2.diag); D1)

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

broadcast(::typeof(abs), D::Diagonal) = Diagonal(abs.(D.diag))
real(D::Diagonal) = Diagonal(real(D.diag))
imag(D::Diagonal) = Diagonal(imag(D.diag))

istriu(D::Diagonal) = true
istril(D::Diagonal) = true
function triu!(D::Diagonal,k::Integer=0)
    n = size(D,1)
    if abs(k) > n
        throw(ArgumentError("requested diagonal, $k, out of bounds in matrix of size ($n,$n)"))
    elseif k > 0
        fill!(D.diag,0)
    end
    return D
end

function tril!(D::Diagonal,k::Integer=0)
    n = size(D,1)
    if abs(k) > n
        throw(ArgumentError("requested diagonal, $k, out of bounds in matrix of size ($n,$n)"))
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

(*)(A::AbstractTriangular, D::Diagonal) = A_mul_B!(copy(A), D)
(*)(D::Diagonal, B::AbstractTriangular) = A_mul_B!(D, copy(B))

(*)(A::AbstractMatrix, D::Diagonal) =
    scale!(similar(A, promote_op(*, eltype(A), eltype(D.diag)), size(A)), A, D.diag)
(*)(D::Diagonal, A::AbstractMatrix) =
    scale!(similar(A, promote_op(*, eltype(A), eltype(D.diag)), size(A)), D.diag, A)

A_mul_B!(A::Union{LowerTriangular,UpperTriangular}, D::Diagonal) =
    typeof(A)(A_mul_B!(A.data, D))
function A_mul_B!(A::UnitLowerTriangular, D::Diagonal)
    A_mul_B!(A.data, D)
    for i = 1:size(A, 1)
        A.data[i,i] = D.diag[i]
    end
    LowerTriangular(A.data)
end
function A_mul_B!(A::UnitUpperTriangular, D::Diagonal)
    A_mul_B!(A.data, D)
    for i = 1:size(A, 1)
        A.data[i,i] = D.diag[i]
    end
    UpperTriangular(A.data)
end
function A_mul_B!(D::Diagonal, B::UnitLowerTriangular)
    A_mul_B!(D, B.data)
    for i = 1:size(B, 1)
        B.data[i,i] = D.diag[i]
    end
    LowerTriangular(B.data)
end
function A_mul_B!(D::Diagonal, B::UnitUpperTriangular)
    A_mul_B!(D, B.data)
    for i = 1:size(B, 1)
        B.data[i,i] = D.diag[i]
    end
    UpperTriangular(B.data)
end

Ac_mul_B(A::AbstractTriangular, D::Diagonal) = A_mul_B!(ctranspose(A), D)
function Ac_mul_B(A::AbstractMatrix, D::Diagonal)
    Ac = similar(A, promote_op(*, eltype(A), eltype(D.diag)), (size(A, 2), size(A, 1)))
    ctranspose!(Ac, A)
    A_mul_B!(Ac, D)
end

At_mul_B(A::AbstractTriangular, D::Diagonal) = A_mul_B!(transpose(A), D)
function At_mul_B(A::AbstractMatrix, D::Diagonal)
    At = similar(A, promote_op(*, eltype(A), eltype(D.diag)), (size(A, 2), size(A, 1)))
    transpose!(At, A)
    A_mul_B!(At, D)
end

A_mul_Bc(D::Diagonal, B::AbstractTriangular) = A_mul_B!(D, ctranspose(B))
A_mul_Bc(D::Diagonal, Q::Union{Base.LinAlg.QRCompactWYQ,Base.LinAlg.QRPackedQ}) = A_mul_Bc!(Array(D), Q)
function A_mul_Bc(D::Diagonal, A::AbstractMatrix)
    Ac = similar(A, promote_op(*, eltype(A), eltype(D.diag)), (size(A, 2), size(A, 1)))
    ctranspose!(Ac, A)
    A_mul_B!(D, Ac)
end

A_mul_Bt(D::Diagonal, B::AbstractTriangular) = A_mul_B!(D, transpose(B))
function A_mul_Bt(D::Diagonal, A::AbstractMatrix)
    At = similar(A, promote_op(*, eltype(A), eltype(D.diag)), (size(A, 2), size(A, 1)))
    transpose!(At, A)
    A_mul_B!(D, At)
end

A_mul_B!(A::Diagonal,B::Diagonal)  = throw(MethodError(A_mul_B!, Tuple{Diagonal,Diagonal}))
At_mul_B!(A::Diagonal,B::Diagonal) = throw(MethodError(At_mul_B!, Tuple{Diagonal,Diagonal}))
Ac_mul_B!(A::Diagonal,B::Diagonal) = throw(MethodError(Ac_mul_B!, Tuple{Diagonal,Diagonal}))
A_mul_B!(A::Base.LinAlg.QRPackedQ, D::Diagonal) = throw(MethodError(A_mul_B!, Tuple{Diagonal,Diagonal}))
A_mul_B!(A::Diagonal,B::AbstractMatrix)  = scale!(A.diag,B)
At_mul_B!(A::Diagonal,B::AbstractMatrix) = scale!(A.diag,B)
Ac_mul_B!(A::Diagonal,B::AbstractMatrix) = scale!(conj(A.diag),B)
A_mul_B!(A::AbstractMatrix,B::Diagonal)  = scale!(A,B.diag)
A_mul_Bt!(A::AbstractMatrix,B::Diagonal) = scale!(A,B.diag)
A_mul_Bc!(A::AbstractMatrix,B::Diagonal) = scale!(A,conj(B.diag))

# Get ambiguous method if try to unify AbstractVector/AbstractMatrix here using AbstractVecOrMat
A_mul_B!(out::AbstractVector, A::Diagonal, in::AbstractVector) = out .= A.diag .* in
Ac_mul_B!(out::AbstractVector, A::Diagonal, in::AbstractVector) = out .= ctranspose.(A.diag) .* in
At_mul_B!(out::AbstractVector, A::Diagonal, in::AbstractVector) = out .= transpose.(A.diag) .* in

A_mul_B!(out::AbstractMatrix, A::Diagonal, in::AbstractMatrix) = out .= A.diag .* in
Ac_mul_B!(out::AbstractMatrix, A::Diagonal, in::AbstractMatrix) = out .= ctranspose.(A.diag) .* in
At_mul_B!(out::AbstractMatrix, A::Diagonal, in::AbstractMatrix) = out .= transpose.(A.diag) .* in

# ambiguities with Symmetric/Hermitian
# RealHermSymComplex[Sym]/[Herm] only include Number; invariant to [c]transpose
A_mul_Bt(A::Diagonal, B::RealHermSymComplexSym) = A*B
At_mul_B(A::RealHermSymComplexSym, B::Diagonal) = A*B
A_mul_Bc(A::Diagonal, B::RealHermSymComplexHerm) = A*B
Ac_mul_B(A::RealHermSymComplexHerm, B::Diagonal) = A*B

(/)(Da::Diagonal, Db::Diagonal) = Diagonal(Da.diag ./ Db.diag)
function A_ldiv_B!(D::Diagonal{T}, v::AbstractVector{T}) where {T}
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
function A_ldiv_B!(D::Diagonal{T}, V::AbstractMatrix{T}) where {T}
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

Ac_ldiv_B!(D::Diagonal{T}, B::AbstractVecOrMat{T}) where {T} = A_ldiv_B!(conj(D), B)
At_ldiv_B!(D::Diagonal{T}, B::AbstractVecOrMat{T}) where {T} = A_ldiv_B!(D, B)

function A_rdiv_B!(A::AbstractMatrix{T}, D::Diagonal{T}) where {T}
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

A_rdiv_Bc!(A::AbstractMatrix{T}, D::Diagonal{T}) where {T} = A_rdiv_B!(A, conj(D))
A_rdiv_Bt!(A::AbstractMatrix{T}, D::Diagonal{T}) where {T} = A_rdiv_B!(A, D)

# Methods to resolve ambiguities with `Diagonal`
@inline *(rowvec::RowVector, D::Diagonal) = transpose(D * transpose(rowvec))
@inline A_mul_Bt(D::Diagonal, rowvec::RowVector) = D*transpose(rowvec)
@inline A_mul_Bc(D::Diagonal, rowvec::RowVector) = D*ctranspose(rowvec)

conj(D::Diagonal) = Diagonal(conj(D.diag))
transpose(D::Diagonal{<:Number}) = D
transpose(D::Diagonal) = Diagonal(transpose.(D.diag))
ctranspose(D::Diagonal{<:Number}) = conj(D)
ctranspose(D::Diagonal) = Diagonal(ctranspose.(D.diag))

diag(D::Diagonal) = D.diag
trace(D::Diagonal) = sum(D.diag)
det(D::Diagonal) = prod(D.diag)
logdet(D::Diagonal{<:Real}) = sum(log, D.diag)
function logdet(D::Diagonal{<:Complex}) # make sure branch cut is correct
    z = sum(log, D.diag)
    complex(real(z), rem2pi(imag(z), RoundNearest))
end
# identity matrices via eye(Diagonal{type},n)
eye(::Type{Diagonal{T}}, n::Int) where {T} = Diagonal(ones(T,n))

# Matrix functions
expm(D::Diagonal) = Diagonal(exp.(D.diag))
expm(D::Diagonal{<:AbstractMatrix}) = Diagonal(expm.(D.diag))
logm(D::Diagonal) = Diagonal(log.(D.diag))
logm(D::Diagonal{<:AbstractMatrix}) = Diagonal(logm.(D.diag))
sqrtm(D::Diagonal) = Diagonal(sqrt.(D.diag))
sqrtm(D::Diagonal{<:AbstractMatrix}) = Diagonal(sqrtm.(D.diag))

#Linear solver
function A_ldiv_B!(D::Diagonal, B::StridedVecOrMat)
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
(\)(D::Diagonal, A::AbstractMatrix) = D.diag .\ A
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
eigvals(D::Diagonal{<:Number}) = D.diag
eigvals(D::Diagonal) = [eigvals(x) for x in D.diag] #For block matrices, etc.
eigvecs(D::Diagonal) = eye(D)
eigfact(D::Diagonal) = Eigen(eigvals(D), eigvecs(D))

#Singular system
svdvals(D::Diagonal{<:Number}) = sort!(abs.(D.diag), rev = true)
svdvals(D::Diagonal) = [svdvals(v) for v in D.diag]
function svd(D::Diagonal{<:Number})
    S   = abs.(D.diag)
    piv = sortperm(S, rev = true)
    U   = Diagonal(D.diag ./ S)
    Up  = hcat([U[:,i] for i = 1:length(D.diag)][piv]...)
    V   = Diagonal(ones(D.diag))
    Vp  = hcat([V[:,i] for i = 1:length(D.diag)][piv]...)
    return (Up, S[piv], Vp)
end
function svdfact(D::Diagonal)
    U, s, V = svd(D)
    SVD(U, s, V')
end
