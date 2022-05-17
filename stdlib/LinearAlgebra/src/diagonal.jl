# This file is a part of Julia. License is MIT: https://julialang.org/license

## Diagonal matrices

struct Diagonal{T,V<:AbstractVector{T}} <: AbstractMatrix{T}
    diag::V

    function Diagonal{T,V}(diag) where {T,V<:AbstractVector{T}}
        require_one_based_indexing(diag)
        new{T,V}(diag)
    end
end
Diagonal{T,V}(d::Diagonal) where {T,V<:AbstractVector{T}} = Diagonal{T,V}(d.diag)
Diagonal(v::AbstractVector{T}) where {T} = Diagonal{T,typeof(v)}(v)
Diagonal{T}(v::AbstractVector) where {T} = Diagonal(convert(AbstractVector{T}, v)::AbstractVector{T})

function Base.promote_rule(A::Type{<:Diagonal{<:Any,V}}, B::Type{<:Diagonal{<:Any,W}}) where {V,W}
    X = promote_type(V, W)
    T = eltype(X)
    isconcretetype(T) && return Diagonal{T,X}
    return typejoin(A, B)
end

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
Matrix(D::Diagonal{T}) where {T} = Matrix{promote_type(T, typeof(zero(T)))}(D)
Array(D::Diagonal{T}) where {T} = Matrix(D)
function Matrix{T}(D::Diagonal) where {T}
    n = size(D, 1)
    B = zeros(T, n, n)
    @inbounds for i in 1:n
        B[i,i] = D.diag[i]
    end
    return B
end

"""
    Diagonal{T}(undef, n)

Construct an uninitialized `Diagonal{T}` of length `n`. See `undef`.
"""
Diagonal{T}(::UndefInitializer, n::Integer) where T = Diagonal(Vector{T}(undef, n))

similar(D::Diagonal, ::Type{T}) where {T} = Diagonal(similar(D.diag, T))
similar(::Diagonal, ::Type{T}, dims::Union{Dims{1},Dims{2}}) where {T} = zeros(T, dims...)

copyto!(D1::Diagonal, D2::Diagonal) = (copyto!(D1.diag, D2.diag); D1)

size(D::Diagonal) = (n = length(D.diag); (n,n))

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
diagzero(::Diagonal{T}, i, j) where {T} = zero(T)
diagzero(D::Diagonal{<:AbstractMatrix{T}}, i, j) where {T} = zeros(T, size(D.diag[i], 1), size(D.diag[j], 2))

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
function triu!(D::Diagonal{T}, k::Integer=0) where T
    n = size(D,1)
    if !(-n + 1 <= k <= n + 1)
        throw(ArgumentError(string("the requested diagonal, $k, must be at least ",
            "$(-n + 1) and at most $(n + 1) in an $n-by-$n matrix")))
    elseif k > 0
        fill!(D.diag, zero(T))
    end
    return D
end

function tril!(D::Diagonal{T}, k::Integer=0) where T
    n = size(D,1)
    if !(-n - 1 <= k <= n - 1)
        throw(ArgumentError(string("the requested diagonal, $k, must be at least ",
            "$(-n - 1) and at most $(n - 1) in an $n-by-$n matrix")))
    elseif k < 0
        fill!(D.diag, zero(T))
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

function _muldiag_size_check(A, B)
    nA = size(A, 2)
    mB = size(B, 1)
    @noinline throw_dimerr(::AbstractMatrix, nA, mB) = throw(DimensionMismatch("second dimension of A, $nA, does not match first dimension of B, $mB"))
    @noinline throw_dimerr(::AbstractVector, nA, mB) = throw(DimensionMismatch("second dimension of D, $nA, does not match length of V, $mB"))
    nA == mB || throw_dimerr(B, nA, mB)
    return nothing
end
# the output matrix should have the same size as the non-diagonal input matrix or vector
@noinline throw_dimerr(szC, szA) = throw(DimensionMismatch("output matrix has size: $szC, but should have size $szA"))
_size_check_out(C, ::Diagonal, A) = _size_check_out(C, A)
_size_check_out(C, A, ::Diagonal) = _size_check_out(C, A)
_size_check_out(C, A::Diagonal, ::Diagonal) = _size_check_out(C, A)
function _size_check_out(C, A)
    szA = size(A)
    szC = size(C)
    szA == szC || throw_dimerr(szC, szA)
    return nothing
end
function _muldiag_size_check(C, A, B)
    _muldiag_size_check(A, B)
    _size_check_out(C, A, B)
end

function (*)(Da::Diagonal, Db::Diagonal)
    _muldiag_size_check(Da, Db)
    return Diagonal(Da.diag .* Db.diag)
end

function (*)(D::Diagonal, V::AbstractVector)
    _muldiag_size_check(D, V)
    return D.diag .* V
end

(*)(A::AbstractMatrix, D::Diagonal) =
    mul!(similar(A, promote_op(*, eltype(A), eltype(D.diag)), size(A)), A, D)
(*)(D::Diagonal, A::AbstractMatrix) =
    mul!(similar(A, promote_op(*, eltype(A), eltype(D.diag)), size(A)), D, A)

rmul!(A::AbstractMatrix, D::Diagonal) = @inline mul!(A, A, D)
lmul!(D::Diagonal, B::AbstractVecOrMat) = @inline mul!(B, D, B)

#TODO: It seems better to call (D' * adjA')' directly?
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

@inline function __muldiag!(out, D::Diagonal, B, alpha, beta)
    require_one_based_indexing(out)
    if iszero(alpha)
        _rmul_or_fill!(out, beta)
    else
        if iszero(beta)
            @inbounds for j in axes(B, 2)
                @simd for i in axes(B, 1)
                    out[i,j] = D.diag[i] * B[i,j] * alpha
                end
            end
        else
            @inbounds for j in axes(B, 2)
                @simd for i in axes(B, 1)
                    out[i,j] = D.diag[i] * B[i,j] * alpha + out[i,j] * beta
                end
            end
        end
    end
    return out
end
@inline function __muldiag!(out, A, D::Diagonal, alpha, beta)
    require_one_based_indexing(out)
    if iszero(alpha)
        _rmul_or_fill!(out, beta)
    else
        if iszero(beta)
            @inbounds for j in axes(A, 2)
                dja = D.diag[j] * alpha
                @simd for i in axes(A, 1)
                    out[i,j] = A[i,j] * dja
                end
            end
        else
            @inbounds for j in axes(A, 2)
                dja = D.diag[j] * alpha
                @simd for i in axes(A, 1)
                    out[i,j] = A[i,j] * dja + out[i,j] * beta
                end
            end
        end
    end
    return out
end
@inline function __muldiag!(out::Diagonal, D1::Diagonal, D2::Diagonal, alpha, beta)
    d1 = D1.diag
    d2 = D2.diag
    if iszero(alpha)
        _rmul_or_fill!(out.diag, beta)
    else
        if iszero(beta)
            @inbounds @simd for i in eachindex(out.diag)
                out.diag[i] = d1[i] * d2[i] * alpha
            end
        else
            @inbounds @simd for i in eachindex(out.diag)
                out.diag[i] = d1[i] * d2[i] * alpha + out.diag[i] * beta
            end
        end
    end
    return out
end
@inline function __muldiag!(out, D1::Diagonal, D2::Diagonal, alpha, beta)
    require_one_based_indexing(out)
    mA = size(D1, 1)
    d1 = D1.diag
    d2 = D2.diag
    _rmul_or_fill!(out, beta)
    if !iszero(alpha)
        @inbounds @simd for i in 1:mA
            out[i,i] += d1[i] * d2[i] * alpha
        end
    end
    return out
end

@inline function _muldiag!(out, A, B, alpha, beta)
    _muldiag_size_check(out, A, B)
    __muldiag!(out, A, B, alpha, beta)
    return out
end

# Get ambiguous method if try to unify AbstractVector/AbstractMatrix here using AbstractVecOrMat
@inline mul!(out::AbstractVector, D::Diagonal, V::AbstractVector, alpha::Number, beta::Number) =
    _muldiag!(out, D, V, alpha, beta)
@inline mul!(out::AbstractMatrix, D::Diagonal, B::AbstractMatrix, alpha::Number, beta::Number) =
    _muldiag!(out, D, B, alpha, beta)
@inline mul!(out::AbstractMatrix, D::Diagonal, B::Adjoint{<:Any,<:AbstractVecOrMat},
             alpha::Number, beta::Number) = _muldiag!(out, D, B, alpha, beta)
@inline mul!(out::AbstractMatrix, D::Diagonal, B::Transpose{<:Any,<:AbstractVecOrMat},
             alpha::Number, beta::Number) = _muldiag!(out, D, B, alpha, beta)

@inline mul!(out::AbstractMatrix, A::AbstractMatrix, D::Diagonal, alpha::Number, beta::Number) =
    _muldiag!(out, A, D, alpha, beta)
@inline mul!(out::AbstractMatrix, A::Adjoint{<:Any,<:AbstractVecOrMat}, D::Diagonal,
             alpha::Number, beta::Number) = _muldiag!(out, A, D, alpha, beta)
@inline mul!(out::AbstractMatrix, A::Transpose{<:Any,<:AbstractVecOrMat}, D::Diagonal,
             alpha::Number, beta::Number) = _muldiag!(out, A, D, alpha, beta)
@inline mul!(C::Diagonal, Da::Diagonal, Db::Diagonal, alpha::Number, beta::Number) =
    _muldiag!(C, Da, Db, alpha, beta)

mul!(C::AbstractMatrix, Da::Diagonal, Db::Diagonal, alpha::Number, beta::Number) =
    _muldiag!(C, Da, Db, alpha, beta)

_init(op, A::AbstractArray{<:Number}, B::AbstractArray{<:Number}) =
    (_ -> zero(typeof(op(oneunit(eltype(A)), oneunit(eltype(B))))))
_init(op, A::AbstractArray, B::AbstractArray) = promote_op(op, eltype(A), eltype(B))

/(A::AbstractVecOrMat, D::Diagonal) = _rdiv!(_init(/, A, D).(A), A, D)
rdiv!(A::AbstractVecOrMat, D::Diagonal) = @inline _rdiv!(A, A, D)
# avoid copy when possible via internal 3-arg backend
function _rdiv!(B::AbstractVecOrMat, A::AbstractVecOrMat, D::Diagonal)
    require_one_based_indexing(A)
    dd = D.diag
    m, n = size(A, 1), size(A, 2)
    if (k = length(dd)) != n
        throw(DimensionMismatch("left hand side has $n columns but D is $k by $k"))
    end
    @inbounds for j in 1:n
        ddj = dd[j]
        iszero(ddj) && throw(SingularException(j))
        for i in 1:m
            B[i, j] = A[i, j] / ddj
        end
    end
    B
end

function \(D::Diagonal, B::AbstractVector)
    j = findfirst(iszero, D.diag)
    isnothing(j) || throw(SingularException(j))
    return D.diag .\ B
end
\(D::Diagonal, B::AbstractMatrix) =
    ldiv!(_init(\, D, B).(B), D, B)

ldiv!(D::Diagonal, B::AbstractVecOrMat) = @inline ldiv!(B, D, B)
function ldiv!(B::AbstractVecOrMat, D::Diagonal, A::AbstractVecOrMat)
    require_one_based_indexing(A, B)
    dd = D.diag
    d = length(dd)
    m, n = size(A, 1), size(A, 2)
    m′, n′ = size(B, 1), size(B, 2)
    m == d || throw(DimensionMismatch("right hand side has $m rows but D is $d by $d"))
    (m, n) == (m′, n′) || throw(DimensionMismatch("expect output to be $m by $n, but got $m′ by $n′"))
    j = findfirst(iszero, D.diag)
    isnothing(j) || throw(SingularException(j))
    @inbounds for j = 1:n, i = 1:m
        B[i, j] = dd[i] \ A[i, j]
    end
    B
end

# Optimizations for \, / between Diagonals
\(D::Diagonal, B::Diagonal) = ldiv!(similar(B, promote_op(\, eltype(D), eltype(B))), D, B)
/(A::Diagonal, D::Diagonal) = _rdiv!(similar(A, promote_op(/, eltype(A), eltype(D))), A, D)
function _rdiv!(Dc::Diagonal, Db::Diagonal, Da::Diagonal)
    n, k = length(Db.diag), length(Da.diag)
    n == k || throw(DimensionMismatch("left hand side has $n columns but D is $k by $k"))
    j = findfirst(iszero, Da.diag)
    isnothing(j) || throw(SingularException(j))
    Dc.diag .= Db.diag ./ Da.diag
    Dc
end
ldiv!(Dc::Diagonal, Da::Diagonal, Db::Diagonal) = Diagonal(ldiv!(Dc.diag, Da, Db.diag))

# optimizations for (Sym)Tridiagonal and Diagonal
@propagate_inbounds _getudiag(T::Tridiagonal, i) = T.du[i]
@propagate_inbounds _getudiag(S::SymTridiagonal, i) = S.ev[i]
@propagate_inbounds _getdiag(T::Tridiagonal, i) = T.d[i]
@propagate_inbounds _getdiag(S::SymTridiagonal, i) = symmetric(S.dv[i], :U)::symmetric_type(eltype(S.dv))
@propagate_inbounds _getldiag(T::Tridiagonal, i) = T.dl[i]
@propagate_inbounds _getldiag(S::SymTridiagonal, i) = transpose(S.ev[i])

function (\)(D::Diagonal, S::SymTridiagonal)
    T = promote_op(\, eltype(D), eltype(S))
    du = similar(S.ev, T, max(length(S.dv)-1, 0))
    d  = similar(S.dv, T, length(S.dv))
    dl = similar(S.ev, T, max(length(S.dv)-1, 0))
    ldiv!(Tridiagonal(dl, d, du), D, S)
end
(\)(D::Diagonal, T::Tridiagonal) = ldiv!(similar(T, promote_op(\, eltype(D), eltype(T))), D, T)
function ldiv!(T::Tridiagonal, D::Diagonal, S::Union{SymTridiagonal,Tridiagonal})
    m = size(S, 1)
    dd = D.diag
    if (k = length(dd)) != m
        throw(DimensionMismatch("diagonal matrix is $k by $k but right hand side has $m rows"))
    end
    if length(T.d) != m
        throw(DimensionMismatch("target matrix size $(size(T)) does not match input matrix size $(size(S))"))
    end
    m == 0 && return T
    j = findfirst(iszero, dd)
    isnothing(j) || throw(SingularException(j))
    ddj = dd[1]
    T.d[1] = ddj \ _getdiag(S, 1)
    @inbounds if m > 1
        T.du[1] = ddj \ _getudiag(S, 1)
        for j in 2:m-1
            ddj = dd[j]
            T.dl[j-1] = ddj \ _getldiag(S, j-1)
            T.d[j]  = ddj \ _getdiag(S, j)
            T.du[j] = ddj \ _getudiag(S, j)
        end
        ddj = dd[m]
        T.dl[m-1] = ddj \ _getldiag(S, m-1)
        T.d[m] = ddj \ _getdiag(S, m)
    end
    return T
end

function (/)(S::SymTridiagonal, D::Diagonal)
    T = promote_op(\, eltype(D), eltype(S))
    du = similar(S.ev, T, max(length(S.dv)-1, 0))
    d  = similar(S.dv, T, length(S.dv))
    dl = similar(S.ev, T, max(length(S.dv)-1, 0))
    _rdiv!(Tridiagonal(dl, d, du), S, D)
end
(/)(T::Tridiagonal, D::Diagonal) = _rdiv!(similar(T, promote_op(/, eltype(T), eltype(D))), T, D)
function _rdiv!(T::Tridiagonal, S::Union{SymTridiagonal,Tridiagonal}, D::Diagonal)
    n = size(S, 2)
    dd = D.diag
    if (k = length(dd)) != n
        throw(DimensionMismatch("left hand side has $n columns but D is $k by $k"))
    end
    if length(T.d) != n
        throw(DimensionMismatch("target matrix size $(size(T)) does not match input matrix size $(size(S))"))
    end
    n == 0 && return T
    j = findfirst(iszero, dd)
    isnothing(j) || throw(SingularException(j))
    ddj = dd[1]
    T.d[1] = _getdiag(S, 1) / ddj
    @inbounds if n > 1
        T.dl[1] = _getldiag(S, 1) / ddj
        for j in 2:n-1
            ddj = dd[j]
            T.dl[j] = _getldiag(S, j) / ddj
            T.d[j] = _getdiag(S, j) / ddj
            T.du[j-1] = _getudiag(S, j-1) / ddj
        end
        ddj = dd[n]
        T.d[n] = _getdiag(S, n) / ddj
        T.du[n-1] = _getudiag(S, n-1) / ddj
    end
    return T
end

# Optimizations for [l/r]mul!, l/rdiv!, *, / and \ between Triangular and Diagonal.
# These functions are generally more efficient if we calculate the whole data field.
# The following code implements them in a unified pattern to avoid missing.
@inline function _setdiag!(data, f, diag, diag′ = nothing)
    @inbounds for i in 1:length(diag)
        data[i,i] = isnothing(diag′) ? f(diag[i]) : f(diag[i],diag′[i])
    end
    data
end
for Tri in (:UpperTriangular, :LowerTriangular)
    UTri = Symbol(:Unit, Tri)
    # 2 args
    for (fun, f) in zip((:*, :rmul!, :rdiv!, :/), (:identity, :identity, :inv, :inv))
        @eval $fun(A::$Tri, D::Diagonal) = $Tri($fun(A.data, D))
        @eval $fun(A::$UTri, D::Diagonal) = $Tri(_setdiag!($fun(A.data, D), $f, D.diag))
    end
    for (fun, f) in zip((:*, :lmul!, :ldiv!, :\), (:identity, :identity, :inv, :inv))
        @eval $fun(D::Diagonal, A::$Tri) = $Tri($fun(D, A.data))
        @eval $fun(D::Diagonal, A::$UTri) = $Tri(_setdiag!($fun(D, A.data), $f, D.diag))
    end
    # 3-arg ldiv!
    @eval ldiv!(C::$Tri, D::Diagonal, A::$Tri) = $Tri(ldiv!(C.data, D, A.data))
    @eval ldiv!(C::$Tri, D::Diagonal, A::$UTri) = $Tri(_setdiag!(ldiv!(C.data, D, A.data), inv, D.diag))
    # 3-arg mul!: invoke 5-arg mul! rather than lmul!
    @eval mul!(C::$Tri, A::Union{$Tri,$UTri}, D::Diagonal) = mul!(C, A, D, true, false)
    # 5-arg mul!
    @eval @inline mul!(C::$Tri, D::Diagonal, A::$Tri, α::Number, β::Number) = $Tri(mul!(C.data, D, A.data, α, β))
    @eval @inline function mul!(C::$Tri, D::Diagonal, A::$UTri, α::Number, β::Number)
        iszero(α) && return _rmul_or_fill!(C, β)
        diag′ = iszero(β) ? nothing : diag(C)
        data = mul!(C.data, D, A.data, α, β)
        $Tri(_setdiag!(data, MulAddMul(α, β), D.diag, diag′))
    end
    @eval @inline mul!(C::$Tri, A::$Tri, D::Diagonal, α::Number, β::Number) = $Tri(mul!(C.data, A.data, D, α, β))
    @eval @inline function mul!(C::$Tri, A::$UTri, D::Diagonal, α::Number, β::Number)
        iszero(α) && return _rmul_or_fill!(C, β)
        diag′ = iszero(β) ? nothing : diag(C)
        data = mul!(C.data, A.data, D, α, β)
        $Tri(_setdiag!(data, MulAddMul(α, β), D.diag, diag′))
    end
end

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
    require_one_based_indexing(B)
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
permutedims(D::Diagonal) = D
permutedims(D::Diagonal, perm) = (Base.checkdims_perm(D, D, perm); D)

function diag(D::Diagonal{T}, k::Integer=0) where T
    # every branch call similar(..., ::Int) to make sure the
    # same vector type is returned independent of k
    if k == 0
        return copyto!(similar(D.diag, length(D.diag)), D.diag)
    elseif -size(D,1) <= k <= size(D,1)
        return fill!(similar(D.diag, size(D,1)-abs(k)), zero(T))
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
eigvals(D::Diagonal{<:Number}; permute::Bool=true, scale::Bool=true) = copy(D.diag)
eigvals(D::Diagonal; permute::Bool=true, scale::Bool=true) =
    [eigvals(x) for x in D.diag] #For block matrices, etc.
eigvecs(D::Diagonal) = Matrix{eltype(D)}(I, size(D))
function eigen(D::Diagonal; permute::Bool=true, scale::Bool=true, sortby::Union{Function,Nothing}=nothing)
    if any(!isfinite, D.diag)
        throw(ArgumentError("matrix contains Infs or NaNs"))
    end
    Td = Base.promote_op(/, eltype(D), eltype(D))
    λ = eigvals(D)
    if !isnothing(sortby)
        p = sortperm(λ; alg=QuickSort, by=sortby)
        λ = λ[p] # make a copy, otherwise this permutes D.diag
        evecs = zeros(Td, size(D))
        @inbounds for i in eachindex(p)
            evecs[p[i],i] = one(Td)
        end
    else
        evecs = Matrix{Td}(I, size(D))
    end
    Eigen(λ, evecs)
end

#Singular system
svdvals(D::Diagonal{<:Number}) = sort!(abs.(D.diag), rev = true)
svdvals(D::Diagonal) = [svdvals(v) for v in D.diag]
function svd(D::Diagonal{T}) where {T<:Number}
    d = D.diag
    s = abs.(d)
    piv = sortperm(s, rev = true)
    S = s[piv]
    Td  = typeof(oneunit(T)/oneunit(T))
    U = zeros(Td, size(D))
    Vt = copy(U)
    for i in 1:length(d)
        j = piv[i]
        U[j,i] = d[j] / S[i]
        Vt[i,j] = one(Td)
    end
    return SVD(U, S, Vt)
end

# disambiguation methods: * and / of Diagonal and Adj/Trans AbsVec
*(x::AdjointAbsVec, D::Diagonal) = Adjoint(map((t,s) -> t'*s, D.diag, parent(x)))
*(x::TransposeAbsVec, D::Diagonal) = Transpose(map((t,s) -> transpose(t)*s, D.diag, parent(x)))
*(x::AdjointAbsVec,   D::Diagonal, y::AbstractVector) = _mapreduce_prod(*, x, D, y)
*(x::TransposeAbsVec, D::Diagonal, y::AbstractVector) = _mapreduce_prod(*, x, D, y)
/(u::AdjointAbsVec, D::Diagonal) = adjoint(adjoint(D) \ u.parent)
/(u::TransposeAbsVec, D::Diagonal) = transpose(transpose(D) \ u.parent)
# disambiguation methods: Call unoptimized version for user defined AbstractTriangular.
*(A::AbstractTriangular, D::Diagonal) = Base.@invoke *(A::AbstractMatrix, D::Diagonal)
*(D::Diagonal, A::AbstractTriangular) = Base.@invoke *(D::Diagonal, A::AbstractMatrix)

dot(x::AbstractVector, D::Diagonal, y::AbstractVector) = _mapreduce_prod(dot, x, D, y)

dot(A::Diagonal, B::Diagonal) = dot(A.diag, B.diag)
function dot(D::Diagonal, B::AbstractMatrix)
    size(D) == size(B) || throw(DimensionMismatch("Matrix sizes $(size(D)) and $(size(B)) differ"))
    return dot(D.diag, view(B, diagind(B)))
end

dot(A::AbstractMatrix, B::Diagonal) = conj(dot(B, A))

function _mapreduce_prod(f, x, D::Diagonal, y)
    if isempty(x) && isempty(D) && isempty(y)
        return zero(promote_op(f, eltype(x), eltype(D), eltype(y)))
    else
        return mapreduce(t -> f(t[1], t[2], t[3]), +, zip(x, D.diag, y))
    end
end

function cholesky!(A::Diagonal, ::NoPivot = NoPivot(); check::Bool = true)
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
@deprecate cholesky!(A::Diagonal, ::Val{false}; check::Bool = true) cholesky!(A::Diagonal, NoPivot(); check) false
@deprecate cholesky(A::Diagonal, ::Val{false}; check::Bool = true) cholesky(A::Diagonal, NoPivot(); check) false

@inline cholcopy(A::Diagonal) = copymutable_oftype(A, choltype(A))
@inline cholcopy(A::RealHermSymComplexHerm{<:Real,<:Diagonal}) = copymutable_oftype(A, choltype(A))

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
