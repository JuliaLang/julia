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

function Base.promote_rule(A::Type{<:Diagonal{<:Any,V}}, B::Type{<:Diagonal{<:Any,W}}) where {V,W}
    X = promote_type(V, W)
    T = eltype(X)
    isconcretetype(T) && return Diagonal{T,X}
    return typejoin(A, B)
end

"""
    Diagonal(V::AbstractVector)

Construct a lazy matrix with `V` as its diagonal.

See also [`UniformScaling`](@ref) for the lazy identity matrix `I`,
[`diagm`](@ref) to make a dense matrix, and [`diag`](@ref) to extract diagonal elements.

# Examples
```jldoctest
julia> d = Diagonal([1, 10, 100])
3×3 Diagonal{$Int, Vector{$Int}}:
 1   ⋅    ⋅
 ⋅  10    ⋅
 ⋅   ⋅  100

julia> diagm([7, 13])
2×2 Matrix{$Int}:
 7   0
 0  13

julia> ans + I
2×2 Matrix{Int64}:
 8   0
 0  14

julia> I(2)
2×2 Diagonal{Bool, Vector{Bool}}:
 1  ⋅
 ⋅  1
```

!!! note
    A one-column matrix is not treated like a vector, but instead calls the
    method `Diagonal(A::AbstractMatrix)` which extracts 1-element `diag(A)`:

```jldoctest
julia> A = transpose([7.0 13.0])
2×1 transpose(::Matrix{Float64}) with eltype Float64:
  7.0
 13.0

julia> Diagonal(A)
1×1 Diagonal{Float64, Vector{Float64}}:
 7.0
```
"""
Diagonal(V::AbstractVector)

"""
    Diagonal(A::AbstractMatrix)

Construct a matrix from the principal diagonal of `A`.
The input matrix `A` may be rectangular, but the output will
be square.

# Examples
```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> D = Diagonal(A)
2×2 Diagonal{Int64, Vector{Int64}}:
 1  ⋅
 ⋅  4

julia> A = [1 2 3; 4 5 6]
2×3 Matrix{Int64}:
 1  2  3
 4  5  6

julia> Diagonal(A)
2×2 Diagonal{Int64, Vector{Int64}}:
 1  ⋅
 ⋅  5
```
"""
Diagonal(A::AbstractMatrix) = Diagonal(diag(A))
Diagonal{T}(A::AbstractMatrix) where T = Diagonal{T}(diag(A))
Diagonal{T,V}(A::AbstractMatrix) where {T,V<:AbstractVector{T}} = Diagonal{T,V}(diag(A))
function convert(::Type{T}, A::AbstractMatrix) where T<:Diagonal
    checksquare(A)
    isdiag(A) ? T(A) : throw(InexactError(:convert, T, A))
end

Diagonal(D::Diagonal) = D
Diagonal{T}(D::Diagonal{T}) where {T} = D
Diagonal{T}(D::Diagonal) where {T} = Diagonal{T}(D.diag)

AbstractMatrix{T}(D::Diagonal) where {T} = Diagonal{T}(D)
AbstractMatrix{T}(D::Diagonal{T}) where {T} = copy(D)
Matrix(D::Diagonal{T}) where {T} = Matrix{promote_type(T, typeof(zero(T)))}(D)
Matrix(D::Diagonal{Any}) = Matrix{Any}(D)
Array(D::Diagonal{T}) where {T} = Matrix(D)
function Matrix{T}(D::Diagonal) where {T}
    B = Matrix{T}(undef, size(D))
    if haszero(T) # optimized path for types with zero(T) defined
        size(B,1) > 1 && fill!(B, zero(T))
        copyto!(view(B, diagind(B)), D.diag)
    else
        copyto!(B, D)
    end
    return B
end

"""
    Diagonal{T}(undef, n)

Construct an uninitialized `Diagonal{T}` of length `n`. See `undef`.
"""
Diagonal{T}(::UndefInitializer, n::Integer) where T = Diagonal(Vector{T}(undef, n))

similar(D::Diagonal, ::Type{T}) where {T} = Diagonal(similar(D.diag, T))
similar(D::Diagonal, ::Type{T}, dims::Union{Dims{1},Dims{2}}) where {T} = similar(D.diag, T, dims)

# copyto! for matching axes
_copyto_banded!(D1::Diagonal, D2::Diagonal) = (copyto!(D1.diag, D2.diag); D1)

size(D::Diagonal) = (n = length(D.diag); (n,n))

axes(D::Diagonal) = (ax = axes(D.diag, 1); (ax, ax))

@inline function Base.isassigned(D::Diagonal, i::Int, j::Int)
    @boundscheck checkbounds(Bool, D, i, j) || return false
    if i == j
        @inbounds r = isassigned(D.diag, i)
    else
        r = true
    end
    r
end

@inline function Base.isstored(D::Diagonal, i::Int, j::Int)
    @boundscheck checkbounds(D, i, j)
    if i == j
        @inbounds r = Base.isstored(D.diag, i)
    else
        r = false
    end
    r
end

function Base.minimum(D::Diagonal{T}) where T <: Number
    mindiag = minimum(D.diag)
    size(D, 1) > 1 && return (min(zero(T), mindiag))
    return mindiag
end

function Base.maximum(D::Diagonal{T}) where T <: Number
    maxdiag = Base.maximum(D.diag)
    size(D, 1) > 1 && return (max(zero(T), maxdiag))
    return maxdiag
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

@inline function getindex(D::Diagonal, b::BandIndex)
    @boundscheck checkbounds(D, b)
    if b.band == 0
        @inbounds r = D.diag[b.index]
    else
        r = diagzero(D, Tuple(_cartinds(b))...)
    end
    r
end

function setindex!(D::Diagonal, v, i::Int, j::Int)
    @boundscheck checkbounds(D, i, j)
    if i == j
        @inbounds D.diag[i] = v
    elseif !iszero(v)
        throw(ArgumentError(lazy"cannot set off-diagonal entry ($i, $j) to a nonzero value ($v)"))
    end
    return v
end


## structured matrix methods ##
function Base.replace_in_print_matrix(A::Diagonal,i::Integer,j::Integer,s::AbstractString)
    i==j ? s : Base.replace_with_centered_mark(s)
end

parent(D::Diagonal) = D.diag

copy(D::Diagonal) = Diagonal(copy(D.diag))

Base._reverse(A::Diagonal, dims) = reverse!(Matrix(A); dims)
Base._reverse(A::Diagonal, ::Colon) = Diagonal(reverse(A.diag))
Base._reverse!(A::Diagonal, ::Colon) = (reverse!(A.diag); A)

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
Base.@constprop :aggressive istriu(D::Diagonal, k::Integer=0) = k <= 0 || iszero(D.diag) ? true : false
Base.@constprop :aggressive istril(D::Diagonal, k::Integer=0) = k >= 0 || iszero(D.diag) ? true : false
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
    @noinline throw_dimerr(::AbstractMatrix, nA, mB) = throw(DimensionMismatch(lazy"second dimension of A, $nA, does not match first dimension of B, $mB"))
    @noinline throw_dimerr(::AbstractVector, nA, mB) = throw(DimensionMismatch(lazy"second dimension of D, $nA, does not match length of V, $mB"))
    nA == mB || throw_dimerr(B, nA, mB)
    return nothing
end
# the output matrix should have the same size as the non-diagonal input matrix or vector
@noinline throw_dimerr(szC, szA) = throw(DimensionMismatch(lazy"output matrix has size: $szC, but should have size $szA"))
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

rmul!(A::AbstractMatrix, D::Diagonal) = @inline mul!(A, A, D)
lmul!(D::Diagonal, B::AbstractVecOrMat) = @inline mul!(B, D, B)

function __muldiag!(out, D::Diagonal, B, _add::MulAddMul{ais1,bis0}) where {ais1,bis0}
    require_one_based_indexing(out, B)
    alpha, beta = _add.alpha, _add.beta
    if iszero(alpha)
        _rmul_or_fill!(out, beta)
    else
        if bis0
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
function __muldiag!(out, A, D::Diagonal, _add::MulAddMul{ais1,bis0}) where {ais1,bis0}
    require_one_based_indexing(out, A)
    alpha, beta = _add.alpha, _add.beta
    if iszero(alpha)
        _rmul_or_fill!(out, beta)
    else
        if bis0
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
function __muldiag!(out::Diagonal, D1::Diagonal, D2::Diagonal, _add::MulAddMul{ais1,bis0}) where {ais1,bis0}
    d1 = D1.diag
    d2 = D2.diag
    alpha, beta = _add.alpha, _add.beta
    if iszero(alpha)
        _rmul_or_fill!(out.diag, beta)
    else
        if bis0
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
function __muldiag!(out, D1::Diagonal, D2::Diagonal, _add::MulAddMul{ais1,bis0}) where {ais1,bis0}
    require_one_based_indexing(out)
    alpha, beta = _add.alpha, _add.beta
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

function _mul_diag!(out, A, B, _add)
    _muldiag_size_check(out, A, B)
    __muldiag!(out, A, B, _add)
    return out
end

_mul!(out::AbstractVecOrMat, D::Diagonal, V::AbstractVector, _add) =
    _mul_diag!(out, D, V, _add)
_mul!(out::AbstractMatrix, D::Diagonal, B::AbstractMatrix, _add) =
    _mul_diag!(out, D, B, _add)
_mul!(out::AbstractMatrix, A::AbstractMatrix, D::Diagonal, _add) =
    _mul_diag!(out, A, D, _add)
_mul!(C::Diagonal, Da::Diagonal, Db::Diagonal, _add) =
    _mul_diag!(C, Da, Db, _add)
_mul!(C::AbstractMatrix, Da::Diagonal, Db::Diagonal, _add) =
    _mul_diag!(C, Da, Db, _add)

function (*)(Da::Diagonal, A::AbstractMatrix, Db::Diagonal)
    _muldiag_size_check(Da, A)
    _muldiag_size_check(A, Db)
    return broadcast(*, Da.diag, A, permutedims(Db.diag))
end

function (*)(Da::Diagonal, Db::Diagonal, Dc::Diagonal)
    _muldiag_size_check(Da, Db)
    _muldiag_size_check(Db, Dc)
    return Diagonal(Da.diag .* Db.diag .* Dc.diag)
end

/(A::AbstractVecOrMat, D::Diagonal) = _rdiv!(matprod_dest(A, D, promote_op(/, eltype(A), eltype(D))), A, D)

rdiv!(A::AbstractVecOrMat, D::Diagonal) = @inline _rdiv!(A, A, D)
# avoid copy when possible via internal 3-arg backend
function _rdiv!(B::AbstractVecOrMat, A::AbstractVecOrMat, D::Diagonal)
    require_one_based_indexing(A)
    dd = D.diag
    m, n = size(A, 1), size(A, 2)
    if (k = length(dd)) != n
        throw(DimensionMismatch(lazy"left hand side has $n columns but D is $k by $k"))
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
\(D::Diagonal, B::AbstractMatrix) = ldiv!(matprod_dest(D, B, promote_op(\, eltype(D), eltype(B))), D, B)

ldiv!(D::Diagonal, B::AbstractVecOrMat) = @inline ldiv!(B, D, B)
function ldiv!(B::AbstractVecOrMat, D::Diagonal, A::AbstractVecOrMat)
    require_one_based_indexing(A, B)
    dd = D.diag
    d = length(dd)
    m, n = size(A, 1), size(A, 2)
    m′, n′ = size(B, 1), size(B, 2)
    m == d || throw(DimensionMismatch(lazy"right hand side has $m rows but D is $d by $d"))
    (m, n) == (m′, n′) || throw(DimensionMismatch(lazy"expect output to be $m by $n, but got $m′ by $n′"))
    j = findfirst(iszero, D.diag)
    isnothing(j) || throw(SingularException(j))
    @inbounds for j = 1:n, i = 1:m
        B[i, j] = dd[i] \ A[i, j]
    end
    B
end

function _rdiv!(Dc::Diagonal, Db::Diagonal, Da::Diagonal)
    n, k = length(Db.diag), length(Da.diag)
    n == k || throw(DimensionMismatch(lazy"left hand side has $n columns but D is $k by $k"))
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
        throw(DimensionMismatch(lazy"diagonal matrix is $k by $k but right hand side has $m rows"))
    end
    if length(T.d) != m
        throw(DimensionMismatch(lazy"target matrix size $(size(T)) does not match input matrix size $(size(S))"))
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
(/)(T::Tridiagonal, D::Diagonal) = _rdiv!(matprod_dest(T, D, promote_op(/, eltype(T), eltype(D))), T, D)
function _rdiv!(T::Tridiagonal, S::Union{SymTridiagonal,Tridiagonal}, D::Diagonal)
    n = size(S, 2)
    dd = D.diag
    if (k = length(dd)) != n
        throw(DimensionMismatch(lazy"left hand side has $n columns but D is $k by $k"))
    end
    if length(T.d) != n
        throw(DimensionMismatch(lazy"target matrix size $(size(T)) does not match input matrix size $(size(S))"))
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
    # 3-arg mul! is disambiguated in special.jl
    # 5-arg mul!
    @eval _mul!(C::$Tri, D::Diagonal, A::$Tri, _add) = $Tri(mul!(C.data, D, A.data, _add.alpha, _add.beta))
    @eval function _mul!(C::$Tri, D::Diagonal, A::$UTri, _add::MulAddMul{ais1,bis0}) where {ais1,bis0}
        α, β = _add.alpha, _add.beta
        iszero(α) && return _rmul_or_fill!(C, β)
        diag′ = bis0 ? nothing : diag(C)
        data = mul!(C.data, D, A.data, α, β)
        $Tri(_setdiag!(data, _add, D.diag, diag′))
    end
    @eval _mul!(C::$Tri, A::$Tri, D::Diagonal, _add) = $Tri(mul!(C.data, A.data, D, _add.alpha, _add.beta))
    @eval function _mul!(C::$Tri, A::$UTri, D::Diagonal, _add::MulAddMul{ais1,bis0}) where {ais1,bis0}
        α, β = _add.alpha, _add.beta
        iszero(α) && return _rmul_or_fill!(C, β)
        diag′ = bis0 ? nothing : diag(C)
        data = mul!(C.data, A.data, D, α, β)
        $Tri(_setdiag!(data, _add, D.diag, diag′))
    end
end

@inline function kron!(C::AbstractMatrix, A::Diagonal, B::Diagonal)
    valA = A.diag; nA = length(valA)
    valB = B.diag; nB = length(valB)
    nC = checksquare(C)
    @boundscheck nC == nA*nB ||
        throw(DimensionMismatch(lazy"expect C to be a $(nA*nB)x$(nA*nB) matrix, got size $(nC)x$(nC)"))
    isempty(A) || isempty(B) || fill!(C, zero(A[1,1] * B[1,1]))
    @inbounds for i = 1:nA, j = 1:nB
        idx = (i-1)*nB+j
        C[idx, idx] = valA[i] * valB[j]
    end
    return C
end

kron(A::Diagonal, B::Diagonal) = Diagonal(kron(A.diag, B.diag))

function kron(A::Diagonal, B::SymTridiagonal)
    kdv = kron(diag(A), B.dv)
    # We don't need to drop the last element
    kev = kron(diag(A), _pushzero(_evview(B)))
    SymTridiagonal(kdv, kev)
end
function kron(A::Diagonal, B::Tridiagonal)
    # `_droplast!` is only guaranteed to work with `Vector`
    kd = _makevector(kron(diag(A), B.d))
    kdl = _droplast!(_makevector(kron(diag(A), _pushzero(B.dl))))
    kdu = _droplast!(_makevector(kron(diag(A), _pushzero(B.du))))
    Tridiagonal(kdl, kd, kdu)
end

@inline function kron!(C::AbstractMatrix, A::Diagonal, B::AbstractMatrix)
    require_one_based_indexing(B)
    (mA, nA) = size(A)
    (mB, nB) = size(B)
    (mC, nC) = size(C)
    @boundscheck (mC, nC) == (mA * mB, nA * nB) ||
        throw(DimensionMismatch(lazy"expect C to be a $(mA * mB)x$(nA * nB) matrix, got size $(mC)x$(nC)"))
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
        throw(DimensionMismatch(lazy"expect C to be a $(mA * mB)x$(nA * nB) matrix, got size $(mC)x$(nC)"))
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
adjoint(D::Diagonal{<:Number}) = Diagonal(vec(adjoint(D.diag)))
adjoint(D::Diagonal{<:Number,<:Base.ReshapedArray{<:Number,1,<:Adjoint}}) = Diagonal(adjoint(parent(D.diag)))
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

# Cube root of a real-valued diagonal matrix
cbrt(A::Diagonal{<:Real}) = Diagonal(cbrt.(A.diag))

function inv(D::Diagonal{T}) where T
    Di = similar(D.diag, typeof(inv(oneunit(T))))
    for i = 1:length(D.diag)
        if iszero(D.diag[i])
            throw(SingularException(i))
        end
        Di[i] = inv(D.diag[i])
    end
    Diagonal(Di)
end

function pinv(D::Diagonal{T}) where T
    Di = similar(D.diag, typeof(inv(oneunit(T))))
    for i = 1:length(D.diag)
        if !iszero(D.diag[i])
            invD = inv(D.diag[i])
            if isfinite(invD)
                Di[i] = invD
                continue
            end
        end
        # fallback
        Di[i] = zero(T)
    end
    Diagonal(Di)
end
function pinv(D::Diagonal{T}, tol::Real) where T
    Di = similar(D.diag, typeof(inv(oneunit(T))))
    if !isempty(D.diag)
        maxabsD = maximum(abs, D.diag)
        for i = 1:length(D.diag)
            if abs(D.diag[i]) > tol*maxabsD
                invD = inv(D.diag[i])
                if isfinite(invD)
                    Di[i] = invD
                    continue
                end
            end
            # fallback
            Di[i] = zero(T)
        end
    end
    Diagonal(Di)
end

# TODO Docstrings for eigvals, eigvecs, eigen all mention permute, scale, sortby as keyword args
# but not all of them below provide them. Do we need to fix that?
#Eigensystem
eigvals(D::Diagonal{<:Number}; permute::Bool=true, scale::Bool=true) = copy(D.diag)
eigvals(D::Diagonal; permute::Bool=true, scale::Bool=true) =
    reduce(vcat, eigvals(x) for x in D.diag) #For block matrices, etc.
function eigvecs(D::Diagonal{T}) where T<:AbstractMatrix
    diag_vecs = [ eigvecs(x) for x in D.diag ]
    matT = reduce((a,b) -> promote_type(typeof(a),typeof(b)), diag_vecs)
    ncols_diag = [ size(x, 2) for x in D.diag ]
    nrows = size(D, 1)
    vecs = Matrix{Vector{eltype(matT)}}(undef, nrows, sum(ncols_diag))
    for j in axes(D, 2), i in axes(D, 1)
        jj = sum(view(ncols_diag,1:j-1))
        if i == j
            for k in 1:ncols_diag[j]
                vecs[i,jj+k] = diag_vecs[i][:,k]
            end
        else
            for k in 1:ncols_diag[j]
                vecs[i,jj+k] = zeros(eltype(T), ncols_diag[i])
            end
        end
    end
    return vecs
end
function eigen(D::Diagonal; permute::Bool=true, scale::Bool=true, sortby::Union{Function,Nothing}=nothing)
    if any(!isfinite, D.diag)
        throw(ArgumentError("matrix contains Infs or NaNs"))
    end
    Td = Base.promote_op(/, eltype(D), eltype(D))
    λ = eigvals(D)
    if !isnothing(sortby)
        p = sortperm(λ; alg=QuickSort, by=sortby)
        λ = λ[p]
        evecs = zeros(Td, size(D))
        @inbounds for i in eachindex(p)
            evecs[p[i],i] = one(Td)
        end
    else
        evecs = Diagonal(ones(Td, length(λ)))
    end
    Eigen(λ, evecs)
end
function eigen(D::Diagonal{<:AbstractMatrix}; permute::Bool=true, scale::Bool=true, sortby::Union{Function,Nothing}=nothing)
    if any(any(!isfinite, x) for x in D.diag)
        throw(ArgumentError("matrix contains Infs or NaNs"))
    end
    λ = eigvals(D)
    evecs = eigvecs(D)
    if !isnothing(sortby)
        p = sortperm(λ; alg=QuickSort, by=sortby)
        λ = λ[p]
        evecs = evecs[:,p]
    end
    Eigen(λ, evecs)
end
function eigen(Da::Diagonal, Db::Diagonal; sortby::Union{Function,Nothing}=nothing)
    if any(!isfinite, Da.diag) || any(!isfinite, Db.diag)
        throw(ArgumentError("matrices contain Infs or NaNs"))
    end
    if any(iszero, Db.diag)
        throw(ArgumentError("right-hand side diagonal matrix is singular"))
    end
    return GeneralizedEigen(eigen(Db \ Da; sortby)...)
end
function eigen(A::AbstractMatrix, D::Diagonal; sortby::Union{Function,Nothing}=nothing)
    if any(iszero, D.diag)
        throw(ArgumentError("right-hand side diagonal matrix is singular"))
    end
    if size(A, 1) == size(A, 2) && isdiag(A)
        return eigen(Diagonal(A), D; sortby)
    elseif all(isposdef, D.diag)
        S = promote_type(eigtype(eltype(A)), eltype(D))
        return eigen(A, cholesky(Diagonal{S}(D)); sortby)
    else
        return eigen!(D \ A; sortby)
    end
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

*(x::AdjointAbsVec,   D::Diagonal, y::AbstractVector) = _mapreduce_prod(*, x, D, y)
*(x::TransposeAbsVec, D::Diagonal, y::AbstractVector) = _mapreduce_prod(*, x, D, y)
/(u::AdjointAbsVec, D::Diagonal) = (D' \ u')'
/(u::TransposeAbsVec, D::Diagonal) = transpose(transpose(D) \ transpose(u))
# disambiguation methods: Call unoptimized version for user defined AbstractTriangular.
*(A::AbstractTriangular, D::Diagonal) = @invoke *(A::AbstractMatrix, D::Diagonal)
*(D::Diagonal, A::AbstractTriangular) = @invoke *(D::Diagonal, A::AbstractMatrix)

dot(x::AbstractVector, D::Diagonal, y::AbstractVector) = _mapreduce_prod(dot, x, D, y)

dot(A::Diagonal, B::Diagonal) = dot(A.diag, B.diag)
function dot(D::Diagonal, B::AbstractMatrix)
    size(D) == size(B) || throw(DimensionMismatch(lazy"Matrix sizes $(size(D)) and $(size(B)) differ"))
    return dot(D.diag, view(B, diagind(B, IndexStyle(B))))
end

dot(A::AbstractMatrix, B::Diagonal) = conj(dot(B, A))

function _mapreduce_prod(f, x, D::Diagonal, y)
    if !(length(x) == length(D.diag) == length(y))
        throw(DimensionMismatch(lazy"x has length $(length(x)), D has size $(size(D)), and y has $(length(y))"))
    end
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

function cholesky!(A::Diagonal, ::RowMaximum; tol=0.0, check=true)
    if !ishermitian(A)
        C = CholeskyPivoted(A, 'U', Vector{BlasInt}(), convert(BlasInt, 1),
                            tol, convert(BlasInt, -1))
        check && checkpositivedefinite(convert(BlasInt, -1))
    else
        d = A.diag
        n = length(d)
        info = 0
        rank = n
        p = sortperm(d, rev = true, by = real)
        tol = tol < 0 ? n*eps(eltype(A))*real(d[p[1]]) : tol # LAPACK behavior
        permute!(d, p)
        @inbounds for i in eachindex(d)
            di = d[i]
            rootdi, j = _cholpivoted!(di, tol)
            if j == 0
                d[i] = rootdi
            else
                rank = i - 1
                info = 1
                break
            end
        end
        C = CholeskyPivoted(A, 'U', p, convert(BlasInt, rank), tol, convert(BlasInt, info))
        check && chkfullrank(C)
    end
    return C
end

inv(C::Cholesky{<:Any,<:Diagonal}) = Diagonal(map(inv∘abs2, C.factors.diag))

cholcopy(A::Diagonal) = copymutable_oftype(A, choltype(A))
cholcopy(A::RealHermSymComplexHerm{<:Any,<:Diagonal}) = Diagonal(copy_similar(diag(A), choltype(A)))

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

uppertriangular(D::Diagonal) = D
lowertriangular(D::Diagonal) = D
