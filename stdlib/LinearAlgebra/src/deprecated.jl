# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: @deprecate, depwarn

# BEGIN 0.7 deprecations

@deprecate cond(F::LinearAlgebra.LU, p::Integer) cond(convert(AbstractArray, F), p)

# PR #22188
@deprecate cholfact!(A::StridedMatrix, uplo::Symbol, ::Type{Val{false}}) cholfact!(Hermitian(A, uplo), Val(false))
@deprecate cholfact!(A::StridedMatrix, uplo::Symbol) cholfact!(Hermitian(A, uplo))
@deprecate cholfact(A::StridedMatrix, uplo::Symbol, ::Type{Val{false}}) cholfact(Hermitian(A, uplo), Val(false))
@deprecate cholfact(A::StridedMatrix, uplo::Symbol) cholfact(Hermitian(A, uplo))
@deprecate cholfact!(A::StridedMatrix, uplo::Symbol, ::Type{Val{true}}; tol = 0.0) cholfact!(Hermitian(A, uplo), Val(true), tol = tol)
@deprecate cholfact(A::StridedMatrix, uplo::Symbol, ::Type{Val{true}}; tol = 0.0) cholfact(Hermitian(A, uplo), Val(true), tol = tol)

# PR #22245
@deprecate isposdef(A::AbstractMatrix, UL::Symbol) isposdef(Hermitian(A, UL))
@deprecate isposdef!(A::StridedMatrix, UL::Symbol) isposdef!(Hermitian(A, UL))

# bkfact
import .LinearAlgebra: bkfact, bkfact!
function bkfact(A::StridedMatrix, uplo::Symbol, symmetric::Bool = issymmetric(A), rook::Bool = false)
    depwarn(string("`bkfact` with uplo and symmetric arguments is deprecated, ",
        "use `bkfact($(symmetric ? "Symmetric(" : "Hermitian(")A, :$uplo))` instead."),
        :bkfact)
    return bkfact(symmetric ? Symmetric(A, uplo) : Hermitian(A, uplo), rook)
end
function bkfact!(A::StridedMatrix, uplo::Symbol, symmetric::Bool = issymmetric(A), rook::Bool = false)
    depwarn(string("`bkfact!` with uplo and symmetric arguments is deprecated, ",
        "use `bkfact!($(symmetric ? "Symmetric(" : "Hermitian(")A, :$uplo))` instead."),
        :bkfact!)
    return bkfact!(symmetric ? Symmetric(A, uplo) : Hermitian(A, uplo), rook)
end

@deprecate sqrtm(A::UpperTriangular{T},::Type{Val{realmatrix}}) where {T,realmatrix} sqrtm(A, Val(realmatrix))
@deprecate lufact(A::AbstractMatrix, ::Type{Val{false}}) lufact(A, Val(false))
@deprecate lufact(A::AbstractMatrix, ::Type{Val{true}}) lufact(A, Val(true))
@deprecate lufact!(A::AbstractMatrix, ::Type{Val{false}}) lufact!(A, Val(false))
@deprecate lufact!(A::AbstractMatrix, ::Type{Val{true}}) lufact!(A, Val(true))
@deprecate qrfact(A::AbstractMatrix, ::Type{Val{false}}) qrfact(A, Val(false))
@deprecate qrfact(A::AbstractMatrix, ::Type{Val{true}}) qrfact(A, Val(true))
@deprecate qrfact!(A::AbstractMatrix, ::Type{Val{false}}) qrfact!(A, Val(false))
@deprecate qrfact!(A::AbstractMatrix, ::Type{Val{true}}) qrfact!(A, Val(true))
@deprecate cholfact(A::AbstractMatrix, ::Type{Val{false}}) cholfact(A, Val(false))
@deprecate cholfact(A::AbstractMatrix, ::Type{Val{true}}; tol = 0.0) cholfact(A, Val(true); tol = tol)
@deprecate cholfact!(A::AbstractMatrix, ::Type{Val{false}}) cholfact!(A, Val(false))
@deprecate cholfact!(A::AbstractMatrix, ::Type{Val{true}}; tol = 0.0) cholfact!(A, Val(true); tol = tol)

# PR #22703
@deprecate Bidiagonal(dv::AbstractVector, ev::AbstractVector, isupper::Bool) Bidiagonal(dv, ev, ifelse(isupper, :U, :L))
@deprecate Bidiagonal(dv::AbstractVector, ev::AbstractVector, uplo::AbstractChar) Bidiagonal(dv, ev, ifelse(uplo == 'U', :U, :L))
@deprecate Bidiagonal(A::AbstractMatrix, isupper::Bool) Bidiagonal(A, ifelse(isupper, :U, :L))

# PR #22925
# also uncomment constructor tests in test/linalg/bidiag.jl
function Bidiagonal(dv::AbstractVector{T}, ev::AbstractVector{S}, uplo::Symbol) where {T,S}
    depwarn(string("`Bidiagonal(dv::AbstractVector{T}, ev::AbstractVector{S}, uplo::Symbol) where {T, S}`",
        " is deprecated, manually convert both vectors to the same type instead."), :Bidiagonal)
    R = promote_type(T, S)
    Bidiagonal(convert(Vector{R}, dv), convert(Vector{R}, ev), uplo)
end

# PR #23035
# also uncomment constructor tests in test/linalg/tridiag.jl
function SymTridiagonal(dv::AbstractVector{T}, ev::AbstractVector{S}) where {T,S}
    depwarn(string("`SymTridiagonal(dv::AbstractVector{T}, ev::AbstractVector{S}) ",
        "where {T, S}` is deprecated, convert both vectors to the same type instead."), :SymTridiagonal)
    R = promote_type(T, S)
    SymTridiagonal(convert(Vector{R}, dv), convert(Vector{R}, ev))
end

# PR #23154
# also uncomment constructor tests in test/linalg/tridiag.jl
function Tridiagonal(dl::AbstractVector{Tl}, d::AbstractVector{Td}, du::AbstractVector{Tu}) where {Tl,Td,Tu}
    depwarn(string("`Tridiagonal(dl::AbstractVector{Tl}, d::AbstractVector{Td}, du::AbstractVector{Tu}) ",
        "where {Tl, Td, Tu}` is deprecated, convert all vectors to the same type instead."), :Tridiagonal)
    Tridiagonal(map(v->convert(Vector{promote_type(Tl,Td,Tu)}, v), (dl, d, du))...)
end

# deprecate sqrtm in favor of sqrt
@deprecate sqrtm sqrt

# deprecate expm in favor of exp
@deprecate expm! exp!
@deprecate expm exp

# deprecate logm in favor of log
@deprecate logm log

# PR #23373
@deprecate diagm(A::BitMatrix) BitMatrix(Diagonal(vec(A)))

# PR 23341
@eval LinearAlgebra.LAPACK @deprecate laver() version() false

# deprecate zeros(D::Diagonal[, opts...])
import Base: zeros
function zeros(D::Diagonal)
    depwarn(string("`zeros(D::Diagonal)` is deprecated, use ",
        "`Diagonal(fill!(similar(D.diag), 0))` instead, or ",
        "`Diagonal(fill!(similar(D.diag), zero(eltype(D.diag))))` where necessary."), :zeros)
    return Diagonal(fill!(similar(D.diag), zero(eltype(D.diag))))
end
function zeros(D::Diagonal, ::Type{T}) where {T}
    depwarn(string("`zeros(D::Diagonal, ::Type{T}) where T` is deprecated, use ",
        "`Diagonal(fill!(similar(D.diag, T), 0))` instead, or ",
        "`Diagonal(fill!(similar(D.diag, T), zero(T)))` where necessary."), :zeros)
    return Diagonal(fill!(similar(D.diag, T), zero(T)))
end
function zeros(D::Diagonal, ::Type{T}, dims::Dims) where {T}
    depwarn(string("`zeros(D::Diagonal, ::Type{T}, dims::Dims) where T` is deprecated, ",
        "use `fill!(similar(D, T, dims), 0)` instead, or ",
        "`fill!(similar(D, T, dims), zero(T))` where necessary."), :zeros)
    return fill!(similar(D, T, dims), zero(T))
end
function zeros(D::Diagonal, ::Type{T}, dims::Integer...) where {T}
    depwarn(string("`zeros(D::Diagonal, ::Type{T}, dims::Integer...) where T` is deprecated, ",
        "use `fill!(similar(D, T, dims), 0)` instead, or ",
        "`fill!(similar(D, T, dims), zero(T))` where necessary."), :zeros)
    return fill!(similar(D, T, dims), zero(T))
end

## goodbeye, eye!
export eye
function eye(m::Integer)
    depwarn(string("`eye(m::Integer)` has been deprecated in favor of `I` and `Matrix` ",
        "constructors. For a direct replacement, consider `Matrix(1.0I, m, m)` or ",
        "`Matrix{Float64}(I, m, m)`. If `Float64` element type is not necessary, ",
        "consider the shorter `Matrix(I, m, m)` (with default `eltype(I)` `Bool`)."), :eye)
    return Matrix{Float64}(I, m, m)
end
function eye(::Type{T}, m::Integer) where T
    depwarn(string("`eye(T::Type, m::Integer)` has been deprecated in favor of `I` and ",
        "`Matrix` constructors. For a direct replacement, consider `Matrix{T}(I, m, m)`. If ",
        "`T` element type is not necessary, consider the shorter `Matrix(I, m, m)`",
        "(with default `eltype(I)` `Bool`)"), :eye)
    return Matrix{T}(I, m, m)
end
function eye(m::Integer, n::Integer)
    depwarn(string("`eye(m::Integer, n::Integer)` has been deprecated in favor of `I` and ",
        "`Matrix` constructors. For a direct replacement, consider `Matrix(1.0I, m, n)` ",
        "or `Matrix{Float64}(I, m, n)`. If `Float64` element type is not necessary, ",
        "consider the shorter `Matrix(I, m, n)` (with default `eltype(I)` `Bool`)."), :eye)
    return Matrix{Float64}(I, m, n)
end
function eye(::Type{T}, m::Integer, n::Integer) where T
    depwarn(string("`eye(T::Type, m::Integer, n::Integer)` has been deprecated in favor of ",
        "`I` and `Matrix` constructors. For a direct replacement, consider `Matrix{T}(I, m, n)`.",
        "If `T` element type is not necessary, consider the shorter `Matrix(I, m, n)` ",
        "(with default `eltype(I)` `Bool`)."), :eye)
    return Matrix{T}(I, m, n)
end
function eye(A::AbstractMatrix{T}) where T
    depwarn(string("`eye(A::AbstractMatrix{T}) where T` has been deprecated in favor of `I` and ",
        "`Matrix` constructors. For a direct replacement, consider `Matrix{eltype(A)}(I, size(A))`.",
        "If `eltype(A)` element type is not necessary, consider the shorter `Matrix(I, size(A))` ",
        "(with default `eltype(I)` `Bool`)."), :eye)
    return Matrix(one(T)I, size(A))
end
function eye(::Type{Diagonal{T}}, n::Int) where T
    depwarn(string("`eye(DT::Type{Diagonal{T}}, n::Int) where T` has been deprecated in favor of `I` ",
        "and `Diagonal` constructors. For a direct replacement, consider `Diagonal{T}(I, n)`. ",
        "If `T` element type is not necessary, consider the shorter `Diagonal(I, n)` ",
        "(with default `eltype(I)` `Bool`)."), :eye)
    return Diagonal{T}(I, n)
end

# PR #23816: deprecation of gradient
export gradient
function gradient(args...)
    Base.depwarn("`gradient` is deprecated and will be removed in the next release.", :gradient)
    return _gradient(args...)
end
_gradient(F::BitVector) = _gradient(Array(F))
_gradient(F::BitVector, h::Real) = _gradient(Array(F), h)
_gradient(F::Vector, h::BitVector) = _gradient(F, Array(h))
_gradient(F::BitVector, h::Vector) = _gradient(Array(F), h)
_gradient(F::BitVector, h::BitVector) = _gradient(Array(F), Array(h))
function _gradient(F::AbstractVector, h::Vector)
    n = length(F)
    T = typeof(oneunit(eltype(F))/oneunit(eltype(h)))
    g = similar(F, T)
    if n == 1
        g[1] = zero(T)
    elseif n > 1
        g[1] = (F[2] - F[1]) / (h[2] - h[1])
        g[n] = (F[n] - F[n-1]) / (h[end] - h[end-1])
        if n > 2
            h = h[3:n] - h[1:n-2]
            g[2:n-1] = (F[3:n] - F[1:n-2]) ./ h
        end
    end
    g
end
_gradient(F::AbstractVector) = _gradient(F, [1:length(F);])
_gradient(F::AbstractVector, h::Real) = _gradient(F, [h*(1:length(F));])

# deprecate odd fill! methods
@deprecate fill!(D::Diagonal, x)           LinearAlgebra.fillstored!(D, x)
@deprecate fill!(A::AbstractTriangular, x) LinearAlgebra.fillstored!(A, x)

# PR #25030
@deprecate fillslots! fillstored! false

function diagm(v::BitVector)
    depwarn(string("`diagm(v::BitVector)` is deprecated, use `diagm(0 => v)` or ",
        "`BitMatrix(Diagonal(v))` instead."), :diagm)
    return BitMatrix(Diagonal(v))
end
function diagm(v::AbstractVector)
    depwarn(string("`diagm(v::AbstractVector)` is deprecated, use `diagm(0 => v)` or ",
        "`Matrix(Diagonal(v))` instead."), :diagm)
    return Matrix(Diagonal(v))
end
@deprecate diagm(v::AbstractVector, k::Integer) diagm(k => v)
@deprecate diagm(x::Number) fill(x, 1, 1)

## deprecate full
import Base: full
# full for structured arrays
function full(A::Union{Diagonal,Bidiagonal,Tridiagonal,SymTridiagonal})
    mattypestr = isa(A, Diagonal)        ? "Diagonal"        :
                 isa(A, Bidiagonal)      ? "Bidiagonal"      :
                 isa(A, Tridiagonal)     ? "Tridiagonal"     :
                 isa(A, SymTridiagonal)  ? "SymTridiagonal"  :
                    error("should not be reachable!")
    depwarn(string(
        "`full(A::$(mattypestr))` (and `full` in general) has been deprecated. ",
        "To replace `full(A::$(mattypestr))`, consider `Matrix(A)` or, if that ",
        "option is too narrow, `Array(A)`. Also consider `SparseMatrixCSC(A)` ",
        "or, if that option is too narrow, `sparse(A)`."),  :full)
    return Matrix(A)
end

# full for factorizations
function full(F::Union{LinearAlgebra.LU,LinearAlgebra.LQ,LinearAlgebra.QR,LinearAlgebra.QRPivoted,LinearAlgebra.QRCompactWY,
                        LinearAlgebra.SVD,LinearAlgebra.LDLt,LinearAlgebra.Schur,LinearAlgebra.Eigen,LinearAlgebra.Hessenberg,
                        LinearAlgebra.Cholesky,LinearAlgebra.CholeskyPivoted})
    facttypestr = isa(F, LinearAlgebra.LU)               ? "LU"              :
                  isa(F, LinearAlgebra.LQ)               ? "LQ"              :
                  isa(F, LinearAlgebra.QR)               ? "QR"              :
                  isa(F, LinearAlgebra.QRPivoted)        ? "QRPivoted"       :
                  isa(F, LinearAlgebra.QRCompactWY)      ? "QRCompactWY"     :
                  isa(F, LinearAlgebra.SVD)              ? "SVD"             :
                  isa(F, LinearAlgebra.LDLt)             ? "LDLt"            :
                  isa(F, LinearAlgebra.Schur)            ? "Schur"           :
                  isa(F, LinearAlgebra.Eigen)            ? "Eigen"           :
                  isa(F, LinearAlgebra.Hessenberg)       ? "Hessenberg"      :
                  isa(F, LinearAlgebra.Cholesky)         ? "Cholesky"        :
                  isa(F, LinearAlgebra.CholeskyPivoted)  ? "CholeskyPivoted" :
                      error("should not be reachable!")
   depwarn(string(
       "`full(F::$(facttypestr))` (and `full` in general) has been deprecated. ",
       "To replace `full(F::$(facttypestr))`, consider `Matrix(F)`, `AbstractMatrix(F)` or, ",
       "if those options are too narrow, `Array(F)` or `AbstractArray(F)`."), :full)
   return AbstractMatrix(F)
end

# full for implicit orthogonal factors
function full(Q::LinearAlgebra.HessenbergQ)
    depwarn(string(
        "`full(Q::HessenbergQ)` (and `full` in general) has been deprecated. ",
        "To replace `full(Q::HessenbergQ)`, consider `Matrix(Q)` or, ",
        "if that option is too narrow, `Array(Q)`."), :full)
    return Matrix(Q)
end
function full(Q::LinearAlgebra.LQPackedQ; thin::Bool = true)
    depwarn(string(
        "`full(Q::LQPackedQ; thin::Bool = true)` (and `full` in general) ",
        "has been deprecated. To replace `full(Q::LQPackedQ, true)`, ",
        "consider `Matrix(Q)` or `Array(Q)`. To replace `full(Q::LQPackedQ, false)`, ",
        "consider `LinearAlgebra.mul!(Q, Matrix{eltype(Q)}(I, size(Q.factors, 2), size(Q.factors, 2)))`."), :full)
    return thin ? Array(Q) : LinearAlgebra.mul!(Q, Matrix{eltype(Q)}(I, size(Q.factors, 2), size(Q.factors, 2)))
end
function full(Q::Union{LinearAlgebra.QRPackedQ,LinearAlgebra.QRCompactWYQ}; thin::Bool = true)
    qtypestr = isa(Q, LinearAlgebra.QRPackedQ)    ? "QRPackedQ"    :
               isa(Q, LinearAlgebra.QRCompactWYQ) ? "QRCompactWYQ" :
                  error("should not be reachable!")
    depwarn(string(
        "`full(Q::$(qtypestr); thin::Bool = true)` (and `full` in general) ",
        "has been deprecated. To replace `full(Q::$(qtypestr), true)`, ",
        "consider `Matrix(Q)` or `Array(Q)`. To replace `full(Q::$(qtypestr), false)`, ",
        "consider `LinearAlgebra.mul!(Q, Matrix{eltype(Q)}(I, size(Q.factors, 1), size(Q.factors, 1)))`."), :full)
    return thin ? Array(Q) : LinearAlgebra.mul!(Q, Matrix{eltype(Q)}(I, size(Q.factors, 1), size(Q.factors, 1)))
end

# full for symmetric / hermitian / triangular wrappers
function full(A::Symmetric)
    depwarn(string(
        "`full(A::Symmetric)` (and `full` in general) has been deprecated. ",
        "To replace `full(A::Symmetric)`, as appropriate consider `Matrix(A)`, ",
        "`Array(A)`, `SparseMatrixCSC(A)`, `sparse(A)`, `copyto!(similar(parent(A)), A)`, ",
        "or `LinearAlgebra.copytri!(copy(parent(A)), A.uplo)`."), :full)
    return Matrix(A)
end
function full(A::Hermitian)
    depwarn(string(
        "`full(A::Hermitian)` (and `full` in general) has been deprecated. ",
        "To replace `full(A::Hermitian)`, as appropriate consider `Matrix(A)`, ",
        "`Array(A)`, `SparseMatrixCSC(A)`, `sparse(A)`, `copyto!(similar(parent(A)), A)`, ",
        "or `LinearAlgebra.copytri!(copy(parent(A)), A.uplo, true)`."), :full)
    return Matrix(A)
end
function full(A::Union{UpperTriangular,LowerTriangular})
    (tritypestr, tri!str) =
        isa(A, UpperTriangular) ? ("UpperTriangular", "triu!") :
        isa(A, LowerTriangular) ? ("LowerTriangular", "tril!") :
            error("should not be reachable!")
    depwarn(string(
        "`full(A::$(tritypestr))` (and `full` in general) has been deprecated. ",
        "To replace `full(A::$(tritypestr))`, as appropriate consider `Matrix(A)`, ",
        "`Array(A)`, `SparseMatrixCSC(A)`, `sparse(A)`, `copyto!(similar(parent(A)), A)`, ",
        "or `$(tri!str)(copy(parent(A)))`."), :full)
    return Matrix(A)
end
function full(A::Union{LinearAlgebra.UnitUpperTriangular,LinearAlgebra.UnitLowerTriangular})
    tritypestr = isa(A, LinearAlgebra.UnitUpperTriangular) ? "LinearAlgebra.UnitUpperTriangular" :
                 isa(A, LinearAlgebra.UnitLowerTriangular) ? "LinearAlgebra.UnitLowerTriangular" :
                     error("should not be reachable!")
    depwarn(string(
        "`full(A::$(tritypestr))` (and `full` in general) has been deprecated. ",
        "To replace `full(A::$(tritypestr))`, as appropriate consider `Matrix(A)`, ",
        "`Array(A)`, `SparseMatrixCSC(A)`, `sparse(A)`, or `copyto!(similar(parent(A)), A)`."), :full)
    return Matrix(A)
end

# TODO: after 0.7, remove thin keyword argument and associated logic from...
# (1) stdlib/LinearAlgebra/src/svd.jl
# (2) stdlib/LinearAlgebra/src/qr.jl
# (3) stdlib/LinearAlgebra/src/lq.jl


@deprecate chol!(x::Number, uplo) chol(x) false

### deprecations for lazier, less jazzy linalg transition in the next several blocks ###

# deprecate ConjArray
# TODO: between 0.7 and 1.0 remove
#       1) the type definitions in stdlib/LinearAlgebra/src/conjarray.jl
#       2) the include("conjarray.jl") from stdlib/LinearAlgebra/src/LinearAlgebra.jl
#       3) the file stdlib/LinearAlgebra/conjarray.jl itself
export ConjArray, ConjVector, ConjMatrix

function ConjArray(a::AbstractArray{T,N}) where {T,N}
    Base.depwarn(_ConjArray_depstring(), :ConjArray)
    return ConjArray{conj_type(T),N,typeof(a)}(a)
end
function ConjVector(v::AbstractVector{T}) where {T}
    Base.depwarn(_ConjArray_depstring(), :ConjArray)
    return ConjArray{conj_type(T),1,typeof(v)}(v)
end
function ConjMatrix(m::AbstractMatrix{T}) where {T}
    Base.depwarn(_ConjArray_depstring(), :ConjArray)
    return ConjArray{conj_type(T),2,typeof(m)}(m)
end

_ConjArray_depstring() = string("`ConjRowVector` and `RowVector` have been deprecated in favor ",
        "of `Adjoint` and `Transpose`, and, as part of the implementation of `ConjRowVector`",
        "/`RowVector`, `ConjArray`s have been deprecated as well. Please see 0.7's NEWS.md ",
        "for a more detailed explanation of the associated changes.")

# This type can cause the element type to change under conjugation - e.g. an array of complex arrays.
@inline conj_type(x) = conj_type(typeof(x))
@inline conj_type(::Type{T}) where {T} = promote_op(conj, T)

@inline parent(c::ConjArray) = c.parent
@inline parent_type(c::ConjArray) = parent_type(typeof(c))
@inline parent_type(::Type{ConjArray{T,N,A}}) where {T,N,A} = A

@inline size(a::ConjArray) = size(a.parent)
IndexStyle(::CA) where {CA<:ConjArray} = IndexStyle(parent_type(CA))
IndexStyle(::Type{CA}) where {CA<:ConjArray} = IndexStyle(parent_type(CA))

@propagate_inbounds getindex(a::ConjArray{T,N}, i::Int) where {T,N} = conj(getindex(a.parent, i))
@propagate_inbounds getindex(a::ConjArray{T,N}, i::Vararg{Int,N}) where {T,N} = conj(getindex(a.parent, i...))
@propagate_inbounds setindex!(a::ConjArray{T,N}, v, i::Int) where {T,N} = setindex!(a.parent, conj(v), i)
@propagate_inbounds setindex!(a::ConjArray{T,N}, v, i::Vararg{Int,N}) where {T,N} = setindex!(a.parent, conj(v), i...)

@inline similar(a::ConjArray, ::Type{T}, dims::Dims{N}) where {T,N} = similar(parent(a), T, dims)

# Currently, this is default behavior for RowVector only
@inline conj(a::ConjArray) = parent(a)

# Helper functions, currently used by RowVector
@inline _conj(a::AbstractArray) = ConjArray(a)
@inline _conj(a::AbstractArray{T}) where {T<:Real} = a
@inline _conj(a::ConjArray) = parent(a)
@inline _conj(a::ConjArray{T}) where {T<:Real} = parent(a)

# deprecate ConjRowVector/RowVector
# TODO: between 0.7 and 1.0 remove
#       1) the type definitions in stdlib/LinearAlgebra/src/rowvector.jl
#       2) the include("rowvector.jl") from stdlib/LinearAlgebra/src/LinearAlgebra.jl
#       3) the file stdlib/LinearAlgebra/src/rowvector.jl itself
#       4) the RowVectors in the Unions in stdlib/SparseArrays/src/sparsevector.jl around lines 995, 1010, 1011, and 1012
export RowVector

_RowVector_depstring() = string("`ConjRowVector` and `RowVector` have been deprecated in favor ",
        "of `Adjoint` and `Transpose`. Please see 0.7's NEWS.md for a more detailed explanation ",
        "of the associated changes.")

@inline check_types(::Type{T1}, ::AbstractVector{T2}) where {T1,T2} = check_types(T1, T2)
@pure check_types(::Type{T1}, ::Type{T2}) where {T1,T2} = T1 === transpose_type(T2) ? nothing :
    error("Element type mismatch. Tried to create a `RowVector{$T1}` from an `AbstractVector{$T2}`")

# The element type may be transformed as transpose is recursive
@inline transpose_type(::Type{T}) where {T} = promote_op(transpose, T)

# Constructors that take a vector
function RowVector(vec::AbstractVector{T}) where {T}
    Base.depwarn(_RowVector_depstring(), :RowVector)
    return RowVector{transpose_type(T),typeof(vec)}(vec)
end
function RowVector{T}(vec::AbstractVector{T}) where {T}
    Base.depwarn(_RowVector_depstring(), :RowVector)
    return RowVector{T,typeof(vec)}(vec)
end

# Constructors that take a size and default to Array
function RowVector{T}(::UndefInitializer, n::Int) where {T}
    Base.depwarn(_RowVector_depstring(), :RowVector)
    return RowVector{T}(Vector{transpose_type(T)}(undef, n))
end
function RowVector{T}(::UndefInitializer, n1::Int, n2::Int) where {T}
    Base.depwarn(_RowVector_depstring(), :RowVector)
    return n1 == 1 ? RowVector{T}(Vector{transpose_type(T)}(undef, n2)) :
        error("RowVector expects 1×N size, got ($n1,$n2)")
end
function RowVector{T}(::UndefInitializer, n::Tuple{Int}) where {T}
    Base.depwarn(_RowVector_depstring(), :RowVector)
    return RowVector{T}(Vector{transpose_type(T)}(undef, n[1]))
end
function RowVector{T}(::UndefInitializer, n::Tuple{Int,Int}) where {T}
    Base.depwarn(_RowVector_depstring(), :RowVector)
    return n[1] == 1 ? RowVector{T}(Vector{transpose_type(T)}(undef, n[2])) :
        error("RowVector expects 1×N size, got $n")
end

# Conversion of underlying storage
convert(::Type{RowVector{T,V}}, rowvec::RowVector) where {T,V<:AbstractVector} =
    RowVector{T,V}(convert(V,rowvec.vec))

# similar tries to maintain the RowVector wrapper and the parent type
@inline similar(rowvec::RowVector) = RowVector(similar(parent(rowvec)))
@inline similar(rowvec::RowVector, ::Type{T}) where {T} = RowVector(similar(parent(rowvec), transpose_type(T)))

# Resizing similar currently loses its RowVector property.
@inline similar(rowvec::RowVector, ::Type{T}, dims::Dims{N}) where {T,N} = similar(parent(rowvec), T, dims)

# Basic methods

# replaced in the Adjoint/Transpose transition
# """
#     transpose(v::AbstractVector)
#
# The transposition operator (`.'`).
#
# # Examples
# ```jldoctest
# julia> v = [1,2,3]
# 3-element Array{Int64,1}:
#  1
#  2
#  3
#
# julia> transpose(v)
# 1×3 RowVector{Int64,Array{Int64,1}}:
#  1  2  3
# ```
# """
# @inline transpose(vec::AbstractVector) = RowVector(vec)
# @inline adjoint(vec::AbstractVector) = RowVector(_conj(vec))

# methods necessary to preserve RowVector's behavior through the Adjoint/Transpose transition
rvadjoint(v::AbstractVector) = RowVector(_conj(v))
rvtranspose(v::AbstractVector) = RowVector(v)
rvadjoint(v::RowVector) = conj(v.vec)
rvadjoint(v::RowVector{<:Real}) = v.vec
rvtranspose(v::RowVector) = v.vec
rvtranspose(v::ConjRowVector) = copy(v.vec)
rvadjoint(x) = adjoint(x)
rvtranspose(x) = transpose(x)

@inline transpose(rowvec::RowVector) = rowvec.vec
@inline transpose(rowvec::ConjRowVector) = copy(rowvec.vec) # remove the ConjArray wrapper from any raw vector
@inline adjoint(rowvec::RowVector) = conj(rowvec.vec)
@inline adjoint(rowvec::RowVector{<:Real}) = rowvec.vec

parent(rowvec::RowVector) = rowvec.vec
vec(rowvec::RowVector) = rowvec.vec

@inline conj(rowvec::RowVector) = RowVector(_conj(rowvec.vec))
@inline conj(rowvec::RowVector{<:Real}) = rowvec

# AbstractArray interface
@inline length(rowvec::RowVector) =  length(rowvec.vec)
@inline size(rowvec::RowVector) = (1, length(rowvec.vec))
@inline size(rowvec::RowVector, d) = ifelse(d==2, length(rowvec.vec), 1)
@inline axes(rowvec::RowVector) = (Base.OneTo(1), axes(rowvec.vec)[1])
@inline axes(rowvec::RowVector, d) = ifelse(d == 2, axes(rowvec.vec)[1], Base.OneTo(1))
IndexStyle(::RowVector) = IndexLinear()
IndexStyle(::Type{<:RowVector}) = IndexLinear()

@propagate_inbounds getindex(rowvec::RowVector, i::Int) = rvtranspose(rowvec.vec[i])
@propagate_inbounds setindex!(rowvec::RowVector, v, i::Int) = (setindex!(rowvec.vec, rvtranspose(v), i); rowvec)

# Keep a RowVector where appropriate
@propagate_inbounds getindex(rowvec::RowVector, ::Colon, i::Int) = rvtranspose.(rowvec.vec[i:i])
@propagate_inbounds getindex(rowvec::RowVector, ::Colon, inds::AbstractArray{Int}) = RowVector(rowvec.vec[inds])
@propagate_inbounds getindex(rowvec::RowVector, ::Colon, ::Colon) = RowVector(rowvec.vec[:])

# helper function for below
@inline to_vec(rowvec::RowVector) = map(rvtranspose, rvtranspose(rowvec))
@inline to_vec(x::Number) = x
@inline to_vecs(rowvecs...) = (map(to_vec, rowvecs)...,)

# map: Preserve the RowVector by un-wrapping and re-wrapping, but note that `f`
# expects to operate within the transposed domain, so to_vec transposes the elements
@inline map(f, rowvecs::RowVector...) = RowVector(map(rvtranspose∘f, to_vecs(rowvecs...)...))

# broacast (other combinations default to higher-dimensional array)
@inline broadcast(f, rowvecs::Union{Number,RowVector}...) =
    RowVector(broadcast(transpose∘f, to_vecs(rowvecs...)...))

# Horizontal concatenation #

@inline hcat(X::RowVector...) = rvtranspose(vcat(map(rvtranspose, X)...))
@inline hcat(X::Union{RowVector,Number}...) = rvtranspose(vcat(map(rvtranspose, X)...))

@inline typed_hcat(::Type{T}, X::RowVector...) where {T} =
    rvtranspose(typed_vcat(T, map(rvtranspose, X)...))
@inline typed_hcat(::Type{T}, X::Union{RowVector,Number}...) where {T} =
    rvtranspose(typed_vcat(T, map(rvtranspose, X)...))

# Multiplication #

# inner product -> dot product specializations
@inline *(rowvec::RowVector{T}, vec::AbstractVector{T}) where {T<:Real} = dot(parent(rowvec), vec)
@inline *(rowvec::ConjRowVector{T}, vec::AbstractVector{T}) where {T<:Real} = dot(rvadjoint(rowvec), vec)
@inline *(rowvec::ConjRowVector, vec::AbstractVector) = dot(rvadjoint(rowvec), vec)

# Generic behavior
@inline function *(rowvec::RowVector, vec::AbstractVector)
    if length(rowvec) != length(vec)
        throw(DimensionMismatch("A has dimensions $(size(rowvec)) but B has dimensions $(size(vec))"))
    end
    sum(@inbounds(return rowvec[i]*vec[i]) for i = 1:length(vec))
end
@inline *(rowvec::RowVector, mat::AbstractMatrix) = rvtranspose(transpose(mat) * rvtranspose(rowvec))
*(::RowVector, ::RowVector) = throw(DimensionMismatch("Cannot multiply two transposed vectors"))
@inline *(vec::AbstractVector, rowvec::RowVector) = vec .* rowvec
*(vec::AbstractVector, rowvec::AbstractVector) = throw(DimensionMismatch("Cannot multiply two vectors"))

# Transposed forms
*(::RowVector, ::Transpose{<:Any,<:AbstractVector}) =
    throw(DimensionMismatch("Cannot multiply two transposed vectors"))
*(rowvec::RowVector, transmat::Transpose{<:Any,<:AbstractMatrix}) =
    (mat = transmat.parent; rvtranspose(mat * rvtranspose(rowvec)))
*(rowvec1::RowVector, transrowvec2::Transpose{<:Any,<:RowVector}) =
    (rowvec2 = transrowvec2.parent; rowvec1*rvtranspose(rowvec2))
*(::AbstractVector, ::Transpose{<:Any,<:RowVector}) =
    throw(DimensionMismatch("Cannot multiply two vectors"))
*(mat::AbstractMatrix, transrowvec::Transpose{<:Any,<:RowVector}) =
    (rowvec = transrowvec.parent; mat * rvtranspose(rowvec))

*(transrowvec::Transpose{<:Any,<:RowVector}, transvec::Transpose{<:Any,<:AbstractVector}) =
    rvtranspose(transrowvec.parent) * transpose(transvec.parent)
*(transrowvec1::Transpose{<:Any,<:RowVector}, transrowvec2::Transpose{<:Any,<:RowVector}) =
    throw(DimensionMismatch("Cannot multiply two vectors"))
*(transvec::Transpose{<:Any,<:AbstractVector}, transrowvec::Transpose{<:Any,<:RowVector}) =
    transpose(transvec.parent)*rvtranspose(transrowvec.parent)
*(transmat::Transpose{<:Any,<:AbstractMatrix}, transrowvec::Transpose{<:Any,<:RowVector}) =
    transmat * rvtranspose(transrowvec.parent)

*(::Transpose{<:Any,<:RowVector}, ::AbstractVector) =
    throw(DimensionMismatch("Cannot multiply two vectors"))
*(transrowvec1::Transpose{<:Any,<:RowVector}, rowvec2::RowVector) =
    rvtranspose(transrowvec1.parent) * rowvec2
*(transvec::Transpose{<:Any,<:AbstractVector}, rowvec::RowVector) =
    throw(DimensionMismatch("Cannot multiply two transposed vectors"))

# Conjugated forms
*(::RowVector, ::Adjoint{<:Any,<:AbstractVector}) =
    throw(DimensionMismatch("Cannot multiply two transposed vectors"))
*(rowvec::RowVector, adjmat::Adjoint{<:Any,<:AbstractMatrix}) =
    rvadjoint(adjmat.parent * rvadjoint(rowvec))
*(rowvec1::RowVector, adjrowvec2::Adjoint{<:Any,<:RowVector}) =
    rowvec1 * rvadjoint(adjrowvec2.parent)
*(vec::AbstractVector, adjrowvec::Adjoint{<:Any,<:RowVector}) =
    throw(DimensionMismatch("Cannot multiply two vectors"))
*(mat::AbstractMatrix, adjrowvec::Adjoint{<:Any,<:RowVector}) =
    mat * rvadjoint(adjrowvec.parent)

*(adjrowvec::Adjoint{<:Any,<:RowVector}, adjvec::Adjoint{<:Any,<:AbstractVector}) =
    rvadjoint(adjrowvec.parent) * adjoint(adjvec.parent)
*(adjrowvec1::Adjoint{<:Any,<:RowVector}, adjrowvec2::Adjoint{<:Any,<:RowVector}) =
    throw(DimensionMismatch("Cannot multiply two vectors"))
*(adjvec::Adjoint{<:Any,<:AbstractVector}, adjrowvec::Adjoint{<:Any,<:RowVector}) =
    adjoint(adjvec.parent)*rvadjoint(adjrowvec.parent)
*(adjmat::Adjoint{<:Any,<:AbstractMatrix}, adjrowvec::Adjoint{<:Any,<:RowVector}) =
    adjoint(adjmat.parent) * rvadjoint(adjrowvec.parent)

*(::Adjoint{<:Any,<:RowVector}, ::AbstractVector) = throw(DimensionMismatch("Cannot multiply two vectors"))
*(adjrowvec1::Adjoint{<:Any,<:RowVector}, rowvec2::RowVector) = rvadjoint(adjrowvec1.parent) * rowvec2
*(adjvec::Adjoint{<:Any,<:AbstractVector}, rowvec::RowVector) = throw(DimensionMismatch("Cannot multiply two transposed vectors"))

# Pseudo-inverse
pinv(v::RowVector, tol::Real=0) = rvadjoint(pinv(rvadjoint(v), tol))

# Left Division #

\(rowvec1::RowVector, rowvec2::RowVector) = pinv(rowvec1) * rowvec2
\(mat::AbstractMatrix, rowvec::RowVector) = throw(DimensionMismatch("Cannot left-divide transposed vector by matrix"))
\(transmat::Transpose{<:Any,<:AbstractMatrix}, rowvec::RowVector) =
    throw(DimensionMismatch("Cannot left-divide transposed vector by matrix"))
\(adjmat::Adjoint{<:Any,<:AbstractMatrix}, rowvec::RowVector) =
    throw(DimensionMismatch("Cannot left-divide transposed vector by matrix"))

# Right Division #

@inline /(rowvec::RowVector, mat::AbstractMatrix) = rvtranspose(transpose(mat) \ rvtranspose(rowvec))
/(rowvec::RowVector, transmat::Transpose{<:Any,<:AbstractMatrix}) = rvtranspose(transmat.parent \ rvtranspose(rowvec))
/(rowvec::RowVector, adjmat::Adjoint{<:Any,<:AbstractMatrix}) = rvadjoint(adjmat.parent \ rvadjoint(rowvec))


# definitions necessary for test/linalg/dense.jl to pass
# should be cleaned up / revised as necessary in the future
/(A::Number, B::Adjoint{<:Any,<:RowVector}) = /(A, rvadjoint(B.parent))
/(A::Matrix, B::RowVector) = rvadjoint(rvadjoint(B) \ adjoint(A))


# dismabiguation methods
*(A::Adjoint{<:Any,<:AbstractVector}, B::Transpose{<:Any,<:RowVector}) = adjoint(A.parent) * B
*(A::Adjoint{<:Any,<:AbstractMatrix}, B::Transpose{<:Any,<:RowVector}) = A * rvtranspose(B.parent)
*(A::Transpose{<:Any,<:AbstractVector}, B::Adjoint{<:Any,<:RowVector}) = transpose(A.parent) * B
*(A::Transpose{<:Any,<:AbstractMatrix}, B::Adjoint{<:Any,<:RowVector}) = A * rvadjoint(B.parent)

# deprecate RowVector{T}(shape...) constructors to RowVector{T}(undef, shape...) equivalents
@deprecate RowVector{T}(n::Int) where {T}               RowVector{T}(undef, n)
@deprecate RowVector{T}(n1::Int, n2::Int) where {T}     RowVector{T}(undef, n1, n2)
@deprecate RowVector{T}(n::Tuple{Int}) where {T}        RowVector{T}(undef, n)
@deprecate RowVector{T}(n::Tuple{Int,Int}) where {T}    RowVector{T}(undef, n)

# operations formerly exported from and imported/extended by LinearAlgebra
import Base: A_mul_Bt, At_ldiv_Bt, A_rdiv_Bc, At_ldiv_B, Ac_mul_Bc, A_mul_Bc, Ac_mul_B,
    Ac_ldiv_B, Ac_ldiv_Bc, At_mul_Bt, A_rdiv_Bt, At_mul_B

# most of these explicit exports are of course obviated by the deprecations below
# but life is easier just leaving them for now...
export A_ldiv_B!,
    A_ldiv_Bc,
    A_ldiv_Bt,
    A_mul_B!,
    A_mul_Bc,
    A_mul_Bc!,
    A_mul_Bt,
    A_mul_Bt!,
    A_rdiv_Bc,
    A_rdiv_Bt,
    Ac_ldiv_B,
    Ac_ldiv_Bc,
    Ac_ldiv_B!,
    Ac_mul_B,
    Ac_mul_B!,
    Ac_mul_Bc,
    Ac_mul_Bc!,
    Ac_rdiv_B,
    Ac_rdiv_Bc,
    At_ldiv_B,
    At_ldiv_Bt,
    At_ldiv_B!,
    At_mul_B,
    At_mul_B!,
    At_mul_Bt,
    At_mul_Bt!,
    At_rdiv_B,
    At_rdiv_Bt

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/bidiag.jl, to deprecate
@deprecate A_mul_B!(C::AbstractMatrix, A::SymTridiagonal, B::BiTriSym)  mul!(C, A, B)
@deprecate A_mul_B!(C::AbstractMatrix, A::BiTri, B::BiTriSym)           mul!(C, A, B)
@deprecate A_mul_B!(C::AbstractMatrix, A::BiTriSym, B::BiTriSym)        mul!(C, A, B)
@deprecate A_mul_B!(C::AbstractMatrix, A::AbstractTriangular, B::BiTriSym)  mul!(C, A, B)
@deprecate A_mul_B!(C::AbstractMatrix, A::AbstractMatrix, B::BiTriSym)      mul!(C, A, B)
@deprecate A_mul_B!(C::AbstractMatrix, A::Diagonal, B::BiTriSym)            mul!(C, A, B)
@deprecate A_mul_B!(C::AbstractVector, A::BiTri, B::AbstractVector)         mul!(C, A, B)
@deprecate A_mul_B!(C::AbstractMatrix, A::BiTri, B::AbstractVecOrMat)       mul!(C, A, B)
@deprecate A_mul_B!(C::AbstractVecOrMat, A::BiTri, B::AbstractVecOrMat)     mul!(C, A, B)
@deprecate Ac_ldiv_B(A::Bidiagonal, v::RowVector)   (\)(adjoint(A), v)
@deprecate At_ldiv_B(A::Bidiagonal, v::RowVector)   (\)(transpose(A), v)
@deprecate Ac_ldiv_B(A::Bidiagonal{<:Number}, v::RowVector{<:Number})   (\)(adjoint(A), v)
@deprecate At_ldiv_B(A::Bidiagonal{<:Number}, v::RowVector{<:Number})   (\)(transpose(A), v)
@deprecate Ac_mul_B(A::Bidiagonal{T}, B::AbstractVector{T}) where {T}   (*)(adjoint(A), B)
@deprecate A_mul_Bc(A::Bidiagonal{T}, B::AbstractVector{T}) where {T}   (*)(A, adjoint(B))
@deprecate A_rdiv_Bc(A::Bidiagonal{T}, B::AbstractVector{T}) where {T}  (/)(A, adjoint(B))
@deprecate A_ldiv_B!(A::Union{Bidiagonal, AbstractTriangular}, b::AbstractVector)   ldiv!(A, b)
@deprecate At_ldiv_B!(A::Bidiagonal, b::AbstractVector)     ldiv!(transpose(A), b)
@deprecate Ac_ldiv_B!(A::Bidiagonal, b::AbstractVector)     ldiv!(adjoint(A), b)
@deprecate A_ldiv_B!(A::Union{Bidiagonal,AbstractTriangular}, B::AbstractMatrix)    ldiv!(A, B)
@deprecate Ac_ldiv_B!(A::Union{Bidiagonal,AbstractTriangular}, B::AbstractMatrix)   ldiv!(adjoint(A), B)
@deprecate At_ldiv_B!(A::Union{Bidiagonal,AbstractTriangular}, B::AbstractMatrix)   ldiv!(transpose(A), B)
@deprecate At_ldiv_B(A::Bidiagonal, B::AbstractVecOrMat)    (\)(transpose(A), B)
@deprecate Ac_ldiv_B(A::Bidiagonal, B::AbstractVecOrMat)    ldiv!(adjoint(A), B)
@deprecate Ac_ldiv_B(A::Bidiagonal{TA}, B::AbstractVecOrMat{TB}) where {TA<:Number,TB<:Number}  (\)(adjoint(A), B)
@deprecate At_ldiv_B(A::Bidiagonal{TA}, B::AbstractVecOrMat{TB}) where {TA<:Number,TB<:Number}  (\)(transpose(A), B)

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/tridiag.jl, to deprecate
@deprecate A_mul_B!(C::StridedVecOrMat, S::SymTridiagonal, B::StridedVecOrMat)  mul!(C, S, B)

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/diagonal.jl, to deprecate
@deprecate A_mul_B!(A::Union{LowerTriangular,UpperTriangular}, D::Diagonal) rmul!(A, D)
@deprecate A_mul_B!(A::UnitLowerTriangular, D::Diagonal)    rmul!(A, D)
@deprecate A_mul_B!(A::UnitUpperTriangular, D::Diagonal)    rmul!(A, D)
@deprecate A_mul_B!(D::Diagonal, B::UnitLowerTriangular)    lmul!(D, B)
@deprecate A_mul_B!(D::Diagonal, B::UnitUpperTriangular)    lmul!(D, B)
@deprecate Ac_mul_B(D::Diagonal, B::Diagonal)           (*)(adjoint(D), B)
@deprecate Ac_mul_B(A::AbstractTriangular, D::Diagonal) (*)(adjoint(A), D)
@deprecate Ac_mul_B(A::AbstractMatrix, D::Diagonal)     (*)(adjoint(A), D)
@deprecate At_mul_B(D::Diagonal, B::Diagonal)           (*)(transpose(D), B)
@deprecate At_mul_B(A::AbstractTriangular, D::Diagonal) (*)(transpose(A), D)
@deprecate At_mul_B(A::AbstractMatrix, D::Diagonal)     (*)(transpose(A), D)
@deprecate A_mul_Bc(D::Diagonal, B::Diagonal)           (*)(D, adjoint(B))
@deprecate A_mul_Bc(D::Diagonal, B::AbstractTriangular) (*)(D, adjoint(B))
@deprecate A_mul_Bc(D::Diagonal, Q::Union{QRCompactWYQ,QRPackedQ})  (*)(D, adjoint(Q))
@deprecate A_mul_Bc(D::Diagonal, A::AbstractMatrix)         (*)(D, adjoint(A))
@deprecate A_mul_Bt(D::Diagonal, B::Diagonal)               (*)(D, transpose(B))
@deprecate A_mul_Bt(D::Diagonal, B::AbstractTriangular)     (*)(D, transpose(B))
@deprecate A_mul_Bt(D::Diagonal, A::AbstractMatrix) (*)(D, transpose(A))
@deprecate Ac_mul_Bc(D::Diagonal, B::Diagonal)      (*)(adjoint(D), adjoint(B))
@deprecate At_mul_Bt(D::Diagonal, B::Diagonal)      (*)(transpose(D), transpose(B))
function A_mul_B!(A::Diagonal,B::Diagonal)
    depwarn("`A_mul_B!(A::Diagonal,B::Diagonal)` should be replaced with `rmul!(A, B)` or `lmul!(A, B)`.", :A_mul_B!)
    throw(MethodError(A_mul_B!, (A, B)))
end
@deprecate At_mul_B!(A::Diagonal,B::Diagonal)       lmul!(transpose(A), B)
@deprecate Ac_mul_B!(A::Diagonal,B::Diagonal)       lmul!(adjoint(A), B)
@deprecate A_mul_B!(A::QRPackedQ, D::Diagonal)      rmul!(A, D)
@deprecate A_mul_B!(A::Diagonal,B::AbstractMatrix)      lmul!(A, B)
@deprecate At_mul_B!(A::Diagonal,B::AbstractMatrix)     lmul!(transpose(A), B)
@deprecate Ac_mul_B!(A::Diagonal,B::AbstractMatrix)     lmul!(adjoint(A), B)
@deprecate A_mul_B!(A::AbstractMatrix,B::Diagonal)      rmul!(A, B)
@deprecate A_mul_Bt!(A::AbstractMatrix,B::Diagonal)     rmul!(A, transpose(B))
@deprecate A_mul_Bc!(A::AbstractMatrix,B::Diagonal)     rmul!(A, adjoint(B))
@deprecate A_mul_B!(out::AbstractVector, A::Diagonal, in::AbstractVector)       mul!(out, A, in)
@deprecate Ac_mul_B!(out::AbstractVector, A::Diagonal, in::AbstractVector)      mul!(out, adjoint(A), in)
@deprecate At_mul_B!(out::AbstractVector, A::Diagonal, in::AbstractVector)      mul!(out, transpose(A), in)
@deprecate A_mul_B!(out::AbstractMatrix, A::Diagonal, in::AbstractMatrix)       mul!(out, A, in)
@deprecate Ac_mul_B!(out::AbstractMatrix, A::Diagonal, in::AbstractMatrix)      mul!(out, adjoint(A), in)
@deprecate At_mul_B!(out::AbstractMatrix, A::Diagonal, in::AbstractMatrix)      mul!(out, transpose(A), in)
@deprecate A_mul_Bt(A::Diagonal, B::RealHermSymComplexSym)      (*)(A, transpose(B))
@deprecate At_mul_B(A::RealHermSymComplexSym, B::Diagonal)      (*)(transpose(A), B)
@deprecate A_mul_Bc(A::Diagonal, B::RealHermSymComplexHerm)     (*)(A, adjoint(B))
@deprecate Ac_mul_B(A::RealHermSymComplexHerm, B::Diagonal)     (*)(adjoint(A), B)
@deprecate A_ldiv_B!(D::Diagonal{T}, v::AbstractVector{T}) where {T}        ldiv!(D, v)
@deprecate A_ldiv_B!(D::Diagonal{T}, V::AbstractMatrix{T}) where {T}        ldiv!(D, V)
@deprecate Ac_ldiv_B!(D::Diagonal{T}, B::AbstractVecOrMat{T}) where {T}     ldiv!(adjoint(D), B)
@deprecate At_ldiv_B!(D::Diagonal{T}, B::AbstractVecOrMat{T}) where {T}     ldiv!(transpose(D), B)
@deprecate A_rdiv_B!(A::AbstractMatrix{T}, D::Diagonal{T}) where {T}    rdiv!(A, D)
@deprecate A_rdiv_Bc!(A::AbstractMatrix{T}, D::Diagonal{T}) where {T}   rdiv!(A, adjoint(D))
@deprecate A_rdiv_Bt!(A::AbstractMatrix{T}, D::Diagonal{T}) where {T}   rdiv!(A, transpose(D))
@deprecate Ac_ldiv_B(F::Factorization, D::Diagonal)     (\)(adjoint(F), D)
@deprecate A_mul_Bt(D::Diagonal, rowvec::RowVector)     (*)(D, transpose(rowvec))
@deprecate A_mul_Bc(D::Diagonal, rowvec::RowVector)     (*)(D, adjoint(rowvec))
@deprecate A_ldiv_B!(D::Diagonal, B::StridedVecOrMat)   ldiv!(D, B)

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/special.jl, to deprecate
@deprecate A_mul_Bc!(A::AbstractTriangular, B::Union{QRCompactWYQ,QRPackedQ})   rmul!(A, adjoint(B))
@deprecate A_mul_Bc(A::AbstractTriangular, B::Union{QRCompactWYQ,QRPackedQ})    (*)(A, adjoint(B))

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/bunchkaufman.jl, to deprecate
@deprecate A_ldiv_B!(B::BunchKaufman{T}, R::StridedVecOrMat{T}) where {T<:BlasReal}     ldiv!(B, R)
@deprecate A_ldiv_B!(B::BunchKaufman{T}, R::StridedVecOrMat{T}) where {T<:BlasComplex}  ldiv!(B, R)
@deprecate A_ldiv_B!(B::BunchKaufman{T}, R::StridedVecOrMat{S}) where {T,S}             ldiv!(B, R)

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/cholesky.jl, to deprecate
@deprecate A_ldiv_B!(C::Cholesky{T,<:AbstractMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat}   ldiv!(C, B)
@deprecate A_ldiv_B!(C::Cholesky{<:Any,<:AbstractMatrix}, B::StridedVecOrMat)           ldiv!(C, B)
@deprecate A_ldiv_B!(C::CholeskyPivoted{T}, B::StridedVector{T}) where {T<:BlasFloat}   ldiv!(C, B)
@deprecate A_ldiv_B!(C::CholeskyPivoted{T}, B::StridedMatrix{T}) where {T<:BlasFloat}   ldiv!(C, B)
@deprecate A_ldiv_B!(C::CholeskyPivoted, B::StridedVector)      ldiv!(C, B)
@deprecate A_ldiv_B!(C::CholeskyPivoted, B::StridedMatrix)      ldiv!(C, B)

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/factorization.jl, to deprecate
@deprecate Ac_ldiv_B(F::Factorization, B::AbstractVecOrMat)     (\)(adjoint(F), B)
@deprecate A_ldiv_B!(Y::AbstractVecOrMat, A::Factorization, B::AbstractVecOrMat)    ldiv!(Y, A, B)
@deprecate Ac_ldiv_B!(Y::AbstractVecOrMat, A::Factorization, B::AbstractVecOrMat)   ldiv!(Y, adjoint(A), B)
@deprecate At_ldiv_B!(Y::AbstractVecOrMat, A::Factorization, B::AbstractVecOrMat)   ldiv!(Y, transpose(A), B)
@deprecate At_ldiv_B(F::Factorization{<:Real}, B::AbstractVecOrMat)     (\)(transpose(F), B)
@deprecate At_ldiv_B(F::Factorization, B)   (\)(transpose(F), B)

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/hessenberg.jl, to deprecate
@deprecate A_mul_B!(Q::HessenbergQ{T}, X::StridedVecOrMat{T}) where {T<:BlasFloat}  lmul!(Q, X)
@deprecate A_mul_B!(X::StridedMatrix{T}, Q::HessenbergQ{T}) where {T<:BlasFloat}    rmul!(X, Q)
@deprecate Ac_mul_B!(Q::HessenbergQ{T}, X::StridedVecOrMat{T}) where {T<:BlasFloat} lmul!(adjoint(Q), X)
@deprecate A_mul_Bc!(X::StridedMatrix{T}, Q::HessenbergQ{T}) where {T<:BlasFloat}   rmul!(X, adjoint(Q))
@deprecate Ac_mul_B(Q::HessenbergQ{T}, X::StridedVecOrMat{S}) where {T,S}   (*)(adjoint(Q), X)
@deprecate A_mul_Bc(X::StridedVecOrMat{S}, Q::HessenbergQ{T}) where {T,S}   (*)(X, adjoint(Q))

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/ldlt.jl, to deprecate
@deprecate A_ldiv_B!(S::LDLt{T,M}, B::AbstractVecOrMat{T}) where {T,M<:SymTridiagonal{T}}   ldiv!(S, B)

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/svd.jl, to deprecate
@deprecate A_ldiv_B!(A::SVD{T}, B::StridedVecOrMat) where {T}   ldiv!(A, B)

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/symmetric.jl, to deprecate
@deprecate A_mul_B!(y::StridedVector{T}, A::Symmetric{T,<:StridedMatrix}, x::StridedVector{T}) where {T<:BlasFloat}     mul!(y, A, x)
@deprecate A_mul_B!(y::StridedVector{T}, A::Hermitian{T,<:StridedMatrix}, x::StridedVector{T}) where {T<:BlasReal}      mul!(y, A, x)
@deprecate A_mul_B!(y::StridedVector{T}, A::Hermitian{T,<:StridedMatrix}, x::StridedVector{T}) where {T<:BlasComplex}   mul!(y, A, x)
@deprecate A_mul_B!(C::StridedMatrix{T}, A::Symmetric{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasFloat} mul!(C, A, B)
@deprecate A_mul_B!(C::StridedMatrix{T}, A::StridedMatrix{T}, B::Symmetric{T,<:StridedMatrix}) where {T<:BlasFloat} mul!(C, A, B)
@deprecate A_mul_B!(C::StridedMatrix{T}, A::Hermitian{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasReal}  mul!(C, A, B)
@deprecate A_mul_B!(C::StridedMatrix{T}, A::StridedMatrix{T}, B::Hermitian{T,<:StridedMatrix}) where {T<:BlasReal}  mul!(C, A, B)
@deprecate A_mul_B!(C::StridedMatrix{T}, A::Hermitian{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasComplex}   mul!(C, A, B)
@deprecate A_mul_B!(C::StridedMatrix{T}, A::StridedMatrix{T}, B::Hermitian{T,<:StridedMatrix}) where {T<:BlasComplex}   mul!(C, A, B)
@deprecate At_mul_B(A::RealHermSymComplexSym, B::AbstractVector)    (*)(transpose(A), B)
@deprecate At_mul_B(A::RealHermSymComplexSym, B::AbstractMatrix)    (*)(transpose(A), B)
@deprecate A_mul_Bt(A::AbstractMatrix, B::RealHermSymComplexSym)    (*)(A, transpose(B))
@deprecate Ac_mul_B(A::RealHermSymComplexHerm, B::AbstractVector)   (*)(adjoint(A), B)
@deprecate Ac_mul_B(A::RealHermSymComplexHerm, B::AbstractMatrix)   (*)(adjoint(A), B)
@deprecate A_mul_Bc(A::AbstractMatrix, B::RealHermSymComplexHerm)   (*)(A, adjoint(B))
@deprecate A_mul_Bt(A::RowVector, B::RealHermSymComplexSym)     (*)(A, transpose(B))
@deprecate A_mul_Bc(A::RowVector, B::RealHermSymComplexHerm)    (*)(A, adjoint(B))
@deprecate At_mul_B(A::RealHermSymComplexSym, B::AbstractTriangular)    (*)(transpose(A), B)
@deprecate A_mul_Bt(A::AbstractTriangular, B::RealHermSymComplexSym)    (*)(A, transpose(B))
@deprecate Ac_mul_B(A::RealHermSymComplexHerm, B::AbstractTriangular)   (*)(adjoint(A), B)
@deprecate A_mul_Bc(A::AbstractTriangular, B::RealHermSymComplexHerm)   (*)(A, adjoint(B))

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/lu.jl, to deprecate
@deprecate A_ldiv_B!(A::LU{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat}  ldiv!(A, B)
@deprecate A_ldiv_B!(A::LU{<:Any,<:StridedMatrix}, B::StridedVecOrMat)  ldiv!(A, B)
@deprecate At_ldiv_B!(A::LU{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat}     ldiv!(transpose(A), B)
@deprecate At_ldiv_B!(A::LU{<:Any,<:StridedMatrix}, B::StridedVecOrMat)     ldiv!(transpose(A), B)
@deprecate Ac_ldiv_B!(F::LU{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:Real}          ldiv!(adjoint(F), B)
@deprecate Ac_ldiv_B!(A::LU{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasComplex}   ldiv!(adjoint(A), B)
@deprecate Ac_ldiv_B!(A::LU{<:Any,<:StridedMatrix}, B::StridedVecOrMat)     ldiv!(adjoint(A), B)
@deprecate At_ldiv_Bt(A::LU{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat}  (\)(transpose(A), transpose(B))
@deprecate At_ldiv_Bt(A::LU, B::StridedVecOrMat)    (\)(transpose(A), transpose(B))
@deprecate Ac_ldiv_Bc(A::LU{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasComplex} (\)(adjoint(A), adjoint(B))
@deprecate Ac_ldiv_Bc(A::LU, B::StridedVecOrMat)    (\)(adjoint(A), adjoint(B))
@deprecate A_ldiv_B!(A::LU{T,Tridiagonal{T,V}}, B::AbstractVecOrMat) where {T,V}    ldiv!(A, B)
@deprecate At_ldiv_B!(A::LU{T,Tridiagonal{T,V}}, B::AbstractVecOrMat) where {T,V}   (\)(transpose(A), B)
@deprecate Ac_ldiv_B!(A::LU{T,Tridiagonal{T,V}}, B::AbstractVecOrMat) where {T,V}   ldiv!(adjoint(A), B)

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/lq.jl, to deprecate
@deprecate A_mul_B!(A::LQ{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat}   lmul!(A, B)
@deprecate A_mul_B!(A::LQ{T}, B::QR{T}) where {T<:BlasFloat}    A*B
@deprecate A_mul_B!(A::QR{T}, B::LQ{T}) where {T<:BlasFloat}    A*B
@deprecate A_mul_B!(A::LQPackedQ{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat}    lmul!(A, B)
@deprecate Ac_mul_B!(A::LQPackedQ{T}, B::StridedVecOrMat{T}) where {T<:BlasReal}    lmul!(adjoint(A), B)
@deprecate Ac_mul_B!(A::LQPackedQ{T}, B::StridedVecOrMat{T}) where {T<:BlasComplex} lmul!(adjoint(A), B)
@deprecate Ac_mul_B(A::LQPackedQ, B::StridedVecOrMat)   (*)(adjoint(A), B)
@deprecate A_mul_Bc(A::LQPackedQ, B::StridedVecOrMat)   (*)(A, adjoint(B))
@deprecate Ac_mul_Bc(A::LQPackedQ, B::StridedVecOrMat)  (*)(adjoint(A), adjoint(B))
@deprecate A_mul_B!(A::StridedMatrix{T}, B::LQPackedQ{T}) where {T<:BlasFloat}      rmul!(A, B)
@deprecate A_mul_Bc!(A::StridedMatrix{T}, B::LQPackedQ{T}) where {T<:BlasReal}      rmul!(A, adjoint(B))
@deprecate A_mul_Bc!(A::StridedMatrix{T}, B::LQPackedQ{T}) where {T<:BlasComplex}   rmul!(A, adjoint(B))
@deprecate A_mul_Bc(A::StridedVecOrMat, Q::LQPackedQ)   (*)(A, adjoint(Q))
@deprecate Ac_mul_Bc(A::StridedMatrix, Q::LQPackedQ)    (*)(adjoint(A), adjoint(Q))
@deprecate Ac_mul_B(A::StridedMatrix, Q::LQPackedQ)     (*)(adjoint(A), Q)
@deprecate A_ldiv_B!(A::LQ{T}, B::StridedVecOrMat{T}) where {T} ldiv!(A, B)

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/qr.jl, to deprecate
@deprecate A_mul_B!(A::QRCompactWYQ{T,S}, B::StridedVecOrMat{T}) where {T<:BlasFloat, S<:StridedMatrix} lmul!(A, B)
@deprecate A_mul_B!(A::QRPackedQ{T,S}, B::StridedVecOrMat{T}) where {T<:BlasFloat, S<:StridedMatrix}    lmul!(A, B)
@deprecate A_mul_B!(A::QRPackedQ, B::AbstractVecOrMat)  lmul!(A, B)
@deprecate Ac_mul_B!(A::QRCompactWYQ{T,S}, B::StridedVecOrMat{T}) where {T<:BlasReal,S<:StridedMatrix}      lmul!(adjoint(A), B)
@deprecate Ac_mul_B!(A::QRCompactWYQ{T,S}, B::StridedVecOrMat{T}) where {T<:BlasComplex,S<:StridedMatrix}   lmul!(adjoint(A), B)
@deprecate Ac_mul_B!(A::QRPackedQ{T,S}, B::StridedVecOrMat{T}) where {T<:BlasReal,S<:StridedMatrix}     lmul!(adjoint(A), B)
@deprecate Ac_mul_B!(A::QRPackedQ{T,S}, B::StridedVecOrMat{T}) where {T<:BlasComplex,S<:StridedMatrix}  lmul!(adjoint(A), B)
@deprecate Ac_mul_B!(A::QRPackedQ, B::AbstractVecOrMat) lmul!(adjoint(A), B)
@deprecate Ac_mul_B(Q::AbstractQ, B::StridedVecOrMat)   (*)(adjoint(Q), B)
@deprecate A_mul_Bc(Q::AbstractQ, B::StridedVecOrMat)   (*)(Q, adjoint(B))
@deprecate Ac_mul_Bc(Q::AbstractQ, B::StridedVecOrMat)  (*)(adjoint(Q), adjoint(B))
@deprecate A_mul_B!(A::StridedVecOrMat{T}, B::QRCompactWYQ{T,S}) where {T<:BlasFloat,S<:StridedMatrix}  rmul!(A, B)
@deprecate A_mul_B!(A::StridedVecOrMat{T}, B::QRPackedQ{T,S}) where {T<:BlasFloat,S<:StridedMatrix}     rmul!(A, B)
@deprecate A_mul_B!(A::StridedMatrix,Q::QRPackedQ)  rmul!(A, Q)
@deprecate A_mul_Bc!(A::StridedVecOrMat{T}, B::QRCompactWYQ{T}) where {T<:BlasReal}     rmul!(A, adjoint(B))
@deprecate A_mul_Bc!(A::StridedVecOrMat{T}, B::QRCompactWYQ{T}) where {T<:BlasComplex}  rmul!(A, adjoint(B))
@deprecate A_mul_Bc!(A::StridedVecOrMat{T}, B::QRPackedQ{T}) where {T<:BlasReal}    rmul!(A, adjoint(B))
@deprecate A_mul_Bc!(A::StridedVecOrMat{T}, B::QRPackedQ{T}) where {T<:BlasComplex} rmul!(A, adjoint(B))
@deprecate A_mul_Bc!(A::StridedMatrix,Q::QRPackedQ)     rmul!(A, adjoint(Q))
@deprecate A_mul_Bc(A::StridedMatrix, B::AbstractQ)     (*)(A, adjoint(B))
@deprecate A_mul_Bc(rowvec::RowVector, B::AbstractQ)    (*)(rowvec, adjoint(B))
@deprecate Ac_mul_B(A::StridedVecOrMat, Q::AbstractQ)   (*)(adjoint(A), Q)
@deprecate Ac_mul_Bc(A::StridedVecOrMat, Q::AbstractQ)  (*)(adjoint(A), adjoint(Q))
@deprecate A_ldiv_B!(A::QRCompactWY{T}, b::StridedVector{T}) where {T<:BlasFloat}   ldiv!(A, b)
@deprecate A_ldiv_B!(A::QRCompactWY{T}, B::StridedMatrix{T}) where {T<:BlasFloat}   ldiv!(A, B)
@deprecate A_ldiv_B!(A::QRPivoted{T}, B::StridedMatrix{T}, rcond::Real) where {T<:BlasFloat}    ldiv!(A, B, rcond)
@deprecate A_ldiv_B!(A::QRPivoted{T}, B::StridedVector{T}) where {T<:BlasFloat}     ldiv!(A, B)
@deprecate A_ldiv_B!(A::QRPivoted{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat}   ldiv!(A, B)
@deprecate A_ldiv_B!(A::QR{T}, B::StridedMatrix{T}) where {T}   ldiv!(A, B)
@deprecate A_ldiv_B!(A::QR, B::StridedVector)   ldiv!(A, B)
@deprecate A_ldiv_B!(A::QRPivoted, b::StridedVector)    ldiv!(A, b)
@deprecate A_ldiv_B!(A::QRPivoted, B::StridedMatrix)    ldiv!(A, B)

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/matmul.jl, to deprecate
@deprecate Ac_mul_Bc(A::AbstractMatrix{T}, B::AbstractMatrix{S}) where {T,S}    (*)(adjoint(A), adjoint(B))
@deprecate Ac_mul_Bc!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat}   mul!(C, adjoint(A), adjoint(B))
@deprecate Ac_mul_Bc!(C::AbstractMatrix, A::AbstractVecOrMat, B::AbstractVecOrMat)  mul!(C, adjoint(A), adjoint(B))
@deprecate Ac_mul_Bt!(C::AbstractMatrix, A::AbstractVecOrMat, B::AbstractVecOrMat)  mul!(C, adjoint(A), transpose(B))
@deprecate A_mul_Bc!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:BlasComplex}  mul!(C, A, adjoint(B))
@deprecate A_mul_Bc!(C::AbstractMatrix, A::AbstractVecOrMat, B::AbstractVecOrMat)   mul!(C, A, adjoint(B))
@deprecate A_mul_Bc(A::AbstractMatrix{T}, B::AbstractMatrix{S}) where {T,S}     (*)(A, adjoint(B))
@deprecate A_mul_Bc(A::StridedMatrix{<:BlasFloat}, B::StridedMatrix{<:BlasReal})    (*)(A, adjoint(B))
@deprecate A_mul_Bc!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{<:BlasReal}) where {T<:BlasFloat}   mul!(C, A, adjoint(B))
@deprecate Ac_mul_B!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:BlasComplex}  mul!(C, adjoint(A), B)
@deprecate Ac_mul_B!(C::AbstractMatrix, A::AbstractVecOrMat, B::AbstractVecOrMat)   mul!(C, adjoint(A), B)
@deprecate Ac_mul_B(A::AbstractMatrix{T}, B::AbstractMatrix{S}) where {T,S}         (*)(adjoint(A), B)
@deprecate Ac_mul_B(A::StridedMatrix{T}, B::StridedMatrix{T}) where {T<:BlasReal}   (*)(adjoint(A), B)
@deprecate Ac_mul_B!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:BlasReal}     mul!(C, adjoint(A), B)
@deprecate At_mul_Bt!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat}   mul!(C, transpose(A), transpose(B))
@deprecate At_mul_Bt!(C::AbstractMatrix, A::AbstractVecOrMat, B::AbstractVecOrMat)  mul!(C, transpose(A), transpose(B))
@deprecate At_mul_Bt(A::AbstractMatrix{T}, B::AbstractVecOrMat{S}) where {T,S}      (*)(transpose(A), transpose(B))
@deprecate A_mul_Bt!(C::AbstractVecOrMat, A::AbstractVecOrMat, B::AbstractVecOrMat) mul!(C, A, transpose(B))
@deprecate A_mul_Bt!(C::StridedMatrix{Complex{Float32}}, A::StridedVecOrMat{Complex{Float32}}, B::StridedVecOrMat{Float32})     mul!(C, A, transpose(B))
@deprecate A_mul_Bt!(C::StridedMatrix{Complex{Float64}}, A::StridedVecOrMat{Complex{Float64}}, B::StridedVecOrMat{Float64})     mul!(C, A, transpose(B))
@deprecate A_mul_Bt!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat}    mul!(C, A, transpose(B))
@deprecate A_mul_Bt(A::AbstractMatrix{T}, B::AbstractMatrix{S}) where {T,S}     (*)(A, transpose(B))
@deprecate At_mul_B!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat}    mul!(C, transpose(A), B)
@deprecate At_mul_B!(C::AbstractMatrix, A::AbstractVecOrMat, B::AbstractVecOrMat)   mul!(C, transpose(A), B)
@deprecate At_mul_B(A::AbstractMatrix{T}, B::AbstractMatrix{S}) where {T,S}     (*)(transpose(A), B)
@deprecate A_mul_B!(C::AbstractMatrix, A::AbstractVecOrMat, B::AbstractVecOrMat)    mul!(C, A, B)
@deprecate A_mul_B!(C::StridedMatrix{Complex{Float32}}, A::StridedVecOrMat{Complex{Float32}}, B::StridedVecOrMat{Float32})  mul!(C, A, B)
@deprecate A_mul_B!(C::StridedMatrix{Complex{Float64}}, A::StridedVecOrMat{Complex{Float64}}, B::StridedVecOrMat{Float64})  mul!(C, A, B)
@deprecate A_mul_B!(C::StridedMatrix{T}, A::StridedVecOrMat{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat}     mul!(C, A, B)
@deprecate Ac_mul_B!(y::StridedVector{T}, A::StridedVecOrMat{T}, x::StridedVector{T}) where {T<:BlasReal}       mul!(y, adjoint(A), x)
@deprecate Ac_mul_B!(y::StridedVector{T}, A::StridedVecOrMat{T}, x::StridedVector{T}) where {T<:BlasComplex}    mul!(y, adjoint(A), x)
@deprecate Ac_mul_B!(y::AbstractVector, A::AbstractVecOrMat, x::AbstractVector)     mul!(y, adjoint(A), x)
@deprecate Ac_mul_B(A::StridedMatrix{T}, x::StridedVector{S}) where {T<:BlasFloat,S}    (*)(adjoint(A), x)
@deprecate Ac_mul_B(A::AbstractMatrix{T}, x::AbstractVector{S}) where {T,S}     (*)(adjoint(A), x)
@deprecate At_mul_B(A::StridedMatrix{T}, x::StridedVector{S}) where {T<:BlasFloat,S}    (*)(transpose(A), x)
@deprecate At_mul_B(A::AbstractMatrix{T}, x::AbstractVector{S}) where {T,S}     (*)(transpose(A), x)
@deprecate At_mul_B!(y::StridedVector{T}, A::StridedVecOrMat{T}, x::StridedVector{T}) where {T<:BlasFloat}  mul!(y, transpose(A), x)
@deprecate At_mul_B!(y::AbstractVector, A::AbstractVecOrMat, x::AbstractVector) mul!(y, transpose(A), x)
@deprecate A_mul_B!(y::AbstractVector, A::AbstractVecOrMat, x::AbstractVector)  mul!(y, A, x)
@deprecate A_mul_B!(y::StridedVector{Complex{Float32}}, A::StridedVecOrMat{Complex{Float32}}, x::StridedVector{Float32})    mul!(y, A, x)
@deprecate A_mul_B!(y::StridedVector{Complex{Float64}}, A::StridedVecOrMat{Complex{Float64}}, x::StridedVector{Float64})    mul!(y, A, x)
@deprecate A_mul_B!(y::StridedVector{T}, A::StridedVecOrMat{T}, x::StridedVector{T}) where {T<:BlasFloat}   mul!(y, A, x)
@deprecate A_mul_Bt(a::AbstractVector, B::AbstractMatrix)   (*)(a, transpose(B))
@deprecate A_mul_Bt(A::AbstractMatrix, b::AbstractVector)   (*)(A, transpose(b))
@deprecate A_mul_Bc(a::AbstractVector, B::AbstractMatrix)   (*)(a, adjoint(B))
@deprecate A_mul_Bc(A::AbstractMatrix, b::AbstractVector)   (*)(A, adjoint(b))
@deprecate At_mul_B(x::StridedVector{T}, y::StridedVector{T}) where {T<:BlasComplex}    (*)(transpose(x), y)

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/triangular.jl, to deprecate
@deprecate A_mul_Bc(A::AbstractTriangular, B::AbstractTriangular)   (*)(A, adjoint(B))
@deprecate A_mul_Bt(A::AbstractTriangular, B::AbstractTriangular)   (*)(A, transpose(B))
@deprecate Ac_mul_B(A::AbstractTriangular, B::AbstractTriangular)   (*)(adjoint(A), B)
@deprecate At_mul_B(A::AbstractTriangular, B::AbstractTriangular)   (*)(transpose(A), B)
@deprecate Ac_ldiv_B(A::Union{UpperTriangular,LowerTriangular}, B::RowVector)           (\)(adjoint(A), B)
@deprecate Ac_ldiv_B(A::Union{UnitUpperTriangular,UnitLowerTriangular}, B::RowVector)   (\)(adjoint(A), B)
@deprecate At_ldiv_B(A::Union{UpperTriangular,LowerTriangular}, B::RowVector)           (\)(transpose(A), B)
@deprecate At_ldiv_B(A::Union{UnitUpperTriangular,UnitLowerTriangular}, B::RowVector)       (\)(transpose(A), B)
@deprecate A_rdiv_Bc(rowvec::RowVector, A::Union{UpperTriangular,LowerTriangular})          (/)(rowvec, adjoint(A))
@deprecate A_rdiv_Bc(rowvec::RowVector, A::Union{UnitUpperTriangular,UnitLowerTriangular})  (/)(rowvec, adjoint(A))
@deprecate A_rdiv_Bt(rowvec::RowVector, A::Union{UpperTriangular,LowerTriangular})          (/)(rowvec, transpose(A))
@deprecate A_rdiv_Bt(rowvec::RowVector, A::Union{UnitUpperTriangular,UnitLowerTriangular})  (/)(rowvec, transpose(A))
@deprecate A_mul_Bt(rowvec::RowVector, A::AbstractTriangular)       (*)(rowvec, transpose(A))
@deprecate A_mul_Bt(A::AbstractTriangular, rowvec::RowVector)       (*)(A, transpose(rowvec))
@deprecate At_mul_Bt(A::AbstractTriangular, rowvec::RowVector)      (*)(transpose(A), transpose(rowvec))
@deprecate A_mul_Bc(rowvec::RowVector, A::AbstractTriangular)       (*)(rowvec, adjoint(A))
@deprecate A_mul_Bc(A::AbstractTriangular, rowvec::RowVector)       (*)(A, adjoint(rowvec))
@deprecate Ac_mul_Bc(A::AbstractTriangular, rowvec::RowVector)      (*)(adjoint(A), adjoint(rowvec))
@deprecate Ac_mul_B(A::AbstractMatrix, B::AbstractTriangular)       (*)(adjoint(A), B)
@deprecate At_mul_B(A::AbstractMatrix, B::AbstractTriangular)       (*)(transpose(A), B)
@deprecate A_mul_Bc(A::AbstractTriangular, B::AbstractMatrix)       (*)(A, adjoint(B))
@deprecate A_mul_Bt(A::AbstractTriangular, B::AbstractMatrix)       (*)(A, transpose(B))
@deprecate Ac_mul_Bc(A::AbstractTriangular, B::AbstractTriangular)  (*)(adjoint(A), adjoint(B))
@deprecate Ac_mul_Bc(A::AbstractTriangular, B::AbstractMatrix)      (*)(adjoint(A), adjoint(B))
@deprecate Ac_mul_Bc(A::AbstractMatrix, B::AbstractTriangular)      (*)(adjoint(A), adjoint(B))
@deprecate At_mul_Bt(A::AbstractTriangular, B::AbstractTriangular)  (*)(transpose(A), transpose(B))
@deprecate At_mul_Bt(A::AbstractTriangular, B::AbstractMatrix)      (*)(transpose(A), transpose(B))
@deprecate At_mul_Bt(A::AbstractMatrix, B::AbstractTriangular)      (*)(transpose(A), transpose(B))
@deprecate A_mul_Bc!(A::UpperTriangular, B::Union{LowerTriangular,UnitLowerTriangular})     rmul!(A, adjoint(B))
@deprecate A_mul_Bc!(A::LowerTriangular, B::Union{UpperTriangular,UnitUpperTriangular})     rmul!(A, adjoint(B))
@deprecate A_mul_Bt!(A::UpperTriangular, B::Union{LowerTriangular,UnitLowerTriangular})     rmul!(A, transpose(B))
@deprecate A_mul_Bt!(A::LowerTriangular, B::Union{UpperTriangular,UnitUpperTriangular})     rmul!(A, transpose(B))
@deprecate A_rdiv_Bc!(A::UpperTriangular, B::Union{LowerTriangular,UnitLowerTriangular})    rdiv!(A, adjoint(B))
@deprecate A_rdiv_Bc!(A::LowerTriangular, B::Union{UpperTriangular,UnitUpperTriangular})    rdiv!(A, adjoint(B))
@deprecate A_rdiv_Bt!(A::UpperTriangular, B::Union{LowerTriangular,UnitLowerTriangular})    rdiv!(A, transpose(B))
@deprecate A_rdiv_Bt!(A::LowerTriangular, B::Union{UpperTriangular,UnitUpperTriangular})    rdiv!(A, transpose(B))
@deprecate A_rdiv_B!(A::UpperTriangular, B::Union{UpperTriangular,UnitUpperTriangular})     rdiv!(A, B)
@deprecate A_rdiv_B!(A::LowerTriangular, B::Union{LowerTriangular,UnitLowerTriangular})     rdiv!(A, B)
@deprecate Ac_mul_B!(A::Union{LowerTriangular,UnitLowerTriangular}, B::UpperTriangular)     lmul!(adjoint(A), B)
@deprecate Ac_mul_B!(A::Union{UpperTriangular,UnitUpperTriangular}, B::LowerTriangular)     lmul!(adjoint(A), B)
@deprecate At_mul_B!(A::Union{LowerTriangular,UnitLowerTriangular}, B::UpperTriangular)     lmul!(transpose(A), B)
@deprecate At_mul_B!(A::Union{UpperTriangular,UnitUpperTriangular}, B::LowerTriangular)     lmul!(transpose(A), B)
@deprecate Ac_ldiv_B!(A::Union{LowerTriangular,UnitLowerTriangular}, B::UpperTriangular)    ldiv!(adjoint(A), B)
@deprecate Ac_ldiv_B!(A::Union{UpperTriangular,UnitUpperTriangular}, B::LowerTriangular)    ldiv!(adjoint(A), B)
@deprecate At_ldiv_B!(A::Union{LowerTriangular,UnitLowerTriangular}, B::UpperTriangular)    ldiv!(transpose(A), B)
@deprecate At_ldiv_B!(A::Union{UpperTriangular,UnitUpperTriangular}, B::LowerTriangular)    ldiv!(transpose(A), B)
@deprecate A_rdiv_Bt!(A::StridedMatrix, B::UnitLowerTriangular) rdiv!(A, transpose(B))
@deprecate A_rdiv_Bt!(A::StridedMatrix, B::LowerTriangular)     rdiv!(A, transpose(B))
@deprecate A_rdiv_Bt!(A::StridedMatrix, B::UnitUpperTriangular) rdiv!(A, transpose(B))
@deprecate A_rdiv_Bt!(A::StridedMatrix, B::UpperTriangular)     rdiv!(A, transpose(B))
@deprecate A_rdiv_Bc!(A::StridedMatrix, B::UnitLowerTriangular) rdiv!(A, adjoint(B))
@deprecate A_rdiv_Bc!(A::StridedMatrix, B::LowerTriangular)     rdiv!(A, adjoint(B))
@deprecate A_rdiv_Bc!(A::StridedMatrix, B::UnitUpperTriangular) rdiv!(A, adjoint(B))
@deprecate A_rdiv_Bc!(A::StridedMatrix, B::UpperTriangular)     rdiv!(A, adjoint(B))
@deprecate A_rdiv_B!(A::StridedMatrix, B::UnitLowerTriangular)  rdiv!(A, B)
@deprecate A_rdiv_B!(A::StridedMatrix, B::LowerTriangular)      rdiv!(A, B)
@deprecate A_rdiv_B!(A::StridedMatrix, B::UnitUpperTriangular)  rdiv!(A, B)
@deprecate A_rdiv_B!(A::StridedMatrix, B::UpperTriangular)      rdiv!(A, B)
@deprecate Ac_ldiv_B!(A::UnitUpperTriangular, b::AbstractVector, x::AbstractVector = b) ldiv!(adjoint(A), b, x)
@deprecate Ac_ldiv_B!(A::UpperTriangular, b::AbstractVector, x::AbstractVector = b)     ldiv!(adjoint(A), b, x)
@deprecate Ac_ldiv_B!(A::UnitLowerTriangular, b::AbstractVector, x::AbstractVector = b) ldiv!(adjoint(A), b, x)
@deprecate Ac_ldiv_B!(A::LowerTriangular, b::AbstractVector, x::AbstractVector = b)     ldiv!(adjoint(A), b, x)
@deprecate At_ldiv_B!(A::UnitUpperTriangular, b::AbstractVector, x::AbstractVector = b) ldiv!(transpose(A), b, x)
@deprecate At_ldiv_B!(A::UpperTriangular, b::AbstractVector, x::AbstractVector = b)     ldiv!(transpose(A), b, x)
@deprecate At_ldiv_B!(A::UnitLowerTriangular, b::AbstractVector, x::AbstractVector = b) ldiv!(transpose(A), b, x)
@deprecate At_ldiv_B!(A::LowerTriangular, b::AbstractVector, x::AbstractVector = b)     ldiv!(transpose(A), b, x)
@deprecate A_mul_Bt!(A::StridedMatrix, B::UnitLowerTriangular)  rmul!(A, transpose(B))
@deprecate A_mul_Bt!(A::StridedMatrix, B::LowerTriangular)      rmul!(A, transpose(B))
@deprecate A_mul_Bt!(A::StridedMatrix, B::UnitUpperTriangular)  rmul!(A, transpose(B))
@deprecate A_mul_Bt!(A::StridedMatrix, B::UpperTriangular)      rmul!(A, transpose(B))
@deprecate A_mul_Bc!(A::StridedMatrix, B::UnitLowerTriangular)  rmul!(A, adjoint(B))
@deprecate A_mul_Bc!(A::StridedMatrix, B::LowerTriangular)      rmul!(A, adjoint(B))
@deprecate A_mul_Bc!(A::StridedMatrix, B::UnitUpperTriangular)  rmul!(A, adjoint(B))
@deprecate A_mul_Bc!(A::StridedMatrix, B::UpperTriangular)      rmul!(A, adjoint(B))
@deprecate A_mul_B!(A::StridedMatrix, B::UnitLowerTriangular)   rmul!(A, B)
@deprecate A_mul_B!(A::StridedMatrix, B::LowerTriangular)       rmul!(A, B)
@deprecate A_mul_B!(A::StridedMatrix, B::UnitUpperTriangular)   rmul!(A, B)
@deprecate A_mul_B!(A::StridedMatrix, B::UpperTriangular)       rmul!(A, B)
@deprecate At_mul_B!(A::UnitLowerTriangular, B::StridedVecOrMat)    lmul!(transpose(A), B)
@deprecate At_mul_B!(A::LowerTriangular, B::StridedVecOrMat)        lmul!(transpose(A), B)
@deprecate At_mul_B!(A::UnitUpperTriangular, B::StridedVecOrMat)    lmul!(transpose(A), B)
@deprecate At_mul_B!(A::UpperTriangular, B::StridedVecOrMat)        lmul!(transpose(A), B)
@deprecate Ac_mul_B!(A::UnitLowerTriangular, B::StridedVecOrMat)    lmul!(adjoint(A), B)
@deprecate Ac_mul_B!(A::LowerTriangular, B::StridedVecOrMat)        lmul!(adjoint(A), B)
@deprecate Ac_mul_B!(A::UnitUpperTriangular, B::StridedVecOrMat)    lmul!(adjoint(A), B)
@deprecate Ac_mul_B!(A::UpperTriangular, B::StridedVecOrMat)    lmul!(adjoint(A), B)
@deprecate A_mul_B!(A::UnitLowerTriangular, B::StridedVecOrMat) lmul!(A, B)
@deprecate A_mul_B!(A::LowerTriangular, B::StridedVecOrMat)     lmul!(A, B)
@deprecate A_mul_B!(A::UnitUpperTriangular, B::StridedVecOrMat) lmul!(A, B)
@deprecate A_mul_B!(A::UpperTriangular, B::StridedVecOrMat)     lmul!(A, B)
@deprecate A_mul_B!(C::AbstractVector  , A::AbstractTriangular, B::AbstractVector)      mul!(C, A, B)
@deprecate A_mul_B!(C::AbstractMatrix  , A::AbstractTriangular, B::AbstractVecOrMat)    mul!(C, A, B)
@deprecate A_mul_B!(C::AbstractVecOrMat, A::AbstractTriangular, B::AbstractVecOrMat)    mul!(C, A, B)
@deprecate Ac_mul_B!(C::AbstractVector  , A::AbstractTriangular, B::AbstractVector)     mul!(C, adjoint(A), B)
@deprecate Ac_mul_B!(C::AbstractMatrix  , A::AbstractTriangular, B::AbstractVecOrMat)   mul!(C, adjoint(A), B)
@deprecate Ac_mul_B!(C::AbstractVecOrMat, A::AbstractTriangular, B::AbstractVecOrMat)   mul!(C, adjoint(A), B)
@deprecate At_mul_B!(C::AbstractVector  , A::AbstractTriangular, B::AbstractVector)     mul!(C, transpose(A), B)
@deprecate At_mul_B!(C::AbstractMatrix  , A::AbstractTriangular, B::AbstractVecOrMat)   mul!(C, transpose(A), B)
@deprecate At_mul_B!(C::AbstractVecOrMat, A::AbstractTriangular, B::AbstractVecOrMat)   mul!(C, transpose(A), B)
@deprecate A_mul_B!(A::Tridiagonal, B::AbstractTriangular)  lmul!(A, B)
@deprecate A_mul_B!(C::AbstractMatrix, A::AbstractTriangular, B::Tridiagonal)   mul!(C, A, B)
@deprecate A_mul_B!(C::AbstractMatrix, A::Tridiagonal, B::AbstractTriangular)   mul!(C, A, B)
@deprecate A_mul_Bt!(C::AbstractVecOrMat, A::AbstractTriangular, B::AbstractVecOrMat)   mul!(C, A, transpose(B))
@deprecate A_mul_Bc!(C::AbstractMatrix, A::AbstractTriangular, B::AbstractVecOrMat)     mul!(C, A, adjoint(B))
@deprecate A_mul_Bc!(C::AbstractVecOrMat, A::AbstractTriangular, B::AbstractVecOrMat)   mul!(C, A, adjoint(B))
for mat in (:AbstractVector, :AbstractMatrix)
    @eval begin
        @deprecate Ac_mul_B(A::AbstractTriangular, B::$mat)     (*)(adjoint(A), B)
        @deprecate At_mul_B(A::AbstractTriangular, B::$mat)     (*)(transpose(A), B)
        @deprecate Ac_ldiv_B(A::Union{UnitUpperTriangular,UnitLowerTriangular}, B::$mat)    (\)(adjoint(A), B)
        @deprecate At_ldiv_B(A::Union{UnitUpperTriangular,UnitLowerTriangular}, B::$mat)    (\)(transpose(A), B)
        @deprecate Ac_ldiv_B(A::Union{UpperTriangular,LowerTriangular}, B::$mat)    (\)(adjoint(A), B)
        @deprecate At_ldiv_B(A::Union{UpperTriangular,LowerTriangular}, B::$mat)    (\)(transpose(A), B)
        @deprecate A_rdiv_Bc(A::$mat, B::Union{UnitUpperTriangular, UnitLowerTriangular})   (/)(A, adjoint(B))
        @deprecate A_rdiv_Bt(A::$mat, B::Union{UnitUpperTriangular, UnitLowerTriangular})   (/)(A, transpose(B))
        @deprecate A_rdiv_Bc(A::$mat, B::Union{UpperTriangular,LowerTriangular})    (/)(A, adjoint(B))
        @deprecate A_rdiv_Bt(A::$mat, B::Union{UpperTriangular,LowerTriangular})    (/)(A, transpose(B))
    end
end
@deprecate A_mul_Bc(A::AbstractMatrix, B::AbstractTriangular)  (*)(A, adjoint(B))
@deprecate A_mul_Bt(A::AbstractMatrix, B::AbstractTriangular)  (*)(A, transpose(B))
for (f, op, transform) in (
        (:A_mul_Bc, :*, :adjoint),
        (:A_mul_Bt, :*, :transpose),
        (:A_rdiv_Bc, :/, :adjoint),
        (:A_rdiv_Bt, :/, :transpose))
    @eval begin
        @deprecate $f(A::LowerTriangular, B::UpperTriangular)       ($op)(A, ($transform)(B))
        @deprecate $f(A::LowerTriangular, B::UnitUpperTriangular)   ($op)(A, ($transform)(B))
        @deprecate $f(A::UpperTriangular, B::LowerTriangular)       ($op)(A, ($transform)(B))
        @deprecate $f(A::UpperTriangular, B::UnitLowerTriangular)   ($op)(A, ($transform)(B))
    end
end
for (f, op, transform) in (
        (:Ac_mul_B, :*, :adjoint),
        (:At_mul_B, :*, :transpose),
        (:Ac_ldiv_B, :\, :adjoint),
        (:At_ldiv_B, :\, :transpose))
    @eval begin
        @deprecate ($f)(A::UpperTriangular, B::LowerTriangular)     ($op)(($transform)(A), B)
        @deprecate ($f)(A::UnitUpperTriangular, B::LowerTriangular) ($op)(($transform)(A), B)
        @deprecate ($f)(A::LowerTriangular, B::UpperTriangular)     ($op)(($transform)(A), B)
        @deprecate ($f)(A::UnitLowerTriangular, B::UpperTriangular) ($op)(($transform)(A), B)
    end
end
for (t, uploc, isunitc) in ((:LowerTriangular, 'L', 'N'),
                            (:UnitLowerTriangular, 'L', 'U'),
                            (:UpperTriangular, 'U', 'N'),
                            (:UnitUpperTriangular, 'U', 'U'))
    @eval begin
        # Vector multiplication
        @deprecate A_mul_B!(A::$t{T,<:StridedMatrix}, b::StridedVector{T}) where {T<:BlasFloat}     lmul!(A, b)
        @deprecate At_mul_B!(A::$t{T,<:StridedMatrix}, b::StridedVector{T}) where {T<:BlasFloat}    lmul!(transpose(A), b)
        @deprecate Ac_mul_B!(A::$t{T,<:StridedMatrix}, b::StridedVector{T}) where {T<:BlasReal}     lmul!(adjoint(A), b)
        @deprecate Ac_mul_B!(A::$t{T,<:StridedMatrix}, b::StridedVector{T}) where {T<:BlasComplex}  lmul!(adjoint(A), b)

        # Matrix multiplication
        @deprecate A_mul_B!(A::$t{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasFloat}     lmul!(A, B)
        @deprecate A_mul_B!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasFloat}     lmul!(A, B)

        @deprecate At_mul_B!(A::$t{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasFloat}       lmul!(transpose(A), B)
        @deprecate Ac_mul_B!(A::$t{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasComplex}     lmul!(adjoint(A), B)
        @deprecate Ac_mul_B!(A::$t{T,<:StridedMatrix}, B::StridedMatrix{T}) where {T<:BlasReal}        lmul!(adjoint(A), B)

        @deprecate A_mul_Bt!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasFloat}    rmul!(A, transpose(B))
        @deprecate A_mul_Bc!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasComplex}  rmul!(A, adjoint(B))
        @deprecate A_mul_Bc!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasReal}     rmul!(A, adjoint(B))

        # Left division
        @deprecate A_ldiv_B!(A::$t{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat}  ldiv!(A, B)
        @deprecate At_ldiv_B!(A::$t{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat} ldiv!(transpose(A), B)
        @deprecate Ac_ldiv_B!(A::$t{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasReal}  ldiv!(adjoint(A), B)
        @deprecate Ac_ldiv_B!(A::$t{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasComplex}   ldiv!(adjoint(A), B)

        # Right division
        @deprecate A_rdiv_B!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasFloat}    rdiv!(A, B)
        @deprecate A_rdiv_Bt!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasFloat}   rdiv!(A, transpose(B))
        @deprecate A_rdiv_Bc!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasReal}    rdiv!(A, adjoint(B))
        @deprecate A_rdiv_Bc!(A::StridedMatrix{T}, B::$t{T,<:StridedMatrix}) where {T<:BlasComplex} rdiv!(A, adjoint(B))
    end
end

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/rowvector.jl, to deprecate
@deprecate A_rdiv_Bt(rowvec::RowVector, mat::AbstractMatrix)    (/)(rowvec, transpose(mat))
@deprecate A_rdiv_Bc(rowvec::RowVector, mat::AbstractMatrix)    (/)(rowvec, adjoint(mat))
@deprecate At_ldiv_B(mat::AbstractMatrix, rowvec::RowVector)    (\)(transpose(mat), rowvec)
@deprecate Ac_ldiv_B(mat::AbstractMatrix, rowvec::RowVector)    (\)(adjoint(mat), rowvec)
@deprecate Ac_mul_B(u::RowVector, v::AbstractVector)        (*)(adjoint(u), v)
@deprecate Ac_mul_B(vec::AbstractVector, mat::AbstractMatrix)   (*)(adjoint(vec), mat)
@deprecate Ac_mul_B(rowvec1::RowVector, rowvec2::RowVector)     (*)(adjoint(rowvec1), rowvec2)
@deprecate Ac_mul_B(vec::AbstractVector, rowvec::RowVector)     (*)(adjoint(vec), rowvec)
@deprecate Ac_mul_B(vec1::AbstractVector, vec2::AbstractVector) (*)(adjoint(vec1), vec2)
@deprecate Ac_mul_Bc(rowvec::RowVector, vec::AbstractVector)    (*)(adjoint(rowvec), adjoint(vec))
@deprecate Ac_mul_Bc(vec::AbstractVector, mat::AbstractMatrix)  (*)(adjoint(vec), adjoint(mat))
@deprecate Ac_mul_Bc(rowvec1::RowVector, rowvec2::RowVector)    (*)(adjoint(rowvec1), adjoint(rowvec2))
@deprecate Ac_mul_Bc(vec::AbstractVector, rowvec::RowVector)    (*)(adjoint(vec), adjoint(rowvec))
@deprecate Ac_mul_Bc(vec::AbstractVector, rowvec::AbstractVector)   (*)(adjoint(vec), adjoint(rowvec))
@deprecate Ac_mul_Bc(mat::AbstractMatrix, rowvec::RowVector)        (*)(adjoint(mat), adjoint(rowvec))
@deprecate A_mul_Bc(u::RowVector, v::AbstractVector)    (*)(u, adjoint(v))
@deprecate A_mul_Bc(rowvec::RowVector, mat::AbstractMatrix) (*)(rowvec, adjoint(mat))
@deprecate A_mul_Bc(rowvec1::RowVector, rowvec2::RowVector)     (*)(rowvec1, adjoint(rowvec2))
@deprecate A_mul_Bc(vec::AbstractVector, rowvec::RowVector)     (*)(vec, adjoint(rowvec))
@deprecate A_mul_Bc(vec1::AbstractVector, vec2::AbstractVector) (*)(vec1, adjoint(vec2))
@deprecate A_mul_Bc(mat::AbstractMatrix, rowvec::RowVector)     (*)(mat, adjoint(rowvec))
@deprecate At_mul_B(v::RowVector, u::AbstractVector)            (*)(transpose(v), u)
@deprecate At_mul_B(vec::AbstractVector, mat::AbstractMatrix)   (*)(transpose(vec), mat)
@deprecate At_mul_B(rowvec1::RowVector, rowvec2::RowVector)     (*)(transpose(rowvec1), rowvec2)
@deprecate At_mul_B(vec::AbstractVector, rowvec::RowVector)     (*)(transpose(vec), rowvec)
@deprecate At_mul_B(vec1::AbstractVector{T}, vec2::AbstractVector{T}) where {T<:Real}   (*)(transpose(vec1), vec2)
@deprecate At_mul_B(vec1::AbstractVector, vec2::AbstractVector)     (*)(transpose(vec1), vec2)
@deprecate At_mul_Bt(rowvec::RowVector, vec::AbstractVector)        (*)(transpose(rowvec), transpose(vec))
@deprecate At_mul_Bt(vec::AbstractVector, mat::AbstractMatrix)      (*)(transpose(vec), transpose(mat))
@deprecate At_mul_Bt(rowvec1::RowVector, rowvec2::RowVector)        (*)(transpose(rowvec1), transpose(rowvec2))
@deprecate At_mul_Bt(vec::AbstractVector, rowvec::RowVector)        (*)(transpose(vec), transpose(rowvec))
@deprecate At_mul_Bt(vec::AbstractVector, rowvec::AbstractVector)   (*)(transpose(vec), transpose(rowvec))
@deprecate At_mul_Bt(mat::AbstractMatrix, rowvec::RowVector)    (*)(transpose(mat), transpose(rowvec))
@deprecate A_mul_Bt(v::RowVector, A::AbstractVector)            (*)(v, transpose(A))
@deprecate A_mul_Bt(rowvec::RowVector, mat::AbstractMatrix)     (*)(rowvec, transpose(mat))
@deprecate A_mul_Bt(rowvec1::RowVector, rowvec2::RowVector)     (*)(rowvec1, transpose(rowvec2))
@deprecate A_mul_Bt(vec::AbstractVector, rowvec::RowVector)     (*)(vec, transpose(rowvec))
@deprecate A_mul_Bt(vec1::AbstractVector, vec2::AbstractVector) (*)(vec1, transpose(vec2))
@deprecate A_mul_Bt(mat::AbstractMatrix, rowvec::RowVector)     (*)(mat, transpose(rowvec))

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/linalg/givens.jl, to deprecate
@deprecate A_mul_Bc!(A::AbstractMatrix, R::Rotation)    rmul!(A, adjoint(R))
@deprecate A_mul_B!(R::Rotation, A::AbstractMatrix)     lmul!(R, A)
@deprecate A_mul_B!(G::Givens, R::Rotation)             lmul!(G, R)
@deprecate A_mul_Bc!(A::AbstractMatrix, G::Givens)      rmul!(A, adjoint(G))
@deprecate A_mul_B!(G::Givens, A::AbstractVecOrMat)     lmul!(G, A)
@deprecate A_mul_B!(G1::Givens, G2::Givens)             G1 * G2
@deprecate A_mul_Bc(A::AbstractVecOrMat{T}, R::AbstractRotation{S}) where {T,S}     (*)(A, adjoint(R))


# methods involving RowVector from base/linalg/bidiag.jl, to deprecate
\(::Diagonal, ::RowVector) = _mat_ldiv_rowvec_error()
\(::Bidiagonal, ::RowVector) = _mat_ldiv_rowvec_error()
\(::Bidiagonal{<:Number}, ::RowVector{<:Number}) = _mat_ldiv_rowvec_error()
\(::Adjoint{<:Any,<:Bidiagonal}, ::RowVector) = _mat_ldiv_rowvec_error()
\(::Transpose{<:Any,<:Bidiagonal}, ::RowVector) = _mat_ldiv_rowvec_error()
\(::Adjoint{<:Number,<:Bidiagonal{<:Number}}, ::RowVector{<:Number}) = _mat_ldiv_rowvec_error()
\(::Transpose{<:Number,<:Bidiagonal{<:Number}}, ::RowVector{<:Number}) = _mat_ldiv_rowvec_error()
_mat_ldiv_rowvec_error() = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))

# methods involving RowVector from base/linalg/diagonal.jl, to deprecate
*(rowvec::RowVector, D::Diagonal) = rvtranspose(D * rvtranspose(rowvec)) # seems potentially incorrect without also transposing D?
*(D::Diagonal, transrowvec::Transpose{<:Any,<:RowVector}) = (rowvec = transrowvec.parent; D*rvtranspose(rowvec))
*(D::Diagonal, adjrowvec::Adjoint{<:Any,<:RowVector}) = (rowvec = adjrowvec.parent; D*rvadjoint(rowvec))

# methods involving RowVector from base/linalg/qr.jl, to deprecate
*(rowvec::RowVector, adjB::Adjoint{<:Any,<:AbstractQ}) = (B = adjB.parent; rvadjoint(B*rvadjoint(rowvec)))

# methods involving RowVector from base/linalg/qr.jl, to deprecate
*(A::RowVector, B::Adjoint{<:Any,<:AbstractRotation}) = A * adjoint(B.parent)

# methods involving RowVector from base/linalg/generic.jl, to deprecate
norm(tv::RowVector, q::Real) = q == Inf ? norm(rvtranspose(tv), 1) : norm(rvtranspose(tv), q/(q-1))
norm(tv::RowVector) = norm(rvtranspose(tv))

# methods involving RowVector from base/linalg/factorization.jl, to deprecate
\(A::Adjoint{<:Any,<:Factorization}, B::RowVector) = adjoint(A.parent) \ B
\(A::Transpose{<:Any,<:Factorization}, B::RowVector) = transpose(A.parent) \ B
\(A::Transpose{<:Any,<:Factorization{<:Real}}, B::RowVector) = transpose(A.parent) \ B

# methods involving RowVector from base/linalg/symmetric.jl, to deprecate
*(A::RowVector, transB::Transpose{<:Any,<:RealHermSymComplexSym}) = A * transB.parent
*(A::RowVector, adjB::Adjoint{<:Any,<:RealHermSymComplexHerm}) = A * adjB.parent
\(A::HermOrSym{<:Any,<:StridedMatrix}, B::RowVector) = invoke(\, Tuple{AbstractMatrix, RowVector}, A, B)
*(A::Adjoint{<:Any,<:RealHermSymComplexHerm}, B::Adjoint{<:Any,<:RowVector}) = A.parent * B
*(A::Adjoint{<:Any,<:RealHermSymComplexHerm}, B::Transpose{<:Any,<:RowVector}) = A.parent * B
*(A::Transpose{<:Any,<:RealHermSymComplexSym}, B::Adjoint{<:Any,<:RowVector}) = A.parent * B
*(A::Transpose{<:Any,<:RealHermSymComplexSym}, B::Transpose{<:Any,<:RowVector}) = A.parent * B

# methods involving RowVector from base/linalg/triangular.jl, to deprecate
*(rowvec::RowVector, A::AbstractTriangular) = rvtranspose(transpose(A) * rvtranspose(rowvec))
*(rowvec::RowVector, transA::Transpose{<:Any,<:AbstractTriangular}) = rvtranspose(transA.parent * rvtranspose(rowvec))
*(A::AbstractTriangular, transrowvec::Transpose{<:Any,<:RowVector}) = A * rvtranspose(transrowvec.parent)
*(transA::Transpose{<:Any,<:AbstractTriangular}, transrowvec::Transpose{<:Any,<:RowVector}) = transA * rvtranspose(transrowvec.parent)
*(rowvec::RowVector, adjA::Adjoint{<:Any,<:AbstractTriangular}) = rvadjoint(adjA.parent * rvadjoint(rowvec))
*(A::AbstractTriangular, adjrowvec::Adjoint{<:Any,<:RowVector}) = A * rvadjoint(adjrowvec.parent)
*(adjA::Adjoint{<:Any,<:AbstractTriangular}, adjrowvec::Adjoint{<:Any,<:RowVector}) = adjA * rvadjoint(adjrowvec.parent)
\(::Union{UpperTriangular,LowerTriangular}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
\(::Union{UnitUpperTriangular,UnitLowerTriangular}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
\(::Adjoint{<:Any,<:Union{UpperTriangular,LowerTriangular}}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
\(::Adjoint{<:Any,<:Union{UnitUpperTriangular,UnitLowerTriangular}}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
\(::Transpose{<:Any,<:Union{UpperTriangular,LowerTriangular}}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
\(::Transpose{<:Any,<:Union{UnitUpperTriangular,UnitLowerTriangular}}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
/(rowvec::RowVector, A::Union{UpperTriangular,LowerTriangular}) = rvtranspose(transpose(A) \ rvtranspose(rowvec))
/(rowvec::RowVector, A::Union{UnitUpperTriangular,UnitLowerTriangular}) = rvtranspose(transpose(A) \ rvtranspose(rowvec))
/(rowvec::RowVector, transA::Transpose{<:Any,<:Union{UpperTriangular,LowerTriangular}}) = rvtranspose(transA.parent \ rvtranspose(rowvec))
/(rowvec::RowVector, transA::Transpose{<:Any,<:Union{UnitUpperTriangular,UnitLowerTriangular}}) = rvtranspose(transA.parent \ rvtranspose(rowvec))
/(rowvec::RowVector, adjA::Adjoint{<:Any,<:Union{UpperTriangular,LowerTriangular}}) = /(rowvec, adjoint(adjA.parent))
/(rowvec::RowVector, adjA::Adjoint{<:Any,<:Union{UnitUpperTriangular,UnitLowerTriangular}}) = /(rowvec, adjoint(adjA.parent))
*(A::Adjoint{<:Any,<:AbstractTriangular}, B::Transpose{<:Any,<:RowVector}) = A * rvtranspose(B.parent)
*(A::Transpose{<:Any,<:AbstractTriangular}, B::Adjoint{<:Any,<:RowVector}) = A * rvadjoint(B.parent)

@deprecate *(A::LQ,B::QR) A*Matrix(B)
@deprecate *(A::QR,B::LQ) A*Matrix(B)
@deprecate *(A::Adjoint{<:Any,<:LQ}, B::LQ) A*Matrix(B)
@deprecate *(A::LQ, B::Adjoint{<:Any,<:LQ}) A*Matrix(B)

# PR #25184. Use getproperty instead of getindex for Factorizations
function getindex(F::Factorization, s::Symbol)
    depwarn("`F[:$s]` is deprecated, use `F.$s` instead.", :getindex)
    return getproperty(F, s)
end
@deprecate getq(F::Factorization) F.Q

# Deprecate scaling
@deprecate scale!(A::AbstractArray, b::Number)                             rmul!(A, b)
@deprecate scale!(a::Number, B::AbstractArray)                             lmul!(a, B)
@deprecate scale!(A::AbstractMatrix, b::AbstractVector)                    rmul!(A, Diagonal(b))
@deprecate scale!(a::AbstractVector, B::AbstractMatrix)                    lmul!(Diagonal(a), B)
@deprecate scale!(C::AbstractMatrix, A::AbstractMatrix, b::AbstractVector) mul!(C, A, Diagonal(b))
@deprecate scale!(C::AbstractMatrix, a::AbstractVector, B::AbstractMatrix) mul!(C, Diagonal(a), B)

Base.@deprecate_binding trace tr
