# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: @deprecate, depwarn

# BEGIN 0.7 deprecations

# deprecate remaining vectorized methods over SparseVectors (zero-preserving)
for op in (:floor, :ceil, :trunc, :round,
        :log1p, :expm1,  :sinpi,
        :sin,   :tan,    :sind,   :tand,
        :asin,  :atan,   :asind,  :atand,
        :sinh,  :tanh,   :asinh,  :atanh)
    @eval import Base.Math: $op
    @eval @deprecate ($op)(x::AbstractSparseVector{<:Number,<:Integer}) ($op).(x)
end
# deprecate remaining vectorized methods over SparseVectors (not-zero-preserving)
for op in (:exp, :exp2, :exp10, :log, :log2, :log10,
           :cos, :cosd, :acos, :cosh, :cospi,
           :csc, :cscd, :acot, :csch, :acsch,
           :cot, :cotd, :acosd, :coth,
           :sec, :secd, :acotd, :sech, :asech)
    @eval import Base.Math: $op
    @eval @deprecate ($op)(x::AbstractSparseVector{<:Number,<:Integer}) ($op).(x)
end

@deprecate sparse(s::UniformScaling, m::Integer) sparse(s, m, m)

# PR #25037
@deprecate spones(A::SparseMatrixCSC) LinearAlgebra.fillstored!(copy(A), 1)
@deprecate spones(A::SparseVector)    LinearAlgebra.fillstored!(copy(A), 1)
export spones

# full for sparse arrays
import Base: full
function full(S::Union{SparseVector,SparseMatrixCSC})
    (arrtypestr, desttypestr) =
        isa(S, SparseVector)    ? ("SparseVector",    "Vector") :
        isa(S, SparseMatrixCSC) ? ("SparseMatrixCSC", "Matrix") :
            error("should not be reachable!")
    depwarn(string(
        "`full(S::$(arrtypestr))` (and `full` in general) has been deprecated. ",
        "To replace `full(S::$(arrtypestr))`, consider `$(desttypestr)(S)` or, ",
        "if that option is too narrow, `Array(S)`."), :full)
    return Array(S)
end

# issue #22849
import Base: reinterpret
@deprecate reinterpret(::Type{T}, a::SparseMatrixCSC{S}, dims::NTuple{N,Int}) where {T, S, N} reinterpret(T, reshape(a, dims))

# deprecate speye
export speye
function speye(n::Integer)
    depwarn(string("`speye(n::Integer)` has been deprecated in favor of `I`, `sparse`, and ",
                    "`SparseMatrixCSC` constructor methods. For a direct replacement, consider ",
                    "`sparse(1.0I, n, n)`, `SparseMatrixCSC(1.0I, n, n)`, or `SparseMatrixCSC{Float64}(I, n, n)`. ",
                    "If `Float64` element type is not necessary, consider the shorter `sparse(I, n, n)` ",
                    "or `SparseMatrixCSC(I, n, n)` (with default `eltype(I)` of `Bool`)."), :speye)
    return sparse(1.0I, n, n)
end
function speye(m::Integer, n::Integer)
    depwarn(string("`speye(m::Integer, n::Integer)` has been deprecated in favor of `I`, ",
                    "`sparse`, and `SparseMatrixCSC` constructor methods. For a direct ",
                    "replacement, consider `sparse(1.0I, m, n)`, `SparseMatrixCSC(1.0I, m, n)`, ",
                    "or `SparseMatrixCSC{Float64}(I, m, n)`. If `Float64` element type is not ",
                    " necessary, consider the shorter `sparse(I, m, n)` or `SparseMatrixCSC(I, m, n)` ",
                    "(with default `eltype(I)` of `Bool`)."), :speye)
    return sparse(1.0I, m, n)
end
function speye(::Type{T}, n::Integer) where T
    depwarn(string("`speye(T, n::Integer)` has been deprecated in favor of `I`, `sparse`, and ",
                    "`SparseMatrixCSC` constructor methods. For a direct replacement, consider ",
                    "`sparse(T(1)I, n, n)` if `T` is concrete or `SparseMatrixCSC{T}(I, n, n)` ",
                    "if `T` is either concrete or abstract. If element type `T` is not necessary, ",
                    "consider the shorter `sparse(I, n, n)` or `SparseMatrixCSC(I, n, n)` ",
                    "(with default `eltype(I)` of `Bool`)."), :speye)
    return SparseMatrixCSC{T}(I, n, n)
end
function speye(::Type{T}, m::Integer, n::Integer) where T
    depwarn(string("`speye(T, m::Integer, n::Integer)` has been deprecated in favor of `I`, ",
                    "`sparse`, and `SparseMatrixCSC` constructor methods. For a direct ",
                    "replacement, consider `sparse(T(1)I, m, n)` if `T` is concrete or ",
                    "`SparseMatrixCSC{T}(I, m, n)` if `T` is either concrete or abstract. ",
                    "If element type `T` is not necessary, consider the shorter ",
                    "`sparse(I, m, n)` or `SparseMatrixCSC(I, m, n)` (with default `eltype(I)` ",
                    "of `Bool`)."), :speye)
    return SparseMatrixCSC{T}(I, m, n)
end
function speye(S::SparseMatrixCSC{T}) where T
    depwarn(string("`speye(S::SparseMatrixCSC{T})` has been deprecated in favor of `I`, ",
                    "`sparse`, and `SparseMatrixCSC` constructor methods. For a direct ",
                    "replacement, consider `sparse(T(1)I, size(S)...)` if `T` is concrete or ",
                    "`SparseMatrixCSC{eltype(S)}(I, size(S))` if `T` is either concrete or abstract. ",
                    "If preserving element type `T` is not necessary, consider the shorter ",
                    "`sparse(I, size(S)...)` or `SparseMatrixCSC(I, size(S))` (with default ",
                    "`eltype(I)` of `Bool`)."), :speye)
    return SparseMatrixCSC{T}(I, size(S)...)
end

# methods involving RowVector from base/sparse/higherorderfns.jl, to deprecate
@eval SparseArrays.HigherOrderFns begin
    BroadcastStyle(::Type{<:RowVector{T,<:Vector}}) where T = Broadcast.MatrixStyle()
end

import Base: asyncmap
@deprecate asyncmap(f, s::AbstractSparseArray...; kwargs...) sparse(asyncmap(f, map(Array, s)...; kwargs...))

# PR 26347: implicit scalar broadcasting within setindex!
@deprecate setindex!(A::SparseMatrixCSC{<:Any,<:Any}, x, i::Union{Integer, AbstractVector{<:Integer}, Colon}, j::Union{Integer, AbstractVector{<:Integer}, Colon}) (A[i, j] .= x; A)

#25395 keywords unlocked
@deprecate dropzeros(x, trim)     dropzeros(x, trim = trim)
@deprecate dropzeros!(x, trim)    dropzeros!(x, trim = trim)
@deprecate droptol!(A, tol, trim) droptol!(A, tol, trim = trim)

Base.@deprecate_binding blkdiag blockdiag

@deprecate complex(x::AbstractSparseVector{<:Real}, y::AbstractSparseVector{<:Real}) complex.(x, y)
@deprecate complex(x::AbstractVector{<:Real}, y::AbstractSparseVector{<:Real}) complex.(x, y)
@deprecate complex(x::AbstractSparseVector{<:Real}, y::AbstractVector{<:Real}) complex.(x, y)

@deprecate diff(a::SparseMatrixCSC, dim::Integer) diff(a, dims=dim)

# END 0.7 deprecations

# BEGIN 1.0 deprecations

# END 1.0 deprecations
