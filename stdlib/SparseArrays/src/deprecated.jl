# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: @deprecate, depwarn

# BEGIN 0.7 deprecations

# PR #22475
import Base: cat
@deprecate cat(::Type{Val{N}}, A::_SparseConcatGroup...) where {N} cat(Val(N), A...)
@deprecate cat(::Type{Val{N}}, A::_DenseConcatGroup...) where {N} cat(Val(N), A...)

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

# PR 23341
import LinearAlgebra: diagm
@deprecate diagm(A::SparseMatrixCSC) sparse(Diagonal(sparsevec(A)))

# PR #23757
@deprecate spdiagm(x::AbstractVector) sparse(Diagonal(x))
function spdiagm(x::AbstractVector, d::Number)
    depwarn(string("`spdiagm(x::AbstractVector, d::Number)` is deprecated, use ",
        "`spdiagm(d => x)` instead, which now returns a square matrix. To preserve the old ",
        "behaviour, use `sparse(SparseArrays.spdiagm_internal(d => x)...)`"), :spdiagm)
    I, J, V = spdiagm_internal(d => x)
    return sparse(I, J, V)
end
function spdiagm(x, d)
    depwarn(string("`spdiagm((x1, x2, ...), (d1, d2, ...))` is deprecated, use ",
        "`spdiagm(d1 => x1, d2 => x2, ...)` instead, which now returns a square matrix. ",
        "To preserve the old behaviour, use ",
        "`sparse(SparseArrays.spdiagm_internal(d1 => x1, d2 => x2, ...)...)`"), :spdiagm)
    I, J, V = spdiagm_internal((d[i] => x[i] for i in 1:length(x))...)
    return sparse(I, J, V)
end
function spdiagm(x, d, m::Integer, n::Integer)
    depwarn(string("`spdiagm((x1, x2, ...), (d1, d2, ...), m, n)` is deprecated, use ",
        "`spdiagm(d1 => x1, d2 => x2, ...)` instead, which now returns a square matrix. ",
        "To specify a non-square matrix and preserve the old behaviour, use ",
        "`I, J, V = SparseArrays.spdiagm_internal(d1 => x1, d2 => x2, ...); sparse(I, J, V, m, n)`"), :spdiagm)
    I, J, V = spdiagm_internal((d[i] => x[i] for i in 1:length(x))...)
    return sparse(I, J, V, m, n)
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

# former imports into SparseArrays
import Base: Ac_mul_B, At_mul_B
import Base: A_mul_Bc, A_mul_Bt, Ac_mul_Bc, At_mul_Bt
import Base: At_ldiv_B, Ac_ldiv_B
import LinearAlgebra: A_mul_B!, Ac_mul_B!, At_mul_B!, A_ldiv_B!
import LinearAlgebra: At_ldiv_B!, Ac_ldiv_B!, A_rdiv_B!, A_rdiv_Bc!, mul!, ldiv!, rdiv!

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/sparse/linalg.jl, to deprecate
using LinearAlgebra: Adjoint, Transpose
@deprecate Ac_ldiv_B(A::SparseMatrixCSC, B::RowVector)  (\)(adjoint(A), B)
@deprecate At_ldiv_B(A::SparseMatrixCSC, B::RowVector)  (\)(transpose(A), B)
@deprecate Ac_ldiv_B(A::SparseMatrixCSC, B::AbstractVecOrMat)   (\)(adjoint(A), B)
@deprecate At_ldiv_B(A::SparseMatrixCSC, B::AbstractVecOrMat)   (\)(transpose(A), B)
@deprecate A_rdiv_Bc!(A::SparseMatrixCSC{T}, D::Diagonal{T}) where {T}  rdiv!(A, adjoint(D))
@deprecate A_rdiv_Bt!(A::SparseMatrixCSC{T}, D::Diagonal{T}) where {T}  rdiv!(A, transpose(D))
@deprecate A_rdiv_B!(A::SparseMatrixCSC{T}, D::Diagonal{T}) where {T}   rdiv!(A, D)
@deprecate A_ldiv_B!(L::LowerTriangular{T,<:SparseMatrixCSCUnion{T}}, B::StridedVecOrMat) where {T}     ldiv!(L, B)
@deprecate A_ldiv_B!(U::UpperTriangular{T,<:SparseMatrixCSCUnion{T}}, B::StridedVecOrMat) where {T}     ldiv!(U, B)
@deprecate A_mul_Bt(A::SparseMatrixCSC{Tv,Ti}, B::SparseMatrixCSC{Tv,Ti}) where {Tv,Ti}     (*)(A, transpose(B))
@deprecate A_mul_Bc(A::SparseMatrixCSC{Tv,Ti}, B::SparseMatrixCSC{Tv,Ti}) where {Tv,Ti}     (*)(A, adjoint(B))
@deprecate At_mul_B(A::SparseMatrixCSC{Tv,Ti}, B::SparseMatrixCSC{Tv,Ti}) where {Tv,Ti}     (*)(transpose(A), B)
@deprecate Ac_mul_B(A::SparseMatrixCSC{Tv,Ti}, B::SparseMatrixCSC{Tv,Ti}) where {Tv,Ti}     (*)(adjoint(A), B)
@deprecate At_mul_Bt(A::SparseMatrixCSC{Tv,Ti}, B::SparseMatrixCSC{Tv,Ti}) where {Tv,Ti}    (*)(transpose(A), transpose(B))
@deprecate Ac_mul_Bc(A::SparseMatrixCSC{Tv,Ti}, B::SparseMatrixCSC{Tv,Ti}) where {Tv,Ti}    (*)(adjoint(A), adjoint(B))
@deprecate A_mul_B!(C::StridedVecOrMat, A::SparseMatrixCSC, B::StridedVecOrMat)     mul!(C, A, B)
@deprecate Ac_mul_B!(C::StridedVecOrMat, A::SparseMatrixCSC, B::StridedVecOrMat)    mul!(C, adjoint(A), B)
@deprecate At_mul_B!(C::StridedVecOrMat, A::SparseMatrixCSC, B::StridedVecOrMat)    mul!(C, transpose(A), B)
@deprecate A_mul_B!(α::Number, A::SparseMatrixCSC, B::StridedVecOrMat, β::Number, C::StridedVecOrMat)   mul!(C, A, B, α, β)
@deprecate A_mul_B(A::SparseMatrixCSC{TA,S}, x::StridedVector{Tx}) where {TA,S,Tx}  (*)(A, x)
@deprecate A_mul_B(A::SparseMatrixCSC{TA,S}, B::StridedMatrix{Tx}) where {TA,S,Tx}  (*)(A, B)
@deprecate Ac_mul_B!(α::Number, A::SparseMatrixCSC, B::StridedVecOrMat, β::Number, C::StridedVecOrMat)  mul!(C, adjoint(A), B, α, β)
@deprecate Ac_mul_B(A::SparseMatrixCSC{TA,S}, x::StridedVector{Tx}) where {TA,S,Tx}     (*)(adjoint(A), x)
@deprecate Ac_mul_B(A::SparseMatrixCSC{TA,S}, B::StridedMatrix{Tx}) where {TA,S,Tx}     (*)(adjoint(A), B)
@deprecate At_mul_B!(α::Number, A::SparseMatrixCSC, B::StridedVecOrMat, β::Number, C::StridedVecOrMat)  mul!(C, transpose(A), B, α, β)
@deprecate At_mul_B(A::SparseMatrixCSC{TA,S}, x::StridedVector{Tx}) where {TA,S,Tx}     (*)(transpose(A), x)
@deprecate At_mul_B(A::SparseMatrixCSC{TA,S}, B::StridedMatrix{Tx}) where {TA,S,Tx}     (*)(transpose(A), B)
@deprecate A_mul_Bt(A::SparseMatrixCSC{TvA,TiA}, B::SparseMatrixCSC{TvB,TiB}) where {TvA,TiA,TvB,TiB}   (*)(A, transpose(B))
@deprecate A_mul_Bc(A::SparseMatrixCSC{TvA,TiA}, B::SparseMatrixCSC{TvB,TiB}) where {TvA,TiA,TvB,TiB}   (*)(A, adjoint(B))
@deprecate At_mul_B(A::SparseMatrixCSC{TvA,TiA}, B::SparseMatrixCSC{TvB,TiB}) where {TvA,TiA,TvB,TiB}   (*)(transpose(A), B)
@deprecate Ac_mul_B(A::SparseMatrixCSC{TvA,TiA}, B::SparseMatrixCSC{TvB,TiB}) where {TvA,TiA,TvB,TiB}   (*)(adjoint(A),B)
@deprecate At_mul_Bt(A::SparseMatrixCSC{TvA,TiA}, B::SparseMatrixCSC{TvB,TiB}) where {TvA,TiA,TvB,TiB}  (*)(transpose(A), transpose(B))
@deprecate Ac_mul_Bc(A::SparseMatrixCSC{TvA,TiA}, B::SparseMatrixCSC{TvB,TiB}) where {TvA,TiA,TvB,TiB}  (*)(adjoint(A), adjoint(B))

# A[ct]_(mul|ldiv|rdiv)_B[ct][!] methods from base/sparse/sparsevector.jl, to deprecate
for isunittri in (true, false), islowertri in (true, false)
    unitstr = isunittri ? "Unit" : ""
    halfstr = islowertri ? "Lower" : "Upper"
    tritype = :(LinearAlgebra.$(Symbol(unitstr, halfstr, "Triangular")))
    @eval #=Base.SparseArrays=# begin
        using LinearAlgebra: Adjoint, Transpose
        @deprecate At_ldiv_B(A::$tritype{TA,<:AbstractMatrix}, b::SparseVector{Tb}) where {TA<:Number,Tb<:Number}   (\)(transpose(A), b)
        @deprecate At_ldiv_B(A::$tritype{TA,<:StridedMatrix}, b::SparseVector{Tb}) where {TA<:Number,Tb<:Number}    (\)(transpose(A), b)
        @deprecate At_ldiv_B(A::$tritype, b::SparseVector)  (\)(transpose(A), b)
        @deprecate Ac_ldiv_B(A::$tritype{TA,<:AbstractMatrix}, b::SparseVector{Tb}) where {TA<:Number,Tb<:Number}   (\)(adjoint(A), b)
        @deprecate Ac_ldiv_B(A::$tritype{TA,<:StridedMatrix}, b::SparseVector{Tb}) where {TA<:Number,Tb<:Number}    (\)(adjoint(A), b)
        @deprecate Ac_ldiv_B(A::$tritype, b::SparseVector)  (\)(adjoint(A), b)
        @deprecate A_ldiv_B!(A::$tritype{<:Any,<:StridedMatrix}, b::SparseVector)   ldiv!(A, b)
        @deprecate At_ldiv_B!(A::$tritype{<:Any,<:StridedMatrix}, b::SparseVector)  ldiv!(transpose(A), b)
        @deprecate Ac_ldiv_B!(A::$tritype{<:Any,<:StridedMatrix}, b::SparseVector)  ldiv!(adjoint(A), b)
    end
end

using LinearAlgebra: Adjoint, Transpose
@deprecate Ac_mul_B(A::SparseMatrixCSC, x::AbstractSparseVector)    (*)(adjoint(A), x)
@deprecate At_mul_B(A::SparseMatrixCSC, x::AbstractSparseVector)    (*)(transpose(A), x)
@deprecate Ac_mul_B!(α::Number, A::SparseMatrixCSC, x::AbstractSparseVector, β::Number, y::StridedVector)   mul!(y, adjoint(A), x, α, β)
@deprecate Ac_mul_B!(y::StridedVector{Ty}, A::SparseMatrixCSC, x::AbstractSparseVector{Tx}) where {Tx,Ty}   mul!(y, adjoint(A), x)
@deprecate At_mul_B!(α::Number, A::SparseMatrixCSC, x::AbstractSparseVector, β::Number, y::StridedVector)   mul!(y, transpose(A), x, α, β)
@deprecate At_mul_B!(y::StridedVector{Ty}, A::SparseMatrixCSC, x::AbstractSparseVector{Tx}) where {Tx,Ty}   mul!(y, transpose(A), x)
@deprecate A_mul_B!(α::Number, A::SparseMatrixCSC, x::AbstractSparseVector, β::Number, y::StridedVector)    mul!(y, A, x, α, β)
@deprecate A_mul_B!(y::StridedVector{Ty}, A::SparseMatrixCSC, x::AbstractSparseVector{Tx}) where {Tx,Ty}    mul!(y, A, x)
@deprecate At_mul_B!(α::Number, A::StridedMatrix, x::AbstractSparseVector, β::Number, y::StridedVector)     mul!(y, transpose(A), x, α, β)
@deprecate At_mul_B!(y::StridedVector{Ty}, A::StridedMatrix, x::AbstractSparseVector{Tx}) where {Tx,Ty}     mul!(y, transpose(A), x)
@deprecate At_mul_B(A::StridedMatrix{Ta}, x::AbstractSparseVector{Tx}) where {Ta,Tx}    (*)(transpose(A), x)
@deprecate A_mul_B!(α::Number, A::StridedMatrix, x::AbstractSparseVector, β::Number, y::StridedVector)  mul!(y, A, x, α, β)
@deprecate A_mul_B!(y::StridedVector{Ty}, A::StridedMatrix, x::AbstractSparseVector{Tx}) where {Tx,Ty}  mul!(y, A, x)

# methods involving RowVector from base/sparse/linalg.jl, to deprecate
\(::SparseMatrixCSC, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
\(::Adjoint{<:Any,<:SparseMatrixCSC}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))
\(::Transpose{<:Any,<:SparseMatrixCSC}, ::RowVector) = throw(DimensionMismatch("Cannot left-divide matrix by transposed vector"))

# methods involving RowVector from base/sparse/higherorderfns.jl, to deprecate
@eval SparseArrays.HigherOrderFns begin
    BroadcastStyle(::Type{<:RowVector{T,<:Vector}}) where T = Broadcast.MatrixStyle()
end

import Base: asyncmap
@deprecate asyncmap(f, s::AbstractSparseArray...; kwargs...) sparse(asyncmap(f, map(Array, s)...; kwargs...))

#25395 keywords unlocked
@deprecate dropzeros(x, trim)     dropzeros(x, trim = trim)
@deprecate dropzeros!(x, trim)    dropzeros!(x, trim = trim)
@deprecate droptol!(A, tol, trim) droptol!(A, tol, trim = trim)

Base.@deprecate_binding blkdiag blockdiag

@deprecate complex(x::AbstractSparseVector{<:Real}, y::AbstractSparseVector{<:Real}) complex.(x, y)
@deprecate complex(x::AbstractVector{<:Real}, y::AbstractSparseVector{<:Real}) complex.(x, y)
@deprecate complex(x::AbstractSparseVector{<:Real}, y::AbstractVector{<:Real}) complex.(x, y)


# END 0.7 deprecations

# BEGIN 1.0 deprecations

# END 1.0 deprecations
