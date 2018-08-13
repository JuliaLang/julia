# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: @propagate_inbounds, _return_type, _default_type, @_inline_meta
import Base: length, size, axes, IndexStyle, getindex, setindex!, parent, vec, convert, similar

### basic definitions (types, aliases, constructors, abstractarray interface, sundry similar)

# note that Adjoint and Transpose must be able to wrap not only vectors and matrices
# but also factorizations, rotations, and other linear algebra objects, including
# user-defined such objects. so do not restrict the wrapped type.
struct Adjoint{T,S} <: AbstractMatrix{T}
    parent::S
    function Adjoint{T,S}(A::S) where {T,S}
        checkeltype_adjoint(T, eltype(A))
        new(A)
    end
end
struct Transpose{T,S} <: AbstractMatrix{T}
    parent::S
    function Transpose{T,S}(A::S) where {T,S}
        checkeltype_transpose(T, eltype(A))
        new(A)
    end
end

function checkeltype_adjoint(::Type{ResultEltype}, ::Type{ParentEltype}) where {ResultEltype,ParentEltype}
    Expected = Base.promote_op(adjoint, ParentEltype)
    ResultEltype === Expected || error(string(
        "Element type mismatch. Tried to create an `Adjoint{", ResultEltype, "}` ",
        "from an object with eltype `", ParentEltype, "`, but the element type of ",
        "the adjoint of an object with eltype `", ParentEltype, "` must be ",
        "`", Expected, "`."))
    return nothing
end

function checkeltype_transpose(::Type{ResultEltype}, ::Type{ParentEltype}) where {ResultEltype, ParentEltype}
    Expected = Base.promote_op(transpose, ParentEltype)
    ResultEltype === Expected || error(string(
        "Element type mismatch. Tried to create a `Transpose{", ResultEltype, "}` ",
        "from an object with eltype `", ParentEltype, "`, but the element type of ",
        "the transpose of an object with eltype `", ParentEltype, "` must be ",
        "`", Expected, "`."))
    return nothing
end

# basic outer constructors
Adjoint(A) = Adjoint{Base.promote_op(adjoint,eltype(A)),typeof(A)}(A)
Transpose(A) = Transpose{Base.promote_op(transpose,eltype(A)),typeof(A)}(A)

Base.dataids(A::Union{Adjoint, Transpose}) = Base.dataids(A.parent)
Base.unaliascopy(A::Union{Adjoint,Transpose}) = typeof(A)(Base.unaliascopy(A.parent))

# wrapping lowercase quasi-constructors
"""
    adjoint(A)

Lazy adjoint (conjugate transposition) (also postfix `'`).
Note that `adjoint` is applied recursively to elements.

This operation is intended for linear algebra usage - for general data manipulation see
[`permutedims`](@ref Base.permutedims).

# Examples
```jldoctest
julia> A = [3+2im 9+2im; 8+7im  4+6im]
2×2 Array{Complex{Int64},2}:
 3+2im  9+2im
 8+7im  4+6im

julia> adjoint(A)
2×2 Adjoint{Complex{Int64},Array{Complex{Int64},2}}:
 3-2im  8-7im
 9-2im  4-6im
```
"""
adjoint(A::AbstractVecOrMat) = Adjoint(A)

"""
    transpose(A)

Lazy transpose. Mutating the returned object should appropriately mutate `A`. Often,
but not always, yields `Transpose(A)`, where `Transpose` is a lazy transpose wrapper. Note
that this operation is recursive.

This operation is intended for linear algebra usage - for general data manipulation see
[`permutedims`](@ref Base.permutedims), which is non-recursive.

# Examples
```jldoctest
julia> A = [3+2im 9+2im; 8+7im  4+6im]
2×2 Array{Complex{Int64},2}:
 3+2im  9+2im
 8+7im  4+6im

julia> transpose(A)
2×2 Transpose{Complex{Int64},Array{Complex{Int64},2}}:
 3+2im  8+7im
 9+2im  4+6im
```
"""
transpose(A::AbstractVecOrMat) = Transpose(A)

# unwrapping lowercase quasi-constructors
adjoint(A::Adjoint) = A.parent
transpose(A::Transpose) = A.parent
adjoint(A::Transpose{<:Real}) = A.parent
transpose(A::Adjoint{<:Real}) = A.parent


# some aliases for internal convenience use
const AdjOrTrans{T,S} = Union{Adjoint{T,S},Transpose{T,S}} where {T,S}
const AdjointAbsVec{T} = Adjoint{T,<:AbstractVector}
const TransposeAbsVec{T} = Transpose{T,<:AbstractVector}
const AdjOrTransAbsVec{T} = AdjOrTrans{T,<:AbstractVector}
const AdjOrTransAbsMat{T} = AdjOrTrans{T,<:AbstractMatrix}

# for internal use below
wrapperop(A::Adjoint) = adjoint
wrapperop(A::Transpose) = transpose

# AbstractArray interface, basic definitions
length(A::AdjOrTrans) = length(A.parent)
size(v::AdjOrTransAbsVec) = (1, length(v.parent))
size(A::AdjOrTransAbsMat) = reverse(size(A.parent))
axes(v::AdjOrTransAbsVec) = (Base.OneTo(1), axes(v.parent)...)
axes(A::AdjOrTransAbsMat) = reverse(axes(A.parent))
IndexStyle(::Type{<:AdjOrTransAbsVec}) = IndexLinear()
IndexStyle(::Type{<:AdjOrTransAbsMat}) = IndexCartesian()
@propagate_inbounds getindex(v::AdjOrTransAbsVec, i::Int) = wrapperop(v)(v.parent[i-1+first(axes(v.parent)[1])])
@propagate_inbounds getindex(A::AdjOrTransAbsMat, i::Int, j::Int) = wrapperop(A)(A.parent[j, i])
@propagate_inbounds setindex!(v::AdjOrTransAbsVec, x, i::Int) = (setindex!(v.parent, wrapperop(v)(x), i-1+first(axes(v.parent)[1])); v)
@propagate_inbounds setindex!(A::AdjOrTransAbsMat, x, i::Int, j::Int) = (setindex!(A.parent, wrapperop(A)(x), j, i); A)
# AbstractArray interface, additional definitions to retain wrapper over vectors where appropriate
@propagate_inbounds getindex(v::AdjOrTransAbsVec, ::Colon, is::AbstractArray{Int}) = wrapperop(v)(v.parent[is])
@propagate_inbounds getindex(v::AdjOrTransAbsVec, ::Colon, ::Colon) = wrapperop(v)(v.parent[:])

# conversion of underlying storage
convert(::Type{Adjoint{T,S}}, A::Adjoint) where {T,S} = Adjoint{T,S}(convert(S, A.parent))
convert(::Type{Transpose{T,S}}, A::Transpose) where {T,S} = Transpose{T,S}(convert(S, A.parent))

# for vectors, the semantics of the wrapped and unwrapped types differ
# so attempt to maintain both the parent and wrapper type insofar as possible
similar(A::AdjOrTransAbsVec) = wrapperop(A)(similar(A.parent))
similar(A::AdjOrTransAbsVec, ::Type{T}) where {T} = wrapperop(A)(similar(A.parent, Base.promote_op(wrapperop(A), T)))
# for matrices, the semantics of the wrapped and unwrapped types are generally the same
# and as you are allocating with similar anyway, you might as well get something unwrapped
similar(A::AdjOrTrans) = similar(A.parent, eltype(A), axes(A))
similar(A::AdjOrTrans, ::Type{T}) where {T} = similar(A.parent, T, axes(A))
similar(A::AdjOrTrans, ::Type{T}, dims::Dims{N}) where {T,N} = similar(A.parent, T, dims)

# sundry basic definitions
parent(A::AdjOrTrans) = A.parent
vec(v::TransposeAbsVec) = parent(v)

cmp(A::AdjOrTransAbsVec, B::AdjOrTransAbsVec) = cmp(parent(A), parent(B))
isless(A::AdjOrTransAbsVec, B::AdjOrTransAbsVec) = isless(parent(A), parent(B))

### concatenation
# preserve Adjoint/Transpose wrapper around vectors
# to retain the associated semantics post-concatenation
hcat(avs::Union{Number,AdjointAbsVec}...) = _adjoint_hcat(avs...)
hcat(tvs::Union{Number,TransposeAbsVec}...) = _transpose_hcat(tvs...)
_adjoint_hcat(avs::Union{Number,AdjointAbsVec}...) = adjoint(vcat(map(adjoint, avs)...))
_transpose_hcat(tvs::Union{Number,TransposeAbsVec}...) = transpose(vcat(map(transpose, tvs)...))
typed_hcat(::Type{T}, avs::Union{Number,AdjointAbsVec}...) where {T} = adjoint(typed_vcat(T, map(adjoint, avs)...))
typed_hcat(::Type{T}, tvs::Union{Number,TransposeAbsVec}...) where {T} = transpose(typed_vcat(T, map(transpose, tvs)...))
# otherwise-redundant definitions necessary to prevent hitting the concat methods in sparse/sparsevector.jl
hcat(avs::Adjoint{<:Any,<:Vector}...) = _adjoint_hcat(avs...)
hcat(tvs::Transpose{<:Any,<:Vector}...) = _transpose_hcat(tvs...)
hcat(avs::Adjoint{T,Vector{T}}...) where {T} = _adjoint_hcat(avs...)
hcat(tvs::Transpose{T,Vector{T}}...) where {T} = _transpose_hcat(tvs...)
# TODO unify and allow mixed combinations


### higher order functions
# preserve Adjoint/Transpose wrapper around vectors
# to retain the associated semantics post-map/broadcast
#
# note that the caller's operation f operates in the domain of the wrapped vectors' entries.
# hence the adjoint->f->adjoint shenanigans applied to the parent vectors' entries.
map(f, avs::AdjointAbsVec...) = adjoint(map((xs...) -> adjoint(f(adjoint.(xs)...)), parent.(avs)...))
map(f, tvs::TransposeAbsVec...) = transpose(map((xs...) -> transpose(f(transpose.(xs)...)), parent.(tvs)...))
quasiparentt(x) = parent(x); quasiparentt(x::Number) = x # to handle numbers in the defs below
quasiparenta(x) = parent(x); quasiparenta(x::Number) = conj(x) # to handle numbers in the defs below
broadcast(f, avs::Union{Number,AdjointAbsVec}...) = adjoint(broadcast((xs...) -> adjoint(f(adjoint.(xs)...)), quasiparenta.(avs)...))
broadcast(f, tvs::Union{Number,TransposeAbsVec}...) = transpose(broadcast((xs...) -> transpose(f(transpose.(xs)...)), quasiparentt.(tvs)...))
# TODO unify and allow mixed combinations

### linear algebra

(-)(A::Adjoint)   = Adjoint(  -A.parent)
(-)(A::Transpose) = Transpose(-A.parent)

## multiplication *

# Adjoint/Transpose-vector * vector
*(u::AdjointAbsVec, v::AbstractVector) = dot(u.parent, v)
*(u::TransposeAbsVec{T}, v::AbstractVector{T}) where {T<:Real} = dot(u.parent, v)
function *(u::TransposeAbsVec, v::AbstractVector)
    @assert !has_offset_axes(u, v)
    @boundscheck length(u) == length(v) || throw(DimensionMismatch())
    return sum(@inbounds(u[k]*v[k]) for k in 1:length(u))
end
# vector * Adjoint/Transpose-vector
*(u::AbstractVector, v::AdjOrTransAbsVec) = broadcast(*, u, v)
# Adjoint/Transpose-vector * Adjoint/Transpose-vector
# (necessary for disambiguation with fallback methods in linalg/matmul)
*(u::AdjointAbsVec, v::AdjointAbsVec) = throw(MethodError(*, (u, v)))
*(u::TransposeAbsVec, v::TransposeAbsVec) = throw(MethodError(*, (u, v)))

# AdjOrTransAbsVec{<:Any,<:AdjOrTransAbsVec} is a lazy conj vectors
# We need to expand the combinations to avoid ambiguities
(*)(u::TransposeAbsVec, v::AdjointAbsVec{<:Any,<:TransposeAbsVec}) =
    sum(uu*vv for (uu, vv) in zip(u, v))
(*)(u::AdjointAbsVec,   v::AdjointAbsVec{<:Any,<:TransposeAbsVec}) =
    sum(uu*vv for (uu, vv) in zip(u, v))
(*)(u::TransposeAbsVec, v::TransposeAbsVec{<:Any,<:AdjointAbsVec}) =
    sum(uu*vv for (uu, vv) in zip(u, v))
(*)(u::AdjointAbsVec,   v::TransposeAbsVec{<:Any,<:AdjointAbsVec}) =
    sum(uu*vv for (uu, vv) in zip(u, v))

## pseudoinversion
pinv(v::AdjointAbsVec, tol::Real = 0) = pinv(v.parent, tol).parent
pinv(v::TransposeAbsVec, tol::Real = 0) = pinv(conj(v.parent)).parent


## left-division \
\(u::AdjOrTransAbsVec, v::AdjOrTransAbsVec) = pinv(u) * v


## right-division \
/(u::AdjointAbsVec, A::AbstractMatrix) = adjoint(adjoint(A) \ u.parent)
/(u::TransposeAbsVec, A::AbstractMatrix) = transpose(transpose(A) \ u.parent)
/(u::AdjointAbsVec, A::Transpose{<:Any,<:AbstractMatrix}) = adjoint(conj(A.parent) \ u.parent) # technically should be adjoint(copy(adjoint(copy(A))) \ u.parent)
/(u::TransposeAbsVec, A::Adjoint{<:Any,<:AbstractMatrix}) = transpose(conj(A.parent) \ u.parent) # technically should be transpose(copy(transpose(copy(A))) \ u.parent)
