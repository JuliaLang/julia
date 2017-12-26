# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: @pure, @propagate_inbounds, _return_type, _default_type, _isleaftype, @_inline_meta
import Base: length, size, axes, IndexStyle, getindex, setindex!, parent, vec, convert, similar

### basic definitions (types, aliases, constructors, abstractarray interface, sundry similar)

# note that Adjoint and Transpose must be able to wrap not only vectors and matrices
# but also factorizations, rotations, and other linear algebra objects, including
# user-defined such objects. so do not restrict the wrapped type.
struct Adjoint{T,S} <: AbstractMatrix{T}
    parent::S
    function Adjoint{T,S}(A::S) where {T,S}
        checkeltype(Adjoint, T, eltype(A))
        new(A)
    end
end
struct Transpose{T,S} <: AbstractMatrix{T}
    parent::S
    function Transpose{T,S}(A::S) where {T,S}
        checkeltype(Transpose, T, eltype(A))
        new(A)
    end
end

function checkeltype(::Type{Transform}, ::Type{ResultEltype}, ::Type{ParentEltype}) where {Transform, ResultEltype, ParentEltype}
    if ResultEltype !== transformtype(Transform, ParentEltype)
        error(string("Element type mismatch. Tried to create an `$Transform{$ResultEltype}` ",
            "from an object with eltype `$ParentEltype`, but the element type of the ",
            "`$Transform` of an object with eltype `$ParentEltype` must be ",
            "`$(transformtype(Transform, ParentEltype))`"))
    end
    return nothing
end
function transformtype(::Type{O}, ::Type{S}) where {O,S}
    # similar to promote_op(::Any, ::Type)
    @_inline_meta
    T = _return_type(O, Tuple{_default_type(S)})
    _isleaftype(S) && return _isleaftype(T) ? T : Any
    return typejoin(S, T)
end

# basic outer constructors
Adjoint(A) = Adjoint{transformtype(Adjoint,eltype(A)),typeof(A)}(A)
Transpose(A) = Transpose{transformtype(Transpose,eltype(A)),typeof(A)}(A)

# numbers are the end of the line
Adjoint(x::Number) = adjoint(x)
Transpose(x::Number) = transpose(x)

# unwrapping constructors
Adjoint(A::Adjoint) = A.parent
Transpose(A::Transpose) = A.parent
# normalizing unwrapping constructors
# technically suspect, but at least fine for now
Adjoint(A::Transpose) = conj(A.parent)
Transpose(A::Adjoint) = conj(A.parent)

# eager lowercase quasi-constructors, unwrapping
adjoint(A::Adjoint) = copy(A.parent)
transpose(A::Transpose) = copy(A.parent)
# eager lowercase quasi-constructors, normalizing
# technically suspect, but at least fine for now
adjoint(A::Transpose) = conj!(copy(A.parent))
transpose(A::Adjoint) = conj!(copy(A.parent))

# lowercase quasi-constructors for vectors, TODO: deprecate
adjoint(sv::AbstractVector) = Adjoint(sv)
transpose(sv::AbstractVector) = Transpose(sv)


# some aliases for internal convenience use
const AdjOrTrans{T,S} = Union{Adjoint{T,S},Transpose{T,S}} where {T,S}
const AdjointAbsVec{T} = Adjoint{T,<:AbstractVector}
const TransposeAbsVec{T} = Transpose{T,<:AbstractVector}
const AdjOrTransAbsVec{T} = AdjOrTrans{T,<:AbstractVector}
const AdjOrTransAbsMat{T} = AdjOrTrans{T,<:AbstractMatrix}

# for internal use below
wrappertype(A::Adjoint) = Adjoint
wrappertype(A::Transpose) = Transpose
wrappertype(::Type{<:Adjoint}) = Adjoint
wrappertype(::Type{<:Transpose}) = Transpose

# AbstractArray interface, basic definitions
length(A::AdjOrTrans) = length(A.parent)
size(v::AdjOrTransAbsVec) = (1, length(v.parent))
size(A::AdjOrTransAbsMat) = reverse(size(A.parent))
axes(v::AdjOrTransAbsVec) = (Base.OneTo(1), axes(v.parent)...)
axes(A::AdjOrTransAbsMat) = reverse(axes(A.parent))
IndexStyle(::Type{<:AdjOrTransAbsVec}) = IndexLinear()
IndexStyle(::Type{<:AdjOrTransAbsMat}) = IndexCartesian()
@propagate_inbounds getindex(v::AdjOrTransAbsVec, i::Int) = wrappertype(v)(v.parent[i])
@propagate_inbounds getindex(A::AdjOrTransAbsMat, i::Int, j::Int) = wrappertype(A)(A.parent[j, i])
@propagate_inbounds setindex!(v::AdjOrTransAbsVec, x, i::Int) = (setindex!(v.parent, wrappertype(v)(x), i); v)
@propagate_inbounds setindex!(A::AdjOrTransAbsMat, x, i::Int, j::Int) = (setindex!(A.parent, wrappertype(A)(x), j, i); A)
# AbstractArray interface, additional definitions to retain wrapper over vectors where appropriate
@propagate_inbounds getindex(v::AdjOrTransAbsVec, ::Colon, is::AbstractArray{Int}) = wrappertype(v)(v.parent[is])
@propagate_inbounds getindex(v::AdjOrTransAbsVec, ::Colon, ::Colon) = wrappertype(v)(v.parent[:])

# conversion of underlying storage
convert(::Type{Adjoint{T,S}}, A::Adjoint) where {T,S} = Adjoint{T,S}(convert(S, A.parent))
convert(::Type{Transpose{T,S}}, A::Transpose) where {T,S} = Transpose{T,S}(convert(S, A.parent))

# for vectors, the semantics of the wrapped and unwrapped types differ
# so attempt to maintain both the parent and wrapper type insofar as possible
similar(A::AdjOrTransAbsVec) = wrappertype(A)(similar(A.parent))
similar(A::AdjOrTransAbsVec, ::Type{T}) where {T} = wrappertype(A)(similar(A.parent, transformtype(wrappertype(A), T)))
# for matrices, the semantics of the wrapped and unwrapped types are generally the same
# and as you are allocating with similar anyway, you might as well get something unwrapped
similar(A::AdjOrTrans) = similar(A.parent, eltype(A), size(A))
similar(A::AdjOrTrans, ::Type{T}) where {T} = similar(A.parent, T, size(A))
similar(A::AdjOrTrans, ::Type{T}, dims::Dims{N}) where {T,N} = similar(A.parent, T, dims)

# sundry basic definitions
parent(A::AdjOrTrans) = A.parent
vec(v::AdjOrTransAbsVec) = v.parent


### concatenation
# preserve Adjoint/Transpose wrapper around vectors
# to retain the associated semantics post-concatenation
hcat(avs::Union{Number,AdjointAbsVec}...) = _adjoint_hcat(avs...)
hcat(tvs::Union{Number,TransposeAbsVec}...) = _transpose_hcat(tvs...)
_adjoint_hcat(avs::Union{Number,AdjointAbsVec}...) = Adjoint(vcat(map(Adjoint, avs)...))
_transpose_hcat(tvs::Union{Number,TransposeAbsVec}...) = Transpose(vcat(map(Transpose, tvs)...))
typed_hcat(::Type{T}, avs::Union{Number,AdjointAbsVec}...) where {T} = Adjoint(typed_vcat(T, map(Adjoint, avs)...))
typed_hcat(::Type{T}, tvs::Union{Number,TransposeAbsVec}...) where {T} = Transpose(typed_vcat(T, map(Transpose, tvs)...))
# otherwise-redundant definitions necessary to prevent hitting the concat methods in sparse/sparsevector.jl
hcat(avs::Adjoint{<:Any,<:Vector}...) = _adjoint_hcat(avs...)
hcat(tvs::Transpose{<:Any,<:Vector}...) = _transpose_hcat(tvs...)
hcat(avs::Adjoint{T,Vector{T}}...) where {T} = _adjoint_hcat(avs...)
hcat(tvs::Transpose{T,Vector{T}}...) where {T} = _transpose_hcat(tvs...)


### higher order functions
# preserve Adjoint/Transpose wrapper around vectors
# to retain the associated semantics post-map/broadcast

# vectorfy takes an Adoint/Transpose-wrapped vector and builds
# an unwrapped vector with the entrywise-same contents
vectorfy(x::Number) = x
vectorfy(adjvec::AdjointAbsVec) = map(Adjoint, adjvec.parent)
vectorfy(transvec::TransposeAbsVec) = map(Transpose, transvec.parent)
vectorfyall(transformedvecs...) = (map(vectorfy, transformedvecs)...,)

# map over collections of Adjoint/Transpose-wrapped vectors
# note that the caller's operation `f` should be applied to the entries of the wrapped
# vectors, rather than the entires of the wrapped vector's parents. so first we use vectorfy
# to build unwrapped vectors with entrywise-same contents as the wrapped input vectors.
# then we map the caller's operation over that set of unwrapped vectors. but now re-wrapping
# the resulting vector would inappropriately transform the result vector's entries. so
# instead of simply mapping the caller's operation over the set of unwrapped vectors,
# we map Adjoint/Transpose composed with the caller's operationt over the set of unwrapped
# vectors. then re-wrapping the result vector yields a wrapped vector with the correct entries.
map(f, avs::AdjointAbsVec...) = Adjoint(map(Adjoint∘f, vectorfyall(avs...)...))
map(f, tvs::TransposeAbsVec...) = Transpose(map(Transpose∘f, vectorfyall(tvs...)...))

# broadcast over collections of Adjoint/Transpose-wrapped vectors and numbers
# similar explanation for these definitions as for map above
broadcast(f, avs::Union{Number,AdjointAbsVec}...) = Adjoint(broadcast(Adjoint∘f, vectorfyall(avs...)...))
broadcast(f, tvs::Union{Number,TransposeAbsVec}...) = Transpose(broadcast(Transpose∘f, vectorfyall(tvs...) ...))


### linear algebra

## multiplication *

# Adjoint/Transpose-vector * vector
*(u::AdjointAbsVec, v::AbstractVector) = dot(u.parent, v)
*(u::TransposeAbsVec{T}, v::AbstractVector{T}) where {T<:Real} = dot(u.parent, v)
function *(u::TransposeAbsVec, v::AbstractVector)
    @boundscheck length(u) == length(v) || throw(DimensionMismatch())
    return sum(@inbounds(return u[k]*v[k]) for k in 1:length(u))
end
# vector * Adjoint/Transpose-vector
*(u::AbstractVector, v::AdjOrTransAbsVec) = broadcast(*, u, v)
# Adjoint/Transpose-vector * Adjoint/Transpose-vector
# (necessary for disambiguation with fallback methods in linalg/matmul)
*(u::AdjointAbsVec, v::AdjointAbsVec) = throw(MethodError(*, (u, v)))
*(u::TransposeAbsVec, v::TransposeAbsVec) = throw(MethodError(*, (u, v)))

# Adjoint/Transpose-vector * matrix
*(u::AdjointAbsVec, A::AbstractMatrix) = Adjoint(Adjoint(A) * u.parent)
*(u::TransposeAbsVec, A::AbstractMatrix) = Transpose(Transpose(A) * u.parent)
# Adjoint/Transpose-vector * Adjoint/Transpose-matrix
*(u::AdjointAbsVec, A::Adjoint{<:Any,<:AbstractMatrix}) = Adjoint(A.parent * u.parent)
*(u::TransposeAbsVec, A::Transpose{<:Any,<:AbstractMatrix}) = Transpose(A.parent * u.parent)


## pseudoinversion
pinv(v::AdjointAbsVec, tol::Real = 0) = pinv(v.parent, tol).parent
pinv(v::TransposeAbsVec, tol::Real = 0) = pinv(conj(v.parent)).parent


## left-division \
\(u::AdjOrTransAbsVec, v::AdjOrTransAbsVec) = pinv(u) * v


## right-division \
/(u::AdjointAbsVec, A::AbstractMatrix) = Adjoint(Adjoint(A) \ u.parent)
/(u::TransposeAbsVec, A::AbstractMatrix) = Transpose(Transpose(A) \ u.parent)


# dismabiguation methods
*(A::AdjointAbsVec, B::Transpose{<:Any,<:AbstractMatrix}) = A * transpose(B.parent)
*(A::TransposeAbsVec, B::Adjoint{<:Any,<:AbstractMatrix}) = A * adjoint(B.parent)
*(A::Transpose{<:Any,<:AbstractMatrix}, B::Adjoint{<:Any,<:AbstractMatrix}) = transpose(A.parent) * B
*(A::Adjoint{<:Any,<:AbstractMatrix}, B::Transpose{<:Any,<:AbstractMatrix}) = A * transpose(B.parent)
# Adj/Trans-vector * Trans/Adj-vector, shouldn't exist, here for ambiguity resolution? TODO: test removal
*(A::Adjoint{<:Any,<:AbstractVector}, B::Transpose{<:Any,<:AbstractVector}) = throw(MethodError(*, (A, B)))
*(A::Transpose{<:Any,<:AbstractVector}, B::Adjoint{<:Any,<:AbstractVector}) = throw(MethodError(*, (A, B)))
# Adj/Trans-matrix * Trans/Adj-vector, shouldn't exist, here for ambiguity resolution? TODO: test removal
*(A::Adjoint{<:Any,<:AbstractMatrix}, B::Adjoint{<:Any,<:AbstractVector}) = throw(MethodError(*, (A, B)))
*(A::Adjoint{<:Any,<:AbstractMatrix}, B::Transpose{<:Any,<:AbstractVector}) = throw(MethodError(*, (A, B)))
*(A::Transpose{<:Any,<:AbstractMatrix}, B::Adjoint{<:Any,<:AbstractVector}) = throw(MethodError(*, (A, B)))
