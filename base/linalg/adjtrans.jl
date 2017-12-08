# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: @pure, @propagate_inbounds, _return_type, _default_type, _isleaftype, @_inline_meta
import Base: length, size, indices, IndexStyle, getindex, setindex!, parent, vec, convert, similar

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

@pure function checkeltype(::Type{Transform}, ::Type{ResultEltype}, ::Type{ParentEltype}) where {Transform, ResultEltype, ParentEltype}
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
# perhaps slightly odd, but necessary (at least till adjoint and transpose names are free)
Adjoint(A::Adjoint) = A.parent
Transpose(A::Transpose) = A.parent

# some aliases for internal convenience use
const AdjOrTrans{T,S} = Union{Adjoint{T,S},Transpose{T,S}} where {T,S}
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
indices(v::AdjOrTransAbsVec) = (Base.OneTo(1), indices(v.parent)...)
indices(A::AdjOrTransAbsMat) = reverse(indices(A.parent))
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


### linear algebra

# definitions necessary for test/linalg/rowvector.jl to pass
# should be cleaned up / revised as necessary in the future
/(A::Transpose{<:Any,<:Vector}, B::Matrix) = /(transpose(A.parent), B)
/(A::Transpose{<:Any,<:Vector}, B::Transpose{<:Any,<:Matrix}) = /(transpose(A.parent), B)
*(A::Adjoint{<:Any,<:Matrix}, B::Adjoint{<:Any,<:Vector}) = *(adjoint(A.parent), adjoint(B.parent))


# dismabiguation methods
*(A::Transpose{<:Any,<:AbstractVector}, B::Adjoint{<:Any,<:AbstractVector}) = transpose(A.parent) * B
*(A::Transpose{<:Any,<:AbstractVector}, B::Adjoint{<:Any,<:AbstractMatrix}) = transpose(A.parent) * B
*(A::Transpose{<:Any,<:AbstractMatrix}, B::Adjoint{<:Any,<:AbstractVector}) = A * adjoint(B.parent)
*(A::Transpose{<:Any,<:AbstractMatrix}, B::Adjoint{<:Any,<:AbstractMatrix}) = transpose(A.parent) * B
*(A::Adjoint{<:Any,<:AbstractVector}, B::Transpose{<:Any,<:AbstractVector}) = adjoint(A.parent) * B
*(A::Adjoint{<:Any,<:AbstractVector}, B::Transpose{<:Any,<:AbstractMatrix}) = adjoint(A.parent) * B
*(A::Adjoint{<:Any,<:AbstractMatrix}, B::Adjoint{<:Any,<:AbstractVector}) = A * adjoint(B.parent)
*(A::Adjoint{<:Any,<:AbstractMatrix}, B::Transpose{<:Any,<:AbstractVector}) = A * transpose(B.parent)
*(A::Adjoint{<:Any,<:AbstractMatrix}, B::Transpose{<:Any,<:AbstractMatrix}) = adjoint(A.parent) * B