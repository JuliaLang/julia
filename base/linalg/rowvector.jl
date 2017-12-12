# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    RowVector(vector)

A lazy-view wrapper of an [`AbstractVector`](@ref), which turns a length-`n` vector into a `1×n`
shaped row vector and represents the transpose of a vector (the elements are also transposed
recursively). This type is usually constructed (and unwrapped) via the [`transpose`](@ref)
function or `.'` operator (or related [`adjoint`](@ref) or `'` operator).

By convention, a vector can be multiplied by a matrix on its left (`A * v`) whereas a row
vector can be multiplied by a matrix on its right (such that `v.' * A = (A.' * v).'`). It
differs from a `1×n`-sized matrix by the facts that its transpose returns a vector and the
inner product `v1.' * v2` returns a scalar, but will otherwise behave similarly.

# Examples
```jldoctest
julia> a = [1; 2; 3; 4]
4-element Array{Int64,1}:
 1
 2
 3
 4

julia> RowVector(a)
1×4 RowVector{Int64,Array{Int64,1}}:
 1  2  3  4

julia> a.'
1×4 RowVector{Int64,Array{Int64,1}}:
 1  2  3  4

julia> a.'[3]
3

julia> a.'[1,3]
3

julia> a.'[3,1]
ERROR: BoundsError: attempt to access 1×4 RowVector{Int64,Array{Int64,1}} at index [3, 1]
[...]

julia> a.'*a
30

julia> B = [1 2; 3 4; 5 6; 7 8]
4×2 Array{Int64,2}:
 1  2
 3  4
 5  6
 7  8

julia> a.'*B
1×2 RowVector{Int64,Array{Int64,1}}:
 50  60
```
"""
struct RowVector{T,V<:AbstractVector} <: AbstractMatrix{T}
    vec::V
    function RowVector{T,V}(v::V) where V<:AbstractVector where T
        check_types(T,v)
        new(v)
    end
end

@inline check_types(::Type{T1}, ::AbstractVector{T2}) where {T1,T2} = check_types(T1, T2)
@pure check_types(::Type{T1}, ::Type{T2}) where {T1,T2} = T1 === transpose_type(T2) ? nothing :
    error("Element type mismatch. Tried to create a `RowVector{$T1}` from an `AbstractVector{$T2}`")

const ConjRowVector{T,CV<:ConjVector} = RowVector{T,CV}

# The element type may be transformed as transpose is recursive
@inline transpose_type(::Type{T}) where {T} = promote_op(transpose, T)

# Constructors that take a vector
@inline RowVector(vec::AbstractVector{T}) where {T} = RowVector{transpose_type(T),typeof(vec)}(vec)
@inline RowVector{T}(vec::AbstractVector{T}) where {T} = RowVector{T,typeof(vec)}(vec)

# Constructors that take a size and default to Array
@inline RowVector{T}(::Uninitialized, n::Int) where {T} =
    RowVector{T}(Vector{transpose_type(T)}(uninitialized, n))
@inline RowVector{T}(::Uninitialized, n1::Int, n2::Int) where {T} =
    n1 == 1 ? RowVector{T}(Vector{transpose_type(T)}(uninitialized, n2)) :
        error("RowVector expects 1×N size, got ($n1,$n2)")
@inline RowVector{T}(::Uninitialized, n::Tuple{Int}) where {T} =
    RowVector{T}(Vector{transpose_type(T)}(uninitialized, n[1]))
@inline RowVector{T}(::Uninitialized, n::Tuple{Int,Int}) where {T} =
    n[1] == 1 ? RowVector{T}(Vector{transpose_type(T)}(uninitialized, n[2])) :
        error("RowVector expects 1×N size, got $n")

# Conversion of underlying storage
convert(::Type{RowVector{T,V}}, rowvec::RowVector) where {T,V<:AbstractVector} =
    RowVector{T,V}(convert(V,rowvec.vec))

# similar tries to maintain the RowVector wrapper and the parent type
@inline similar(rowvec::RowVector) = RowVector(similar(parent(rowvec)))
@inline similar(rowvec::RowVector, ::Type{T}) where {T} = RowVector(similar(parent(rowvec), transpose_type(T)))

# Resizing similar currently loses its RowVector property.
@inline similar(rowvec::RowVector, ::Type{T}, dims::Dims{N}) where {T,N} = similar(parent(rowvec), T, dims)

# Basic methods
"""
    transpose(v::AbstractVector)

The transposition operator (`.'`).

# Examples
```jldoctest
julia> v = [1,2,3]
3-element Array{Int64,1}:
 1
 2
 3

julia> transpose(v)
1×3 RowVector{Int64,Array{Int64,1}}:
 1  2  3
```
"""
@inline transpose(vec::AbstractVector) = RowVector(vec)
@inline adjoint(vec::AbstractVector) = RowVector(_conj(vec))

@inline transpose(rowvec::RowVector) = rowvec.vec
@inline transpose(rowvec::ConjRowVector) = copy(rowvec.vec) # remove the ConjArray wrapper from any raw vector
@inline adjoint(rowvec::RowVector) = conj(rowvec.vec)
@inline adjoint(rowvec::RowVector{<:Real}) = rowvec.vec

parent(rowvec::RowVector) = rowvec.vec
vec(rowvec::RowVector) = rowvec.vec

"""
    conj(v::RowVector)

Return a [`ConjArray`](@ref) lazy view of the input, where each element is conjugated.

# Examples
```jldoctest
julia> v = [1+im, 1-im].'
1×2 RowVector{Complex{Int64},Array{Complex{Int64},1}}:
 1+1im  1-1im

julia> conj(v)
1×2 RowVector{Complex{Int64},ConjArray{Complex{Int64},1,Array{Complex{Int64},1}}}:
 1-1im  1+1im
```
"""
@inline conj(rowvec::RowVector) = RowVector(_conj(rowvec.vec))
@inline conj(rowvec::RowVector{<:Real}) = rowvec

# AbstractArray interface
@inline length(rowvec::RowVector) =  length(rowvec.vec)
@inline size(rowvec::RowVector) = (1, length(rowvec.vec))
@inline size(rowvec::RowVector, d) = ifelse(d==2, length(rowvec.vec), 1)
@inline indices(rowvec::RowVector) = (Base.OneTo(1), indices(rowvec.vec)[1])
@inline indices(rowvec::RowVector, d) = ifelse(d == 2, indices(rowvec.vec)[1], Base.OneTo(1))
IndexStyle(::RowVector) = IndexLinear()
IndexStyle(::Type{<:RowVector}) = IndexLinear()

@propagate_inbounds getindex(rowvec::RowVector, i::Int) = transpose(rowvec.vec[i])
@propagate_inbounds setindex!(rowvec::RowVector, v, i::Int) = (setindex!(rowvec.vec, transpose(v), i); rowvec)

# Keep a RowVector where appropriate
@propagate_inbounds getindex(rowvec::RowVector, ::Colon, i::Int) = transpose.(rowvec.vec[i:i])
@propagate_inbounds getindex(rowvec::RowVector, ::Colon, inds::AbstractArray{Int}) = RowVector(rowvec.vec[inds])
@propagate_inbounds getindex(rowvec::RowVector, ::Colon, ::Colon) = RowVector(rowvec.vec[:])

# helper function for below
@inline to_vec(rowvec::RowVector) = map(transpose, transpose(rowvec))
@inline to_vec(x::Number) = x
@inline to_vecs(rowvecs...) = (map(to_vec, rowvecs)...,)

# map: Preserve the RowVector by un-wrapping and re-wrapping, but note that `f`
# expects to operate within the transposed domain, so to_vec transposes the elements
@inline map(f, rowvecs::RowVector...) = RowVector(map(transpose∘f, to_vecs(rowvecs...)...))

# broacast (other combinations default to higher-dimensional array)
@inline broadcast(f, rowvecs::Union{Number,RowVector}...) =
    RowVector(broadcast(transpose∘f, to_vecs(rowvecs...)...))

# Horizontal concatenation #

@inline hcat(X::RowVector...) = transpose(vcat(map(transpose, X)...))
@inline hcat(X::Union{RowVector,Number}...) = transpose(vcat(map(transpose, X)...))

@inline typed_hcat(::Type{T}, X::RowVector...) where {T} =
    transpose(typed_vcat(T, map(transpose, X)...))
@inline typed_hcat(::Type{T}, X::Union{RowVector,Number}...) where {T} =
    transpose(typed_vcat(T, map(transpose, X)...))

# Multiplication #

# inner product -> dot product specializations
@inline *(rowvec::RowVector{T}, vec::AbstractVector{T}) where {T<:Real} = dot(parent(rowvec), vec)
@inline *(rowvec::ConjRowVector{T}, vec::AbstractVector{T}) where {T<:Real} = dot(rowvec', vec)
@inline *(rowvec::ConjRowVector, vec::AbstractVector) = dot(rowvec', vec)

# Generic behavior
@inline function *(rowvec::RowVector, vec::AbstractVector)
    if length(rowvec) != length(vec)
        throw(DimensionMismatch("A has dimensions $(size(rowvec)) but B has dimensions $(size(vec))"))
    end
    sum(@inbounds(return rowvec[i]*vec[i]) for i = 1:length(vec))
end
@inline *(rowvec::RowVector, mat::AbstractMatrix) = transpose(mat.' * transpose(rowvec))
*(::RowVector, ::RowVector) = throw(DimensionMismatch("Cannot multiply two transposed vectors"))
@inline *(vec::AbstractVector, rowvec::RowVector) = vec .* rowvec
*(vec::AbstractVector, rowvec::AbstractVector) = throw(DimensionMismatch("Cannot multiply two vectors"))

# Transposed forms
*(::RowVector, ::Transpose{<:Any,<:AbstractVector}) =
    throw(DimensionMismatch("Cannot multiply two transposed vectors"))
*(rowvec::RowVector, transmat::Transpose{<:Any,<:AbstractMatrix}) =
    (mat = transmat.parent; transpose(mat * transpose(rowvec)))
*(rowvec1::RowVector, transrowvec2::Transpose{<:Any,<:RowVector}) =
    (rowvec2 = transrowvec2.parent; rowvec1*transpose(rowvec2))
*(::AbstractVector, ::Transpose{<:Any,<:RowVector}) =
    throw(DimensionMismatch("Cannot multiply two vectors"))
*(vec1::AbstractVector, transvec2::Transpose{<:Any,<:AbstractVector}) =
    (vec2 = transvec2.parent; vec1 * transpose(vec2))
*(mat::AbstractMatrix, transrowvec::Transpose{<:Any,<:RowVector}) =
    (rowvec = transrowvec.parent; mat * transpose(rowvec))

*(transrowvec::Transpose{<:Any,<:RowVector}, transvec::Transpose{<:Any,<:AbstractVector}) =
    transpose(transrowvec.parent) * transpose(transvec.parent)
*(transvec::Transpose{<:Any,<:AbstractVector}, transmat::Transpose{<:Any,<:AbstractMatrix}) =
    transpose(transmat.parent * transvec.parent)
*(transrowvec1::Transpose{<:Any,<:RowVector}, transrowvec2::Transpose{<:Any,<:RowVector}) =
    throw(DimensionMismatch("Cannot multiply two vectors"))
*(transvec::Transpose{<:Any,<:AbstractVector}, transrowvec::Transpose{<:Any,<:RowVector}) =
    transpose(transvec.parent)*transpose(transrowvec.parent)
*(transvec::Transpose{<:Any,<:AbstractVector}, transrowvec::Transpose{<:Any,<:AbstractVector}) =
    throw(DimensionMismatch("Cannot multiply two transposed vectors"))
*(transmat::Transpose{<:Any,<:AbstractMatrix}, transrowvec::Transpose{<:Any,<:RowVector}) =
    (transmat.parent).' * transpose(transrowvec.parent)

*(::Transpose{<:Any,<:RowVector}, ::AbstractVector) =
    throw(DimensionMismatch("Cannot multiply two vectors"))
*(transvec::Transpose{<:Any,<:AbstractVector}, mat::AbstractMatrix) =
    transpose(*(Transpose(mat), transvec.parent))
*(transrowvec1::Transpose{<:Any,<:RowVector}, rowvec2::RowVector) =
    transpose(transrowvec1.parent) * rowvec2
*(transvec::Transpose{<:Any,<:AbstractVector}, rowvec::RowVector) =
    throw(DimensionMismatch("Cannot multiply two transposed vectors"))
*(transvec1::Transpose{<:Any,<:AbstractVector{T}}, vec2::AbstractVector{T}) where {T<:Real} =
    reduce(+, map(*, transvec1.parent, vec2)) # Seems to be overloaded...
*(transvec1::Transpose{<:Any,<:AbstractVector}, vec2::AbstractVector) =
    transpose(transvec1.parent) * vec2

# Conjugated forms
*(::RowVector, ::Adjoint{<:Any,<:AbstractVector}) =
    throw(DimensionMismatch("Cannot multiply two transposed vectors"))
*(rowvec::RowVector, adjmat::Adjoint{<:Any,<:AbstractMatrix}) =
    adjoint(adjmat.parent * adjoint(rowvec))
*(rowvec1::RowVector, adjrowvec2::Adjoint{<:Any,<:RowVector}) =
    rowvec1 * adjoint(adjrowvec2.parent)
*(vec::AbstractVector, adjrowvec::Adjoint{<:Any,<:RowVector}) =
    throw(DimensionMismatch("Cannot multiply two vectors"))
*(vec1::AbstractVector, adjvec2::Adjoint{<:Any,<:AbstractVector}) =
    vec1 * adjoint(adjvec2.parent)
*(mat::AbstractMatrix, adjrowvec::Adjoint{<:Any,<:RowVector}) =
    mat * adjoint(adjrowvec.parent)

*(adjrowvec::Adjoint{<:Any,<:RowVector}, adjvec::Adjoint{<:Any,<:AbstractVector}) =
    adjoint(adjrowvec.parent) * adjoint(adjvec.parent)
*(adjvec::Adjoint{<:Any,<:AbstractVector}, adjmat::Adjoint{<:Any,<:AbstractMatrix}) =
    adjoint(adjmat.parent * adjvec.parent)
*(adjrowvec1::Adjoint{<:Any,<:RowVector}, adjrowvec2::Adjoint{<:Any,<:RowVector}) =
    throw(DimensionMismatch("Cannot multiply two vectors"))
*(adjvec::Adjoint{<:Any,<:AbstractVector}, adjrowvec::Adjoint{<:Any,<:RowVector}) =
    adjoint(adjvec.parent)*adjoint(adjrowvec.parent)
*(adjvec::Adjoint{<:Any,<:AbstractVector}, adjrowvec::Adjoint{<:Any,<:AbstractVector}) =
    throw(DimensionMismatch("Cannot multiply two transposed vectors"))
*(adjmat::Adjoint{<:Any,<:AbstractMatrix}, adjrowvec::Adjoint{<:Any,<:RowVector}) =
    (adjmat.parent)' * adjoint(adjrowvec.parent)

*(::Adjoint{<:Any,<:RowVector}, ::AbstractVector) = throw(DimensionMismatch("Cannot multiply two vectors"))
*(adjvec::Adjoint{<:Any,<:AbstractVector}, mat::AbstractMatrix) = adjoint(*(Adjoint(mat), adjvec.parent))
*(adjrowvec1::Adjoint{<:Any,<:RowVector}, rowvec2::RowVector) = adjoint(adjrowvec1.parent) * rowvec2
*(adjvec::Adjoint{<:Any,<:AbstractVector}, rowvec::RowVector) = throw(DimensionMismatch("Cannot multiply two transposed vectors"))
*(adjvec1::Adjoint{<:Any,<:AbstractVector}, vec2::AbstractVector) = adjoint(adjvec1.parent)*vec2

# Pseudo-inverse
pinv(v::RowVector, tol::Real=0) = pinv(v', tol)'

# Left Division #

\(rowvec1::RowVector, rowvec2::RowVector) = pinv(rowvec1) * rowvec2
\(mat::AbstractMatrix, rowvec::RowVector) = throw(DimensionMismatch("Cannot left-divide transposed vector by matrix"))
\(transmat::Transpose{<:Any,<:AbstractMatrix}, rowvec::RowVector) =
    throw(DimensionMismatch("Cannot left-divide transposed vector by matrix"))
\(adjmat::Adjoint{<:Any,<:AbstractMatrix}, rowvec::RowVector) =
    throw(DimensionMismatch("Cannot left-divide transposed vector by matrix"))

# Right Division #

@inline /(rowvec::RowVector, mat::AbstractMatrix) = transpose(transpose(mat) \ transpose(rowvec))
/(rowvec::RowVector, transmat::Transpose{<:Any,<:AbstractMatrix}) = transpose(transmat.parent \ transpose(rowvec))
/(rowvec::RowVector, adjmat::Adjoint{<:Any,<:AbstractMatrix}) = adjoint(adjmat.parent \ adjoint(rowvec))


# definitions necessary for test/linalg/dense.jl to pass
# should be cleaned up / revised as necessary in the future
/(A::Number, B::Adjoint{<:Any,<:RowVector}) = /(A, adjoint(B.parent))
/(A::Matrix, B::RowVector) = adjoint(adjoint(B) \ adjoint(A))


# dismabiguation methods
*(A::Adjoint{<:Any,<:AbstractVector}, B::Transpose{<:Any,<:RowVector}) = adjoint(A.parent) * B
*(A::Adjoint{<:Any,<:AbstractMatrix}, B::Transpose{<:Any,<:RowVector}) = A * transpose(B.parent)
*(A::Transpose{<:Any,<:AbstractVector}, B::Adjoint{<:Any,<:RowVector}) = transpose(A.parent) * B
*(A::Transpose{<:Any,<:AbstractMatrix}, B::Adjoint{<:Any,<:RowVector}) = A * adjoint(B.parent)
