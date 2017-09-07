# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    RowVector(vector)

A lazy-view wrapper of an [`AbstractVector`](@ref), which turns a length-`n` vector into a `1×n`
shaped row vector and represents the transpose of a vector (the elements are also transposed
recursively).

By convention, a vector can be multiplied by a matrix on its left (`A * v`) whereas a row
vector can be multiplied by a matrix on its right (such that `RowVector(v) * A = RowVector(Transpose(A) * v)`). It
differs from a `1×n`-sized matrix by the facts that its transpose returns a vector and the
inner product `RowVector(v1) * v2` returns a scalar, but will otherwise behave similarly.

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

julia> RowVector(a)[3]
3

julia> RowVector(a)[1,3]
3

julia> RowVector(a)[3,1]
ERROR: BoundsError: attempt to access 1×4 RowVector{Int64,Array{Int64,1}} at index [3, 1]
[...]

julia> RowVector(a)*a
30

julia> B = [1 2; 3 4; 5 6; 7 8]
4×2 Array{Int64,2}:
 1  2
 3  4
 5  6
 7  8

julia> RowVector(a)*B
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
RowVector{T,V}(rowvec::RowVector) where {T,V<:AbstractVector} =
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

"""
    conj(v::RowVector)

Return a [`ConjArray`](@ref) lazy view of the input, where each element is conjugated.

# Examples
```jldoctest
julia> v = RowVector([1+im, 1-im])
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
@inline *(rowvec::RowVector, mat::AbstractMatrix) = rvtranspose(Transpose(mat) * rvtranspose(rowvec))
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
