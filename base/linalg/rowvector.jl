"""
    RowVector(vector)

A lazy-view wrapper of an `AbstractVector`, which turns a length-`n` vector into
a `1×n` shaped row vector and represents the transpose of a vector (the elements
are also transposed recursively). This type is usually constructed (and
unwrapped) via the `transpose()` function or `.'` operator (or related
`ctranspose()` or `'` operator).

By convention, a vector can be multiplied by a matrix on its left (`A * v`)
whereas a row vector can be multiplied by a matrix on its right (such that
`v.' * A = (A.' * v).'`). It differs from a `1×n`-sized matrix by the facts that
its transpose returns a vector and the inner product `v1.' * v2` returns a
scalar, but will otherwise behave similarly.
"""
immutable RowVector{T,V<:AbstractVector} <: AbstractMatrix{T}
    vec::V
    function RowVector{T,V}(v::V) where V<:AbstractVector where T
        check_types(T,v)
        new(v)
    end
end


@inline check_types{T1,T2}(::Type{T1},::AbstractVector{T2}) = check_types(T1, T2)
@pure check_types{T1,T2}(::Type{T1},::Type{T2}) = T1 === transpose_type(T2) ? nothing :
    error("Element type mismatch. Tried to create a `RowVector{$T1}` from an `AbstractVector{$T2}`")

# The element type may be transformed as transpose is recursive
@inline transpose_type{T}(::Type{T}) = promote_op(transpose, T)

# Constructors that take a vector
@inline RowVector(vec::AbstractVector{T}) where T = RowVector{transpose_type(T),typeof(vec)}(vec)
@inline RowVector{T}(vec::AbstractVector{T}) where T = RowVector{T,typeof(vec)}(vec)

# Constructors that take a size and default to Array
@inline RowVector{T}(n::Int) where T = RowVector{T}(Vector{transpose_type(T)}(n))
@inline RowVector{T}(n1::Int, n2::Int) where T = n1 == 1 ?
    RowVector{T}(Vector{transpose_type(T)}(n2)) :
    error("RowVector expects 1×N size, got ($n1,$n2)")
@inline RowVector{T}(n::Tuple{Int}) where T = RowVector{T}(Vector{transpose_type(T)}(n[1]))
@inline RowVector{T}(n::Tuple{Int,Int}) where T = n[1] == 1 ?
    RowVector{T}(Vector{transpose_type(T)}(n[2])) :
    error("RowVector expects 1×N size, got $n")

# Conversion of underlying storage
convert{T,V<:AbstractVector}(::Type{RowVector{T,V}}, rowvec::RowVector) =
    RowVector{T,V}(convert(V,rowvec.vec))

# similar()
@inline similar(rowvec::RowVector) = RowVector(similar(rowvec.vec))
@inline similar{T}(rowvec::RowVector, ::Type{T}) = RowVector(similar(rowvec.vec, transpose_type(T)))
# There is no resizing similar() because it would be ambiguous if the result were a Matrix or a RowVector

# Basic methods
"""
    transpose(v::AbstractVector)

The transposition operator (`.'`).

# Example

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
@inline ctranspose{T}(vec::AbstractVector{T}) = RowVector(conj(vec))
@inline ctranspose{T<:Real}(vec::AbstractVector{T}) = RowVector(vec)

@inline transpose(rowvec::RowVector) = rowvec.vec
@inline ctranspose{T}(rowvec::RowVector{T}) = conj(rowvec.vec)
@inline ctranspose{T<:Real}(rowvec::RowVector{T}) = rowvec.vec

parent(rowvec::RowVector) = rowvec.vec

# Strictly, these are unnecessary but will make things stabler if we introduce
# a "view" for conj(::AbstractArray)
@inline conj(rowvec::RowVector) = RowVector(conj(rowvec.vec))
@inline conj{T<:Real}(rowvec::RowVector{T}) = rowvec

# AbstractArray interface
@inline length(rowvec::RowVector) =  length(rowvec.vec)
@inline size(rowvec::RowVector) = (1, length(rowvec.vec))
@inline size(rowvec::RowVector, d) = ifelse(d==2, length(rowvec.vec), 1)
@inline indices(rowvec::RowVector) = (Base.OneTo(1), indices(rowvec.vec)[1])
@inline indices(rowvec::RowVector, d) = ifelse(d == 2, indices(rowvec.vec)[1], Base.OneTo(1))
linearindexing(::RowVector) = LinearFast()
linearindexing{V<:RowVector}(::Type{V}) = LinearFast()

@propagate_inbounds getindex(rowvec::RowVector, i) = transpose(rowvec.vec[i])
@propagate_inbounds setindex!(rowvec::RowVector, v, i) = setindex!(rowvec.vec, transpose(v), i)

# Cartesian indexing is distorted by getindex
# Furthermore, Cartesian indexes don't have to match shape, apparently!
@inline function getindex(rowvec::RowVector, i::CartesianIndex)
    @boundscheck if !(i.I[1] == 1 && i.I[2] ∈ indices(rowvec.vec)[1] && check_tail_indices(i.I...))
        throw(BoundsError(rowvec, i.I))
    end
    @inbounds return transpose(rowvec.vec[i.I[2]])
end
@inline function setindex!(rowvec::RowVector, v, i::CartesianIndex)
    @boundscheck if !(i.I[1] == 1 && i.I[2] ∈ indices(rowvec.vec)[1] && check_tail_indices(i.I...))
        throw(BoundsError(rowvec, i.I))
    end
    @inbounds rowvec.vec[i.I[2]] = transpose(v)
end

@propagate_inbounds getindex(rowvec::RowVector, ::CartesianIndex{0}) = getindex(rowvec)
@propagate_inbounds getindex(rowvec::RowVector, i::CartesianIndex{1}) = getindex(rowvec, i.I[1])

@propagate_inbounds setindex!(rowvec::RowVector, v, ::CartesianIndex{0}) = setindex!(rowvec, v)
@propagate_inbounds setindex!(rowvec::RowVector, v, i::CartesianIndex{1}) = setindex!(rowvec, v, i.I[1])

@inline check_tail_indices(i1, i2) = true
@inline check_tail_indices(i1, i2, i3, is...) = i3 == 1 ? check_tail_indices(i1, i2, is...) : false

# helper function for below
@inline to_vec(rowvec::RowVector) = transpose(rowvec)
@inline to_vec(x::Number) = x
@inline to_vecs(rowvecs...) = (map(to_vec, rowvecs)...)

# map
@inline map(f, rowvecs::RowVector...) = RowVector(map(f, to_vecs(rowvecs...)...))

# broacast (other combinations default to higher-dimensional array)
@inline broadcast(f, rowvecs::Union{Number,RowVector}...) =
    RowVector(broadcast(f, to_vecs(rowvecs...)...))

# Horizontal concatenation #

@inline hcat(X::RowVector...) = transpose(vcat(map(transpose, X)...))
@inline hcat(X::Union{RowVector,Number}...) = transpose(vcat(map(transpose, X)...))

@inline typed_hcat{T}(::Type{T}, X::RowVector...) =
    transpose(typed_vcat(T, map(transpose, X)...))
@inline typed_hcat{T}(::Type{T}, X::Union{RowVector,Number}...) =
    transpose(typed_vcat(T, map(transpose, X)...))

# Multiplication #

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
A_mul_Bt(::RowVector, ::AbstractVector) = throw(DimensionMismatch("Cannot multiply two transposed vectors"))
@inline A_mul_Bt(rowvec::RowVector, mat::AbstractMatrix) = transpose(mat * transpose(rowvec))
@inline A_mul_Bt(rowvec1::RowVector, rowvec2::RowVector) = rowvec1*transpose(rowvec2)
A_mul_Bt(vec::AbstractVector, rowvec::RowVector) = throw(DimensionMismatch("Cannot multiply two vectors"))
@inline A_mul_Bt(vec1::AbstractVector, vec2::AbstractVector) = vec1 * transpose(vec2)
@inline A_mul_Bt(mat::AbstractMatrix, rowvec::RowVector) = mat * transpose(rowvec)

@inline At_mul_Bt(rowvec::RowVector, vec::AbstractVector) = transpose(rowvec) * transpose(vec)
@inline At_mul_Bt(vec::AbstractVector, mat::AbstractMatrix) = transpose(mat * vec)
At_mul_Bt(rowvec1::RowVector, rowvec2::RowVector) = throw(DimensionMismatch("Cannot multiply two vectors"))
@inline At_mul_Bt(vec::AbstractVector, rowvec::RowVector) = transpose(vec)*transpose(rowvec)
At_mul_Bt(vec::AbstractVector, rowvec::AbstractVector) = throw(DimensionMismatch(
    "Cannot multiply two transposed vectors"))
@inline At_mul_Bt(mat::AbstractMatrix, rowvec::RowVector) = mat.' * transpose(rowvec)

At_mul_B(::RowVector, ::AbstractVector) = throw(DimensionMismatch("Cannot multiply two vectors"))
@inline At_mul_B(vec::AbstractVector, mat::AbstractMatrix) = transpose(At_mul_B(mat,vec))
@inline At_mul_B(rowvec1::RowVector, rowvec2::RowVector) = transpose(rowvec1) * rowvec2
At_mul_B(vec::AbstractVector, rowvec::RowVector) = throw(DimensionMismatch(
    "Cannot multiply two transposed vectors"))
@inline At_mul_B{T<:Real}(vec1::AbstractVector{T}, vec2::AbstractVector{T}) =
    reduce(+, map(At_mul_B, vec1, vec2)) # Seems to be overloaded...
@inline At_mul_B(vec1::AbstractVector, vec2::AbstractVector) = transpose(vec1) * vec2

# Conjugated forms
A_mul_Bc(::RowVector, ::AbstractVector) = throw(DimensionMismatch("Cannot multiply two transposed vectors"))
@inline A_mul_Bc(rowvec::RowVector, mat::AbstractMatrix) = ctranspose(mat * ctranspose(rowvec))
@inline A_mul_Bc(rowvec1::RowVector, rowvec2::RowVector) = rowvec1 * ctranspose(rowvec2)
A_mul_Bc(vec::AbstractVector, rowvec::RowVector) = throw(DimensionMismatch("Cannot multiply two vectors"))
@inline A_mul_Bc(vec1::AbstractVector, vec2::AbstractVector) = vec1 * ctranspose(vec2)
@inline A_mul_Bc(mat::AbstractMatrix, rowvec::RowVector) = mat * ctranspose(rowvec)

@inline Ac_mul_Bc(rowvec::RowVector, vec::AbstractVector) = ctranspose(rowvec) * ctranspose(vec)
@inline Ac_mul_Bc(vec::AbstractVector, mat::AbstractMatrix) = ctranspose(mat * vec)
Ac_mul_Bc(rowvec1::RowVector, rowvec2::RowVector) = throw(DimensionMismatch("Cannot multiply two vectors"))
@inline Ac_mul_Bc(vec::AbstractVector, rowvec::RowVector) = ctranspose(vec)*ctranspose(rowvec)
Ac_mul_Bc(vec::AbstractVector, rowvec::AbstractVector) = throw(DimensionMismatch("Cannot multiply two transposed vectors"))
@inline Ac_mul_Bc(mat::AbstractMatrix, rowvec::RowVector) = mat' * ctranspose(rowvec)

Ac_mul_B(::RowVector, ::AbstractVector) = throw(DimensionMismatch("Cannot multiply two vectors"))
@inline Ac_mul_B(vec::AbstractVector, mat::AbstractMatrix) = ctranspose(Ac_mul_B(mat,vec))
@inline Ac_mul_B(rowvec1::RowVector, rowvec2::RowVector) = ctranspose(rowvec1) * rowvec2
Ac_mul_B(vec::AbstractVector, rowvec::RowVector) = throw(DimensionMismatch("Cannot multiply two transposed vectors"))
@inline Ac_mul_B(vec1::AbstractVector, vec2::AbstractVector) = ctranspose(vec1)*vec2

# Left Division #

\(mat::AbstractMatrix, rowvec::RowVector) = throw(DimensionMismatch("Cannot left-divide transposed vector by matrix"))
At_ldiv_B(mat::AbstractMatrix, rowvec::RowVector) = throw(DimensionMismatch("Cannot left-divide transposed vector by matrix"))
Ac_ldiv_B(mat::AbstractMatrix, rowvec::RowVector) = throw(DimensionMismatch("Cannot left-divide transposed vector by matrix"))

# Right Division #

@inline /(rowvec::RowVector, mat::AbstractMatrix) = transpose(transpose(mat) \ transpose(rowvec))
@inline A_rdiv_Bt(rowvec::RowVector, mat::AbstractMatrix) = transpose(mat \ transpose(rowvec))
@inline A_rdiv_Bc(rowvec::RowVector, mat::AbstractMatrix) = ctranspose(mat  \ ctranspose(rowvec))
