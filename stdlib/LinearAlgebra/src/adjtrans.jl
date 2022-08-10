# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: @propagate_inbounds
import Base: length, size, axes, IndexStyle, getindex, setindex!, parent, vec, convert, similar

### basic definitions (types, aliases, constructors, abstractarray interface, sundry similar)

# note that Adjoint and Transpose must be able to wrap not only vectors and matrices
# but also factorizations, rotations, and other linear algebra objects, including
# user-defined such objects. so do not restrict the wrapped type.
"""
    Adjoint

Lazy wrapper type for an adjoint view of the underlying linear algebra object,
usually an `AbstractVector`/`AbstractMatrix`, but also some `Factorization`, for instance.
Usually, the `Adjoint` constructor should not be called directly, use [`adjoint`](@ref)
instead. To materialize the view use [`copy`](@ref).

This type is intended for linear algebra usage - for general data manipulation see
[`permutedims`](@ref Base.permutedims).

# Examples
```jldoctest
julia> A = [3+2im 9+2im; 0 0]
2×2 Matrix{Complex{Int64}}:
 3+2im  9+2im
 0+0im  0+0im

julia> Adjoint(A)
2×2 adjoint(::Matrix{Complex{Int64}}) with eltype Complex{Int64}:
 3-2im  0+0im
 9-2im  0+0im
```
"""
struct Adjoint{T,S} <: AbstractMatrix{T}
    parent::S
end
"""
    Transpose

Lazy wrapper type for a transpose view of the underlying linear algebra object,
usually an `AbstractVector`/`AbstractMatrix`, but also some `Factorization`, for instance.
Usually, the `Transpose` constructor should not be called directly, use [`transpose`](@ref)
instead. To materialize the view use [`copy`](@ref).

This type is intended for linear algebra usage - for general data manipulation see
[`permutedims`](@ref Base.permutedims).

# Examples
```jldoctest
julia> A = [2 3; 0 0]
2×2 Matrix{Int64}:
 2  3
 0  0

julia> Transpose(A)
2×2 transpose(::Matrix{Int64}) with eltype Int64:
 2  0
 3  0
```
"""
struct Transpose{T,S} <: AbstractMatrix{T}
    parent::S
end

# basic outer constructors
Adjoint(A) = Adjoint{Base.promote_op(adjoint,eltype(A)),typeof(A)}(A)
Transpose(A) = Transpose{Base.promote_op(transpose,eltype(A)),typeof(A)}(A)

Base.dataids(A::Union{Adjoint, Transpose}) = Base.dataids(A.parent)
Base.unaliascopy(A::Union{Adjoint,Transpose}) = typeof(A)(Base.unaliascopy(A.parent))

# wrapping lowercase quasi-constructors
"""
    A'
    adjoint(A)

Lazy adjoint (conjugate transposition). Note that `adjoint` is applied recursively to
elements.

For number types, `adjoint` returns the complex conjugate, and therefore it is equivalent to
the identity function for real numbers.

This operation is intended for linear algebra usage - for general data manipulation see
[`permutedims`](@ref Base.permutedims).

# Examples
```jldoctest
julia> A = [3+2im 9+2im; 0  0]
2×2 Matrix{Complex{Int64}}:
 3+2im  9+2im
 0+0im  0+0im

julia> B = A' # equivalently adjoint(A)
2×2 adjoint(::Matrix{Complex{Int64}}) with eltype Complex{Int64}:
 3-2im  0+0im
 9-2im  0+0im

julia> B isa Adjoint
true

julia> adjoint(B) === A # the adjoint of an adjoint unwraps the parent
true

julia> Adjoint(B) # however, the constructor always wraps its argument
2×2 adjoint(adjoint(::Matrix{Complex{Int64}})) with eltype Complex{Int64}:
 3+2im  9+2im
 0+0im  0+0im

julia> B[1,2] = 4 + 5im; # modifying B will modify A automatically

julia> A
2×2 Matrix{Complex{Int64}}:
 3+2im  9+2im
 4-5im  0+0im
```

For real matrices, the `adjoint` operation is equivalent to a `transpose`.

```jldoctest
julia> A = reshape([x for x in 1:4], 2, 2)
2×2 Matrix{Int64}:
 1  3
 2  4

julia> A'
2×2 adjoint(::Matrix{Int64}) with eltype Int64:
 1  2
 3  4

julia> adjoint(A) == transpose(A)
true
```

The adjoint of an `AbstractVector` is a row-vector:
```jldoctest
julia> x = [3, 4im]
2-element Vector{Complex{Int64}}:
 3 + 0im
 0 + 4im

julia> x'
1×2 adjoint(::Vector{Complex{Int64}}) with eltype Complex{Int64}:
 3+0im  0-4im

julia> x'x # compute the dot product, equivalently x' * x
25 + 0im
```

For a matrix of matrices, the individual blocks are recursively operated on:
```jldoctest
julia> A = reshape([x + im*x for x in 1:4], 2, 2)
2×2 Matrix{Complex{Int64}}:
 1+1im  3+3im
 2+2im  4+4im

julia> C = reshape([A, 2A, 3A, 4A], 2, 2)
2×2 Matrix{Matrix{Complex{Int64}}}:
 [1+1im 3+3im; 2+2im 4+4im]  [3+3im 9+9im; 6+6im 12+12im]
 [2+2im 6+6im; 4+4im 8+8im]  [4+4im 12+12im; 8+8im 16+16im]

julia> C'
2×2 adjoint(::Matrix{Matrix{Complex{Int64}}}) with eltype Adjoint{Complex{Int64}, Matrix{Complex{Int64}}}:
 [1-1im 2-2im; 3-3im 4-4im]    [2-2im 4-4im; 6-6im 8-8im]
 [3-3im 6-6im; 9-9im 12-12im]  [4-4im 8-8im; 12-12im 16-16im]
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
julia> A = [3 2; 0 0]
2×2 Matrix{Int64}:
 3  2
 0  0

julia> B = transpose(A)
2×2 transpose(::Matrix{Int64}) with eltype Int64:
 3  0
 2  0

julia> B isa Transpose
true

julia> transpose(B) === A # the transpose of a transpose unwraps the parent
true

julia> Transpose(B) # however, the constructor always wraps its argument
2×2 transpose(transpose(::Matrix{Int64})) with eltype Int64:
 3  2
 0  0

julia> B[1,2] = 4; # modifying B will modify A automatically

julia> A
2×2 Matrix{Int64}:
 3  2
 4  0
```

For complex matrices, the `adjoint` operation is equivalent to a conjugate-transpose.
```jldoctest
julia> A = reshape([Complex(x, x) for x in 1:4], 2, 2)
2×2 Matrix{Complex{Int64}}:
 1+1im  3+3im
 2+2im  4+4im

julia> adjoint(A) == conj(transpose(A))
true
```

The `transpose` of an `AbstractVector` is a row-vector:
```jldoctest
julia> v = [1,2,3]
3-element Vector{Int64}:
 1
 2
 3

julia> transpose(v) # returns a row-vector
1×3 transpose(::Vector{Int64}) with eltype Int64:
 1  2  3

julia> transpose(v) * v # compute the dot product
14
```

For a matrix of matrices, the individual blocks are recursively operated on:
```jldoctest
julia> C = reshape(1:4, 2, 2)
2×2 reshape(::UnitRange{Int64}, 2, 2) with eltype Int64:
 1  3
 2  4

julia> D = reshape([C, 2C, 3C, 4C], 2, 2) # construct a block matrix
2×2 Matrix{Matrix{Int64}}:
 [1 3; 2 4]  [3 9; 6 12]
 [2 6; 4 8]  [4 12; 8 16]

julia> transpose(D) # blocks are recursively transposed
2×2 transpose(::Matrix{Matrix{Int64}}) with eltype Transpose{Int64, Matrix{Int64}}:
 [1 2; 3 4]   [2 4; 6 8]
 [3 6; 9 12]  [4 8; 12 16]
```
"""
transpose(A::AbstractVecOrMat) = Transpose(A)

# unwrapping lowercase quasi-constructors
adjoint(A::Adjoint) = A.parent
transpose(A::Transpose) = A.parent
adjoint(A::Transpose{<:Real}) = A.parent
transpose(A::Adjoint{<:Real}) = A.parent

# printing
function Base.showarg(io::IO, v::Adjoint, toplevel)
    print(io, "adjoint(")
    Base.showarg(io, parent(v), false)
    print(io, ')')
    toplevel && print(io, " with eltype ", eltype(v))
    return nothing
end
function Base.showarg(io::IO, v::Transpose, toplevel)
    print(io, "transpose(")
    Base.showarg(io, parent(v), false)
    print(io, ')')
    toplevel && print(io, " with eltype ", eltype(v))
    return nothing
end

# some aliases for internal convenience use
const AdjOrTrans{T,S} = Union{Adjoint{T,S},Transpose{T,S}} where {T,S}
const AdjointAbsVec{T} = Adjoint{T,<:AbstractVector}
const AdjointAbsMat{T} = Adjoint{T,<:AbstractMatrix}
const TransposeAbsVec{T} = Transpose{T,<:AbstractVector}
const TransposeAbsMat{T} = Transpose{T,<:AbstractMatrix}
const AdjOrTransAbsVec{T} = AdjOrTrans{T,<:AbstractVector}
const AdjOrTransAbsMat{T} = AdjOrTrans{T,<:AbstractMatrix}

# for internal use below
wrapperop(_) = identity
wrapperop(::Adjoint) = adjoint
wrapperop(::Transpose) = transpose

# AbstractArray interface, basic definitions
length(A::AdjOrTrans) = length(A.parent)
size(v::AdjOrTransAbsVec) = (1, length(v.parent))
size(A::AdjOrTransAbsMat) = reverse(size(A.parent))
axes(v::AdjOrTransAbsVec) = (Base.OneTo(1), axes(v.parent)...)
axes(A::AdjOrTransAbsMat) = reverse(axes(A.parent))
IndexStyle(::Type{<:AdjOrTransAbsVec}) = IndexLinear()
IndexStyle(::Type{<:AdjOrTransAbsMat}) = IndexCartesian()
@propagate_inbounds getindex(v::AdjOrTransAbsVec{T}, i::Int) where {T} = wrapperop(v)(v.parent[i-1+first(axes(v.parent)[1])])::T
@propagate_inbounds getindex(A::AdjOrTransAbsMat{T}, i::Int, j::Int) where {T} = wrapperop(A)(A.parent[j, i])::T
@propagate_inbounds setindex!(v::AdjOrTransAbsVec, x, i::Int) = (setindex!(v.parent, wrapperop(v)(x), i-1+first(axes(v.parent)[1])); v)
@propagate_inbounds setindex!(A::AdjOrTransAbsMat, x, i::Int, j::Int) = (setindex!(A.parent, wrapperop(A)(x), j, i); A)
# AbstractArray interface, additional definitions to retain wrapper over vectors where appropriate
@propagate_inbounds getindex(v::AdjOrTransAbsVec, ::Colon, is::AbstractArray{Int}) = wrapperop(v)(v.parent[is])
@propagate_inbounds getindex(v::AdjOrTransAbsVec, ::Colon, ::Colon) = wrapperop(v)(v.parent[:])

# conversion of underlying storage
convert(::Type{Adjoint{T,S}}, A::Adjoint) where {T,S} = Adjoint{T,S}(convert(S, A.parent))
convert(::Type{Transpose{T,S}}, A::Transpose) where {T,S} = Transpose{T,S}(convert(S, A.parent))

# Strides and pointer for transposed strided arrays — but only if the elements are actually stored in memory
Base.strides(A::Adjoint{<:Real, <:AbstractVector}) = (stride(A.parent, 2), stride(A.parent, 1))
Base.strides(A::Transpose{<:Any, <:AbstractVector}) = (stride(A.parent, 2), stride(A.parent, 1))
# For matrices it's slightly faster to use reverse and avoid calling stride twice
Base.strides(A::Adjoint{<:Real, <:AbstractMatrix}) = reverse(strides(A.parent))
Base.strides(A::Transpose{<:Any, <:AbstractMatrix}) = reverse(strides(A.parent))

Base.unsafe_convert(::Type{Ptr{T}}, A::Adjoint{<:Real, <:AbstractVecOrMat}) where {T} = Base.unsafe_convert(Ptr{T}, A.parent)
Base.unsafe_convert(::Type{Ptr{T}}, A::Transpose{<:Any, <:AbstractVecOrMat}) where {T} = Base.unsafe_convert(Ptr{T}, A.parent)

Base.elsize(::Type{<:Adjoint{<:Real, P}}) where {P<:AbstractVecOrMat} = Base.elsize(P)
Base.elsize(::Type{<:Transpose{<:Any, P}}) where {P<:AbstractVecOrMat} = Base.elsize(P)

# for vectors, the semantics of the wrapped and unwrapped types differ
# so attempt to maintain both the parent and wrapper type insofar as possible
similar(A::AdjOrTransAbsVec) = wrapperop(A)(similar(A.parent))
similar(A::AdjOrTransAbsVec, ::Type{T}) where {T} = wrapperop(A)(similar(A.parent, Base.promote_op(wrapperop(A), T)))
# for matrices, the semantics of the wrapped and unwrapped types are generally the same
# and as you are allocating with similar anyway, you might as well get something unwrapped
similar(A::AdjOrTrans) = similar(A.parent, eltype(A), axes(A))
similar(A::AdjOrTrans, ::Type{T}) where {T} = similar(A.parent, T, axes(A))
similar(A::AdjOrTrans, ::Type{T}, dims::Dims{N}) where {T,N} = similar(A.parent, T, dims)

# AbstractMatrix{T} constructor for adjtrans vector: preserve wrapped type
AbstractMatrix{T}(A::AdjOrTransAbsVec) where {T} = wrapperop(A)(AbstractVector{T}(A.parent))

# sundry basic definitions
parent(A::AdjOrTrans) = A.parent
vec(v::TransposeAbsVec{<:Number}) = parent(v)
vec(v::AdjointAbsVec{<:Real}) = parent(v)

### concatenation
# preserve Adjoint/Transpose wrapper around vectors
# to retain the associated semantics post-concatenation
hcat(avs::Union{Number,AdjointAbsVec}...) = _adjoint_hcat(avs...)
hcat(tvs::Union{Number,TransposeAbsVec}...) = _transpose_hcat(tvs...)
_adjoint_hcat(avs::Union{Number,AdjointAbsVec}...) = adjoint(vcat(map(adjoint, avs)...))
_transpose_hcat(tvs::Union{Number,TransposeAbsVec}...) = transpose(vcat(map(transpose, tvs)...))
typed_hcat(::Type{T}, avs::Union{Number,AdjointAbsVec}...) where {T} = adjoint(typed_vcat(T, map(adjoint, avs)...))
typed_hcat(::Type{T}, tvs::Union{Number,TransposeAbsVec}...) where {T} = transpose(typed_vcat(T, map(transpose, tvs)...))
# otherwise-redundant definitions necessary to prevent hitting the concat methods in LinearAlgebra/special.jl
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
# Hack to preserve behavior after #32122; this needs to be done with a broadcast style instead to support dotted fusion
Broadcast.broadcast_preserving_zero_d(f, avs::Union{Number,AdjointAbsVec}...) = adjoint(broadcast((xs...) -> adjoint(f(adjoint.(xs)...)), quasiparenta.(avs)...))
Broadcast.broadcast_preserving_zero_d(f, tvs::Union{Number,TransposeAbsVec}...) = transpose(broadcast((xs...) -> transpose(f(transpose.(xs)...)), quasiparentt.(tvs)...))
# TODO unify and allow mixed combinations with a broadcast style


### reductions
# faster to sum the Array than to work through the wrapper
Base._mapreduce_dim(f, op, init::Base._InitialValue, A::Transpose, dims::Colon) =
    transpose(Base._mapreduce_dim(_sandwich(transpose, f), _sandwich(transpose, op), init, parent(A), dims))
Base._mapreduce_dim(f, op, init::Base._InitialValue, A::Adjoint, dims::Colon) =
    adjoint(Base._mapreduce_dim(_sandwich(adjoint, f), _sandwich(adjoint, op), init, parent(A), dims))
# sum(A'; dims)
Base.mapreducedim!(f, op, B::AbstractArray, A::TransposeAbsMat) =
    transpose(Base.mapreducedim!(_sandwich(transpose, f), _sandwich(transpose, op), transpose(B), parent(A)))
Base.mapreducedim!(f, op, B::AbstractArray, A::AdjointAbsMat) =
    adjoint(Base.mapreducedim!(_sandwich(adjoint, f), _sandwich(adjoint, op), adjoint(B), parent(A)))

_sandwich(adj::Function, fun) = (xs...,) -> adj(fun(map(adj, xs)...))
for fun in [:identity, :add_sum, :mul_prod] #, :max, :min]
    @eval _sandwich(::Function, ::typeof(Base.$fun)) = Base.$fun
end


### linear algebra

(-)(A::Adjoint)   = Adjoint(  -A.parent)
(-)(A::Transpose) = Transpose(-A.parent)

## multiplication *

function _dot_nonrecursive(u, v)
    lu = length(u)
    if lu != length(v)
        throw(DimensionMismatch("first array has length $(lu) which does not match the length of the second, $(length(v))."))
    end
    if lu == 0
        zero(eltype(u)) * zero(eltype(v))
    else
        sum(uu*vv for (uu, vv) in zip(u, v))
    end
end

# Adjoint/Transpose-vector * vector
*(u::AdjointAbsVec{<:Number}, v::AbstractVector{<:Number}) = dot(u.parent, v)
*(u::TransposeAbsVec{T}, v::AbstractVector{T}) where {T<:Real} = dot(u.parent, v)
*(u::AdjOrTransAbsVec, v::AbstractVector) = _dot_nonrecursive(u, v)


# vector * Adjoint/Transpose-vector
*(u::AbstractVector, v::AdjOrTransAbsVec) = broadcast(*, u, v)
# Adjoint/Transpose-vector * Adjoint/Transpose-vector
# (necessary for disambiguation with fallback methods in linalg/matmul)
*(u::AdjointAbsVec, v::AdjointAbsVec) = throw(MethodError(*, (u, v)))
*(u::TransposeAbsVec, v::TransposeAbsVec) = throw(MethodError(*, (u, v)))

# AdjOrTransAbsVec{<:Any,<:AdjOrTransAbsVec} is a lazy conj vectors
# We need to expand the combinations to avoid ambiguities
(*)(u::TransposeAbsVec, v::AdjointAbsVec{<:Any,<:TransposeAbsVec}) = _dot_nonrecursive(u, v)
(*)(u::AdjointAbsVec,   v::AdjointAbsVec{<:Any,<:TransposeAbsVec}) = _dot_nonrecursive(u, v)
(*)(u::TransposeAbsVec, v::TransposeAbsVec{<:Any,<:AdjointAbsVec}) = _dot_nonrecursive(u, v)
(*)(u::AdjointAbsVec,   v::TransposeAbsVec{<:Any,<:AdjointAbsVec}) = _dot_nonrecursive(u, v)

## pseudoinversion
pinv(v::AdjointAbsVec, tol::Real = 0) = pinv(v.parent, tol).parent
pinv(v::TransposeAbsVec, tol::Real = 0) = pinv(conj(v.parent)).parent


## left-division \
\(u::AdjOrTransAbsVec, v::AdjOrTransAbsVec) = pinv(u) * v


## right-division /
/(u::AdjointAbsVec, A::AbstractMatrix) = adjoint(adjoint(A) \ u.parent)
/(u::TransposeAbsVec, A::AbstractMatrix) = transpose(transpose(A) \ u.parent)
/(u::AdjointAbsVec, A::Transpose{<:Any,<:AbstractMatrix}) = adjoint(conj(A.parent) \ u.parent) # technically should be adjoint(copy(adjoint(copy(A))) \ u.parent)
/(u::TransposeAbsVec, A::Adjoint{<:Any,<:AbstractMatrix}) = transpose(conj(A.parent) \ u.parent) # technically should be transpose(copy(transpose(copy(A))) \ u.parent)

## complex conjugate
conj(A::Transpose) = adjoint(A.parent)
conj(A::Adjoint) = transpose(A.parent)
