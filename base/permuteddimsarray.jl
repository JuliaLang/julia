# This file is a part of Julia. License is MIT: https://julialang.org/license

module PermutedDimsArrays

import Base: permutedims, permutedims!
export PermutedDimsArray

# Some day we will want storage-order-aware iteration, so put perm in the parameters
struct PermutedDimsArray{T,N,perm,iperm,AA<:AbstractArray} <: AbstractArray{T,N}
    parent::AA

    function PermutedDimsArray{T,N,perm,iperm,AA}(data::AA) where {T,N,perm,iperm,AA<:AbstractArray}
        (isa(perm, NTuple{N,Int}) && isa(iperm, NTuple{N,Int})) || error("perm and iperm must both be NTuple{$N,Int}")
        isperm(perm) || throw(ArgumentError(string(perm, " is not a valid permutation of dimensions 1:", N)))
        all(d->iperm[perm[d]]==d, 1:N) || throw(ArgumentError(string(perm, " and ", iperm, " must be inverses")))
        new(data)
    end
end

"""
    PermutedDimsArray(A, perm) -> B

Given an AbstractArray `A`, create a view `B` such that the
dimensions appear to be permuted. Similar to `permutedims`, except
that no copying occurs (`B` shares storage with `A`).

See also [`permutedims`](@ref), [`invperm`](@ref).

# Examples
```jldoctest
julia> A = rand(3,5,4);

julia> B = PermutedDimsArray(A, (3,1,2));

julia> size(B)
(4, 3, 5)

julia> B[3,1,2] == A[1,2,3]
true
```
"""
Base.@constprop :aggressive function PermutedDimsArray(data::AbstractArray{T,N}, perm) where {T,N}
    length(perm) == N || throw(ArgumentError(string(perm, " is not a valid permutation of dimensions 1:", N)))
    iperm = invperm(perm)
    PermutedDimsArray{T,N,(perm...,),(iperm...,),typeof(data)}(data)
end

Base.parent(A::PermutedDimsArray) = A.parent
Base.size(A::PermutedDimsArray{T,N,perm}) where {T,N,perm} = genperm(size(parent(A)), perm)
Base.axes(A::PermutedDimsArray{T,N,perm}) where {T,N,perm} = genperm(axes(parent(A)), perm)
Base.has_offset_axes(A::PermutedDimsArray) = Base.has_offset_axes(A.parent)
Base.similar(A::PermutedDimsArray, T::Type, dims::Base.Dims) = similar(parent(A), T, dims)
Base.cconvert(::Type{Ptr{T}}, A::PermutedDimsArray{T}) where {T} = Base.cconvert(Ptr{T}, parent(A))

# It's OK to return a pointer to the first element, and indeed quite
# useful for wrapping C routines that require a different storage
# order than used by Julia. But for an array with unconventional
# storage order, a linear offset is ambiguous---is it a memory offset
# or a linear index?
Base.pointer(A::PermutedDimsArray, i::Integer) = throw(ArgumentError("pointer(A, i) is deliberately unsupported for PermutedDimsArray"))

function Base.strides(A::PermutedDimsArray{T,N,perm}) where {T,N,perm}
    s = strides(parent(A))
    ntuple(d->s[perm[d]], Val(N))
end
Base.elsize(::Type{<:PermutedDimsArray{<:Any, <:Any, <:Any, <:Any, P}}) where {P} = Base.elsize(P)

@inline function Base.getindex(A::PermutedDimsArray{T,N,perm,iperm}, I::Vararg{Int,N}) where {T,N,perm,iperm}
    @boundscheck checkbounds(A, I...)
    @inbounds val = getindex(A.parent, genperm(I, iperm)...)
    val
end
@inline function Base.setindex!(A::PermutedDimsArray{T,N,perm,iperm}, val, I::Vararg{Int,N}) where {T,N,perm,iperm}
    @boundscheck checkbounds(A, I...)
    @inbounds setindex!(A.parent, val, genperm(I, iperm)...)
    val
end

function Base.isassigned(A::PermutedDimsArray{T,N,perm,iperm}, I::Vararg{Int,N}) where {T,N,perm,iperm}
    @boundscheck checkbounds(Bool, A, I...) || return false
    @inbounds x = isassigned(A.parent, genperm(I, iperm)...)
    x
end

@inline genperm(I::NTuple{N,Any}, perm::Dims{N}) where {N} = ntuple(d -> I[perm[d]], Val(N))
@inline genperm(I, perm::AbstractVector{Int}) = genperm(I, (perm...,))

"""
    permutedims(A::AbstractArray, perm)
    permutedims(A::AbstractMatrix)

Permute the dimensions (axes) of array `A`. `perm` is a tuple or vector of `ndims(A)` integers
specifying the permutation.

If `A` is a 2d array ([`AbstractMatrix`](@ref)), then
`perm` defaults to `(2,1)`, swapping the two axes of `A` (the rows and columns
of the matrix).   This differs from [`transpose`](@ref) in that the
operation is not recursive, which is especially useful for arrays of non-numeric values
(where the recursive `transpose` would throw an error) and/or 2d arrays that do not represent
linear operators.

For 1d arrays, see [`permutedims(v::AbstractVector)`](@ref), which returns a 1-row “matrix”.

See also [`permutedims!`](@ref), [`PermutedDimsArray`](@ref), [`transpose`](@ref), [`invperm`](@ref).

# Examples

## 2d arrays:
Unlike `transpose`, `permutedims` can be used to swap rows and columns of 2d arrays of
arbitrary non-numeric elements, such as strings:
```jldoctest
julia> A = ["a" "b" "c"
            "d" "e" "f"]
2×3 Matrix{String}:
 "a"  "b"  "c"
 "d"  "e"  "f"

julia> permutedims(A)
3×2 Matrix{String}:
 "a"  "d"
 "b"  "e"
 "c"  "f"
```
And `permutedims` produces results that differ from `transpose`
for matrices whose elements are themselves numeric matrices:
```jldoctest; setup = :(using LinearAlgebra)
julia> a = [1 2; 3 4];

julia> b = [5 6; 7 8];

julia> c = [9 10; 11 12];

julia> d = [13 14; 15 16];

julia> X = [[a] [b]; [c] [d]]
2×2 Matrix{Matrix{Int64}}:
 [1 2; 3 4]     [5 6; 7 8]
 [9 10; 11 12]  [13 14; 15 16]

julia> permutedims(X)
2×2 Matrix{Matrix{Int64}}:
 [1 2; 3 4]  [9 10; 11 12]
 [5 6; 7 8]  [13 14; 15 16]

julia> transpose(X)
2×2 transpose(::Matrix{Matrix{Int64}}) with eltype Transpose{Int64, Matrix{Int64}}:
 [1 3; 2 4]  [9 11; 10 12]
 [5 7; 6 8]  [13 15; 14 16]
```

## Multi-dimensional arrays
```jldoctest
julia> A = reshape(Vector(1:8), (2,2,2))
2×2×2 Array{Int64, 3}:
[:, :, 1] =
 1  3
 2  4

[:, :, 2] =
 5  7
 6  8

julia> perm = (3, 1, 2); # put the last dimension first

julia> B = permutedims(A, perm)
2×2×2 Array{Int64, 3}:
[:, :, 1] =
 1  2
 5  6

[:, :, 2] =
 3  4
 7  8

julia> A == permutedims(B, invperm(perm)) # the inverse permutation
true
```

For each dimension `i` of `B = permutedims(A, perm)`, its corresponding dimension of `A`
will be `perm[i]`. This means the equality `size(B, i) == size(A, perm[i])` holds.

```jldoctest
julia> A = randn(5, 7, 11, 13);

julia> perm = [4, 1, 3, 2];

julia> B = permutedims(A, perm);

julia> size(B)
(13, 5, 11, 7)

julia> size(A)[perm] == ans
true
```
"""
function permutedims(A::AbstractArray, perm)
    dest = similar(A, genperm(axes(A), perm))
    permutedims!(dest, A, perm)
end

permutedims(A::AbstractMatrix) = permutedims(A, (2,1))

"""
    permutedims(v::AbstractVector)

Reshape vector `v` into a `1 × length(v)` row matrix.
Differs from [`transpose`](@ref) in that
the operation is not recursive, which is especially useful for arrays of non-numeric values
(where the recursive `transpose` might throw an error).

# Examples
Unlike `transpose`, `permutedims` can be used on vectors of
arbitrary non-numeric elements, such as strings:
```jldoctest
julia> permutedims(["a", "b", "c"])
1×3 Matrix{String}:
 "a"  "b"  "c"
```
For vectors of numbers, `permutedims(v)` works much like `transpose(v)`
except that the return type differs (it uses [`reshape`](@ref)
rather than a `LinearAlgebra.Transpose` view, though both
share memory with the original array `v`):
```jldoctest; setup = :(using LinearAlgebra)
julia> v = [1, 2, 3, 4]
4-element Vector{Int64}:
 1
 2
 3
 4

julia> p = permutedims(v)
1×4 Matrix{Int64}:
 1  2  3  4

julia> r = transpose(v)
1×4 transpose(::Vector{Int64}) with eltype Int64:
 1  2  3  4

julia> p == r
true

julia> typeof(r)
Transpose{Int64, Vector{Int64}}

julia> p[1] = 5; r[2] = 6; # mutating p or r also changes v

julia> v # shares memory with both p and r
4-element Vector{Int64}:
 5
 6
 3
 4
```
However, `permutedims` produces results that differ from `transpose`
for vectors whose elements are themselves numeric matrices:
```jldoctest; setup = :(using LinearAlgebra)
julia> V = [[[1 2; 3 4]]; [[5 6; 7 8]]]
2-element Vector{Matrix{Int64}}:
 [1 2; 3 4]
 [5 6; 7 8]

julia> permutedims(V)
1×2 Matrix{Matrix{Int64}}:
 [1 2; 3 4]  [5 6; 7 8]

julia> transpose(V)
1×2 transpose(::Vector{Matrix{Int64}}) with eltype Transpose{Int64, Matrix{Int64}}:
 [1 3; 2 4]  [5 7; 6 8]
```
"""
permutedims(v::AbstractVector) = reshape(v, (1, length(v)))

"""
    permutedims!(dest, src, perm)

Permute the dimensions of array `src` and store the result in the array `dest`. `perm` is a
vector specifying a permutation of length `ndims(src)`. The preallocated array `dest` should
have `size(dest) == size(src)[perm]` and is completely overwritten. No in-place permutation
is supported and unexpected results will happen if `src` and `dest` have overlapping memory
regions.

See also [`permutedims`](@ref).
"""
function permutedims!(dest, src::AbstractArray, perm)
    Base.checkdims_perm(axes(dest), axes(src), perm)
    P = PermutedDimsArray(dest, invperm(perm))
    _copy!(P, src)
    return dest
end

function Base.copyto!(dest::PermutedDimsArray{T,N}, src::AbstractArray{T,N}) where {T,N}
    checkbounds(dest, axes(src)...)
    _copy!(dest, src)
end
Base.copyto!(dest::PermutedDimsArray, src::AbstractArray) = _copy!(dest, src)

function _copy!(P::PermutedDimsArray{T,N,perm}, src) where {T,N,perm}
    # If dest/src are "close to dense," then it pays to be cache-friendly.
    # Determine the first permuted dimension
    d = 0  # d+1 will hold the first permuted dimension of src
    while d < ndims(src) && perm[d+1] == d+1
        d += 1
    end
    if d == ndims(src)
        copyto!(parent(P), src) # it's not permuted
    else
        R1 = CartesianIndices(axes(src)[1:d])
        d1 = findfirst(isequal(d+1), perm)::Int  # first permuted dim of dest
        R2 = CartesianIndices(axes(src)[d+2:d1-1])
        R3 = CartesianIndices(axes(src)[d1+1:end])
        _permutedims!(P, src, R1, R2, R3, d+1, d1)
    end
    return P
end

@noinline function _permutedims!(P::PermutedDimsArray, src, R1::CartesianIndices{0}, R2, R3, ds, dp)
    ip, is = axes(src, dp), axes(src, ds)
    for jo in first(ip):8:last(ip), io in first(is):8:last(is)
        for I3 in R3, I2 in R2
            for j in jo:min(jo+7, last(ip))
                for i in io:min(io+7, last(is))
                    @inbounds P[i, I2, j, I3] = src[i, I2, j, I3]
                end
            end
        end
    end
    P
end

@noinline function _permutedims!(P::PermutedDimsArray, src, R1, R2, R3, ds, dp)
    ip, is = axes(src, dp), axes(src, ds)
    for jo in first(ip):8:last(ip), io in first(is):8:last(is)
        for I3 in R3, I2 in R2
            for j in jo:min(jo+7, last(ip))
                for i in io:min(io+7, last(is))
                    for I1 in R1
                        @inbounds P[I1, i, I2, j, I3] = src[I1, i, I2, j, I3]
                    end
                end
            end
        end
    end
    P
end

const CommutativeOps = Union{typeof(+),typeof(Base.add_sum),typeof(min),typeof(max),typeof(Base._extrema_rf),typeof(|),typeof(&)}

function Base._mapreduce_dim(f, op::CommutativeOps, init::Base._InitialValue, A::PermutedDimsArray, dims::Colon)
    Base._mapreduce_dim(f, op, init, parent(A), dims)
end
function Base._mapreduce_dim(f::typeof(identity), op::Union{typeof(Base.mul_prod),typeof(*)}, init::Base._InitialValue, A::PermutedDimsArray{<:Union{Real,Complex}}, dims::Colon)
    Base._mapreduce_dim(f, op, init, parent(A), dims)
end

function Base.mapreducedim!(f, op::CommutativeOps, B::AbstractArray{T,N}, A::PermutedDimsArray{S,N,perm,iperm}) where {T,S,N,perm,iperm}
    C = PermutedDimsArray{T,N,iperm,perm,typeof(B)}(B) # make the inverse permutation for the output
    Base.mapreducedim!(f, op, C, parent(A))
    B
end
function Base.mapreducedim!(f::typeof(identity), op::Union{typeof(Base.mul_prod),typeof(*)}, B::AbstractArray{T,N}, A::PermutedDimsArray{<:Union{Real,Complex},N,perm,iperm}) where {T,N,perm,iperm}
    C = PermutedDimsArray{T,N,iperm,perm,typeof(B)}(B) # make the inverse permutation for the output
    Base.mapreducedim!(f, op, C, parent(A))
    B
end

function Base.showarg(io::IO, A::PermutedDimsArray{T,N,perm}, toplevel) where {T,N,perm}
    print(io, "PermutedDimsArray(")
    Base.showarg(io, parent(A), false)
    print(io, ", ", perm, ')')
    toplevel && print(io, " with eltype ", eltype(A))
    return nothing
end

end
