# This file is a part of Julia. License is MIT: https://julialang.org/license

## Unary operators ##

"""
    conj!(A)

Transform an array to its complex conjugate in-place.

See also [`conj`](@ref).

# Examples
```jldoctest
julia> A = [1+im 2-im; 2+2im 3+im]
2×2 Matrix{Complex{Int64}}:
 1+1im  2-1im
 2+2im  3+1im

julia> conj!(A);

julia> A
2×2 Matrix{Complex{Int64}}:
 1-1im  2+1im
 2-2im  3-1im
```
"""
conj!(A::AbstractArray{<:Number}) = (@inbounds broadcast!(conj, A, A); A)

for f in (:-, :conj, :real, :imag)
    @eval ($f)(A::AbstractArray) = broadcast_preserving_zero_d($f, A)
end


## Binary arithmetic operators ##

for f in (:+, :-)
    @eval function ($f)(A::AbstractArray, B::AbstractArray)
        promote_shape(A, B) # check size compatibility
        broadcast_preserving_zero_d($f, A, B)
    end
end

function +(A::Array, Bs::Array...)
    for B in Bs
        promote_shape(A, B) # check size compatibility
    end
    broadcast_preserving_zero_d(+, A, Bs...)
end

for f in (:/, :\, :*)
    if f !== :/
        @eval ($f)(A::Number, B::AbstractArray) = broadcast_preserving_zero_d($f, A, B)
    end
    if f !== :\
        @eval ($f)(A::AbstractArray, B::Number) = broadcast_preserving_zero_d($f, A, B)
    end
end

## data movement ##

"""
    reverse(A; dims=:)

Reverse `A` along dimension `dims`, which can be an integer (a
single dimension), a tuple of integers (a tuple of dimensions)
or `:` (reverse along all the dimensions, the default).  See
also [`reverse!`](@ref) for in-place reversal.

# Examples
```jldoctest
julia> b = Int64[1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> reverse(b, dims=2)
2×2 Matrix{Int64}:
 2  1
 4  3

julia> reverse(b)
2×2 Matrix{Int64}:
 4  3
 2  1
```

!!! compat "Julia 1.6"
    Prior to Julia 1.6, only single-integer `dims` are supported in `reverse`.
"""
reverse(A::AbstractArray; dims=:) = _reverse(A, dims)
_reverse(A, dims) = reverse!(copymutable(A); dims)

"""
    reverse!(A; dims=:)

Like [`reverse`](@ref), but operates in-place in `A`.

!!! compat "Julia 1.6"
    Multidimensional `reverse!` requires Julia 1.6.
"""
reverse!(A::AbstractArray; dims=:) = _reverse!(A, dims)
_reverse!(A::AbstractArray{<:Any,N}, ::Colon) where {N} = _reverse!(A, ntuple(identity, Val{N}()))
_reverse!(A, dim::Integer) = _reverse!(A, (Int(dim),))
_reverse!(A, dims::NTuple{M,Integer}) where {M} = _reverse!(A, Int.(dims))
function _reverse!(A::AbstractArray{<:Any,N}, dims::NTuple{M,Int}) where {N,M}
    dimrev = ntuple(k -> k in dims, Val{N}()) # boolean tuple indicating reversed dims

    if N < M || M != sum(dimrev)
        throw(ArgumentError("invalid dimensions $dims in reverse!"))
    end
    M == 0 && return A # nothing to reverse

    # swapping loop only needs to traverse ≈half of the array
    halfsz = ntuple(k -> k == dims[1] ? size(A,k) ÷ 2 : size(A,k), Val{N}())

    last1 = ntuple(k -> lastindex(A,k)+firstindex(A,k), Val{N}()) # offset for reversed index
    for i in CartesianIndices(ntuple(k -> firstindex(A,k):firstindex(A,k)-1+@inbounds(halfsz[k]), Val{N}()))
        iₜ = Tuple(i)
        iᵣ = CartesianIndex(ifelse.(dimrev, last1 .- iₜ, iₜ))
        @inbounds A[iᵣ], A[i] = A[i], A[iᵣ]
    end
    if M > 1 && isodd(size(A, dims[1]))
        # middle slice for odd dimensions must be recursively flipped
        mid = firstindex(A, dims[1]) + (size(A, dims[1]) ÷ 2)
        midslice = CartesianIndices(ntuple(k -> k == dims[1] ? (mid:mid) : (firstindex(A,k):lastindex(A,k)), Val{N}()))
        _reverse!(view(A, midslice), dims[2:end])
    end
    return A
end
# fix ambiguity with array.jl:
_reverse!(A::AbstractVector, dim::Tuple{Int}) = _reverse!(A, first(dim))


"""
    rotl90(A)

Rotate matrix `A` left 90 degrees.

# Examples
```jldoctest
julia> a = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> rotl90(a)
2×2 Matrix{Int64}:
 2  4
 1  3
```
"""
function rotl90(A::AbstractMatrix)
    ind1, ind2 = axes(A)
    B = similar(A, (ind2,ind1))
    n = first(ind2)+last(ind2)
    for i=axes(A,1), j=ind2
        B[n-j,i] = A[i,j]
    end
    return B
end

"""
    rotr90(A)

Rotate matrix `A` right 90 degrees.

# Examples
```jldoctest
julia> a = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> rotr90(a)
2×2 Matrix{Int64}:
 3  1
 4  2
```
"""
function rotr90(A::AbstractMatrix)
    ind1, ind2 = axes(A)
    B = similar(A, (ind2,ind1))
    m = first(ind1)+last(ind1)
    for i=ind1, j=axes(A,2)
        B[j,m-i] = A[i,j]
    end
    return B
end
"""
    rot180(A)

Rotate matrix `A` 180 degrees.

# Examples
```jldoctest
julia> a = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> rot180(a)
2×2 Matrix{Int64}:
 4  3
 2  1
```
"""
function rot180(A::AbstractMatrix)
    B = similar(A)
    ind1, ind2 = axes(A,1), axes(A,2)
    m, n = first(ind1)+last(ind1), first(ind2)+last(ind2)
    for j=ind2, i=ind1
        B[m-i,n-j] = A[i,j]
    end
    return B
end
"""
    rotl90(A, k)

Left-rotate matrix `A` 90 degrees counterclockwise an integer `k` number of times.
If `k` is a multiple of four (including zero), this is equivalent to a `copy`.

# Examples
```jldoctest
julia> a = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> rotl90(a,1)
2×2 Matrix{Int64}:
 2  4
 1  3

julia> rotl90(a,2)
2×2 Matrix{Int64}:
 4  3
 2  1

julia> rotl90(a,3)
2×2 Matrix{Int64}:
 3  1
 4  2

julia> rotl90(a,4)
2×2 Matrix{Int64}:
 1  2
 3  4
```
"""
function rotl90(A::AbstractMatrix, k::Integer)
    k = mod(k, 4)
    k == 1 ? rotl90(A) :
    k == 2 ? rot180(A) :
    k == 3 ? rotr90(A) : copy(A)
end
"""
    rotr90(A, k)

Right-rotate matrix `A` 90 degrees clockwise an integer `k` number of times.
If `k` is a multiple of four (including zero), this is equivalent to a `copy`.

# Examples
```jldoctest
julia> a = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> rotr90(a,1)
2×2 Matrix{Int64}:
 3  1
 4  2

julia> rotr90(a,2)
2×2 Matrix{Int64}:
 4  3
 2  1

julia> rotr90(a,3)
2×2 Matrix{Int64}:
 2  4
 1  3

julia> rotr90(a,4)
2×2 Matrix{Int64}:
 1  2
 3  4
```
"""
rotr90(A::AbstractMatrix, k::Integer) = rotl90(A,-k)
"""
    rot180(A, k)

Rotate matrix `A` 180 degrees an integer `k` number of times.
If `k` is even, this is equivalent to a `copy`.

# Examples
```jldoctest
julia> a = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> rot180(a,1)
2×2 Matrix{Int64}:
 4  3
 2  1

julia> rot180(a,2)
2×2 Matrix{Int64}:
 1  2
 3  4
```
"""
rot180(A::AbstractMatrix, k::Integer) = mod(k, 2) == 1 ? rot180(A) : copy(A)
