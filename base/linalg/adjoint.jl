# This file is a part of Julia. License is MIT: https://julialang.org/license

adjoint(a::AbstractArray) = error("adjoint not defined for $(typeof(a)). Consider using `permutedims` for higher-dimensional arrays.")

## adjoint ##

"""
    adjoint(v::AbstractVector)

Creates a `RowVector` from `v` where `adjoint` has been applied recursively to the elements.
Conceptually, this is intended create the "dual vector" of `v` (note however that the output
is strictly an `AbstractMatrix`). Note also that the output is a view of `v`.
"""
@inline adjoint(vec::AbstractVector) = RowVector(_map(adjoint, vec))
@inline adjoint(rowvec::RowVector) = _map(adjoint, parent(rowvec))

"""
    adjoint(m::AbstractMatrix)

Returns the Hermitian adjoint of `m`, where `m` has been transposed and `adjoint` is applied
recursively to the elements.
"""
function adjoint(a::AbstractMatrix)
    (ind1, ind2) = indices(a)
    b = similar(a, promote_op(adjoint, eltype(a)), (ind2, ind1))
    adjoint!(b, a)
end

"""
adjoint!(dest,src)

Conjugate transpose array `src` and store the result in the preallocated array `dest`, which
should have a size corresponding to `(size(src,2),size(src,1))`. No in-place transposition
is supported and unexpected results will happen if `src` and `dest` have overlapping memory
regions.
"""
adjoint!(b::AbstractMatrix, a::AbstractMatrix) = transpose_f!(adjoint, b, a)
function adjoint!(b::AbstractVector, a::AbstractMatrix)
    if indices(b, 1) != indices(a, 2) || indices(a, 1) != 1:1
        throw(DimensionMismatch("adjoint"))
    end
    adjointcopy!(b, a)
end
function adjoint!(b::AbstractMatrix, a::AbstractVector)
    if indices(b, 2) != indices(a, 1) || indices(b, 1) != 1:1
        throw(DimensionMismatch("adjoint"))
    end
    adjointcopy!(b, a)
end

function adjointcopy!(b, a)
    ra = eachindex(a)
    rb = eachindex(b)
    if rb == ra
        for i ∈ rb
            b[i] = adjoint(a[i])
        end
    else
        for (i, j) ∈ zip(rb, ra)
            b[i] = adjoint(a[j])
        end
    end
end

"""
adjoint(x::Number)

The (complex) conjugate of `x`, `conj(x)`.
"""
adjoint(x::Number) = conj(x)


## conjadjoint ##

"""
conjadjoint(a)

Returns `conj(adjoint(a))`.
"""
conjadjoint(a::AbstractArray) = conj(adjoint(a))

∘(::typeof(conj), ::typeof(conj)) = identity
∘(::typeof(adjoint), ::typeof(adjoint)) = identity
∘(::typeof(conjadjoint), ::typeof(conjadjoint)) = identity

∘(::typeof(conj), ::typeof(adjoint)) = conjadjoint
∘(::typeof(adjoint), ::typeof(conj)) = conjadjoint
∘(::typeof(conj), ::typeof(conjadjoint)) = adjoint
∘(::typeof(conjadjoint), ::typeof(conj)) = adjoint
∘(::typeof(adjoint), ::typeof(conjadjoint)) = conj
∘(::typeof(conjadjoint), ::typeof(adjoint)) = conj

## mapped array aliases ##

"""
    ConjArray(array)

Constructs a lazy view of `array` where all elements are conjugated via `conj`. An alias of
[`MappedArray`](@ref).
"""
const ConjArray{T,N,A<:AbstractArray{<:Any,N}} = MappedArray{T,N,typeof(conj),typeof(conj),A}
const ConjVector{T,A<:AbstractVector} = MappedVector{T,typeof(conj),typeof(conj),A}
const ConjMatrix{T,A<:AbstractMatrix} = MappedMatrix{T,typeof(conj),typeof(conj),A}

inv_func(::typeof(adjoint)) = adjoint
inv_func(::typeof(conjadjoint)) = conjadjoint

@inline _map(f, a::AbstractArray) = MappedArray(f, a)
@inline _map(f, a::MappedArray) = map(f, a)

## lazy conj ##

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
@inline conj(rowvec::RowVector) = RowVector(_map(conj, parent(rowvec)))

