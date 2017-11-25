# This file is a part of Julia. License is MIT: https://julialang.org/license

module MappedArrays

export MappedArray, MappedVector, MappedMatrix

"""
    MappedArray(f, a)
    MappedArray(f, f_inv, a)

Returns a lazily mapped array where function `f` is applied the elements of array `a`.

`f_inv` is the inverse function to `f`, and should satisfy `f(f_inv(x)) == x`. It is
optional and used to enable `setindex!` on the output array, so that the appropriate values
can be stored in `a`. Some inverse functions are known (e.g. `conj` is its own inverse) and
users may overload the `Base.MappedArrays.inv_func` function with their own definitions
(e.g. `Base.MappedArrays.inv_func(::typeof(conj)) = conj`) so that `f_inv` is created
automatically by the constructor.

# Example

```julia
julia> a = [1, 2, 3]
3-element Array{Int64,1}:
 1
 2
 3

julia> b = MappedArray(x -> x + 10, x -> x - 10, a)
3-element MappedArray{Int64,1,getfield(Main, Symbol("##3#5")),getfield(Main, Symbol("##4#6")),Array{Int64,1}}:
 11
 12
 13

julia> b[2] = 20
20

julia> a[2]
10
```
"""
struct MappedArray{T, N, F, F_inv, A <: AbstractArray{<:Any, N}} <: AbstractArray{T, N}
    f::F
    f_inv::F_inv
    parent::A
end

MappedArray(f, a::AbstractArray) = MappedArray(f, inv_func(f), a)
function MappedArray(f, f_inv, a::AbstractArray)
    MappedArray{Base.promote_op(f, eltype(a)), ndims(a), typeof(f), typeof(f_inv), typeof(a)}(f, f_inv, a)
end

noinverse(x) = error("No inverse function defined")
∘(::typeof(noinverse), ::Any) = noinverse
∘(::Any, ::typeof(noinverse)) = noinverse
∘(::typeof(noinverse), ::typeof(noinverse)) = noinverse

inv_func(f) = noinverse
inv_func(::typeof(identity)) = identity
inv_func(::typeof(conj)) = conj

const MappedVector{T, F, F_inv, A <: AbstractVector} = MappedArray{T, 1, F, F_inv, A}
MappedVector(f, a::AbstractVector) = MappedVector(f, inv_func(f), a)
function MappedVector(f, f_inv, a::AbstractVector)
    MappedArray{Base.promote_op(f, eltype(a)), 1, typeof(f), typeof(f_inv), typeof(a)}(f, f_inv, a)
end

const MappedMatrix{T, F, F_inv, A <: AbstractMatrix} = MappedArray{T, 2, F, F_inv, A}
MappedMatrix(f, a::AbstractMatrix) = MappedMatrix(f, inv_func(f), a)
function MappedMatrix(f, f_inv, a::AbstractMatrix)
    MappedArray{Base.promote_op(f, eltype(a)), 2, typeof(f), typeof(f_inv), typeof(a)}(f, f_inv, a)
end

Base.parent(a::MappedArray) = a.parent
parent_type(::Type{<:MappedArray{<:Any, <:Any, <:Any, <:Any, A}}) where {A} = A

Base.size(a::MappedArray) = size(parent(a))
Base.indices(a::MappedArray) = indices(parent(a))
Base.IndexStyle(::Type{MA}) where {MA <: MappedArray} = Base.IndexStyle(parent_type(MA))

Base.@propagate_inbounds Base.getindex(a::MappedArray, i::Int...) = a.f(a.parent[i...])
Base.@propagate_inbounds function Base.setindex!(a::MappedArray{T}, v::T, i::Int...) where {T}
    a.parent[i...] = a.f_inv(v)
end
Base.@propagate_inbounds function Base.setindex!(a::MappedArray{T}, v, i::Int...) where {T}
    a[i...] = convert(T, v)
end

# MappedArray preserves laziness under `map`
Base.map(f, a::MappedArray) = MappedArray(f ∘ a.f, inv_func(f) ∘ a.f_inv, a.parent)

end # module
