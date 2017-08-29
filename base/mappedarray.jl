# This file is a part of Julia. License is MIT: https://julialang.org/license

struct MappedArray{T, N, F, F_inv, A <: AbstractArray{<:Any, N}} <: AbstractArray{T, N}
    f::F
    f_inv::F_inv
    parent::A
end

MappedArray(f, a::AbstractArray) = MappedArray(f, inv_func(f), a)
function MappedArray(f, f_inv, a::AbstractArray) 
    MappedArray{promote_op(f, eltype(a)), ndims(a), typeof(f), typeof(f_inv)}(f, f_inv, a)
end

noinverse(x) = error("No inverse function defined")

inv_func(f) = noinverse
inv_func(::typeof(identity)) = identity
inv_func(::typeof(conj)) = conj
inv_func(::typeof(transpose)) = transpose
inv_func(::typeof(adjoint)) = adjoint

const MappedVector{T, F, F_inv, A <: AbstractVector} = MappedArray{T, 1, F, F_inv, A}
MappedVector(f, a::AbstractVector) = MappedVector(f, inv_func(f), a)
function MappedVector(f, f_inv, a::AbstractVector) 
    MappedArray{promote_op(f, eltype(a)), 1, typeof(f), typeof(f_inv)}(f, f_inv, a)
end

const MappedMatrix{T, F, F_inv, A <: AbstractMatrix} = MappedArray{T, 2, F, F_inv, A}
MappedMatrix(f, a::AbstractMatrix) = MappedMatrix(f, inv_func(f), a)
function MappedMatrix(f, f_inv, a::AbstractMatrix) 
    MappedArray{promote_op(f, eltype(a)), 2, typeof(f), typeof(f_inv)}(f, f_inv, a)
end

parent(a::MappedArray) = a.parent

size(a::MappedArray) = size(parent(a))
indices(a::MappedArray) = indices(parent(a))
IndexStyle(::Type{MA}) where {MA<:AbstractMappedArray} = IndexStyle(parenttype(MA))

@propagate_inbounds getindex(a::MappedArray, i::Int...) = a.f(a.a[i...])
@propagate_inbounds function setindex!(a::MappedArray{T}, v::T, i::Int...) where {T} 
    a.a[i...] = a.f_inv(v)
end
@propagate_inbounds setindex!(a::MappedArray, v, i::Int...) = a[i...] = convert(T, v)