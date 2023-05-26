module FillArrays

struct Fill{T, N, S<:NTuple{N,Integer}} <: AbstractArray{T,N}
    value::T
    size::S
end

Fill(v, size::Vararg{Integer}) = Fill(v, size)

Base.size(F::Fill) = F.size

@inline getindex_value(F::Fill) = F.value

@inline function Base.getindex(F::Fill{<:Any,N}, i::Vararg{Int,N}) where {N}
    @boundscheck checkbounds(F, i...)
    getindex_value(F)
end

@inline function Base.setindex!(F::Fill, v, k::Integer)
    @boundscheck checkbounds(F, k)
    v == getindex_value(F) || throw(ArgumentError("Cannot setindex! to $v for a Fill with value $(getindex_value(F))."))
    F
end

@inline function Base.fill!(F::Fill, v)
    v == getindex_value(F) || throw(ArgumentError("Cannot fill! with $v a Fill with value $(getindex_value(F))."))
    F
end

Base.show(io::IO, F::Fill) = print(io, "Fill($(F.value), $(F.size))")
Base.show(io::IO, ::MIME"text/plain", F::Fill) = show(io, F)

end
