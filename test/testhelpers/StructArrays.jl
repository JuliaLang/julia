module StructArrays

struct StructArray{T,N,C <: Tuple{Vararg{AbstractArray{<:Any,N}}}} <: AbstractArray{T,N}
    components :: C

    function StructArray{T,N,C}(components::C) where {T,N,C}
        fieldcount(T) == length(components) || throw(ArgumentError("number of components incompatible with eltype"))
        allequal(axes.(components)) || throw(ArgumentError("component arrays must have the same axes"))
        new{T,N,C}(components)
    end
end

function StructArray{T}(components::Tuple{Vararg{AbstractArray{<:Any,N}}}) where {T,N}
    StructArray{T,N,typeof(components)}(components)
end

Base.size(S::StructArray) = size(S.components[1])
Base.axes(S::StructArray) = axes(S.components[1])
function Base.getindex(S::StructArray{T,N}, inds::Vararg{Int,N}) where {T,N}
    vals = map(x -> x[inds...], S.components)
    T(vals...)
end
function Base.setindex!(S::StructArray{T,N}, val, inds::Vararg{Int,N}) where {T,N}
    vals = getfield.(Ref(convert(T, val)), fieldnames(T))
    for (A,v) in zip(S.components, vals)
        A[inds...] = v
    end
    S
end

isnonemptystructtype(::Type{T}) where {T} = isstructtype(T) && fieldcount(T) != 0

function Base.similar(S::StructArray, ::Type{T}, dims::Tuple{Int, Vararg{Int}}) where {T}
    isnonemptystructtype(T) || return similar(S.components[1], T, dims)
    arrs = similar.(S.components, fieldtypes(T), Ref(dims))
    StructArray{T}(arrs)
end

end
