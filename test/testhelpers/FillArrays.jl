module FillArrays

struct Fill{T, N, S<:NTuple{N,Integer}} <: AbstractArray{T,N}
    value::T
    size::S
end

Fill(v, size::Vararg{Integer}) = Fill(v, size)

Base.size(F::Fill) = F.size

Base.copy(F::Fill) = F

Base.AbstractArray{T,N}(F::Fill{<:Any,N}) where {T,N} = Fill(T(F.value), F.size)

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

Base.zero(F::Fill) = Fill(zero(F.value), size(F))

Base.show(io::IO, F::Fill) = print(io, "Fill($(F.value), $(F.size))")
Base.show(io::IO, ::MIME"text/plain", F::Fill) = show(io, F)

_first_or_one(t::Tuple) = t[1]
_first_or_one(t::Tuple{}) = 1

_match_size(sz::Tuple{}, inner::Tuple{}, outer::Tuple{}) = ()
function _match_size(sz::Tuple, inner::Tuple, outer::Tuple)
    t1 = (_first_or_one(sz), _first_or_one(inner), _first_or_one(outer))
    t2 = _match_size(sz[2:end], inner[2:end], outer[2:end])
    (t1, t2...)
end

function _repeat_size(sz::Tuple, inner::Tuple, outer::Tuple)
    t = _match_size(sz, inner, outer)
    map(*, getindex.(t, 1), getindex.(t, 2), getindex.(t, 3))
end

function Base.repeat(A::Fill; inner=ntuple(x->1, ndims(A)), outer=ntuple(x->1, ndims(A)))
    Base.require_one_based_indexing(A)
    length(inner) >= ndims(A) ||
        throw(ArgumentError("number of inner repetitions $(length(inner)) cannot be "*
            "less than number of dimensions of input array $(ndims(A))"))
    length(outer) >= ndims(A) ||
        throw(ArgumentError("number of outer repetitions $(length(outer)) cannot be "*
            "less than number of dimensions of input array $(ndims(A))"))
    sz = _repeat_size(size(A), Tuple(inner), Tuple(outer))
    Fill(getindex_value(A), sz)
end

end
