# This is a vector wrapper that we use as a workaround for `push!`
# being slow (it always calls into the runtime even if the underlying buffer,
# has enough space). Here we keep track of the length using an extra field
mutable struct PushVector{T, A<:AbstractVector{T}} <: AbstractVector{T}
    v::A
    l::Int
end

# Default length of 20 should be enough to never need to grow in most cases
PushVector{T}() where {T} = PushVector(Vector{T}(undef, 20), 0)

Base.unsafe_convert(::Type{Ptr{UInt8}}, v::PushVector) = pointer(v.v)
Base.length(v::PushVector) = v.l
Base.size(v::PushVector) = (v.l,)
@inline function Base.getindex(v::PushVector, i)
    @boundscheck checkbounds(v, i)
    @inbounds v.v[i]
end

function Base.push!(v::PushVector, i)
    v.l += 1
    if v.l > length(v.v)
        resize!(v.v, v.l * 2)
    end
    v.v[v.l] = i
    return v
end

function Base.resize!(v::PushVector, l::Integer)
    # Only support shrinking for now, since that is all we need
    @assert l <= v.l
    v.l = l
end
