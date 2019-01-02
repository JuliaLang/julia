struct SkipOfType{T, A}
    x::A
end

"""
    skipoftype(T, itr)
Return an iterator over the elements in `itr` skipping values of type T.
Use [`collect`](@ref) to obtain an `Array` containing the non-`T` values in
`itr`. Note that even if `itr` is a multidimensional array, the result will always
be a `Vector` since it is not possible to remove nothings while preserving dimensions
of the input.
# Examples
```jldoctest
julia> sum(skipoftype(Nothing, [1, nothing, 2]))
3
julia> collect(skipoftype(Missing, [1, missing, 2]))
2-element Array{Int64,1}:
 1
 2
julia> collect(skipoftype(Nothing, [1 nothing; 2 nothing]))
2-element Array{Int64,1}:
 1
 2
```
"""
skipoftype(::Type{T}, itr::A) where {T, A} = SkipOfType{T, A}(itr)
skipoftype(::T, itr) where T = skipoftype(T, itr)

IteratorSize(::Type{<:SkipOfType}) = SizeUnknown()
IteratorEltype(::Type{SkipOfType{T, A}}) where {T, A} = IteratorEltype(A)
eltype(::Type{SkipOfType{T, A}}) where {T, A} = union_poptype(T, eltype(A))

function iterate(itr::SkipOfType{T, <:Any}, state...) where T
    y = iterate(itr.x, state...)
    y === nothing && return nothing
    item, state = y
    while item isa T
        y = iterate(itr.x, state)
        y === nothing && return nothing
        item, state = y
    end
    item, state
end

IndexStyle(::Type{<:SkipOfType{T}}) where {T} = IndexStyle(T)
eachindex(itr::SkipOfType) =
    Iterators.filter(i -> @inbounds(itr.x[i]) !== missing, eachindex(itr.x))
keys(itr::SkipOfType) =
    Iterators.filter(i -> @inbounds(itr.x[i]) !== missing, keys(itr.x))
@propagate_inbounds function getindex(itr::SkipOfType, I...)
    v = itr.x[I...]
    v === missing && throw(MissingException("the value at index $I is missing"))
    v
end

# Optimized mapreduce implementation
# The generic method is faster when !(eltype(A) >: T) since it does not need
# additional loops to identify the two first non-nothing values of each block
function mapreduce(f, op, itr::SkipOfType{T, <:AbstractArray}) where T
    _mapreduce(f, op, IndexStyle(itr.x), eltype(itr.x) >: T ? itr : itr.x)
end

function _mapreduce(
    f, op, ::IndexLinear, itr::SkipOfType{T, <:AbstractArray}
) where T
    A = itr.x
    local ai
    inds = LinearIndices(A)
    i = first(inds)
    ilast = last(inds)
    while i <= ilast
        @inbounds ai = A[i]
        ai isa T  || break
        i += 1
    end
    i > ilast && return mapreduce_empty(f, op, eltype(itr))
    a1 = ai
    i += 1
    while i <= ilast
        @inbounds ai = A[i]
        ai isa T || break
        i += 1
    end
    i > ilast && return mapreduce_first(f, op, a1)
    # We know A contains at least two non-nothing entries: the result cannot be nothing
    something(mapreduce_impl(f, op, itr, first(inds), last(inds)))
end

_mapreduce(f, op, ::IndexCartesian, itr::SkipOfType) = mapfoldl(f, op, itr)

mapreduce_impl(f, op, A::SkipOfType, ifirst::Integer, ilast::Integer) =
    mapreduce_impl(f, op, A, ifirst, ilast, pairwise_blocksize(f, op))

# Returns nothing when the input contains only nothing values, and Some(x) otherwise
@noinline function mapreduce_impl(
    f, op, itr::SkipOfType{T, <:AbstractArray}, ifirst::Integer, ilast::Integer,
    blksize::Int
) where T
    A = itr.x
    if ifirst == ilast
        @inbounds a1 = A[ifirst]
        if a1 isa T
            return nothing
        else
            return Some(mapreduce_first(f, op, a1))
        end
    elseif ifirst + blksize > ilast
        # sequential portion
        local ai
        i = ifirst
        while i <= ilast
            @inbounds ai = A[i]
            ai isa T || break
            i += 1
        end
        i > ilast && return nothing
        a1 = ai::eltype(itr)
        i += 1
        while i <= ilast
            @inbounds ai = A[i]
            ai isa T || break
            i += 1
        end
        i > ilast && return Some(mapreduce_first(f, op, a1))
        a2 = ai::eltype(itr)
        i += 1
        v = op(f(a1), f(a2))
        @simd for i = i:ilast
            @inbounds ai = A[i]
            if !(ai isa T)
                v = op(v, f(ai))
            end
        end
        return Some(v)
    else
        # pairwise portion
        imid = (ifirst + ilast) >> 1
        v1 = mapreduce_impl(f, op, itr, ifirst, imid, blksize)
        v2 = mapreduce_impl(f, op, itr, imid+1, ilast, blksize)
        if v1 === nothing && v2 === nothing
            return nothing
        elseif v1 === nothing
            return v2
        elseif v2 === nothing
            return v1
        else
            return Some(op(something(v1), something(v2)))
        end
    end
end

union_poptype(::Type{T}, ::Type{Union{T,S}}) where {T, S} = S
union_poptype(::Type{T}, ::Type{S}) where {T, S} = S
union_poptype(::Type{T}, ::Type{T}) where {T} = Union{}
