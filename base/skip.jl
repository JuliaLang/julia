struct Skip{T, A}
    x::A
end

"""
    skip(T, itr)
Return an iterator over the elements in `itr` skipping values of type T.
The returned object can be indexed using indices of `itr` if the latter is
indexable. Indices corresponding to values of type T are not valid: they are
skipped by [`keys`](@ref) and [`eachindex`](@ref), and a `MissingException` is
thrown when trying to use them.

Use [`collect`](@ref) to obtain an `Array` containing the non-`T` values in
`itr`. Note that even if `itr` is a multidimensional array, the result will
always be a `Vector` since it is not possible to remove elements while
preserving dimensions of the input.

The values `missing` and `nothing` can also be used to create an iterator that
skips over elements of type `Nothing` and `Missing`, respectively.

# Examples
```jldoctest
julia> x = skip(Missing, [1, missing, 2])
Base.Skip{Missing,Array{Union{Missing, Int64},1}}(Union{Missing, Int64}[1, missing, 2])
julia> x = skip(missing, [1, missing, 2])
Base.Skip{Missing,Array{Union{Missing, Int64},1}}(Union{Missing, Int64}[1, missing, 2])
julia> sum(x)
3
julia> x[1]
1
julia> x[2]
ERROR: MissingException: the value at index (2,) is of type Missing
[...]
julia> argmax(x)
3

julia> collect(keys(x))
2-element Array{Int64,1}:
 1
 3

julia> collect(skip(Missing, [1, missing, 2]))
2-element Array{Int64,1}:
 1
 2

julia> collect(skip(Nothing, [1 nothing; 2 nothing]))
2-element Array{Int64,1}:
 1
 2
```
"""
skip(::Type{T}, itr::A) where {T, A} = Skip{T, A}(itr)
skip(::T, itr) where T<:Union{Nothing, Missing} = skip(T, itr)

IteratorSize(::Type{<:Skip}) = SizeUnknown()
IteratorEltype(::Type{Skip{T, A}}) where {T, A} = IteratorEltype(A)
eltype(::Type{Skip{T, A}}) where {T, A} =
    Core.Compiler.typesubtract(eltype(A), T)

function iterate(itr::Skip{T, <:Any}, state...) where T
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

IndexStyle(::Type{<:Skip{T, A}}) where {T, A} = IndexStyle(A)
eachindex(itr::Skip{T, A}) where {T, A} =
    Iterators.filter(i -> !isa(@inbounds(itr.x[i]), T) , eachindex(itr.x))
function keys(itr::Skip{T, A}) where {T, A}
    Iterators.filter(i -> !isa(@inbounds(itr.x[i]), T), keys(itr.x))
end

@propagate_inbounds function getindex(itr::Skip{T, A}, I...) where {T, A}
    v = itr.x[I...]
    v isa T && throw(MissingException("the value at index $I is of type $T"))
    v
end

# Optimized mapreduce implementation
# The generic method is faster when !(eltype(A) >: T) since it does not need
# additional loops to identify the two first non-nothing values of each block
function mapreduce(f, op, itr::Skip{T, <:AbstractArray}) where T
    _mapreduce(f, op, IndexStyle(itr.x), eltype(itr.x) >: T ? itr : itr.x)
end

function _mapreduce(
    f, op, ::IndexLinear, itr::Skip{T, <:AbstractArray}
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

_mapreduce(f, op, ::IndexCartesian, itr::Skip) = mapfoldl(f, op, itr)

mapreduce_impl(f, op, A::Skip, ifirst::Integer, ilast::Integer) =
    mapreduce_impl(f, op, A, ifirst, ilast, pairwise_blocksize(f, op))

# Returns nothing when the input contains only nothing values, and Some(x) otherwise
@noinline function mapreduce_impl(
    f, op, itr::Skip{T, <:AbstractArray}, ifirst::Integer, ilast::Integer,
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

"""
    filter(f, itr::Skip{<:AbstractArray})

Return a vector similar to the array wrapped by the given `Skip` iterator
but with all elements of the type to be skipped over, and those for which `f`
returns `false`, removed.

!!! compat "Julia 1.2"
    This method requires Julia 1.2 or later.

# Examples
```jldoctest
julia> x = [1 2; missing 4]
2×2 Array{Union{Missing, Int64},2}:
 1         2
  missing  4

julia> filter(isodd, skip(missing, x))
1-element Array{Int64,1}:
 1
```
"""
function filter(f, itr::Skip{T, <:AbstractArray}) where T
    y = similar(itr.x, eltype(itr), 0)
    for xi in itr.x
        if !isa(xi, T) && f(xi)
            push!(y, xi)
        end
    end
    y
end
