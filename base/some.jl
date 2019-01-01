# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Some{T}

A wrapper type used in `Union{Some{T}, Nothing}` to distinguish between the absence
of a value ([`nothing`](@ref)) and the presence of a `nothing` value (i.e. `Some(nothing)`).

Use [`something`](@ref) to access the value wrapped by a `Some` object.
"""
struct Some{T}
    value::T
end

promote_rule(::Type{Some{T}}, ::Type{Some{S}}) where {T, S<:T} = Some{T}
promote_rule(::Type{Some{T}}, ::Type{Nothing}) where {T} = Union{Some{T}, Nothing}

convert(::Type{Some{T}}, x::Some) where {T} = Some{T}(convert(T, x.value))
convert(::Type{Union{Some{T}, Nothing}}, x::Some) where {T} = convert(Some{T}, x)

convert(::Type{Union{T, Nothing}}, x::Union{T, Nothing}) where {T} = x
convert(::Type{Union{T, Nothing}}, x::Any) where {T} = convert(T, x)
convert(::Type{Nothing}, x::Nothing) = nothing
convert(::Type{Nothing}, x::Any) = throw(MethodError(convert, (Nothing, x)))

function show(io::IO, x::Some)
    if get(io, :typeinfo, Any) == typeof(x)
        show(io, x.value)
    else
        print(io, "Some(")
        show(io, x.value)
        print(io, ')')
    end
end

"""
    notnothing(x)

Throw an error if `x === nothing`, and return `x` if not.
"""
notnothing(x::Any) = x
notnothing(::Nothing) = throw(ArgumentError("nothing passed to notnothing"))

"""
    isnothing(x)

Return `true` if `x === nothing`, and return `false` if not.

!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.
"""
isnothing(::Any) = false
isnothing(::Nothing) = true


"""
    something(x, y...)

Return the first value in the arguments which is not equal to [`nothing`](@ref),
if any. Otherwise throw an error.
Arguments of type [`Some`](@ref) are unwrapped.

# Examples
```jldoctest
julia> something(nothing, 1)
1

julia> something(Some(1), nothing)
1

julia> something(missing, nothing)
missing

julia> something(nothing, nothing)
ERROR: ArgumentError: No value arguments present
```
"""
function something end

something() = throw(ArgumentError("No value arguments present"))
something(x::Nothing, y...) = something(y...)
something(x::Some, y...) = x.value
something(x::Any, y...) = x

"""
    skipnothing(itr)
Return an iterator over the elements in `itr` skipping [`nothing`](@ref) values.
Use [`collect`](@ref) to obtain an `Array` containing the non-`nothing` values in
`itr`. Note that even if `itr` is a multidimensional array, the result will always
be a `Vector` since it is not possible to remove nothings while preserving dimensions
of the input.
# Examples
```jldoctest
julia> sum(skipnothing([1, nothing, 2]))
3
julia> collect(skipnothing([1, nothing, 2]))
2-element Array{Int64,1}:
 1
 2
julia> collect(skipnothing([1 nothing; 2 nothing]))
2-element Array{Int64,1}:
 1
 2
```
"""
skipnothing(itr) = SkipNothing(itr)

struct SkipNothing{T}
    x::T
end
IteratorSize(::Type{<:SkipNothing}) = SizeUnknown()
IteratorEltype(::Type{SkipNothing{T}}) where {T} = IteratorEltype(T)
eltype(::Type{SkipNothing{T}}) where {T} = nonnothingtype(eltype(T))

function iterate(itr::SkipNothing, state...)
    y = iterate(itr.x, state...)
    y === nothing && return nothing
    item, state = y
    while item === nothing
        y = iterate(itr.x, state)
        y === nothing && return nothing
        item, state = y
    end
    item, state
end

# Optimized mapreduce implementation
# The generic method is faster when !(eltype(A) >: Nothing) since it does not need
# additional loops to identify the two first non-nothing values of each block
mapreduce(f, op, itr::SkipNothing{<:AbstractArray}) =
    _mapreduce(f, op, IndexStyle(itr.x), eltype(itr.x) >: Nothing ? itr : itr.x)

function _mapreduce(f, op, ::IndexLinear, itr::SkipNothing{<:AbstractArray})
    A = itr.x
    local ai
    inds = LinearIndices(A)
    i = first(inds)
    ilast = last(inds)
    while i <= ilast
        @inbounds ai = A[i]
        ai === nothing || break
        i += 1
    end
    i > ilast && return mapreduce_empty(f, op, eltype(itr))
    a1 = ai
    i += 1
    while i <= ilast
        @inbounds ai = A[i]
        ai === nothing || break
        i += 1
    end
    i > ilast && return mapreduce_first(f, op, a1)
    # We know A contains at least two non-nothing entries: the result cannot be nothing
    mapreduce_impl(f, op, itr, first(inds), last(inds))
end

_mapreduce(f, op, ::IndexCartesian, itr::SkipNothing) = mapfoldl(f, op, itr)

mapreduce_impl(f, op, A::SkipNothing, ifirst::Integer, ilast::Integer) =
    mapreduce_impl(f, op, A, ifirst, ilast, pairwise_blocksize(f, op))

# Returns nothing when the input contains only nothing values
@noinline function mapreduce_impl(f, op, itr::SkipNothing{<:AbstractArray},
                                  ifirst::Integer, ilast::Integer, blksize::Int)
    A = itr.x
    if ifirst == ilast
        @inbounds a1 = A[ifirst]
        if a1 === nothing
            return nothing
        else
            return mapreduce_first(f, op, a1)
        end
    elseif ifirst + blksize > ilast
        # sequential portion
        local ai
        i = ifirst
        while i <= ilast
            @inbounds ai = A[i]
            ai === nothing || break
            i += 1
        end
        i > ilast && return nothing
        a1 = ai::eltype(itr)
        i += 1
        while i <= ilast
            @inbounds ai = A[i]
            ai === nothing || break
            i += 1
        end
        i > ilast && return mapreduce_first(f, op, a1)
        a2 = ai::eltype(itr)
        i += 1
        v = op(f(a1), f(a2))
        @simd for i = i:ilast
            @inbounds ai = A[i]
            if ai !== nothing
                v = op(v, f(ai))
            end
        end
        return v
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
            return op(v1, v2)
        end
    end
end

nonnothingtype(::Type{Union{T, Nothing}}) where {T} = T
nonnothingtype(::Type{Nothing}) = Union{}
nonnothingtype(::Type{T}) where {T} = T
nonnothingtype(::Type{Any}) = Any
