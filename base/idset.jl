# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    IdSet{T}([itr])
    IdSet()

`IdSet{T}()` constructs a set (see [`Set`](@ref)) using
`===` as equality with values of type `T`.

In the example below, the values are all `isequal` so they get overwritten in the ordinary `Set`.
The `IdSet` compares by `===` and so preserves the 3 different values.

# Examples
```jldoctest; filter = r"\\n\\s*(1|1\\.0|true)"
julia> Set(Any[true, 1, 1.0])
Set{Any} with 1 element:
  1.0

julia> IdSet{Any}(Any[true, 1, 1.0])
IdSet{Any} with 3 elements:
  1.0
  1
  true
```
"""
mutable struct IdSet{K} <: AbstractSet{K}
    list::Memory{Any}
    idxs::Union{Memory{UInt8}, Memory{UInt16}, Memory{UInt32}}
    count::Int
    max::Int # n.b. always <= length(list)
    IdSet{T}() where {T} = new(Memory{Any}(undef, 0), Memory{UInt8}(undef, 0), 0, 0)
    IdSet{T}(s::IdSet{T}) where {T} = new(copy(s.list), copy(s.idxs), s.count, s.max)
end
IdSet{T}(itr) where {T} = union!(IdSet{T}(), itr)
IdSet() = IdSet{Any}()

copymutable(s::IdSet) = typeof(s)(s)
emptymutable(s::IdSet{T}, ::Type{U}=T) where {T,U} = IdSet{U}()
copy(s::IdSet) = typeof(s)(s)

haskey(s::IdSet, @nospecialize(key)) = ccall(:jl_idset_peek_bp, Int, (Any, Any, Any), s.list, s.idxs, key) != -1
isempty(s::IdSet) = s.count == 0
length(s::IdSet)  = s.count
in(@nospecialize(x), s::IdSet) = haskey(s, x)
function push!(s::IdSet, @nospecialize(x))
    idx = ccall(:jl_idset_peek_bp, Int, (Any, Any, Any), s.list, s.idxs, x)
    if idx >= 0
        s.list[idx + 1] = x
    else
        if s.max < length(s.list)
            idx = s.max
            @assert !isassigned(s.list, idx + 1)
            s.list[idx + 1] = x
            s.max = idx + 1
        else
            newidx = RefValue{Int}(0)
            setfield!(s, :list, ccall(:jl_idset_put_key, Any, (Any, Any, Ptr{Int}), s.list, x, newidx))
            idx = newidx[]
            s.max = idx < 0 ? -idx : idx + 1
        end
        @assert s.list[s.max] === x
        setfield!(s, :idxs, ccall(:jl_idset_put_idx, Any, (Any, Any, Int), s.list, s.idxs, idx))
        s.count += 1
    end
    s
end
function _pop!(s::IdSet, @nospecialize(x))
    removed = ccall(:jl_idset_pop, Int, (Any, Any, Any), s.list, s.idxs, x)
    if removed != -1
        s.count -= 1
        while s.max > 0 && !isassigned(s.list, s.max)
            s.max -= 1
        end
    end
    removed
end
pop!(s::IdSet, @nospecialize(x)) = _pop!(s, x) == -1 ? throw(KeyError(x)) : x
pop!(s::IdSet, @nospecialize(x), @nospecialize(default)) = _pop!(s, x) == -1 ? default : x
delete!(s::IdSet, @nospecialize(x)) = (_pop!(s, x); s)

function sizehint!(s::IdSet, newsz)
    # TODO: grow/compact list and perform rehash, if profitable?
    # TODO: shrink?
    # s.list = resize(s.list, newsz)
    # newsz = _tablesz(newsz)
    # oldsz = length(s.idxs)
    # #grow at least 25%
    # if newsz < (oldsz*5)>>2
    #     return s
    # end
    # rehash!(s, newsz)
    nothing
end

function empty!(s::IdSet)
    fill!(s.idxs, 0x00)
    list = s.list
    for i = 1:s.max
        _unsetindex!(list, i)
    end
    s.count = 0
    s.max = 0
    s
end

filter!(f, d::IdSet) = unsafe_filter!(f, d)

function iterate(s::IdSet{S}, state=0) where {S}
    while true
        state += 1
        state > s.max && return nothing
        isassigned(s.list, state) && return s.list[state]::S, state
    end
end
