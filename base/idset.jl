# This file is a part of Julia. License is MIT: https://julialang.org/license

# Like Set, but using IdDict
mutable struct IdSet{T} <: AbstractSet{T}
    dict::IdDict{T,Nothing}

    IdSet{T}() where {T} = new(IdDict{T,Nothing}())
    IdSet{T}(s::IdSet{T}) where {T} = new(copy(s.dict))
end

IdSet{T}(itr) where {T} = union!(IdSet{T}(), itr)
IdSet() = IdSet{Any}()

copymutable(s::IdSet) = typeof(s)(s)
copy(s::IdSet) = typeof(s)(s)

isempty(s::IdSet) = isempty(s.dict)
length(s::IdSet)  = length(s.dict)
in(@nospecialize(x), s::IdSet) = haskey(s.dict, x)
push!(s::IdSet, @nospecialize(x)) = (s.dict[x] = nothing; s)
pop!(s::IdSet, @nospecialize(x)) = (pop!(s.dict, x); x)
pop!(s::IdSet, @nospecialize(x), @nospecialize(default)) = (x in s ? pop!(s, x) : default)
delete!(s::IdSet, @nospecialize(x)) = (delete!(s.dict, x); s)

sizehint!(s::IdSet, newsz) = (sizehint!(s.dict, newsz); s)
empty!(s::IdSet) = (empty!(s.dict); s)

filter!(f, d::IdSet) = unsafe_filter!(f, d)

function iterate(s::IdSet, state...)
    y = iterate(s.dict, state...)
    y === nothing && return nothing
    ((k, _), i) = y
    return (k, i)
end
