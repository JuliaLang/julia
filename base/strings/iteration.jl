# A specialized iterator for EachIndex of strings
struct EachStringIndex{T<:AbstractString}
    s::T
end
keys(s::AbstractString) = EachStringIndex(s)

length(e::EachStringIndex) = length(e.s)
first(::EachStringIndex) = 1
last(e::EachStringIndex) = lastindex(e.s)
eltype(::Type{<:EachStringIndex}) = Int

# Iteration over StringNext
#
# Any new subtype of AbstractString, should override
#
#   next(::StringNext{MyString}, state)
#
# to provide iteration over the string and its indices. All other iteration methods,
# including iteration over strings, iteration over pairs, indexing into string,
# iteration over indicies alone are derived from this method.

const StringNext{T<:AbstractString} = Iterators.Next{T, EachStringIndex{T}}
StringNext(x::T) where {T<:AbstractString} = Next(x)
StringNext(x::T, idx) where {T<:AbstractString} = Next(x, idx)
StringNext(x::T, idx, itr) where {T<:AbstractString} = Next(x, idx, itr)

start(sp::StringNext) = 1
function done(s::StringNext, i)
    if isa(i, Integer)
        return i > ncodeunits(s.data)
    else
        throw(MethodError(done, (s, i)))
    end
end
function next(s::StringNext, i)
    if isa(i, Integer) && !isa(i, Int)
        return next(s, Int(i))
    else
        throw(MethodError(next, (s, i)))
    end
end

# Derive iteration over pairs from `StringNext`
const StringPairs{T<:AbstractString} = Iterators.Pairs{Int, Char, EachStringIndex{T}, T}
StringPairs{T}(x::T) where {T<:AbstractString} = Iterators.Pairs(x, eachindex(x))
StringPairs(x::T) where {T<:AbstractString} = StringPairs{T}(x)

Iterators.pairs(s::AbstractString) = StringPairs(s)

start(e::StringPairs) = (firstindex(e.data), start(StringNext(e.data)))
done(e::StringPairs, (idx, state)) = done(StringNext(e.data), state)
function next(s::StringPairs, (idx, state))
    ((c, nidx), state) = next(StringNext(s.data), state)
    Pair(idx, c), (nidx, state)
end

# Derive reverse pair iteration.
# N.B. String implementers may wish to override
#
#    next(s::Iterators.Reverse{<:StringPairs}, idx)
#
# to provide efficient variable-length reverse decoding
Iterators.reverse(s::StringPairs) = Iterators.Reverse(s)

start(e::Iterators.Reverse{<:StringPairs}) = ncodeunits(e.itr.data)+1
done(e::Iterators.Reverse{<:StringPairs}, idx) = idx == firstindex(e.itr.data)
function next(s::Iterators.Reverse{<:StringPairs}, idx)
    tidx = thisind(s.itr.data, idx-1)
    (c, nidx) = first(Next(s.itr.data, tidx))
    Pair(tidx, c), tidx
end

function prev(s::AbstractString, idx)
    (i, c), _ = next(Iterators.Reverse(StringPairs(s)), idx)
    (c, i)
end


# Derive iteration over strings from `StringNext`
start(s::AbstractString) = start(StringNext(s))
done(s::AbstractString, state) = done(StringNext(s), state)
function next(s::AbstractString, state)
    ((c, _), state) = next(StringNext(s), state)
    (c, state)
end

eltype(::Type{<:AbstractString}) = Char
sizeof(s::AbstractString) = ncodeunits(s) * sizeof(codeunit(s))
firstindex(s::AbstractString) = 1
lastindex(s::AbstractString) = thisind(s, ncodeunits(s))

function getindex(s::AbstractString, i::Integer)
    @boundscheck checkbounds(s, i)
    @inbounds return isvalid(s, i) ? first(first(Next(s, i))) : string_index_err(s, i)
end

getindex(s::AbstractString, i::Colon) = s
# TODO: handle other ranges with stride ±1 specially?
# TODO: add more @propagate_inbounds annotations?
getindex(s::AbstractString, v::AbstractVector{<:Integer}) =
    sprint(io->(for i in v; write(io, s[i]) end), sizehint=length(v))
getindex(s::AbstractString, v::AbstractVector{Bool}) =
    throw(ArgumentError("logical indexing not supported for strings"))

function get(s::AbstractString, i::Integer, default)
# TODO: use ternary once @inbounds is expression-like
    if checkbounds(Bool, s, i)
        @inbounds return s[i]
    else
        return default
    end
end

# Derive iteration over indices from `StringNext`
start(e::EachStringIndex) = start(StringPairs(e.s))
done(e::EachStringIndex, state) = done(StringPairs(e.s), state)
function next(e::EachStringIndex, state)
    ((idx, _), state) = next(StringPairs(e.s), state)
    (idx, state)
end