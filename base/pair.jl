# This file is a part of Julia. License is MIT: https://julialang.org/license

struct Pair{A,B}
    first::A
    second::B
end

const => = Pair

start(p::Pair) = 1
done(p::Pair, i) = i>2
next(p::Pair, i) = (getfield(p,i), i+1)
eltype(p::Pair{A,B}) where {A,B} = Union{A,B}

indexed_next(p::Pair, i::Int, state) = (getfield(p,i), i+1)

hash(p::Pair, h::UInt) = hash(p.second, hash(p.first, h))

==(p::Pair, q::Pair) = (p.first==q.first) & (p.second==q.second)
isequal(p::Pair, q::Pair) = isequal(p.first,q.first) & isequal(p.second,q.second)

isless(p::Pair, q::Pair) = ifelse(!isequal(p.first,q.first), isless(p.first,q.first),
                                                             isless(p.second,q.second))
getindex(p::Pair,i::Int) = getfield(p,i)
getindex(p::Pair,i::Real) = getfield(p, convert(Int, i))
reverse{A,B}(p::Pair{A,B}) = Pair{B,A}(p.second, p.first)

endof(p::Pair) = 2
length(p::Pair) = 2
first(p::Pair) = p.first
last(p::Pair) = p.second

convert(::Type{Pair{A,B}}, x::Pair{A,B}) where {A,B} = x
function convert(::Type{Pair{A,B}}, x::Pair) where {A,B}
    Pair{A,B}(convert(A, x[1]), convert(B, x[2]))
end

promote_rule(::Type{Pair{A1,B1}}, ::Type{Pair{A2,B2}}) where {A1,B1,A2,B2} =
    Pair{promote_type(A1, A2), promote_type(B1, B2)}
