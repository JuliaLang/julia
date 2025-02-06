# This file is a part of Julia. License is MIT: https://julialang.org/license

const => = Pair

"""
    Pair(x, y)
    x => y

Construct a `Pair` object with type `Pair{typeof(x), typeof(y)}`. The elements
are stored in the fields `first` and `second`. They can also be accessed via
iteration (but a `Pair` is treated as a single "scalar" for broadcasting operations).

See also [`Dict`](@ref).

# Examples
```jldoctest
julia> p = "foo" => 7
"foo" => 7

julia> typeof(p)
Pair{String, Int64}

julia> p.first
"foo"

julia> for x in p
           println(x)
       end
foo
7

julia> replace.(["xops", "oxps"], "x" => "o")
2-element Vector{String}:
 "oops"
 "oops"
```
"""
Pair, =>

eltype(p::Type{Pair{A, B}}) where {A, B} = Union{A, B}
iterate(p::Pair, i=1) = i > 2 ? nothing : (getfield(p, i), i + 1)
indexed_iterate(p::Pair, i::Int, state=1) = (getfield(p, i), i + 1)

const _pairhash_seed = 0x94cb2bb20a28ce96 % UInt
hash(p::Pair, h::UInt) = hash(p.second, hash(p.first, hash(_pairhash_seed, h)))

==(p::Pair, q::Pair) = (p.first==q.first) & (p.second==q.second)
isequal(p::Pair, q::Pair) = isequal(p.first,q.first)::Bool & isequal(p.second,q.second)::Bool

isless(p::Pair, q::Pair) = ifelse(!isequal(p.first,q.first), isless(p.first,q.first),
                                                             isless(p.second,q.second))
getindex(p::Pair,i::Int) = getfield(p,i)
getindex(p::Pair,i::Real) = getfield(p, convert(Int, i))
reverse(p::Pair{A,B}) where {A,B} = Pair{B,A}(p.second, p.first)

firstindex(p::Pair) = 1
lastindex(p::Pair) = 2
length(p::Pair) = 2
first(p::Pair) = p.first
last(p::Pair) = p.second

convert(::Type{Pair{A,B}}, x::Pair{A,B}) where {A,B} = x
function convert(::Type{Pair{A,B}}, x::Pair) where {A,B}
    a = getfield(x, :first)
    a isa A || (a = convert(A, a))
    b = getfield(x, :second)
    b isa B || (b = convert(B, b))
    return Pair{A,B}(a, b)::Pair{A,B}
end

promote_rule(::Type{Pair{A1,B1}}, ::Type{Pair{A2,B2}}) where {A1,B1,A2,B2} =
    Pair{promote_type(A1, A2), promote_type(B1, B2)}
