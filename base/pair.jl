# This file is a part of Julia. License is MIT: https://julialang.org/license

struct Pair{A, B}
    first::A
    second::B
    function Pair{A, B}(@nospecialize(a), @nospecialize(b)) where {A, B}
        @_inline_meta
        # if we didn't inline this, it's probably because the callsite was actually dynamic
        # to avoid potentially compiling many copies of this, we mark the arguments with `@nospecialize`
        # but also mark the whole function with `@inline` to ensure we will inline it whenever possible
        # (even if `convert(::Type{A}, a::A)` for some reason was expensive)
        return new(a, b)
    end
end
Pair(a, b) = Pair{typeof(a), typeof(b)}(a, b)
const => = Pair

"""
    Pair(x, y)
    x => y

Construct a `Pair` object with type `Pair{typeof(x), typeof(y)}`. The elements
are stored in the fields `first` and `second`. They can also be accessed via
iteration (but a `Pair` is treated as a single "scalar" for broadcasting operations).

See also: [`Dict`](@ref)

# Examples
```jldoctest
julia> p = "foo" => 7
"foo" => 7

julia> typeof(p)
Pair{String,Int64}

julia> p.first
"foo"

julia> for x in p
           println(x)
       end
foo
7
```
"""
Pair, =>

eltype(p::Type{Pair{A, B}}) where {A, B} = Union{A, B}
iterate(p::Pair, i=1) = i > 2 ? nothing : (getfield(p, i), i + 1)
indexed_iterate(p::Pair, i::Int, state=1) = (getfield(p, i), i + 1)

hash(p::Pair, h::UInt) = hash(p.second, hash(p.first, h))

==(p::Pair, q::Pair) = (p.first==q.first) & (p.second==q.second)
isequal(p::Pair, q::Pair) = isequal(p.first,q.first) & isequal(p.second,q.second)

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
    Pair{A,B}(convert(A, x[1]), convert(B, x[2]))
end

promote_rule(::Type{Pair{A1,B1}}, ::Type{Pair{A2,B2}}) where {A1,B1,A2,B2} =
    Pair{promote_type(A1, A2), promote_type(B1, B2)}

"""
    keytype(type)

Get the key type of a pair type. Behaves similarly to [`eltype`](@ref).

# Examples
```jldoctest
julia> keytype(Int32(1) => "foo")
Int32

julia> keytype(Pair(Int32(1) => "foo"))
Int32
```
"""
keytype(::Type{<:Pair{K,V}}) where {K,V} = K
keytype(a::Pair) = keytype(typeof(a))

"""
    valtype(type)

Get the value type of a pair type. Behaves similarly to [`eltype`](@ref).

# Examples
```jldoctest
julia> valtype(Int32(1) => "foo")
String

julia> valtype(Pair(Int32(1), "foo"))
String
```
"""
valtype(::Type{<:Pair{K,V}}) where {K,V} = V
valtype(a::Pair) = valtype(typeof(a))