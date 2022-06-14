"""
    LazyString <: AbstractString

A lazy representation of string interpolation. This is useful when a string
needs to be constructed in a context where performing the actual interpolation
and string construction is unnecessary or undesirable (e.g. in error paths
of functions).

This type is designed to be cheap to construct at runtime, trying to offload
as much work as possible to either the macro or later printing operations.

# Examples

```jldoctest
julia> n = 5; str = LazyString("n is ", n)
"n is 5"
```

See also [`@lazy_str`](@ref).

!!! compat "Julia 1.8"
    `LazyString` requires Julia 1.8 or later.
"""
struct LazyString <: AbstractString
    parts::Tuple
    LazyString(args...) = new(args)
end

"""
    lazy"str"

Create a [`LazyString`](@ref) using regular string interpolation syntax.
Note that interpolations are *evaluated* at LazyString construction time,
but *printing* is delayed until the first access to the string.

# Examples

```
julia> n = 5; str = lazy"n is \$n"
"n is 5"

julia> typeof(str)
LazyString
```

!!! compat "Julia 1.8"
    `lazy"str"` requires Julia 1.8 or later.
"""
macro lazy_str(text)
    parts = Any[]
    lastidx = idx = 1
    while (idx = findnext('$', text, idx)) !== nothing
        lastidx < idx && push!(parts, text[lastidx:idx-1])
        idx += 1
        expr, idx = Meta.parseatom(text, idx; filename=string(__source__.file))
        push!(parts, esc(expr))
        lastidx = idx
    end
    lastidx <= lastindex(text) && push!(parts, text[lastidx:end])
    :(LazyString($(parts...)))
end

function String(l::LazyString)
    return sprint() do io
        for p in l.parts
            print(io, p)
        end
    end
end

hash(s::LazyString, h::UInt64) = hash(String(s), h)
lastindex(s::LazyString) = lastindex(String(s))
iterate(s::LazyString) = iterate(String(s))
iterate(s::LazyString, i::Integer) = iterate(String(s), i)
isequal(a::LazyString, b::LazyString) = isequal(String(a), String(b))
==(a::LazyString, b::LazyString) = (String(a) == String(b))
ncodeunits(s::LazyString) = ncodeunits(String(s))
codeunit(s::LazyString) = codeunit(String(s))
codeunit(s::LazyString, i::Integer) = codeunit(String(s), i)
isvalid(s::LazyString, i::Integer) = isvalid(String(s), i)
