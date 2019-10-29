"""
    LazyString <: AbstractString

A lazy representation of string interpolation. This is useful when a string
needs to be constructed in a context where performing the actual interpolation
and string construction is unnecessary or undesirable (e.g. in error paths
of of functions).

This type is designed to be cheap to construct at runtime, trying to offload
as much work as possible to either the macro or later printing operations.
"""
struct LazyString <: AbstractString
    parts::Tuple
    LazyString(args...) = new(args)
end

macro lazy_str(string)
    parts = Any[]
    lastidx = idx = 1
    while (idx = findnext('$', string, idx)) !== nothing
        lastidx < idx && push!(parts, string[lastidx:idx-1])
        idx += 1
        expr, idx = Meta.parse(string, idx; greedy=false, raise=false)
        push!(parts, esc(expr))
        lastidx = idx
    end
    lastidx <= lastindex(string) && push!(parts, string[lastidx:end])
    :(LazyString($(parts...)))
end

function show(io::IO, s::LazyString)
    for part in s.parts
        print(io, part)
    end
end
