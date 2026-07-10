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

# Extended help
## Safety properties for concurrent programs

A lazy string itself does not introduce any concurrency problems even if it is printed in
multiple Julia tasks.  However, if `print` methods on a captured value can have a
concurrency issue when invoked without synchronizations, printing the lazy string may cause
an issue.  Furthermore, the `print` methods on the captured values may be invoked multiple
times, though only exactly one result will be returned.

!!! compat "Julia 1.9"
    `LazyString` is safe in the above sense in Julia 1.9 and later.
"""
mutable struct LazyString <: AbstractString
    const parts::Tuple
    const compact::Bool # compact & limited output
    # Created on first access
    @atomic str::Union{String,Nothing}
    global _LazyString(parts, str) = new(parts, false, str)
    LazyString(args...; compact::Bool=false) = new(args, compact, nothing)
end

"""
    lazy"str"
    lazy"str"c

Create a [`LazyString`](@ref) using regular string interpolation syntax.
Note that interpolations are *evaluated* at LazyString construction time,
but *printing* is delayed until the first access to the string.  The `lazy"str"c`
variant uses an [`IOContext`](@ref) with `:compact=>true, :limit=>true` when
printing, in order to keep the output string compact.

See [`LazyString`](@ref) documentation for the safety properties for concurrent programs.

# Examples

```
julia> n = 5; str = lazy"n is \$n"
"n is 5"

julia> typeof(str)
LazyString
```

!!! compat "Julia 1.8"
    `lazy"str"` requires Julia 1.8 or later.

!!! compat "Julia 1.14"
    `lazy"str"c` requires Julia 1.14 or later.
"""
macro lazy_str(text, flags...)
    compact = flags == ("c",)
    isempty(flags) || compact || throw(ArgumentError("unknown lazy-string flag: $flags"))
    parts = Any[]
    lastidx = idx = 1
    while (idx = findnext('$', text, idx)) !== nothing
        lastidx < idx && push!(parts, text[lastidx:prevind(text, idx)])
        idx += 1
        expr, idx = Meta.parseatom(text, idx; filename=string(__source__.file))
        push!(parts, esc(expr))
        lastidx = idx
    end
    lastidx <= lastindex(text) && push!(parts, text[lastidx:end])
    :(LazyString($(parts...); compact=$compact))
end

function String(l::LazyString)
    old = @atomic :acquire l.str
    old === nothing || return old
    str = sprint() do io
        if l.compact
            print(IOContext(io, :compact=>true, :limit=>true), l.parts...)
        else
            print(io, l.parts...)
        end
    end
    old, ok = @atomicreplace :acquire_release :acquire l.str nothing => str
    return ok ? str : (old::String)
end

hash(s::LazyString, h::UInt64) = hash(String(s), h)
lastindex(s::LazyString) = lastindex(String(s))
iterate(s::LazyString) = iterate(String(s))
iterate(s::LazyString, i::Integer) = iterate(String(s), i)
isequal(a::LazyString, b::LazyString) = isequal(String(a), String(b))
==(a::LazyString, b::LazyString) = (String(a) == String(b))
ncodeunits(s::LazyString) = ncodeunits(String(s))
codeunit(s::LazyString) = codeunit("") # returns UInt8
codeunit(s::LazyString, i::Integer) = codeunit(String(s), i)
codeunits(s::LazyString) = codeunits(String(s))
isvalid(s::LazyString, i::Integer) = isvalid(String(s), i)
