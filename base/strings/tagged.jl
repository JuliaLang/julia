# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    TaggedString{S <: AbstractString} <: AbstractString

A string with annotated regions (often styling information).

More specifically, this is a thin wrapper around any other [`AbstractString`](@ref),
allows arbitary tagged values to be attached to regions of the wrapped string.

Each tag takes the form of a `Pair{Symbol, <:Any}`, the first value being the
tag name, and the second the value. A single region can be annotated with the
same tag multiple times.

See also [`TaggedChar`](@ref), [`taggedstring`](@ref), [`textproperties`](@ref), and
[`textproperty!`](@ref).

!!! warning
    While the constructors are part of the Base public API, the fields
    of `TaggedString` are not. This is to allow for potential future
    changes in the implementation of this type. Instead use the
    [`textproperties`](@ref), and [`textproperty!`](@ref) getter/setter
    functions.

# Constructors

```julia
TaggedString(s::S<:AbstractString) -> TaggedString{S}
TaggedString(s::S<:AbstractString, properties::Vector{Tuple{UnitRange{Int}, Pair{Symbol, <:Any}}})
```

A TaggedString can also be created with [`taggedstring`](@ref), which acts much
like [`string`](@ref) but preserves any tags present in the arguments.
"""
struct TaggedString{S <: AbstractString} <: AbstractString
    string::S
    properties::Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}
end

"""
    TaggedChar{S <: AbstractChar} <: AbstractChar

A Char annotated with tags.

More specifically, this is a thin wrapper around any other [`AbstractChar`](@ref),
which adds arbitrary tags to the wrapped character.

See also: [`TaggedString`](@ref), [`taggedstring`](@ref), `textproperties`, and
`textproperty!`.

!!! warning
    While the constructors are part of the Base public API, the fields
    of `TaggedChar` are not. This it to allow for potential future
    changes in the implementation of this type. Instead use the
    [`textproperties`](@ref), and [`textproperty!`](@ref) getter/setter
    functions.

# Constructors

```julia
TaggedChar(s::S) -> TaggedChar{S}
TaggedChar(s::S, properties::Vector{Pair{Symbol, <:Any}})
```

# Examples

```julia-repl
julia> TaggedChar('j', :tag => 1)
'j': ASCII/Unicode U+006A (category Ll: Letter, lowercase)
```
"""
struct TaggedChar{C <: AbstractChar} <: AbstractChar
    char::C
    properties::Vector{Pair{Symbol, Any}}
end

## Constructors ##

# When called with overly-specialised arguments

TaggedString(s::AbstractString, props::Vector{<:Tuple{UnitRange{Int}, <:Pair{Symbol, <:Any}}}) =
    TaggedString(s, Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}(props))

TaggedChar(c::AbstractChar, props::Vector{<:Pair{Symbol, <:Any}}) =
    TaggedChar(c, Vector{Pair{Symbol, Any}}(props))

# Constructors to avoid recursive wrapping

TaggedString(s::TaggedString, props::Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}) =
    TaggedString(s.string, vcat(s.properties, props))

TaggedChar(c::TaggedChar, props::Vector{Pair{Symbol, Any}}) =
    TaggedChar(c.char, vcat(s.properties, props))

String(s::TaggedString{String}) = s.string # To avoid pointless overhead

## Conversion/promotion ##

convert(::Type{TaggedString}, s::TaggedString) = s
convert(::Type{TaggedString{S}}, s::S) where {S <: AbstractString} =
    TaggedString(s, Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}())
convert(::Type{TaggedString}, s::S) where {S <: AbstractString} =
    convert(TaggedString{S}, s)
TaggedString(s::S) where {S <: AbstractString} = convert(TaggedString{S}, s)

convert(::Type{TaggedChar}, c::TaggedChar) = c
convert(::Type{TaggedChar{C}}, c::C) where { C <: AbstractChar } =
    TaggedChar{C}(c, Vector{Pair{Symbol, Any}}())
convert(::Type{TaggedChar}, c::C) where { C <: AbstractChar } =
    convert(TaggedChar{C}, c)

TaggedChar(c::AbstractChar) = convert(TaggedChar, c)
TaggedChar(c::UInt32) = convert(TaggedChar, Char(c))
TaggedChar{C}(c::UInt32) where {C <: AbstractChar} = convert(TaggedChar, C(c))

promote_rule(::Type{<:TaggedString}, ::Type{<:AbstractString}) = TaggedString

## AbstractString interface ##

ncodeunits(s::TaggedString) = ncodeunits(s.string)
codeunits(s::TaggedString) = codeunits(s.string)
codeunit(s::TaggedString) = codeunit(s.string)
codeunit(s::TaggedString, i::Integer) = codeunit(s.string, i)
isvalid(s::TaggedString, i::Integer) = isvalid(s.string, i)
@propagate_inbounds iterate(s::TaggedString, i::Integer=firstindex(s)) =
    if i <= lastindex(s.string); (s[i], nextind(s, i)) end
eltype(::Type{<:TaggedString{S}}) where {S} = TaggedChar{eltype(S)}
firstindex(s::TaggedString) = firstindex(s.string)
lastindex(s::TaggedString) = lastindex(s.string)

function getindex(s::TaggedString, i::Integer)
    @boundscheck checkbounds(s, i)
    @inbounds if isvalid(s, i)
        TaggedChar(s.string[i], textproperties(s, i))
    else
        string_index_err(s, i)
    end
end

## AbstractChar interface ##

ncodeunits(c::TaggedChar) = ncodeunits(c.char)
codepoint(c::TaggedChar) = codepoint(c.char)

# Avoid the iteration fallback with comparison
cmp(a::TaggedString, b::AbstractString) = cmp(a.string, b)
cmp(a::AbstractString, b::TaggedString) = cmp(a, b.string)
# To avoid method ambiguity
cmp(a::TaggedString, b::TaggedString) = cmp(a.string, b.string)

==(a::TaggedString, b::TaggedString) =
    a.string == b.string && a.properties == b.properties

==(a::TaggedString, b::AbstractString) = isempty(a.properties) && a.string == b
==(a::AbstractString, b::TaggedString) = isempty(b.properties) && a == b.string

"""
    taggedstring(values...)

Create a `TaggedString` from any number of `values` using their
[`print`](@ref)ed representation.

This acts like [`string`](@ref), but takes care to preserve any properties
present (in the form of [`TaggedString`](@ref) or [`TaggedChar`](@ref) values).

See also [`TaggedString`](@ref) and [`TaggedChar`](@ref).

## Examples

```
julia> taggedstring("now a TaggedString")
"now a TaggedString"

julia> taggedstring(TaggedString("tagged", [(1:6, :tag => 1)]), ", and untagged")
"tagged, and untagged"
```
"""
function taggedstring(xs...)
    isempty(xs) && return TaggedString("")
    size = mapreduce(_str_sizehint, +, xs)
    s = IOContext(IOBuffer(sizehint=size), :color => true)
    properties = Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}()
    for x in xs
        if x isa TaggedString
            for (region, prop) in x.properties
                push!(properties, (s.io.size .+ (region), prop))
            end
            print(s, x.string)
        elseif x isa SubString{<:TaggedString}
            for (region, prop) in x.string.properties
                start, stop = first(region), last(region)
                if start <= x.offset + x.ncodeunits && stop > x.offset
                    rstart = s.io.size + max(0, start - x.offset) + 1
                    rstop = s.io.size + min(stop, x.offset + x.ncodeunits) - x.offset
                    push!(properties, (rstart:rstop, prop))
                end
            end
            print(s, SubString(x.string.string, x.offset, x.ncodeunits, Val(:noshift)))
        elseif x isa TaggedChar
            for prop in x.properties
                push!(properties, (1+s.io.size:1+s.io.size, prop))
            end
            print(s, x.char)
        else
            print(s, x)
        end
    end
    str = String(resize!(s.io.data, s.io.size))
    TaggedString(str, properties)
end

taggedstring(s::TaggedString) = s
taggedstring(c::TaggedChar) =
    TaggedString(string(c.char), [(1:ncodeunits(c), c.properties)])

TaggedString(s::SubString{<:TaggedString}) = taggedstring(s)

"""
    taggedstring_optimize!(str::TaggedString)

Merge contiguous identical tags in `str`.
"""
function taggedstring_optimize!(s::TaggedString)
    last_seen = Dict{Pair{Symbol, Any}, Int}()
    i = 1
    while i <= length(s.properties)
        region, keyval = s.properties[i]
        prev = get(last_seen, keyval, 0)
        if prev > 0
            lregion, _ = s.properties[prev]
            if last(lregion) + 1 == first(region)
                s.properties[prev] =
                    setindex(s.properties[prev],
                             first(lregion):last(region),
                             1)
                deleteat!(s.properties, i)
            else
                delete!(last_seen, keyval)
            end
        else
            last_seen[keyval] = i
            i += 1
        end
    end
    s
end

function repeat(str::TaggedString, r::Integer)
    r == 0 && return one(TaggedString)
    r == 1 && return str
    untagged = repeat(str.string, r)
    properties = Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}()
    len = ncodeunits(str)
    fullregion = firstindex(str):lastindex(str)
    for (region, prop) in str.properties
        if region == fullregion
            push!(properties, (firstindex(untagged):lastindex(untagged), prop))
        end
    end
    for offset in 0:len:(r-1)*len
        for (region, prop) in str.properties
            if region != fullregion
                push!(properties, (region .+ offset, prop))
            end
        end
    end
    TaggedString(untagged, properties) |> taggedstring_optimize!
end

repeat(str::SubString{<:TaggedString}, r::Integer) =
    repeat(TaggedString(str), r)

function repeat(c::TaggedChar, r::Integer)
    str = repeat(c.char, r)
    fullregion = firstindex(str):lastindex(str)
    TaggedString(str, [(fullregion, prop) for prop in c.properties])
end

function reverse(s::TaggedString)
    lastind = lastindex(s)
    TaggedString(reverse(s.string),
                 [(UnitRange(1 + lastind - last(region),
                             1 + lastind - first(region)),
                   prop)
                  for (region, prop) in s.properties])
end

# TODO optimise?
reverse(s::SubString{<:TaggedString}) = reverse(TaggedString(s))

# TODO implement `replace(::TaggedString, ...)`

## End AbstractString interface ##

"""
    textproperty!(str::TaggedString, [range::UnitRange{Int}], tag::Symbol, value)
    textproperty!(str::SubString{TaggedString}, [range::UnitRange{Int}], tag::Symbol, value)

Add `tag` with `value` in `str`, over `range` if specified or the whole
string otherwise.
"""
function textproperty!(s::TaggedString, range::UnitRange{Int}, tag::Symbol, val)
    indices = searchsorted(s.properties, (range,), by=first)
    tagindex = filter(i -> first(s.properties[i][2]) === tag, indices)
    if length(tagindex) == 1
        if val === nothing
            deleteat!(s.properties, first(tagindex))
        else
            s.properties[first(tagindex)] = (range, Pair{Symbol, Any}(tag, val))
        end
    else
        splice!(s.properties, indices, [(range, Pair{Symbol, Any}(tag, val))])
    end
    s
end

textproperty!(ss::TaggedString, tag::Symbol, value) =
    textproperty!(ss, firstindex(ss):lastindex(ss), tag, value)

textproperty!(s::SubString{<:TaggedString}, range::UnitRange{Int}, tag::Symbol, value) =
    (textproperty!(s.string, s.offset .+ (range), tag, value); s)

textproperty!(s::SubString{<:TaggedString}, tag::Symbol, value) =
    (textproperty!(s.string, s.offset .+ (1:s.ncodeunits), tag, value); s)

"""
    textproperties(s::TaggedString, i::Integer)
    textproperties(s::SubString{TaggedString}, i::Integer)

Get the text properties that apply to `s` at index `i`.
"""
function textproperties(s::TaggedString, i::Integer)
    # TODO optimise
    props = filter(tag -> !isempty(intersect(i:i, first(tag))),
                   s.properties)
    last.(props)
end

textproperties(s::SubString{<:TaggedString}, i::Integer) =
    textproperties(s.string, s.offset + i)

"""
    textproperties(c::TaggedChar)

Get the properties that apply to `c`.
"""
textproperties(c::TaggedChar) = c.properties

"""
    textproperty!(char::TaggedChar, tag::Symbol, value)

 Add a `tag` with `value` to `char`.
"""
textproperty!(c::TaggedString, tag::Symbol, value)
