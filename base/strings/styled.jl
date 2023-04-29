# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    StyledString{S <: AbstractString} <: AbstractString

A string with annotated regions (often styling information).

More specifically, this is a thin wrapper around any other [`AbstractString`](@ref),
which adds arbitrary named annotations to regions of the wrapped string.

See also [`StyledChar`](@ref), [`styledstring`](@ref), [`S""`](@ref @S_str), and [`Face`](@ref).

# Constructors

In most cases the easiest way to construct a `StyledString` is via the
[`S""`](@ref @S_str) string macro (which see), however a number of constructors
are also availible.

```julia
StyledString(s::S<:AbstractString) -> StyledString{S}
StyledString(s::S<:AbstractString, props::Pair{Symbol, <:Any}...)
StyledString(s::S<:AbstractString, properties::Vector{Tuple{UnitRange{Int}, Pair{Symbol, <:Any}}})
```

A StyledString can also be created with [`styledstring`](@ref), which acts much
like [`string`](@ref) but preserves any styling present in the arguments.

# Examples

```jldoctest
julia> StyledString("hello there", :face => :italic)
"hello there"

julia> StyledString("more text", :tag => 1)
"more text"
```
"""
struct StyledString{S <: AbstractString} <: AbstractString
    string::S
    properties::Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}
end

"""
    StyledChar{S <: AbstractChar} <: AbstractChar

A Char annotated by properties (often styling information).

More specifically, this is a thin wrapper around any other [`AbstractChar`](@ref),
which adds arbitrary named annotations to the wrapped character.

# Constructors

```julia
StyledChar(s::S) -> StyledChar{S}
StyledChar(s::S, props::Pair{Symbol, <:Any}...)
StyledChar(s::S, properties::Vector{Pair{Symbol, <:Any}})
```

# Examples

```jldoctest
julia> StyledChar('j', :face => :blue)
'j': ASCII/Unicode U+006A (category Ll: Letter, lowercase)

julia> StyledChar('j', :tag => :1)
'j': ASCII/Unicode U+006A (category Ll: Letter, lowercase)
```
"""
struct StyledChar{C <: AbstractChar} <: AbstractChar
    char::C
    properties::Vector{Pair{Symbol, Any}}
end

## Constructors ##

StyledString(s::AbstractString, prop::Pair{Symbol, <:Any}, props::Pair{Symbol, <:Any}...) =
    StyledString(s, firstindex(s):lastindex(s), prop, props...)

StyledString(s::AbstractString, region::UnitRange{Int}, props::Pair{Symbol, <:Any}...) =
    StyledString(s, [(region, Pair{Symbol, Any}(first(p), last(p)))
                      for p in props])

StyledString(s::AbstractString, props::Vector{<:Pair{Symbol, <:Any}}) =
    StyledString(s, [(firstindex(s):lastindex(s), p) for p in props])

# Constructors called with overly-specialised arguments

StyledString(s::AbstractString, props::Vector{<:Tuple{UnitRange{Int}, <:Pair{Symbol, <:Any}}}) =
    StyledString(s, Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}(props))

StyledChar(c::AbstractChar, prop::Pair{Symbol, <:Any}, props::Pair{Symbol, <:Any}...) =
    StyledChar(c, Vector{Pair{Symbol, Any}}(vcat(prop, props...)))

# Constructors to avoid recursive wrapping

StyledString(s::StyledString, props::Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}) =
    StyledString(s.string, vcat(s.properties, props))

StyledChar(c::StyledChar, props::Vector{Pair{Symbol, Any}}) =
    StyledChar(c.char, vcat(s.properties, props))

# To avoid pointless overhead
String(s::StyledString{String}) = s.string

## Conversion/promotion ##

convert(::Type{StyledString}, s::StyledString) = s
convert(::Type{StyledString{S}}, s::S) where {S <: AbstractString} =
    StyledString(s, Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}())
convert(::Type{StyledString}, s::S) where {S <: AbstractString} =
    convert(StyledString{S}, s)
StyledString(s::S) where {S <: AbstractString} = convert(StyledString{S}, s)

convert(::Type{StyledChar}, c::StyledChar) = c
convert(::Type{StyledChar{C}}, c::C) where { C <: AbstractChar } =
    StyledChar{C}(c, Vector{Pair{Symbol, Any}}())
convert(::Type{StyledChar}, c::C) where { C <: AbstractChar } =
    convert(StyledChar{C}, c)

StyledChar(c::AbstractChar) = convert(StyledChar, c)
StyledChar(c::UInt32) = convert(StyledChar, Char(c))
StyledChar{C}(c::UInt32) where {C <: AbstractChar} = convert(StyledChar, C(c))

promote_rule(::Type{<:StyledString}, ::Type{<:AbstractString}) = StyledString

## AbstractString interface ##

ncodeunits(s::StyledString) = ncodeunits(s.string)
codeunits(s::StyledString) = codeunits(s.string)
codeunit(s::StyledString) = codeunit(s.string)
codeunit(s::StyledString, i::Integer) = codeunit(s.string, i)
isvalid(s::StyledString, i::Integer) = isvalid(s.string, i)
@propagate_inbounds iterate(s::StyledString, i::Integer=firstindex(s)) =
    if i <= lastindex(s.string); (s[i], nextind(s, i)) end
eltype(::Type{<:StyledString{S}}) where {S} = StyledChar{eltype(S)}
firstindex(s::StyledString) = firstindex(s.string)
lastindex(s::StyledString) = lastindex(s.string)

function getindex(s::StyledString, i::Integer)
    @boundscheck checkbounds(s, i)
    @inbounds if isvalid(s, i)
        StyledChar(s.string[i], textproperties(s, i))
    else
        string_index_err(s, i)
    end
end

## AbstractChar interface ##

ncodeunits(c::StyledChar) = ncodeunits(c.char)
codepoint(c::StyledChar) = codepoint(c.char)

# Avoid the iteration fallback with comparison
cmp(a::StyledString, b::AbstractString) = cmp(a.string, b)
cmp(a::AbstractString, b::StyledString) = cmp(a, b.string)
# To avoid method ambiguity
cmp(a::StyledString, b::StyledString) = cmp(a.string, b.string)

==(a::StyledString, b::StyledString) =
    a.string == b.string && a.properties == b.properties

==(a::StyledString, b::AbstractString) = isempty(a.properties) && a.string == b
==(a::AbstractString, b::StyledString) = isempty(b.properties) && a == b.string

"""
    styledstring(values...)

Create a `StyledString` from any number of `values` using their
[`print`](@ref)ed representation.

This acts like [`string`](@ref), but takes care to preserve any properties
present (in the form of [`StyledString`](@ref) or [`StyledChar`](@ref) values).

See also [`StyledString`](@ref), [`StyledChar`](@ref), and [`S""`](@ref @S_str).

## Examples

```
julia> styledstring("now a StyledString")
"now a StyledString"

julia> styledstring(S"{yellow:styled text}", ", and unstyled")
"styled text, and unstyled"
```
"""
function styledstring(xs...)
    isempty(xs) && return StyledString("")
    size = mapreduce(_str_sizehint, +, xs)
    s = IOContext(IOBuffer(sizehint=size), :color => true)
    properties = Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}()
    for x in xs
        if x isa StyledString
            for (region, prop) in x.properties
                push!(properties, (s.io.size .+ (region), prop))
            end
            print(s, x.string)
        elseif x isa SubString{<:StyledString}
            for (substr, props) in eachstyle(x)
                region = s.io.size .+ (1+substr.offset:prevind(substr.string, 1+substr.offset+substr.ncodeunits)) .- x.offset
                for prop in props
                    push!(properties, (region, prop))
                end
            end
            print(s, SubString(x.string.string, x.offset, x.ncodeunits, Val(:noshift)))
        elseif x isa StyledChar
            for prop in x.properties
                push!(properties, (1+s.io.size:1+s.io.size, prop))
            end
            print(s, x.char)
        else
            print(s, x)
        end
    end
    str = String(resize!(s.io.data, s.io.size))
    StyledString(str, properties)
end

"""
    styledstring_optimize!(str::StyledString)

Merge contiguous identical properties in `str`.
"""
function styledstring_optimize!(s::StyledString)
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

styledstring(s::StyledString) = s
styledstring(c::StyledChar) = StyledString(string(c.char), c.properties)

StyledString(s::SubString{<:StyledString}) = styledstring(s)

function join(iterator, delim::StyledString, last=delim)
    xs = zip(iterator, Iterators.repeated(delim)) |> Iterators.flatten |> collect
    xs = xs[1:end-1]
    if length(xs) > 1
        xs[end-1] = last
    end
    styledstring(xs...)
end

function repeat(str::StyledString, r::Integer)
    r == 0 && return one(StyledString)
    r == 1 && return str
    unstyled = repeat(str.string, r)
    properties = Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}()
    len = ncodeunits(str)
    fullregion = firstindex(str):lastindex(str)
    for (region, prop) in str.properties
        if region == fullregion
            push!(properties, (firstindex(unstyled):lastindex(unstyled), prop))
        end
    end
    for offset in 0:len:(r-1)*len
        for (region, prop) in str.properties
            if region != fullregion
                push!(properties, (region .+ offset, prop))
            end
        end
    end
    StyledString(unstyled, properties) |> styledstring_optimize!
end

repeat(str::SubString{<:StyledString}, r::Integer) =
    repeat(StyledString(str), r)

function repeat(c::StyledChar, r::Integer)
    str = repeat(c.char, r)
    fullregion = firstindex(str):lastindex(str)
    StyledString(str, [(fullregion, prop) for prop in c.properties])
end

function reverse(s::StyledString)
    StyledString(reverse(s.string),
                 [(UnitRange(1 + lastindex(s) - last(region),
                             1 + lastindex(s) - first(region)),
                   prop)
                  for (region, prop) in s.properties])
end

# TODO optimise?
reverse(s::SubString{<:StyledString}) = reverse(StyledString(s))

# TODO implement `replace(::StyledString, ...)`

## End AbstractString interface ##

"""
    textproperty!(s::StyledString, [range::UnitRange{Int}], prop::Symbol, val)
    textproperty!(s::SubString{StyledString}, [range::UnitRange{Int}], prop::Symbol, val)

Set `prop` to `val` in `s`, over either `range` if specified or the whole string.
"""
function textproperty!(s::StyledString, range::UnitRange{Int}, prop::Symbol, val)
    indices = searchsorted(s.properties, (range,), by=first)
    propindex = filter(i -> first(s.properties[i][2]) === prop, indices)
    if length(propindex) == 1
        if val === nothing
            deleteat!(s.properties, first(propindex))
        else
            s.properties[first(propindex)] = (range, Pair{Symbol, Any}(prop, val))
        end
    else
        splice!(s.properties, indices, [(range, Pair{Symbol, Any}(prop, val))])
    end
    s
end

textproperty!(ss::StyledString, prop::Symbol, value) =
    textproperty!(ss, firstindex(ss):lastindex(ss), prop, value)

textproperty!(s::SubString{<:StyledString}, range::UnitRange{Int}, prop::Symbol, value) =
    (textproperty!(s.string, s.offset .+ (range), prop, value); s)

textproperty!(s::SubString{<:StyledString}, prop::Symbol, value) =
    (textproperty!(s.string, s.offset .+ (1:s.ncodeunits), prop, value); s)

# TODO optimise
"""
    textproperties(s::StyledString, i::Integer)
    textproperties(s::SubString{StyledString}, i::Integer)

Get the text properties that apply to `s` at index `i`.
"""
function textproperties(s::StyledString, i::Integer)
    props = filter(prop -> !isempty(intersect(i:i, first(prop))),
                   s.properties)
    last.(props)
end

textproperties(s::SubString{<:StyledString}, i::Integer) =
    textproperties(s.string, s.offset + i)

"""
    textproperties(c::StyledChar)

Get the properties that apply to `c`.
"""
textproperties(c::StyledChar) = c.properties

## Iterating over styles ##

struct StyleIterator{S <: AbstractString}
    str::S
    regions::Vector{UnitRange{Int}}
    styles::Vector{Vector{Pair{Symbol, Any}}}
end

length(si::StyleIterator) = length(si.regions)

@propagate_inbounds function iterate(si::StyleIterator, i::Integer=1)
    if i <= length(si.regions)
        @inbounds ((SubString(si.str, si.regions[i]), si.styles[i]), i+1)
    end
end

eltype(::StyleIterator{S}) where { S <: AbstractString} =
    Tuple{SubString{S}, Vector{Pair{Symbol, Any}}}

"""
    eachstyle(s::StyledString{S})
    eachstyle(s::SubString{StyledString{S}})

Identify the contiguous substrings of `s` with a constant style, and return
an iterator which provides each substring and the applicable styles as a
`Tuple{SubString{S}, Vector{Pair{Symbol, Any}}}`.

# Examples

```jldoctest
julia> eachstyle(StyledString("hey there", [(1:3, :face => :bold),
                                            (5:9, :face => :italic)])) |> collect
3-element Vector{Tuple{SubString{String}, Vector{Pair{Symbol, Any}}}}:
 ("hey", [:face => :bold])
 (" ", [])
 ("there", [:face => :italic])
```
"""
function eachstyle(s::StyledString, region::UnitRange{Int}=firstindex(s):lastindex(s))
    isempty(s) || isempty(region) &&
        return StyleIterator(s, Vector{UnitRange{Int}}(), Vector{Vector{Pair{Symbol, Any}}}())
    regions = Vector{UnitRange{Int}}()
    styles = Vector{Vector{Pair{Symbol, Any}}}()
    changepoints = filter(c -> c in region,
                          Iterators.flatten((first(region), nextind(s, last(region)))
                                            for region in first.(s.properties)) |>
                                                unique |> sort)
    isempty(changepoints) &&
        return StyleIterator(s.string, [region], [textproperties(s, first(region))])
    function registerchange!(start, stop)
        push!(regions, start:stop)
        push!(styles, textproperties(s, start))
    end
    if first(region) < first(changepoints)
        registerchange!(first(region), prevind(s, first(changepoints)))
    end
    for (start, stop) in zip(changepoints, changepoints[2:end])
        registerchange!(start, prevind(s, stop))
    end
    if last(changepoints) <= last(region)
        registerchange!(last(changepoints), last(region))
    end
    StyleIterator(s.string, regions, styles)
end

function eachstyle(s::SubString{<:StyledString}, region::UnitRange{Int}=firstindex(s):lastindex(s))
    if isempty(s)
        StyleIterator(s, Vector{UnitRange{Int}}(), Vector{Vector{Pair{Symbol, Any}}}())
    else
        eachstyle(s.string, first(region)+s.offset:last(region)+s.offset)
    end
end
