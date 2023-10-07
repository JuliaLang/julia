# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    TaggedString{S <: AbstractString} <: AbstractString

A string with metadata, in the form of annotated regions.

More specifically, this is a simple wrapper around any other
[`AbstractString`](@ref) that allows for regions of the wrapped string to be
annotated with tagged values.

```text
                        C
                    ┌───┸─────────┐
  "this is an example tagged string"
  └──┰────────┼─────┘      │
     A        └───┰────────┘
                  B
```

The above diagram represents a `TaggedString` where three ranges have been
annotated (labeled `A`, `B`, and `C`). Each annotation must take the form of a
`Pair{Symbol, <:Any}`, where a `Symbol` "tag" is used to label `Any` "value".

Tags do not need to be unique, the same region can be annotated with the same
tag multiple times.

See also [`TaggedChar`](@ref), [`taggedstring`](@ref), [`annotations`](@ref), and
[`annotate!`](@ref).

!!! warning
    While the constructors are part of the Base public API, the fields
    of `TaggedString` are not. This is to allow for potential future
    changes in the implementation of this type. Instead use the
    [`annotations`](@ref), and [`annotate!`](@ref) getter/setter
    functions.

# Constructors

```julia
TaggedString(s::S<:AbstractString) -> TaggedString{S}
TaggedString(s::S<:AbstractString, annotations::Vector{Tuple{UnitRange{Int}, Pair{Symbol, <:Any}}})
```

A TaggedString can also be created with [`taggedstring`](@ref), which acts much
like [`string`](@ref) but preserves any tags present in the arguments.

# Example

```julia-repl
julia> TaggedString("this is an example tagged string",
                    [(1:18, :A => 1), (12:25, :B => 2), (18:32, :C => 3)])
"this is an example tagged string"
```
"""
struct TaggedString{S <: AbstractString} <: AbstractString
    string::S
    annotations::Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}
end

"""
    TaggedChar{S <: AbstractChar} <: AbstractChar

A Char annotated with tagged values.

More specifically, this is a simple wrapper around any other
[`AbstractChar`](@ref), which holds a list of arbitrary tagged values
(`Pair{Symbol, <:Any}`) with the wrapped character.

See also: [`TaggedString`](@ref), [`taggedstring`](@ref), `annotations`,
and `annotate!`.

!!! warning
    While the constructors are part of the Base public API, the fields
    of `TaggedChar` are not. This it to allow for potential future
    changes in the implementation of this type. Instead use the
    [`annotations`](@ref), and [`annotate!`](@ref) getter/setter
    functions.

# Constructors

```julia
TaggedChar(s::S) -> TaggedChar{S}
TaggedChar(s::S, annotations::Vector{Pair{Symbol, <:Any}})
```

# Examples

```julia-repl
julia> TaggedChar('j', :tag => 1)
'j': ASCII/Unicode U+006A (category Ll: Letter, lowercase)
```
"""
struct TaggedChar{C <: AbstractChar} <: AbstractChar
    char::C
    annotations::Vector{Pair{Symbol, Any}}
end

## Constructors ##

# When called with overly-specialised arguments

TaggedString(s::AbstractString, annots::Vector{<:Tuple{UnitRange{Int}, <:Pair{Symbol, <:Any}}}) =
    TaggedString(s, Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}(annots))

TaggedChar(c::AbstractChar, annots::Vector{<:Pair{Symbol, <:Any}}) =
    TaggedChar(c, Vector{Pair{Symbol, Any}}(annots))

# Constructors to avoid recursive wrapping

TaggedString(s::TaggedString, annots::Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}) =
    TaggedString(s.string, vcat(s.annotations, annots))

TaggedChar(c::TaggedChar, annots::Vector{Pair{Symbol, Any}}) =
    TaggedChar(c.char, vcat(s.annotations, annots))

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
        TaggedChar(s.string[i], annotations(s, i))
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
    a.string == b.string && a.annotations == b.annotations

==(a::TaggedString, b::AbstractString) = isempty(a.annotations) && a.string == b
==(a::AbstractString, b::TaggedString) = isempty(b.annotations) && a == b.string

"""
    taggedstring(values...)

Create a `TaggedString` from any number of `values` using their
[`print`](@ref)ed representation.

This acts like [`string`](@ref), but takes care to preserve any annotations
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
    annotations = Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}()
    for x in xs
        if x isa TaggedString
            for (region, annot) in x.annotations
                push!(annotations, (s.io.size .+ (region), annot))
            end
            print(s, x.string)
        elseif x isa SubString{<:TaggedString}
            for (region, annot) in x.string.annotations
                start, stop = first(region), last(region)
                if start <= x.offset + x.ncodeunits && stop > x.offset
                    rstart = s.io.size + max(0, start - x.offset) + 1
                    rstop = s.io.size + min(stop, x.offset + x.ncodeunits) - x.offset
                    push!(annotations, (rstart:rstop, annot))
                end
            end
            print(s, SubString(x.string.string, x.offset, x.ncodeunits, Val(:noshift)))
        elseif x isa TaggedChar
            for annot in x.annotations
                push!(annotations, (1+s.io.size:1+s.io.size, annot))
            end
            print(s, x.char)
        else
            print(s, x)
        end
    end
    str = String(resize!(s.io.data, s.io.size))
    TaggedString(str, annotations)
end

taggedstring(s::TaggedString) = s
taggedstring(c::TaggedChar) =
    TaggedString(string(c.char), [(1:ncodeunits(c), annot) for annot in c.annotations])

TaggedString(s::SubString{<:TaggedString}) = taggedstring(s)

"""
    taggedstring_optimize!(str::TaggedString)

Merge contiguous identical tags in `str`.
"""
function taggedstring_optimize!(s::TaggedString)
    last_seen = Dict{Pair{Symbol, Any}, Int}()
    i = 1
    while i <= length(s.annotations)
        region, keyval = s.annotations[i]
        prev = get(last_seen, keyval, 0)
        if prev > 0
            lregion, _ = s.annotations[prev]
            if last(lregion) + 1 == first(region)
                s.annotations[prev] =
                    setindex(s.annotations[prev],
                             first(lregion):last(region),
                             1)
                deleteat!(s.annotations, i)
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
    annotations = Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}()
    len = ncodeunits(str)
    fullregion = firstindex(str):lastindex(str)
    for (region, annot) in str.annotations
        if region == fullregion
            push!(annotations, (firstindex(untagged):lastindex(untagged), annot))
        end
    end
    for offset in 0:len:(r-1)*len
        for (region, annot) in str.annotations
            if region != fullregion
                push!(annotations, (region .+ offset, annot))
            end
        end
    end
    TaggedString(untagged, annotations) |> taggedstring_optimize!
end

repeat(str::SubString{<:TaggedString}, r::Integer) =
    repeat(TaggedString(str), r)

function repeat(c::TaggedChar, r::Integer)
    str = repeat(c.char, r)
    fullregion = firstindex(str):lastindex(str)
    TaggedString(str, [(fullregion, annot) for annot in c.annotations])
end

function reverse(s::TaggedString)
    lastind = lastindex(s)
    TaggedString(reverse(s.string),
                 [(UnitRange(1 + lastind - last(region),
                             1 + lastind - first(region)),
                   annot)
                  for (region, annot) in s.annotations])
end

# TODO optimise?
reverse(s::SubString{<:TaggedString}) = reverse(TaggedString(s))

# TODO implement `replace(::TaggedString, ...)`

## End AbstractString interface ##

"""
    annotate!(str::TaggedString, [range::UnitRange{Int}], tag::Symbol => value)
    annotate!(str::SubString{TaggedString}, [range::UnitRange{Int}], tag::Symbol => value)

Annotate a `range` of `str` (or the entire string) with a tagged value (`tag` =>
`value`). To remove existing `tag` annotations, use a value of `nothing`.
"""
function annotate!(s::TaggedString, range::UnitRange{Int}, @nospecialize(tagval::Pair{Symbol, <:Any}))
    tag, val = tagval
    indices = searchsorted(s.annotations, (range,), by=first)
    if val === nothing
        tagindex = filter(i -> first(s.annotations[i][2]) === tag, indices)
        for index in Iterators.reverse(tagindex)
            deleteat!(s.annotations, index)
        end
    else
        splice!(s.annotations, indices, [(range, Pair{Symbol, Any}(tag, val))])
    end
    s
end

annotate!(ss::TaggedString, @nospecialize(tagval::Pair{Symbol, <:Any})) =
    annotate!(ss, firstindex(ss):lastindex(ss), tagval)

annotate!(s::SubString{<:TaggedString}, range::UnitRange{Int}, @nospecialize(tagval::Pair{Symbol, <:Any})) =
    (annotate!(s.string, s.offset .+ (range), tagval); s)

annotate!(s::SubString{<:TaggedString}, @nospecialize(tagval::Pair{Symbol, <:Any})) =
    (annotate!(s.string, s.offset .+ (1:s.ncodeunits), tagval); s)

"""
    annotate!(char::TaggedChar, tag::Symbol => value)

Annotate `char` with the pair `tag => value`.
"""
annotate!(c::TaggedChar, @nospecialize(tagval::Pair{Symbol, <:Any})) =
    (push!(c.annotations, tagval); c)

"""
    annotations(str::TaggedString, [position::Union{Integer, UnitRange}])
    annotations(str::SubString{TaggedString}, [position::Union{Integer, UnitRange}])

Get all annotations that apply to `str`. Should `position` be provided, only
annotations that overlap with `position` will be returned.

See also: `annotate!`.
"""
annotations(s::TaggedString) = s.annotations

annotations(s::SubString{<:TaggedString}) =
    annotations(s, s.offset+1:s.offset+s.ncodeunits)

function annotations(s::TaggedString, pos::UnitRange{<:Integer})
    # TODO optimise
    annots = filter(tag -> !isempty(intersect(pos, first(tag))),
                    s.annotations)
    last.(annots)
end

annotations(s::TaggedString, pos::Integer) = annotations(s, pos:pos)

annotations(s::SubString{<:TaggedString}, pos::Integer) =
    annotations(s.string, s.offset + pos)
annotations(s::SubString{<:TaggedString}, pos::UnitRange{<:Integer}) =
    annotations(s.string, first(pos)+s.offset:last(pos)+s.offset)

"""
    annotations(chr::TaggedChar)

Get all annotations of `chr`.
"""
annotations(c::TaggedChar) = c.annotations
