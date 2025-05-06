# This file is a part of Julia. License is MIT: https://julialang.org/license

const Annotation = NamedTuple{(:label, :value), Tuple{Symbol, Any}}
const RegionAnnotation = NamedTuple{(:region, :label, :value), Tuple{UnitRange{Int}, Symbol, Any}}

"""
    AnnotatedString{S <: AbstractString} <: AbstractString

A string with metadata, in the form of annotated regions.

More specifically, this is a simple wrapper around any other
[`AbstractString`](@ref) that allows for regions of the wrapped string to be
annotated with labeled values.

```text
                           C
                    ┌──────┸─────────┐
  "this is an example annotated string"
  └──┰────────┼─────┘         │
     A        └─────┰─────────┘
                    B
```

The above diagram represents a `AnnotatedString` where three ranges have been
annotated (labeled `A`, `B`, and `C`). Each annotation holds a label (`Symbol`)
and a value (`Any`). These three pieces of information are held as a
`$RegionAnnotation`.

Labels do not need to be unique, the same region can hold multiple annotations
with the same label.

Code written for `AnnotatedString`s in general should conserve the following
properties:
- Which characters an annotation is applied to
- The order in which annotations are applied to each character

Additional semantics may be introduced by specific uses of `AnnotatedString`s.

A corollary of these rules is that adjacent, consecutively placed, annotations
with identical labels and values are equivalent to a single annotation spanning
the combined range.

See also [`AnnotatedChar`](@ref), [`annotatedstring`](@ref),
[`annotations`](@ref), and [`annotate!`](@ref).

# Constructors

```julia
AnnotatedString(s::S<:AbstractString) -> AnnotatedString{S}
AnnotatedString(s::S<:AbstractString, annotations::Vector{$RegionAnnotation})
```

A AnnotatedString can also be created with [`annotatedstring`](@ref), which acts much
like [`string`](@ref) but preserves any annotations present in the arguments.

# Examples

```jldoctest; setup=:(using Base: AnnotatedString)
julia> AnnotatedString("this is an example annotated string",
                    [(1:18, :A, 1), (12:28, :B, 2), (18:35, :C, 3)])
"this is an example annotated string"
```
"""
struct AnnotatedString{S <: AbstractString} <: AbstractString
    string::S
    annotations::Vector{RegionAnnotation}
end

"""
    AnnotatedChar{S <: AbstractChar} <: AbstractChar

A Char with annotations.

More specifically, this is a simple wrapper around any other
[`AbstractChar`](@ref), which holds a list of arbitrary labelled annotations
(`$Annotation`) with the wrapped character.

See also: [`AnnotatedString`](@ref), [`annotatedstring`](@ref), `annotations`,
and `annotate!`.

# Constructors

```julia
AnnotatedChar(s::S) -> AnnotatedChar{S}
AnnotatedChar(s::S, annotations::Vector{$Annotation})
```

# Examples

```jldoctest; setup=:(using Base: AnnotatedChar)
julia> AnnotatedChar('j', [(:label, 1)])
'j': ASCII/Unicode U+006A (category Ll: Letter, lowercase)
```
"""
struct AnnotatedChar{C <: AbstractChar} <: AbstractChar
    char::C
    annotations::Vector{Annotation}
end

## Constructors ##

# When called with overly-specialised arguments

AnnotatedString(s::AbstractString, annots::Vector) =
    AnnotatedString(s, Vector{RegionAnnotation}(annots))

AnnotatedString(s::AbstractString, annots) =
    AnnotatedString(s, collect(RegionAnnotation, annots))

AnnotatedChar(c::AbstractChar, annots::Vector) =
    AnnotatedChar(c, Vector{Annotation}(annots))

AnnotatedChar(c::AbstractChar, annots) =
    AnnotatedChar(c, collect(Annotation, annots))

# Constructors to avoid recursive wrapping

AnnotatedString(s::AnnotatedString, annots::Vector{RegionAnnotation}) =
    AnnotatedString(s.string, vcat(s.annotations, annots))

AnnotatedChar(c::AnnotatedChar, annots::Vector{Annotation}) =
    AnnotatedChar(c.char, vcat(c.annotations, Vector{Annotation}(annots)))

# To avoid pointless overhead
String(s::AnnotatedString{String}) = s.string

## Conversion/promotion ##

convert(::Type{AnnotatedString}, s::AnnotatedString) = s
convert(::Type{AnnotatedString{S}}, s::S) where {S <: AbstractString} =
    AnnotatedString(s, Vector{RegionAnnotation}())
convert(::Type{AnnotatedString}, s::S) where {S <: AbstractString} =
    convert(AnnotatedString{S}, s)
AnnotatedString(s::S) where {S <: AbstractString} = convert(AnnotatedString{S}, s)

convert(::Type{AnnotatedChar}, c::AnnotatedChar) = c
convert(::Type{AnnotatedChar{C}}, c::C) where { C <: AbstractChar } =
    AnnotatedChar{C}(c, Vector{Annotation}())
convert(::Type{AnnotatedChar}, c::C) where { C <: AbstractChar } =
    convert(AnnotatedChar{C}, c)

AnnotatedChar(c::AbstractChar) = convert(AnnotatedChar, c)
AnnotatedChar(c::UInt32) = convert(AnnotatedChar, Char(c))
AnnotatedChar{C}(c::UInt32) where {C <: AbstractChar} = convert(AnnotatedChar, C(c))

promote_rule(::Type{<:AnnotatedString}, ::Type{<:AbstractString}) = AnnotatedString

## AbstractString interface ##

ncodeunits(s::AnnotatedString) = ncodeunits(s.string)::Int
codeunits(s::AnnotatedString) = codeunits(s.string)
codeunit(s::AnnotatedString) = codeunit(s.string)
codeunit(s::AnnotatedString, i::Integer) = codeunit(s.string, i)
isvalid(s::AnnotatedString, i::Integer) = isvalid(s.string, i)::Bool
@propagate_inbounds iterate(s::AnnotatedString, i::Integer=firstindex(s)) =
    if i <= lastindex(s.string); (s[i], nextind(s, i)) end
eltype(::Type{<:AnnotatedString{S}}) where {S} = AnnotatedChar{eltype(S)}
firstindex(s::AnnotatedString) = firstindex(s.string)
lastindex(s::AnnotatedString) = lastindex(s.string)

function getindex(s::AnnotatedString, i::Integer)
    @boundscheck checkbounds(s, i)
    @inbounds if isvalid(s, i)
        AnnotatedChar(s.string[i], Annotation[(; label, value) for (; label, value) in annotations(s, i)])
    else
        string_index_err(s, i)
    end
end

# To make `AnnotatedString`s repr-evaluable, we need to override
# the generic `AbstractString` 2-arg show method.

function show(io::IO, s::A) where {A <: AnnotatedString}
    show(io, A)
    print(io, '(')
    show(io, s.string)
    print(io, ", ")
    tupanns = Vector{Tuple{UnitRange{Int}, Symbol, Any}}(map(values, s.annotations))
    show(IOContext(io, :typeinfo => typeof(tupanns)), tupanns)
    print(io, ')')
end

# But still use the generic `AbstractString` fallback for the 3-arg show.
show(io::IO, ::MIME"text/plain", s::AnnotatedString) =
    invoke(show, Tuple{IO, AbstractString}, io, s)

## AbstractChar interface ##

ncodeunits(c::AnnotatedChar) = ncodeunits(c.char)
codepoint(c::AnnotatedChar) = codepoint(c.char)

# Avoid the iteration fallback with comparison
cmp(a::AnnotatedString, b::AbstractString) = cmp(a.string, b)
cmp(a::AbstractString, b::AnnotatedString) = cmp(a, b.string)
# To avoid method ambiguity
cmp(a::AnnotatedString, b::AnnotatedString) = cmp(a.string, b.string)

==(a::AnnotatedString, b::AnnotatedString) =
    a.string == b.string && a.annotations == b.annotations

==(a::AnnotatedString, b::AbstractString) = isempty(a.annotations) && a.string == b
==(a::AbstractString, b::AnnotatedString) = isempty(b.annotations) && a == b.string

# To prevent substring equality from hitting the generic fallback

function ==(a::SubString{<:AnnotatedString}, b::SubString{<:AnnotatedString})
    SubString(a.string.string, a.offset, a.ncodeunits, Val(:noshift)) ==
        SubString(b.string.string, b.offset, b.ncodeunits, Val(:noshift)) &&
        annotations(a) == annotations(b)
end

==(a::SubString{<:AnnotatedString}, b::AnnotatedString) =
    annotations(a) == annotations(b) && SubString(a.string.string, a.offset, a.ncodeunits, Val(:noshift)) == b.string

==(a::SubString{<:AnnotatedString}, b::AbstractString) =
    isempty(annotations(a)) && SubString(a.string.string, a.offset, a.ncodeunits, Val(:noshift)) == b

==(a::AbstractString, b::SubString{<:AnnotatedString}) = b == a

==(a::AnnotatedString, b::SubString{<:AnnotatedString}) = b == a

"""
    annotatedstring(values...)

Create a `AnnotatedString` from any number of `values` using their
[`print`](@ref)ed representation.

This acts like [`string`](@ref), but takes care to preserve any annotations
present (in the form of [`AnnotatedString`](@ref) or [`AnnotatedChar`](@ref) values).

See also [`AnnotatedString`](@ref) and [`AnnotatedChar`](@ref).

## Examples

```jldoctest; setup=:(using Base: AnnotatedString, annotatedstring)
julia> annotatedstring("now a AnnotatedString")
"now a AnnotatedString"

julia> annotatedstring(AnnotatedString("annotated", [(1:9, :label, 1)]), ", and unannotated")
"annotated, and unannotated"
```
"""
function annotatedstring(xs...)
    isempty(xs) && return AnnotatedString("")
    size = mapreduce(_str_sizehint, +, xs)
    buf = IOBuffer(sizehint=size)
    s = IOContext(buf, :color => true)
    annotations = Vector{RegionAnnotation}()
    for x in xs
        size = filesize(s.io)
        if x isa AnnotatedString
            for annot in x.annotations
                push!(annotations, setindex(annot, annot.region .+ size, :region))
            end
            print(s, x.string)
        elseif x isa SubString{<:AnnotatedString}
            for annot in x.string.annotations
                start, stop = first(annot.region), last(annot.region)
                if start <= x.offset + x.ncodeunits && stop > x.offset
                    rstart = size + max(0, start - x.offset - 1) + 1
                    rstop = size + min(stop, x.offset + x.ncodeunits) - x.offset
                    push!(annotations, setindex(annot, rstart:rstop, :region))
                end
            end
            print(s, SubString(x.string.string, x.offset, x.ncodeunits, Val(:noshift)))
        elseif x isa AnnotatedChar
            for annot in x.annotations
                push!(annotations, (region=1+size:1+size, annot...))
            end
            print(s, x.char)
        else
            print(s, x)
        end
    end
    str = String(take!(buf))
    AnnotatedString(str, annotations)
end

annotatedstring(s::AnnotatedString) = s
annotatedstring(c::AnnotatedChar) =
    AnnotatedString(string(c.char), [(region=1:ncodeunits(c), annot...) for annot in c.annotations])

AnnotatedString(s::SubString{<:AnnotatedString}) = annotatedstring(s)

function repeat(str::AnnotatedString, r::Integer)
    r == 0 && return one(AnnotatedString)
    r == 1 && return str
    unannot = repeat(str.string, r)
    annotations = Vector{RegionAnnotation}()
    len = ncodeunits(str)
    fullregion = firstindex(str):lastindex(str)
    if isempty(str.annotations)
    elseif allequal(a -> a.region, str.annotations) && first(str.annotations).region == fullregion
        newfullregion = firstindex(unannot):lastindex(unannot)
        for annot in str.annotations
            push!(annotations, setindex(annot, newfullregion, :region))
        end
    else
        for offset in 0:len:(r-1)*len
            for annot in str.annotations
                push!(annotations, setindex(annot, annot.region .+ offset, :region))
            end
        end
    end
    AnnotatedString(unannot, annotations)
end

repeat(str::SubString{<:AnnotatedString}, r::Integer) =
    repeat(AnnotatedString(str), r)

function repeat(c::AnnotatedChar, r::Integer)
    str = repeat(c.char, r)
    fullregion = firstindex(str):lastindex(str)
    AnnotatedString(str, [(region=fullregion, annot...) for annot in c.annotations])
end

function reverse(s::AnnotatedString)
    lastind = lastindex(s)
    AnnotatedString(
        reverse(s.string),
        [setindex(annot,
                  UnitRange(1 + lastind - last(annot.region),
                            1 + lastind - first(annot.region)),
                  :region)
         for annot in s.annotations])
end

# TODO optimise?
reverse(s::SubString{<:AnnotatedString}) = reverse(AnnotatedString(s))

# TODO implement `replace(::AnnotatedString, ...)`

## End AbstractString interface ##

function _annotate!(annlist::Vector{RegionAnnotation}, region::UnitRange{Int}, label::Symbol, @nospecialize(value::Any))
    if value === nothing
        deleteat!(annlist, findall(ann -> ann.region == region && ann.label === label, annlist))
    else
        push!(annlist, RegionAnnotation((; region, label, value)))
    end
end

"""
    annotate!(str::AnnotatedString, [range::UnitRange{Int}], label::Symbol, value)
    annotate!(str::SubString{AnnotatedString}, [range::UnitRange{Int}], label::Symbol, value)

Annotate a `range` of `str` (or the entire string) with a labeled value `(label, value)`.
To remove existing `label` annotations, use a value of `nothing`.

The order in which annotations are applied to `str` is semantically meaningful,
as described in [`AnnotatedString`](@ref).
"""
annotate!(s::AnnotatedString, range::UnitRange{Int}, label::Symbol, @nospecialize(val::Any)) =
    (_annotate!(s.annotations, range, label, val); s)

annotate!(ss::AnnotatedString, label::Symbol, @nospecialize(val::Any)) =
    annotate!(ss, firstindex(ss):lastindex(ss), label, val)

annotate!(s::SubString{<:AnnotatedString}, range::UnitRange{Int}, label::Symbol, @nospecialize(val::Any)) =
    (annotate!(s.string, s.offset .+ (range), label, val); s)

annotate!(s::SubString{<:AnnotatedString}, label::Symbol, @nospecialize(val::Any)) =
    (annotate!(s.string, s.offset .+ (1:s.ncodeunits), label, val); s)

"""
    annotate!(char::AnnotatedChar, label::Symbol, value::Any)

Annotate `char` with the labeled value `(label, value)`.
"""
annotate!(c::AnnotatedChar, label::Symbol, @nospecialize(val::Any)) =
    (push!(c.annotations, Annotation((; label, val))); c)

"""
    annotations(str::Union{AnnotatedString, SubString{AnnotatedString}},
                [position::Union{Integer, UnitRange}]) ->
        Vector{$RegionAnnotation}

Get all annotations that apply to `str`. Should `position` be provided, only
annotations that overlap with `position` will be returned.

Annotations are provided together with the regions they apply to, in the form of
a vector of region–annotation tuples.

In accordance with the semantics documented in [`AnnotatedString`](@ref), the
order of annotations returned matches the order in which they were applied.

See also: [`annotate!`](@ref).
"""
annotations(s::AnnotatedString) = s.annotations

function annotations(s::SubString{<:AnnotatedString})
    RegionAnnotation[
        setindex(ann, first(ann.region)-s.offset:last(ann.region)-s.offset, :region)
        for ann in annotations(s.string, s.offset+1:s.offset+s.ncodeunits)]
end

function annotations(s::AnnotatedString, pos::UnitRange{<:Integer})
    # TODO optimise
    RegionAnnotation[
        setindex(ann, max(first(pos), first(ann.region)):min(last(pos), last(ann.region)), :region)
        for ann in s.annotations if !isempty(intersect(pos, ann.region))]
end

annotations(s::AnnotatedString, pos::Integer) = annotations(s, pos:pos)

annotations(s::SubString{<:AnnotatedString}, pos::Integer) =
    annotations(s.string, s.offset + pos)

annotations(s::SubString{<:AnnotatedString}, pos::UnitRange{<:Integer}) =
    annotations(s.string, first(pos)+s.offset:last(pos)+s.offset)

"""
    annotations(chr::AnnotatedChar)::Vector{$Annotation}

Get all annotations of `chr`, in the form of a vector of annotation pairs.
"""
annotations(c::AnnotatedChar) = c.annotations

## Character transformation helper function, c.f. `unicode.jl`.

"""
    annotated_chartransform(f::Function, str::AnnotatedString, state=nothing)

Transform every character in `str` with `f`, adjusting annotation regions as
appropriate. `f` must take one of two forms, either:
- `f(c::Char) -> Char`, or
- `f(c::Char, state) -> (Char, state)`.

This works by comparing the number of code units of each character before and
after transforming with `f`, recording and aggregating any differences, then
applying them to the annotation regions.

Returns an `AnnotatedString{String}` (regardless of the original underling
string type of `str`).
"""
function annotated_chartransform(f::Function, str::AnnotatedString, state=nothing)
    outstr = IOBuffer()
    annots = RegionAnnotation[]
    bytepos = firstindex(str) - 1
    offsets = [bytepos => 0]
    for c in str.string
        oldnb = ncodeunits(c)
        bytepos += oldnb
        if isnothing(state)
            c = f(c)
        else
            c, state = f(c, state)
        end
        nb = write(outstr, c)
        if nb != oldnb
            push!(offsets, bytepos => last(last(offsets)) + nb - oldnb)
        end
    end
    for annot in str.annotations
        start, stop = first(annot.region), last(annot.region)
        start_offset = last(offsets[findlast(<=(start) ∘ first, offsets)::Int])
        stop_offset  = last(offsets[findlast(<=(stop) ∘ first, offsets)::Int])
        push!(annots, setindex(annot, (start + start_offset):(stop + stop_offset), :region))
    end
    AnnotatedString(String(take!(outstr)), annots)
end

struct RegionIterator{S <: AbstractString}
    str::S
    regions::Vector{UnitRange{Int}}
    annotations::Vector{Vector{Annotation}}
end

Base.length(si::RegionIterator) = length(si.regions)

Base.@propagate_inbounds function Base.iterate(si::RegionIterator, i::Integer=1)
    if i <= length(si.regions)
        @inbounds ((SubString(si.str, si.regions[i]), si.annotations[i]), i+1)
    end
end

Base.eltype(::RegionIterator{S}) where { S <: AbstractString} =
    Tuple{SubString{S}, Vector{Annotation}}

"""
    eachregion(s::AnnotatedString{S})
    eachregion(s::SubString{AnnotatedString{S}})

Identify the contiguous substrings of `s` with a constant annotations, and return
an iterator which provides each substring and the applicable annotations as a
`Tuple{SubString{S}, Vector{$Annotation}}`.

# Examples

```jldoctest; setup=:(using Base: AnnotatedString, eachregion)
julia> collect(eachregion(AnnotatedString(
           "hey there", [(1:3, :face, :bold),
                         (5:9, :face, :italic)])))
3-element Vector{Tuple{SubString{String}, Vector{$Annotation}}}:
 ("hey", [$Annotation((:face, :bold))])
 (" ", [])
 ("there", [$Annotation((:face, :italic))])
```
"""
function eachregion(s::AnnotatedString, subregion::UnitRange{Int}=firstindex(s):lastindex(s))
    isempty(s) || isempty(subregion) &&
        return RegionIterator(s.string, UnitRange{Int}[], Vector{Annotation}[])
    events = annotation_events(s, subregion)
    isempty(events) && return RegionIterator(s.string, [subregion], [Annotation[]])
    annotvals = Annotation[
        (; label, value) for (; label, value) in annotations(s)]
    regions = Vector{UnitRange{Int}}()
    annots = Vector{Vector{Annotation}}()
    pos = first(events).pos
    if pos > first(subregion)
        push!(regions, thisind(s, first(subregion)):prevind(s, pos))
        push!(annots, [])
    end
    activelist = Int[]
    for event in events
        if event.pos != pos
            push!(regions, pos:prevind(s, event.pos))
            push!(annots, annotvals[activelist])
            pos = event.pos
        end
        if event.active
            insert!(activelist, searchsortedfirst(activelist, event.index), event.index)
        else
            deleteat!(activelist, searchsortedfirst(activelist, event.index))
        end
    end
    if last(events).pos < nextind(s, last(subregion))
        push!(regions, last(events).pos:thisind(s, last(subregion)))
        push!(annots, [])
    end
    RegionIterator(s.string, regions, annots)
end

function eachregion(s::SubString{<:AnnotatedString}, pos::UnitRange{Int}=firstindex(s):lastindex(s))
    if isempty(s)
        RegionIterator(s.string, Vector{UnitRange{Int}}(), Vector{Vector{Annotation}}())
    else
        eachregion(s.string, first(pos)+s.offset:last(pos)+s.offset)
    end
end

"""
    annotation_events(string::AbstractString, annots::Vector{$RegionAnnotation}, subregion::UnitRange{Int})
    annotation_events(string::AnnotatedString, subregion::UnitRange{Int})

Find all annotation "change events" that occur within a `subregion` of `annots`,
with respect to `string`. When `string` is styled, `annots` is inferred.

Each change event is given in the form of a `@NamedTuple{pos::Int, active::Bool,
index::Int}` where `pos` is the position of the event, `active` is a boolean
indicating whether the annotation is being activated or deactivated, and `index`
is the index of the annotation in question.
"""
function annotation_events(s::AbstractString, annots::Vector{RegionAnnotation}, subregion::UnitRange{Int})
    events = Vector{NamedTuple{(:pos, :active, :index), Tuple{Int, Bool, Int}}}() # Position, Active?, Annotation index
    for (i, (; region)) in enumerate(annots)
        if !isempty(intersect(subregion, region))
            start, stop = max(first(subregion), first(region)), min(last(subregion), last(region))
            start <= stop || continue # Currently can't handle empty regions
            push!(events, (pos=thisind(s, start), active=true, index=i))
            push!(events, (pos=nextind(s, stop), active=false, index=i))
        end
    end
    sort(events, by=e -> e.pos)
end

annotation_events(s::AnnotatedString, subregion::UnitRange{Int}) =
    annotation_events(s.string, annotations(s), subregion)
