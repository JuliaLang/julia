# This file is a part of Julia. License is MIT: https://julialang.org/license

const RegionAnnotation{V} = NamedTuple{(:region, :value), Tuple{UnitRange{Int}, V}}

"""
    AnnotatedString{S <: AbstractString, V} <: AbstractString

A string with metadata, in the form of annotated regions.

More specifically, this is a simple wrapper around any other
[`AbstractString`](@ref) that allows for regions of the wrapped string to be
annotated with values.

```text
                           C
                    ┌──────┸─────────┐
  "this is an example annotated string"
  └──┰────────┼─────┘         │
     A        └─────┰─────────┘
                    B
```

The above diagram represents a `AnnotatedString` where three ranges have been
annotated (`A`, `B`, and `C`). Each annotation holds a region and a value (`V`),
as a `$(RegionAnnotation{TypeVar(:V)})`. The type of a value says what kind of
annotation it is.

The same region can hold multiple annotations of the same kind.

Code written for `AnnotatedString`s in general should conserve the following
properties:
- Which characters an annotation is applied to
- The order in which annotations are applied to each character

Additional semantics may be introduced by specific uses of `AnnotatedString`s.

A corollary of these rules is that adjacent, consecutively placed, annotations
with identical values are equivalent to a single annotation spanning the
combined range.

See also [`AnnotatedChar`](@ref), [`annotatedstring`](@ref),
[`annotations`](@ref), and [`annotate!`](@ref).

# Constructors

```julia
AnnotatedString(s::S<:AbstractString) -> AnnotatedString{S, Any}
AnnotatedString(s::S<:AbstractString, annotations::Vector{$(RegionAnnotation{TypeVar(:V)})}) -> AnnotatedString{S, V}
AnnotatedString{S, V}(s::S, annotations) -> AnnotatedString{S, V}
```

Each annotation may also be given as a `(region, value)` tuple or a `region => value`
pair. Constructing from an existing `AnnotatedString` copies its annotations.

An AnnotatedString can also be created with [`annotatedstring`](@ref), which acts much
like [`string`](@ref) but preserves any annotations present in the arguments.

# Examples

```jldoctest; setup=:(using Base: AnnotatedString)
julia> AnnotatedString("this is an example annotated string",
                    [(1:18, 1), (12:28, 2), (18:35, 3)])
"this is an example annotated string"
```
"""
struct AnnotatedString{S <: AbstractString, V} <: AbstractString
    string::S
    annotations::Vector{RegionAnnotation{V}}
end

"""
    AnnotatedChar{C <: AbstractChar, V} <: AbstractChar

A Char with annotations.

More specifically, this is a simple wrapper around any other
[`AbstractChar`](@ref), which holds a list of annotation values (`V`) with the
wrapped character.

See also [`AnnotatedString`](@ref), [`annotatedstring`](@ref), `annotations`,
`annotate!`.

# Constructors

```julia
AnnotatedChar(c::C<:AbstractChar) -> AnnotatedChar{C, Any}
AnnotatedChar(c::C<:AbstractChar, annotations::Vector{V}) -> AnnotatedChar{C, V}
AnnotatedChar{C, V}(c::C, annotations) -> AnnotatedChar{C, V}
```

Constructing from an existing `AnnotatedChar` copies its annotations.

# Examples

```jldoctest; setup=:(using Base: AnnotatedChar)
julia> AnnotatedChar('j', [1])
'j': ASCII/Unicode U+006A (category Ll: Letter, lowercase)
```
"""
struct AnnotatedChar{C <: AbstractChar, V} <: AbstractChar
    char::C
    annotations::Vector{V}
end

## Constructors ##

function AnnotatedString{S}(s::S, annots) where {S <: AbstractString}
    valtype(::Vector{RegionAnnotation{V}}) where {V} = V
    valtype(::Vector) = Any
    function annotation(annot)
        if annot isa Union{Tuple{Any, Symbol, Any}, NamedTuple{(:region, :label, :value)}} # Deprecated
            region, label, value = annot
            return (region = UnitRange{Int}(region), value = labelled_value(Symbol(label), value))
        end
        length(annot) == 2 || throw(ArgumentError(LazyString("an annotation is a region and a value, as `(region, value)`, not ", annot)))
        region, value = annot
        (region = UnitRange{Int}(region), value)
    end
    vannots = [annotation(annot) for annot in annots]
    AnnotatedString{S, valtype(vannots)}(s, vannots)
end

AnnotatedString(s::AbstractString, annots) = AnnotatedString{typeof(s)}(s, annots)

AnnotatedString{S, V}(s::S) where {S, V} = AnnotatedString{S, V}(s, RegionAnnotation{V}[])

function AnnotatedChar{C}(c::C, annots) where {C <: AbstractChar}
    vannots = collect(annots)
    AnnotatedChar{C, eltype(vannots)}(c, vannots)
end

AnnotatedChar(c::AbstractChar, annots) = AnnotatedChar{typeof(c)}(c, annots)

# Copy, as the annotations are mutable

AnnotatedString{S, V}(s::AnnotatedString{S}) where {S, V} =
    AnnotatedString{S, V}(s.string, Vector{RegionAnnotation{V}}(s.annotations))
AnnotatedString(s::AnnotatedString{S, V}) where {S, V} = AnnotatedString{S, V}(s)

AnnotatedChar{C, V}(c::AnnotatedChar{C}) where {C, V} =
    AnnotatedChar{C, V}(c.char, Vector{V}(c.annotations))
AnnotatedChar(c::AnnotatedChar{C, V}) where {C, V} = AnnotatedChar{C, V}(c)

# Constructors to avoid recursive wrapping, which widen the value type as `vcat` does. The
# `Vector{RegionAnnotation}` method also settles an ambiguity with the default constructor.

AnnotatedString(s::AnnotatedString{S, V}, annots::Vector{RegionAnnotation{T}}) where {S <: AbstractString, V, T} =
    AnnotatedString(s.string, typed_vcat(RegionAnnotation{annot_promote_valtype(s, annots)}, s.annotations, annots))

AnnotatedString(s::AnnotatedString, annots) = AnnotatedString(s, AnnotatedString(s.string, annots).annotations)

function AnnotatedChar(c::AnnotatedChar{C}, annots::Vector) where {C <: AbstractChar}
    V = annot_promote_valtype(c, AnnotatedChar(c.char, annots))
    AnnotatedChar{C, V}(c.char, typed_vcat(V, c.annotations, annots))
end

AnnotatedChar(c::AnnotatedChar, annots) = AnnotatedChar(c, AnnotatedChar(c.char, annots).annotations)

# To avoid pointless overhead
String(s::AnnotatedString{String}) = s.string

## Conversion/promotion ##

convert(::Type{AnnotatedString}, s::AnnotatedString) = s
convert(::Type{AnnotatedString}, s::S) where {S <: AbstractString} =
    convert(AnnotatedString{S}, s)
convert(::Type{AnnotatedString{S}}, s::S) where {S <: AbstractString} =
    convert(AnnotatedString{S, Any}, s)
convert(::Type{AnnotatedString{S, V}}, s::S) where {S <: AbstractString, V} =
    AnnotatedString{S, V}(s)

AnnotatedString(s::S) where {S <: AbstractString} = convert(AnnotatedString{S}, s)

convert(::Type{AnnotatedChar}, c::AnnotatedChar) = c
convert(::Type{AnnotatedChar{C, V}}, c::C) where { C <: AbstractChar, V } =
    AnnotatedChar{C, V}(c, Vector{V}())
convert(::Type{AnnotatedChar}, c::C) where { C <: AbstractChar } =
    convert(AnnotatedChar{C, Any}, c)

AnnotatedChar(c::AbstractChar) = convert(AnnotatedChar, c)
AnnotatedChar(c::UInt32) = convert(AnnotatedChar, Char(c))
AnnotatedChar{C}(c::UInt32) where {C <: AbstractChar} = convert(AnnotatedChar, C(c))
AnnotatedChar{C, V}(c::UInt32) where {C <: AbstractChar, V} = convert(AnnotatedChar{C, V}, C(c))

promote_rule(::Type{<:AnnotatedString}, ::Type{<:AbstractString}) = AnnotatedString

## AbstractString interface ##

ncodeunits(s::AnnotatedString) = ncodeunits(s.string)::Int
codeunits(s::AnnotatedString) = codeunits(s.string)
codeunit(s::AnnotatedString) = codeunit(s.string)
codeunit(s::AnnotatedString, i::Integer) = codeunit(s.string, i)
isvalid(s::AnnotatedString, i::Integer) = isvalid(s.string, i)::Bool
@propagate_inbounds iterate(s::AnnotatedString, i::Integer=firstindex(s)) =
    if i <= lastindex(s.string); (s[i], nextind(s, i)) end
eltype(::Type{<:AnnotatedString{S, V}}) where {S, V} = AnnotatedChar{eltype(S), V}
eltype(::Type{<:AnnotatedString{S}}) where {S} = AnnotatedChar{eltype(S)}
firstindex(s::AnnotatedString) = firstindex(s.string)
lastindex(s::AnnotatedString) = lastindex(s.string)

"""
    unannotate(s::AnnotatedString{S})::S
    unannotate(s::SubString{AnnotatedString{S}})::SubString{S}
    unannotate(s::SubString{AnnotatedString{SubString{S}}})::SubString{S}

Get the underlying string of `s`, without copying.

# Examples
```jldoctest; setup=:(using Base: AnnotatedString)
julia> s = AnnotatedString("abcde", [(1:3, 4)])
"abcde"

julia> u = Base.unannotate(s)
"abcde"

julia> typeof(u)
String
```

!!! compat "Julia 1.14"
    This function requires at least Julia 1.14.

"""
unannotate(s::AnnotatedString) = s.string

function unannotate(s::SubString{<:AnnotatedString})
    start_index = first(parentindices(s)[1])
    @inbounds raw_substring(parent(s).string, start_index, ncodeunits(s))
end


function getindex(s::AnnotatedString{S, V}, i::Integer) where {S, V}
    @boundscheck checkbounds(s, i)
    @inbounds if isvalid(s, i)
        c = s.string[i]
        AnnotatedChar{typeof(c), V}(c, V[a.value for a in s.annotations if i in a.region])
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
    tupanns = Vector{Tuple{UnitRange{Int}, Any}}(map(values, s.annotations))
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
    unannotate(a) == unannotate(b) && annotations(a) == annotations(b)
end

==(a::SubString{<:AnnotatedString}, b::AnnotatedString) =
    annotations(a) == annotations(b) && unannotate(a) == b.string

==(a::SubString{<:AnnotatedString}, b::AbstractString) =
    isempty(annotations(a)) && unannotate(a) == b

==(a::AbstractString, b::SubString{<:AnnotatedString}) = b == a

==(a::AnnotatedString, b::SubString{<:AnnotatedString}) = b == a

"""
    annotatedstring(values...)

Create a `AnnotatedString` from any number of `values` using their
[`print`](@ref)ed representation.

This acts like [`string`](@ref), but takes care to preserve any annotations
present (in the form of [`AnnotatedString`](@ref) or [`AnnotatedChar`](@ref) values).

See also [`AnnotatedString`](@ref), [`AnnotatedChar`](@ref).

## Examples

```jldoctest; setup=:(using Base: AnnotatedString, annotatedstring)
julia> annotatedstring("now an AnnotatedString")
"now an AnnotatedString"

julia> annotatedstring(AnnotatedString("annotated", [(1:9, 1)]), ", and unannotated")
"annotated, and unannotated"
```
"""
function annotatedstring(xs...)
    isempty(xs) && return AnnotatedString("")
    size = 0 # Summed in a loop, as `mapreduce` would compile a fold for each combination of argument types
    for x in xs
        size += _str_sizehint(x)
    end
    buf = IOBuffer(sizehint=size)
    s = IOContext(buf, :color => true)
    V = annot_promote_valtype(xs...)
    annotations = Vector{RegionAnnotation{V}}()
    for x in xs
        size = filesize(s.io)
        if x isa AnnotatedString
            for annot in x.annotations
                push!(annotations, @inline(setindex(annot, annot.region .+ size, :region)))
            end
            print(s, x.string)
        elseif x isa SubString{<:AnnotatedString}
            for annot in x.string.annotations
                start, stop = first(annot.region), last(annot.region)
                if start <= x.offset + x.ncodeunits && stop > x.offset
                    rstart = size + max(0, start - x.offset - 1) + 1
                    rstop = size + min(stop, x.offset + x.ncodeunits) - x.offset
                    push!(annotations, @inline(setindex(annot, rstart:rstop, :region)))
                end
            end
            print(s, unannotate(x))
        elseif x isa AnnotatedChar
            for value in x.annotations
                push!(annotations, (region=1+size:1+size, value))
            end
            print(s, x.char)
        else
            print(s, x)
        end
    end
    str = takestring!(buf)
    AnnotatedString(str, annotations)
end

annot_valtype(::Type{AnnotatedString{S, V}}) where {S, V} = V
annot_valtype(::Type{AnnotatedChar{C, V}}) where {C, V} = V
annot_valtype(::Type{SubString{S}}) where {S} = annot_valtype(S)
annot_valtype(::Type{Vector{RegionAnnotation{V}}}) where {V} = V
annot_valtype(::Type{Pair{P, V}}) where {P, V} = annot_valtype(V)
# An abstract type that may hold annotated values, such as the eltype of a mixed vector, gives
# `Any`. This uses subtyping because `typeintersect` does not fold at compile time.
annot_valtype(::Type{T}) where {T} =
    if T === Union{} Union{}
    elseif T isa Union Union{annot_valtype(T.a), annot_valtype(T.b)}
    elseif T <: Union{AnnotatedString, AnnotatedChar, SubString{<:AnnotatedString}} ||
           AnnotatedString <: T || AnnotatedChar <: T || SubString{<:AnnotatedString} <: T Any
    else Union{} end
annot_valtype(x) = annot_valtype(typeof(x))

# The union of the arguments' value types, widened to `Any` past 4 members, the most that
# inference splits. Without an annotated argument, `Any`, so the result can take annotations.
function annot_promote_valtype(x)
    V = annot_valtype(x)
    if V === Union{} Any else V end
end
function annot_promote_valtype(a, b, xs...)
    @inline
    union = Union{annot_valtype(a), annot_valtype(b), map(annot_valtype, xs)...}
    if union === Union{} || unionlen(union) > 4 Any else union end
end

annotatedstring(s::AnnotatedString) = s
annotatedstring(c::AnnotatedChar{C, V}) where {C, V} =
    AnnotatedString(string(c.char), RegionAnnotation{V}[(region=1:ncodeunits(c), value) for value in c.annotations])

AnnotatedString(s::SubString{<:AnnotatedString}) = annotatedstring(s)
AnnotatedString{S, V}(s::SubString{<:AnnotatedString{S}}) where {S, V} =
    AnnotatedString{S, V}(annotatedstring(s))

function repeat(str::AnnotatedString{S, V}, r::Integer) where {S, V}
    unannot = repeat(str.string, r)
    annotations = Vector{RegionAnnotation{V}}()
    len = ncodeunits(str)
    fullregion = firstindex(str):lastindex(str)
    if r == 0 || isempty(str.annotations)
    elseif allequal(a -> a.region, str.annotations) && first(str.annotations).region == fullregion
        newfullregion = firstindex(unannot):lastindex(unannot)
        for annot in str.annotations
            push!(annotations, @inline(setindex(annot, newfullregion, :region)))
        end
    else
        for offset in 0:len:(r-1)*len
            for annot in str.annotations
                push!(annotations, @inline(setindex(annot, annot.region .+ offset, :region)))
            end
        end
    end
    AnnotatedString(unannot, annotations)
end

repeat(str::SubString{<:AnnotatedString}, r::Integer) =
    repeat(AnnotatedString(str), r)

function repeat(c::AnnotatedChar{C, V}, r::Integer) where {C, V}
    str = repeat(c.char, r)
    fullregion = firstindex(str):lastindex(str)
    AnnotatedString(str, RegionAnnotation{V}[(region=fullregion, value) for value in c.annotations])
end

function reverse(s::AnnotatedString)
    lastind = lastindex(s)
    AnnotatedString(
        reverse(s.string),
        [@inline(setindex(annot,
                  UnitRange(1 + lastind - last(annot.region),
                            1 + lastind - first(annot.region)),
                  :region))
         for annot in s.annotations])
end

# TODO optimise?
reverse(s::SubString{<:AnnotatedString}) = reverse(AnnotatedString(s))

# TODO implement `replace(::AnnotatedString, ...)`

## End AbstractString interface ##

_annotate!(annlist::Vector{RegionAnnotation{V}}, region::UnitRange{Int}, value::V) where {V} =
    push!(annlist, RegionAnnotation{V}((region, value)))
_annotate!(annlist::Vector{RegionAnnotation{V}}, region::UnitRange{Int}, @nospecialize(value)) where {V} =
    throw(ArgumentError(LazyString("cannot annotate with a ", typeof(value), " value, as the value type is ", V,
                                   "; construct the string or buffer with a wider value type, such as Any")))

"""
    annotate!(str::AnnotatedString, [range::UnitRange{Int}], value)
    annotate!(str::SubString{AnnotatedString}, [range::UnitRange{Int}], value)

Annotate a `range` of `str` (or the entire string) with `value`. A `value` outside the
value type `V` of `str` raises an `ArgumentError`.

The order in which annotations are applied to `str` is semantically meaningful,
as described in [`AnnotatedString`](@ref).
"""
annotate!(s::AnnotatedString, range::UnitRange{Int}, value) =
    (_annotate!(s.annotations, range, value); s)

annotate!(s::AnnotatedString, value) =
    annotate!(s, firstindex(s):lastindex(s), value)

annotate!(s::SubString{<:AnnotatedString}, range::UnitRange{Int}, value) =
    (annotate!(s.string, s.offset .+ (range), value); s)

annotate!(s::SubString{<:AnnotatedString}, value) =
    (annotate!(s.string, s.offset .+ (1:s.ncodeunits), value); s)

"""
    annotate!(char::AnnotatedChar, value)

Annotate `char` with `value`. A `value` outside the value type `V` of `char` raises an
`ArgumentError`.
"""
annotate!(c::AnnotatedChar{C, V}, value::V) where {C, V} =
    (push!(c.annotations, value); c)
annotate!(::AnnotatedChar{C, V}, @nospecialize(value)) where {C, V} =
    throw(ArgumentError(LazyString("cannot annotate with a ", typeof(value), " value, as the value type is ", V,
                                   "; construct the char with a wider value type, such as Any")))

"""
    annotations(str::Union{AnnotatedString, SubString{AnnotatedString}},
                [position::Union{Integer, UnitRange}]) ->
        Vector{$(RegionAnnotation{TypeVar(:V)})}

Get all annotations that apply to `str`. Should `position` be provided, only
annotations that overlap with `position` will be returned.

Annotations are provided together with the regions they apply to, in the form of
a vector of region–annotation tuples.

In accordance with the semantics documented in [`AnnotatedString`](@ref), the
order of annotations returned matches the order in which they were applied.

See also [`annotate!`](@ref).
"""
annotations(s::AnnotatedString) = s.annotations

function annotations(s::SubString{AnnotatedString{S, V}}) where {S, V}
    substr_range = s.offset+1:s.offset+s.ncodeunits
    result = RegionAnnotation{V}[]
    for ann in annotations(s.string, substr_range)
        # Shift the region to be relative to the substring start
        shifted_region = first(ann.region)-s.offset:last(ann.region)-s.offset
        # @inline setindex makes :region const knowable (#60365)
        push!(result, @inline(setindex(ann, shifted_region, :region)))
    end
    return result
end

function annotations(s::AnnotatedString{S, V}, pos::UnitRange{<:Integer}) where {S, V}
    # TODO optimise
    result = RegionAnnotation{V}[]
    for ann in s.annotations
        if !isempty(intersect(pos, ann.region))
            clamped_region = max(first(pos), first(ann.region)):min(last(pos), last(ann.region))
            # @inline setindex makes :region const knowable (#60365)
            push!(result, @inline(setindex(ann, clamped_region, :region)))
        end
    end
    return result
end

annotations(s::AnnotatedString, pos::Integer) = annotations(s, pos:pos)

annotations(s::SubString{<:AnnotatedString}, pos::Integer) =
    annotations(s.string, s.offset + pos)

annotations(s::SubString{<:AnnotatedString}, pos::UnitRange{<:Integer}) =
    annotations(s.string, first(pos)+s.offset:last(pos)+s.offset)

"""
    annotations(chr::AnnotatedChar)::Vector{V}

Get the values of all annotations of `chr`.
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

Returns an `AnnotatedString{String, V}` (regardless of the original underlying
string type of `str`), where `V` is the value type of `str`.
"""
function annotated_chartransform(f::Function, str::AnnotatedString{S, V}, state=nothing) where {S, V}
    outstr = IOBuffer()
    annots = RegionAnnotation{V}[]
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
        push!(annots, @inline(setindex(annot, (start + start_offset):(stop + stop_offset), :region)))
    end
    AnnotatedString{String, V}(takestring!(outstr), annots)
end

struct RegionIterator{S <: AbstractString, V}
    str::S
    annotations::Vector{RegionAnnotation{V}}
    events::Vector{UInt64} # Each an annotation's start or end, see `annotation_events`
    shift::UInt            # Bit offset of the positions in `events`, fixed when they were made
    subregion::UnitRange{Int}
end

# One pass over the regions, so that passes made at once don't share their state
mutable struct RegionCursor{S, V}
    const regions::RegionIterator{S, V}
    const active::Vector{Int} # Sorted indices of the annotations over the current region
    pos::Int                  # Where the current region starts
end

# The annotations of one region. While the cursor is still on that region they are read
# from its state; afterwards they are found again by position, in time linear in the annotations.
struct ActiveAnnotations{S, V} <: AbstractVector{V}
    cursor::RegionCursor{S, V}
    pos::Int
end

IndexStyle(::Type{<:ActiveAnnotations}) = IndexLinear()
size(a::ActiveAnnotations) =
    (if a.cursor.pos == a.pos length(a.cursor.active) else count(Returns(true), a) end,)
function getindex(a::ActiveAnnotations, i::Int)
    @boundscheck checkbounds(a, i)
    a.cursor.pos == a.pos || return first(Iterators.drop(a, i - 1))
    a.cursor.regions.annotations[@inbounds a.cursor.active[i]].value
end
# For a region the cursor has left, one pass over the annotations rather than a scan per element
@inline function iterate(a::ActiveAnnotations, i::Int = 1)
    (; cursor) = a
    (; annotations) = cursor.regions
    if cursor.pos == a.pos # Not through `length(a)`, so this inlines and a tuple of a union of values isn't boxed
        i <= length(cursor.active) || return nothing
        return (annotations[@inbounds cursor.active[i]].value, i + 1)
    end
    # An annotation that starts within a character covers the region of that character
    stop = nextind(cursor.regions.str, a.pos)
    index = findnext(annot -> !isempty(annot.region) && first(annot.region) < stop && a.pos <= last(annot.region),
                     annotations, i)
    isnothing(index) && return nothing
    (@inbounds(annotations[index]).value, index + 1)
end

IteratorSize(::Type{<:RegionIterator}) = SizeUnknown()
eltype(::Type{RegionIterator{S, V}}) where {S, V} = Tuple{SubString{S}, ActiveAnnotations{S, V}}
eltype(::Type{RegionIterator{SubString{S}, V}}) where {S, V} = Tuple{SubString{S}, ActiveAnnotations{SubString{S}, V}}

@inline function iterate(r::RegionIterator, (i, pos, cursor) = (1, first(r.subregion), RegionCursor(r, Int[], 0)))
    pos > last(r.subregion) && return nothing
    (; events, shift) = r
    (; active) = cursor
    while i <= length(events) && events[i] >> shift == pos
        event = events[i]
        index, isstart = Int(event & (UInt64(1) << shift - 1)) >> 1, isodd(event)
        at = searchsortedfirst(active, index)
        if isstart # Shift the entries by hand: `insert!` and `deleteat!` near the front reallocate on alternate calls
            push!(active, index)
            copyto!(active, at + 1, active, at, length(active) - at)
            active[at] = index
        else
            copyto!(active, at, active, at + 1, length(active) - at)
            pop!(active)
        end
        i += 1
    end
    cursor.pos = pos
    next = if i <= length(events)
        Int(events[i] >> shift)
    else # To the end of the character the subregion ends in
        nextind(r.str, thisind(r.str, last(r.subregion)))
    end
    ((@inbounds(raw_substring(r.str, pos, next - pos)), ActiveAnnotations(cursor, pos)), (i, next, cursor))
end

"""
    eachregion(s::AnnotatedString{S})
    eachregion(s::SubString{AnnotatedString{S}})

Identify the contiguous substrings of `s` with constant annotations, and return an
iterator which provides each substring and the values of the applicable annotations
as a `Tuple{SubString{S}, AbstractVector{V}}`.

The annotations of each region are read from `s` when they are used, so `s` must
not be changed while its regions are in use.

# Examples

```jldoctest; setup=:(using Base: AnnotatedString, eachregion)
julia> a = AnnotatedString("hey there", [(1:3, :bold), (5:9, :italic)]);

julia> map(collect, eachregion(a))
3-element Vector{Vector{Any}}:
 ["hey", [:bold]]
 [" ", Symbol[]]
 ["there", [:italic]]
```
"""
function eachregion(s::AnnotatedString{S, V}, subregion::UnitRange{Int}=firstindex(s):lastindex(s), ::Type{T}=V) where {S, V, T}
    # Whole characters, so a region's annotations read the same during iteration and after it
    isempty(subregion) || (subregion = thisind(s, first(subregion)):nextind(s, thisind(s, last(subregion))) - 1)
    # Only this reads the values as `V`, so the iteration is compiled for `T`
    annots = if V <: T s.annotations else RegionAnnotation{T}[annot for annot in s.annotations if annot.value isa T] end
    RegionIterator(s.string, annots, annotation_events(s.string, annots, subregion),
                   unsigned(top_set_bit(2 * length(annots) + 1)), subregion)
end

eachregion(s::SubString{AnnotatedString{S, V}}, pos::UnitRange{Int}=firstindex(s):lastindex(s), ::Type{T}=V) where {S, V, T} =
    eachregion(s.string, first(pos)+s.offset:last(pos)+s.offset, T)

"""
    annotation_events(string::AbstractString, annots::Vector{$RegionAnnotation}, subregion::UnitRange{Int})

Find all annotation "change events" of `annots` that occur within a `subregion` of `string`.

Each change event is a `UInt64` packing the position of the event above the index of
the annotation in question, followed by a bit set when the annotation is being
activated, so that sorting the events orders them by position. The index and bit take
the low `top_set_bit(2length(annots) + 1)` bits, as the region iterator reads them.
"""
function annotation_events(s::AbstractString, annots::Vector{<:RegionAnnotation}, subregion::UnitRange{Int})
    events = UInt64[]
    sizehint!(events, 2 * length(annots))
    shift = unsigned(top_set_bit(2 * length(annots) + 1))
    leading_zeros(UInt64(ncodeunits(s) + 1)) >= shift || throw(ArgumentError(LazyString(
        "too many annotations (", length(annots), ") to order the regions of a ", ncodeunits(s), "-byte string")))
    for (i, (; region)) in enumerate(annots)
        start, stop = max(first(subregion), first(region)), min(last(subregion), last(region))
        start <= stop || continue # Outside the subregion, or empty
        push!(events, UInt64(thisind(s, start)) << shift | UInt64(i) << 1 | true)
        push!(events, UInt64(nextind(s, stop)) << shift | UInt64(i) << 1)
    end
    sort!(events) # Integer keys, so an out-of-order list gets a radix sort
end
