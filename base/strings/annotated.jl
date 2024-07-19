# This file is a part of Julia. License is MIT: https://julialang.org/license

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
and a value (`Any`), paired together as a `Pair{Symbol, <:Any}`.

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

!!! warning
    While the constructors are part of the Base public API, the fields
    of `AnnotatedString` are not. This is to allow for potential future
    changes in the implementation of this type. Instead use the
    [`annotations`](@ref), and [`annotate!`](@ref) getter/setter
    functions.

# Constructors

```julia
AnnotatedString(s::S<:AbstractString) -> AnnotatedString{S}
AnnotatedString(s::S<:AbstractString, annotations::Vector{Tuple{UnitRange{Int}, Pair{Symbol, <:Any}}})
```

A AnnotatedString can also be created with [`annotatedstring`](@ref), which acts much
like [`string`](@ref) but preserves any annotations present in the arguments.

# Examples

```julia-repl
julia> AnnotatedString("this is an example annotated string",
                    [(1:18, :A => 1), (12:28, :B => 2), (18:35, :C => 3)])
"this is an example annotated string"
```
"""
struct AnnotatedString{S <: AbstractString} <: AbstractString
    string::S
    annotations::Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}
end

"""
    AnnotatedChar{S <: AbstractChar} <: AbstractChar

A Char with annotations.

More specifically, this is a simple wrapper around any other
[`AbstractChar`](@ref), which holds a list of arbitrary labeled annotations
(`Pair{Symbol, <:Any}`) with the wrapped character.

See also: [`AnnotatedString`](@ref), [`annotatedstring`](@ref), `annotations`,
and `annotate!`.

!!! warning
    While the constructors are part of the Base public API, the fields
    of `AnnotatedChar` are not. This it to allow for potential future
    changes in the implementation of this type. Instead use the
    [`annotations`](@ref), and [`annotate!`](@ref) getter/setter
    functions.

# Constructors

```julia
AnnotatedChar(s::S) -> AnnotatedChar{S}
AnnotatedChar(s::S, annotations::Vector{Pair{Symbol, <:Any}})
```

# Examples

```julia-repl
julia> AnnotatedChar('j', :label => 1)
'j': ASCII/Unicode U+006A (category Ll: Letter, lowercase)
```
"""
struct AnnotatedChar{C <: AbstractChar} <: AbstractChar
    char::C
    annotations::Vector{Pair{Symbol, Any}}
end

## Constructors ##

# When called with overly-specialised arguments

AnnotatedString(s::AbstractString, annots::Vector{<:Tuple{UnitRange{Int}, <:Pair{Symbol, <:Any}}}) =
    AnnotatedString(s, Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}(annots))

AnnotatedChar(c::AbstractChar, annots::Vector{<:Pair{Symbol, <:Any}}) =
    AnnotatedChar(c, Vector{Pair{Symbol, Any}}(annots))

# Constructors to avoid recursive wrapping

AnnotatedString(s::AnnotatedString, annots::Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}) =
    AnnotatedString(s.string, vcat(s.annotations, annots))

AnnotatedChar(c::AnnotatedChar, annots::Vector{Pair{Symbol, Any}}) =
    AnnotatedChar(c.char, vcat(c.annotations, annots))

String(s::AnnotatedString{String}) = s.string # To avoid pointless overhead

## Conversion/promotion ##

convert(::Type{AnnotatedString}, s::AnnotatedString) = s
convert(::Type{AnnotatedString{S}}, s::S) where {S <: AbstractString} =
    AnnotatedString(s, Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}())
convert(::Type{AnnotatedString}, s::S) where {S <: AbstractString} =
    convert(AnnotatedString{S}, s)
AnnotatedString(s::S) where {S <: AbstractString} = convert(AnnotatedString{S}, s)

convert(::Type{AnnotatedChar}, c::AnnotatedChar) = c
convert(::Type{AnnotatedChar{C}}, c::C) where { C <: AbstractChar } =
    AnnotatedChar{C}(c, Vector{Pair{Symbol, Any}}())
convert(::Type{AnnotatedChar}, c::C) where { C <: AbstractChar } =
    convert(AnnotatedChar{C}, c)

AnnotatedChar(c::AbstractChar) = convert(AnnotatedChar, c)
AnnotatedChar(c::UInt32) = convert(AnnotatedChar, Char(c))
AnnotatedChar{C}(c::UInt32) where {C <: AbstractChar} = convert(AnnotatedChar, C(c))

promote_rule(::Type{<:AnnotatedString}, ::Type{<:AbstractString}) = AnnotatedString

## AbstractString interface ##

ncodeunits(s::AnnotatedString) = ncodeunits(s.string)
codeunits(s::AnnotatedString) = codeunits(s.string)
codeunit(s::AnnotatedString) = codeunit(s.string)
codeunit(s::AnnotatedString, i::Integer) = codeunit(s.string, i)
isvalid(s::AnnotatedString, i::Integer) = isvalid(s.string, i)
@propagate_inbounds iterate(s::AnnotatedString, i::Integer=firstindex(s)) =
    if i <= lastindex(s.string); (s[i], nextind(s, i)) end
eltype(::Type{<:AnnotatedString{S}}) where {S} = AnnotatedChar{eltype(S)}
firstindex(s::AnnotatedString) = firstindex(s.string)
lastindex(s::AnnotatedString) = lastindex(s.string)

function getindex(s::AnnotatedString, i::Integer)
    @boundscheck checkbounds(s, i)
    @inbounds if isvalid(s, i)
        AnnotatedChar(s.string[i], Pair{Symbol, Any}[last(x) for x in annotations(s, i)])
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
    show(IOContext(io, :typeinfo => typeof(annotations(s))), annotations(s))
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

```julia-repl
julia> annotatedstring("now a AnnotatedString")
"now a AnnotatedString"

julia> annotatedstring(AnnotatedString("annotated", [(1:9, :label => 1)]), ", and unannotated")
"annotated, and unannotated"
```
"""
function annotatedstring(xs...)
    isempty(xs) && return AnnotatedString("")
    size = mapreduce(_str_sizehint, +, xs)
    buf = IOBuffer(sizehint=size)
    s = IOContext(buf, :color => true)
    annotations = Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}()
    for x in xs
        size = filesize(s.io)
        if x isa AnnotatedString
            for (region, annot) in x.annotations
                push!(annotations, (size .+ (region), annot))
            end
            print(s, x.string)
        elseif x isa SubString{<:AnnotatedString}
            for (region, annot) in x.string.annotations
                start, stop = first(region), last(region)
                if start <= x.offset + x.ncodeunits && stop > x.offset
                    rstart = size + max(0, start - x.offset - 1) + 1
                    rstop = size + min(stop, x.offset + x.ncodeunits) - x.offset
                    push!(annotations, (rstart:rstop, annot))
                end
            end
            print(s, SubString(x.string.string, x.offset, x.ncodeunits, Val(:noshift)))
        elseif x isa AnnotatedChar
            for annot in x.annotations
                push!(annotations, (1+size:1+size, annot))
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
    AnnotatedString(string(c.char), [(1:ncodeunits(c), annot) for annot in c.annotations])

AnnotatedString(s::SubString{<:AnnotatedString}) = annotatedstring(s)

function repeat(str::AnnotatedString, r::Integer)
    r == 0 && return one(AnnotatedString)
    r == 1 && return str
    unannot = repeat(str.string, r)
    annotations = Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}()
    len = ncodeunits(str)
    fullregion = firstindex(str):lastindex(str)
    if allequal(first, str.annotations) && first(first(str.annotations)) == fullregion
        newfullregion = firstindex(unannot):lastindex(unannot)
        for (_, annot) in str.annotations
            push!(annotations, (newfullregion, annot))
        end
    else
        for offset in 0:len:(r-1)*len
            for (region, annot) in str.annotations
                push!(annotations, (region .+ offset, annot))
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
    AnnotatedString(str, [(fullregion, annot) for annot in c.annotations])
end

function reverse(s::AnnotatedString)
    lastind = lastindex(s)
    AnnotatedString(reverse(s.string),
                 [(UnitRange(1 + lastind - last(region),
                             1 + lastind - first(region)),
                   annot)
                  for (region, annot) in s.annotations])
end

# TODO optimise?
reverse(s::SubString{<:AnnotatedString}) = reverse(AnnotatedString(s))

# TODO implement `replace(::AnnotatedString, ...)`

## End AbstractString interface ##

function _annotate!(annlist::Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}, range::UnitRange{Int}, @nospecialize(labelval::Pair{Symbol, <:Any}))
    label, val = labelval
    if val === nothing
        deleteat!(annlist, findall(ann -> ann[1] == range && first(ann[2]) === label, annlist))
    else
        push!(annlist, (range, Pair{Symbol, Any}(label, val)))
    end
end

"""
    annotate!(str::AnnotatedString, [range::UnitRange{Int}], label::Symbol => value)
    annotate!(str::SubString{AnnotatedString}, [range::UnitRange{Int}], label::Symbol => value)

Annotate a `range` of `str` (or the entire string) with a labeled value (`label` => `value`).
To remove existing `label` annotations, use a value of `nothing`.

The order in which annotations are applied to `str` is semantically meaningful,
as described in [`AnnotatedString`](@ref).
"""
annotate!(s::AnnotatedString, range::UnitRange{Int}, @nospecialize(labelval::Pair{Symbol, <:Any})) =
    (_annotate!(s.annotations, range, labelval); s)

annotate!(ss::AnnotatedString, @nospecialize(labelval::Pair{Symbol, <:Any})) =
    annotate!(ss, firstindex(ss):lastindex(ss), labelval)

annotate!(s::SubString{<:AnnotatedString}, range::UnitRange{Int}, @nospecialize(labelval::Pair{Symbol, <:Any})) =
    (annotate!(s.string, s.offset .+ (range), labelval); s)

annotate!(s::SubString{<:AnnotatedString}, @nospecialize(labelval::Pair{Symbol, <:Any})) =
    (annotate!(s.string, s.offset .+ (1:s.ncodeunits), labelval); s)

"""
    annotate!(char::AnnotatedChar, label::Symbol => value)

Annotate `char` with the pair `label => value`.
"""
annotate!(c::AnnotatedChar, @nospecialize(labelval::Pair{Symbol, <:Any})) =
    (push!(c.annotations, labelval); c)

"""
    annotations(str::Union{AnnotatedString, SubString{AnnotatedString}},
                [position::Union{Integer, UnitRange}]) ->
        Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}

Get all annotations that apply to `str`. Should `position` be provided, only
annotations that overlap with `position` will be returned.

Annotations are provided together with the regions they apply to, in the form of
a vector of region–annotation tuples.

In accordance with the semantics documented in [`AnnotatedString`](@ref), the
order of annotations returned matches the order in which they were applied.

See also: `annotate!`.
"""
annotations(s::AnnotatedString) = s.annotations

function annotations(s::SubString{<:AnnotatedString})
    map(((region, annot),) -> (first(region)-s.offset:last(region)-s.offset, annot),
        annotations(s.string, s.offset+1:s.offset+s.ncodeunits))
end

function annotations(s::AnnotatedString, pos::UnitRange{<:Integer})
    # TODO optimise
    Tuple{UnitRange{Int64}, Pair{Symbol, Any}}[
        (max(first(pos), first(region)):min(last(pos), last(region)), annot)
        for (region, annot) in s.annotations if !isempty(intersect(pos, region))]
end

annotations(s::AnnotatedString, pos::Integer) = annotations(s, pos:pos)

annotations(s::SubString{<:AnnotatedString}, pos::Integer) =
    annotations(s.string, s.offset + pos)

annotations(s::SubString{<:AnnotatedString}, pos::UnitRange{<:Integer}) =
    annotations(s.string, first(pos)+s.offset:last(pos)+s.offset)

"""
    annotations(chr::AnnotatedChar) -> Vector{Pair{Symbol, Any}}

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
    annots = Tuple{UnitRange{Int}, Pair{Symbol, Any}}[]
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
        region, value = annot
        start, stop = first(region), last(region)
        start_offset = last(offsets[findlast(<=(start) ∘ first, offsets)::Int])
        stop_offset  = last(offsets[findlast(<=(stop) ∘ first, offsets)::Int])
        push!(annots, ((start + start_offset):(stop + stop_offset), value))
    end
    AnnotatedString(String(take!(outstr)), annots)
end

## AnnotatedIOBuffer

struct AnnotatedIOBuffer <: AbstractPipe
    io::IOBuffer
    annotations::Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}
end

AnnotatedIOBuffer(io::IOBuffer) = AnnotatedIOBuffer(io, Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}())
AnnotatedIOBuffer() = AnnotatedIOBuffer(IOBuffer())

function show(io::IO, aio::AnnotatedIOBuffer)
    show(io, AnnotatedIOBuffer)
    size = filesize(aio.io)
    print(io, '(', size, " byte", ifelse(size == 1, "", "s"), ", ",
          length(aio.annotations), " annotation", ifelse(length(aio.annotations) == 1, "", "s"), ")")
end

pipe_reader(io::AnnotatedIOBuffer) = io.io
pipe_writer(io::AnnotatedIOBuffer) = io.io

# Useful `IOBuffer` methods that we don't get from `AbstractPipe`
position(io::AnnotatedIOBuffer) = position(io.io)
seek(io::AnnotatedIOBuffer, n::Integer) = (seek(io.io, n); io)
seekend(io::AnnotatedIOBuffer) = (seekend(io.io); io)
skip(io::AnnotatedIOBuffer, n::Integer) = (skip(io.io, n); io)
copy(io::AnnotatedIOBuffer) = AnnotatedIOBuffer(copy(io.io), copy(io.annotations))

annotations(io::AnnotatedIOBuffer) = io.annotations

annotate!(io::AnnotatedIOBuffer, range::UnitRange{Int}, @nospecialize(labelval::Pair{Symbol, <:Any})) =
    (_annotate!(io.annotations, range, labelval); io)

function write(io::AnnotatedIOBuffer, astr::Union{AnnotatedString, SubString{<:AnnotatedString}})
    astr = AnnotatedString(astr)
    offset = position(io.io)
    eof(io) || _clear_annotations_in_region!(io.annotations, offset+1:offset+ncodeunits(astr))
    _insert_annotations!(io, astr.annotations)
    write(io.io, String(astr))
end

write(io::AnnotatedIOBuffer, c::AnnotatedChar) =
    write(io, AnnotatedString(string(c), map(a -> (1:ncodeunits(c), a), annotations(c))))
write(io::AnnotatedIOBuffer, x::AbstractString) = write(io.io, x)
write(io::AnnotatedIOBuffer, s::Union{SubString{String}, String}) = write(io.io, s)
write(io::AnnotatedIOBuffer, b::UInt8) = write(io.io, b)

function write(dest::AnnotatedIOBuffer, src::AnnotatedIOBuffer)
    destpos = position(dest)
    isappending = eof(dest)
    srcpos = position(src)
    nb = write(dest.io, src.io)
    isappending || _clear_annotations_in_region!(dest.annotations, destpos:destpos+nb)
    srcannots = [(max(1 + srcpos, first(region)):last(region), annot)
                 for (region, annot) in src.annotations if first(region) >= srcpos]
    _insert_annotations!(dest, srcannots, destpos - srcpos)
    nb
end

# So that read/writes with `IOContext` (and any similar `AbstractPipe` wrappers)
# work as expected.
function write(io::AbstractPipe, s::Union{AnnotatedString, SubString{<:AnnotatedString}})
    if pipe_writer(io) isa AnnotatedIOBuffer
        write(pipe_writer(io), s)
    else
        invoke(write, Tuple{IO, typeof(s)}, io, s)
    end::Int
end
# Can't be part of the `Union` above because it introduces method ambiguities
function write(io::AbstractPipe, c::AnnotatedChar)
    if pipe_writer(io) isa AnnotatedIOBuffer
        write(pipe_writer(io), c)
    else
        invoke(write, Tuple{IO, typeof(c)}, io, c)
    end::Int
end

"""
    _clear_annotations_in_region!(annotations::Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}, span::UnitRange{Int})

Erase the presence of `annotations` within a certain `span`.

This operates by removing all elements of `annotations` that are entirely
contained in `span`, truncating ranges that partially overlap, and splitting
annotations that subsume `span` to just exist either side of `span`.
"""
function _clear_annotations_in_region!(annotations::Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}, span::UnitRange{Int})
    # Clear out any overlapping pre-existing annotations.
    filter!(((region, _),) -> first(region) < first(span) || last(region) > last(span), annotations)
    extras = Tuple{Int, Tuple{UnitRange{Int}, Pair{Symbol, Any}}}[]
    for i in eachindex(annotations)
        region, annot = annotations[i]
        # Test for partial overlap
        if first(region) <= first(span) <= last(region) || first(region) <= last(span) <= last(region)
            annotations[i] = (if first(region) < first(span)
                                        first(region):first(span)-1
                                    else last(span)+1:last(region) end, annot)
            # If `span` fits exactly within `region`, then we've only copied over
            # the beginning overhang, but also need to conserve the end overhang.
            if first(region) < first(span) && last(span) < last(region)
                push!(extras, (i, (last(span)+1:last(region), annot)))
            end
        end
    end
    # Insert any extra entries in the appropriate position
    for (offset, (i, entry)) in enumerate(extras)
        insert!(annotations, i + offset, entry)
    end
    annotations
end

"""
    _insert_annotations!(io::AnnotatedIOBuffer, annotations::Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}, offset::Int = position(io))

Register new `annotations` in `io`, applying an `offset` to their regions.

The largely consists of simply shifting the regions of `annotations` by `offset`
and pushing them onto `io`'s annotations. However, when it is possible to merge
the new annotations with recent annotations in accordance with the semantics
outlined in [`AnnotatedString`](@ref), we do so. More specifically, when there
is a run of the most recent annotations that are also present as the first
`annotations`, with the same value and adjacent regions, the new annotations are
merged into the existing recent annotations by simply extending their range.

This is implemented so that one can say write an `AnnotatedString` to an
`AnnotatedIOBuffer` one character at a time without needlessly producing a
new annotation for each character.
"""
function _insert_annotations!(io::AnnotatedIOBuffer, annotations::Vector{Tuple{UnitRange{Int}, Pair{Symbol, Any}}}, offset::Int = position(io))
    run = 0
    if !isempty(io.annotations) && last(first(last(io.annotations))) == offset
        for i in reverse(axes(annotations, 1))
            annot = annotations[i]
            first(first(annot)) == 1 || continue
            if last(annot) == last(last(io.annotations))
                valid_run = true
                for runlen in 1:i
                    new_range, new_annot = annotations[begin+runlen-1]
                    old_range, old_annot = io.annotations[end-i+runlen]
                    if last(old_range) != offset || first(new_range) != 1 || old_annot != new_annot
                        valid_run = false
                        break
                    end
                end
                if valid_run
                    run = i
                    break
                end
            end
        end
    end
    for runindex in 0:run-1
        old_index = lastindex(io.annotations) - run + 1 + runindex
        old_region, annot = io.annotations[old_index]
        new_region, _ = annotations[begin+runindex]
        io.annotations[old_index] = (first(old_region):last(new_region)+offset, annot)
    end
    for index in run+1:lastindex(annotations)
        region, annot = annotations[index]
        start, stop = first(region), last(region)
        push!(io.annotations, (start+offset:stop+offset, annot))
    end
end

function read(io::AnnotatedIOBuffer, ::Type{AnnotatedString{T}}) where {T <: AbstractString}
    if (start = position(io)) == 0
        AnnotatedString(read(io.io, T), copy(io.annotations))
    else
        annots = [(UnitRange{Int}(max(1, first(region) - start), last(region)-start), val)
                  for (region, val) in io.annotations if last(region) > start]
        AnnotatedString(read(io.io, T), annots)
    end
end
read(io::AnnotatedIOBuffer, ::Type{AnnotatedString{AbstractString}}) = read(io, AnnotatedString{String})
read(io::AnnotatedIOBuffer, ::Type{AnnotatedString}) = read(io, AnnotatedString{String})

function read(io::AnnotatedIOBuffer, ::Type{AnnotatedChar{T}}) where {T <: AbstractChar}
    pos = position(io)
    char = read(io.io, T)
    annots = Pair{Symbol, Any}[annot for (range, annot) in io.annotations if pos+1 in range]
    AnnotatedChar(char, annots)
end
read(io::AnnotatedIOBuffer, ::Type{AnnotatedChar{AbstractChar}}) = read(io, AnnotatedChar{Char})
read(io::AnnotatedIOBuffer, ::Type{AnnotatedChar}) = read(io, AnnotatedChar{Char})

function truncate(io::AnnotatedIOBuffer, size::Integer)
    truncate(io.io, size)
    filter!(((range, _),) -> first(range) <= size, io.annotations)
    map!(((range, val),) -> (first(range):min(size, last(range)), val),
         io.annotations, io.annotations)
    io
end
