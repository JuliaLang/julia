# This file is a part of Julia. License is MIT: https://julialang.org/license

## AnnotatedIOBuffer

struct AnnotatedIOBuffer{V} <: AbstractPipe
    io::IOBuffer
    annotations::Vector{RegionAnnotation{V}}
end

AnnotatedIOBuffer{V}(io::IOBuffer = IOBuffer()) where {V} = AnnotatedIOBuffer(io, Vector{RegionAnnotation{V}}())
AnnotatedIOBuffer(io::IOBuffer = IOBuffer()) = AnnotatedIOBuffer{Any}(io)

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

annotate!(io::AnnotatedIOBuffer, range::UnitRange{Int}, @nospecialize(val::Any)) =
    (_annotate!(io.annotations, range, val); io)

function write(io::AnnotatedIOBuffer{V}, astr::Union{AnnotatedString{S}, SubString{<:AnnotatedString{S}}}) where {V, S}
    annots = convert(Vector{RegionAnnotation{V}}, annotations(astr))
    offset = position(io.io)
    eof(io) || _clear_annotations_in_region!(io.annotations, offset+1:offset+ncodeunits(astr))
    _insert_annotations!(io, annots)
    write(io.io, unannotate(astr))::Int
end

write(io::AnnotatedIOBuffer, c::AnnotatedChar) = write(io, annotatedstring(c))
write(io::AnnotatedIOBuffer, x::AbstractString) = write(io.io, x)::Int
write(io::AnnotatedIOBuffer, s::Union{SubString{String}, String}) = write(io.io, s)
write(io::AnnotatedIOBuffer, s::StringViewAndSub) = write(io.io, s)::Int
write(io::AnnotatedIOBuffer, b::UInt8) = write(io.io, b)

function write(dest::AnnotatedIOBuffer{V}, src::AnnotatedIOBuffer) where {V}
    destpos = position(dest)
    isappending = eof(dest)
    srcpos = position(src)
    srcannots = RegionAnnotation{V}[ # Before the text, so a value that doesn't fit leaves `dest` as it was
        @inline(setindex(annot, max(1 + srcpos, first(annot.region)):last(annot.region), :region))
        for annot in src.annotations if first(annot.region) >= srcpos]
    nb = write(dest.io, src.io)
    isappending || _clear_annotations_in_region!(dest.annotations, destpos:destpos+nb)
    _insert_annotations!(dest, srcannots, destpos - srcpos)
    nb
end

# So that read/writes with `IOContext` (and any similar `AbstractPipe` wrappers) work as expected.
function write(io::AbstractPipe, s::Union{AnnotatedString{S}, SubString{<:AnnotatedString{S}}}) where {S}
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

function read(io::AnnotatedIOBuffer, ::Type{AnnotatedString{S, V}}) where {S, V}
    start = position(io)
    annots = RegionAnnotation{V}[
        (region = max(1, first(annot.region) - start):last(annot.region)-start,
         value = annot.value)
        for annot in io.annotations if last(annot.region) > start]
    AnnotatedString{S, V}(read(io.io, S), annots)
end
read(io::AnnotatedIOBuffer{V}, ::Type{AnnotatedString{S}}) where {S, V} = read(io, AnnotatedString{S, V})
read(io::AnnotatedIOBuffer, ::Type{AnnotatedString{AbstractString}}) = read(io, AnnotatedString{String})
read(io::AnnotatedIOBuffer, ::Type{AnnotatedString}) = read(io, AnnotatedString{String})

function read(io::AnnotatedIOBuffer, ::Type{AnnotatedChar{T, V}}) where {T <: AbstractChar, V}
    pos = position(io)
    char = read(io.io, T)
    annots = V[annot.value for annot in io.annotations if pos+1 in annot.region]
    AnnotatedChar{T, V}(char, annots)
end
read(io::AnnotatedIOBuffer{V}, ::Type{AnnotatedChar{T}}) where {T <: AbstractChar, V} = read(io, AnnotatedChar{T, V})
read(io::AnnotatedIOBuffer, ::Type{AnnotatedChar{AbstractChar}}) = read(io, AnnotatedChar{Char})
read(io::AnnotatedIOBuffer, ::Type{AnnotatedChar}) = read(io, AnnotatedChar{Char})

function truncate(io::AnnotatedIOBuffer, size::Integer)
    truncate(io.io, size)
    filter!(ann -> first(ann.region) <= size, io.annotations)
    map!(ann -> @inline(setindex(ann, first(ann.region):min(size, last(ann.region)), :region)),
         io.annotations, io.annotations)
    io
end

"""
    _clear_annotations_in_region!(annotations::Vector{$RegionAnnotation}, span::UnitRange{Int})

Erase the presence of `annotations` within a certain `span`.

This operates by removing all elements of `annotations` that are entirely
contained in `span`, truncating ranges that partially overlap, and splitting
annotations that subsume `span` to just exist either side of `span`.
"""
function _clear_annotations_in_region!(annotations::Vector{RegionAnnotation{V}}, span::UnitRange{Int}) where {V}
    # Clear out any overlapping pre-existing annotations.
    filter!(ann -> first(ann.region) < first(span) || last(ann.region) > last(span), annotations)
    extras = Tuple{Int, RegionAnnotation{V}}[]
    for i in eachindex(annotations)
        annot = annotations[i]
        region = annot.region
        # Test for partial overlap
        if first(region) <= first(span) <= last(region) || first(region) <= last(span) <= last(region)
            annotations[i] =
                @inline(setindex(annot,
                         if first(region) < first(span)
                             first(region):first(span)-1
                         else
                             last(span)+1:last(region)
                         end,
                         :region))
            # If `span` fits exactly within `region`, then we've only copied over
            # the beginning overhang, but also need to conserve the end overhang.
            if first(region) < first(span) && last(span) < last(region)
                push!(extras, (i, @inline(setindex(annot, last(span)+1:last(region), :region))))
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
    _insert_annotations!(io::AnnotatedIOBuffer, annotations::Vector{$RegionAnnotation}, offset::Int = position(io))

Register new `annotations` in `io`, applying an `offset` to their regions.

This largely consists of simply shifting the regions of `annotations` by `offset`
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
function _insert_annotations!(annots::Vector{RegionAnnotation{V}}, newannots::Vector{RegionAnnotation{V′}}, offset::Int = 0) where {V, V′ <: V}
    run = @label search begin
        if !isempty(annots) && last(last(annots).region) == offset
            for i in reverse(axes(newannots, 1))
                annot = newannots[i]
                first(annot.region) == 1 || continue
                i <= length(annots) || continue
                annot.value === last(annots).value || continue
                all(1:i) do runlen
                    new = newannots[begin+runlen-1]
                    old = annots[end-i+runlen]
                    !(last(old.region) != offset ||
                    first(new.region) != 1 ||
                    old.value !== new.value)
                end || continue
                break search i
            end
        end
        0
    end
    for runindex in 0:run-1
        old_index = lastindex(annots) - run + 1 + runindex
        old = annots[old_index]
        new = newannots[begin+runindex]
        annots[old_index] = RegionAnnotation{V}((first(old.region):last(new.region)+offset, old.value))
    end
    for index in run+1:lastindex(newannots)
        annot = newannots[index]
        start, stop = first(annot.region), last(annot.region)
        push!(annots, RegionAnnotation{V}((start+offset:stop+offset, annot.value)))
    end
end

_insert_annotations!(io::AnnotatedIOBuffer, newannots::Vector{<:RegionAnnotation}, offset::Int = position(io)) =
    _insert_annotations!(io.annotations, newannots, offset)

# String replacement

# `Vararg{Pair, N}`, as the keyword body would otherwise not specialise on the pairs it passes on
function replace(out::AnnotatedIOBuffer{V}, str::AnnotatedString, pat_f::Vararg{Pair, N}; count = typemax(Int)) where {V, N}
    if count == 0 || isempty(pat_f)
        write(out, str)
        return out
    end
    e1, patterns, replacers, repspans, notfound = _replace_init(str.string, pat_f, count)
    if notfound
        foreach(_free_pat_replacer, patterns)
        write(out, str)
        return out
    end
    # Modelled after `Base.annotated_chartransform`, but needing
    # to handle a bit more complexity.
    isappending = eof(out)
    newannots = empty(out.annotations)
    bytepos = bytestart = firstindex(str.string)
    replacements = [(region = (bytestart - 1):(bytestart - 1), offset = position(out))]
    nrep = 1
    while nrep <= count
        repspans, ridx, xspan, newbytes, bytepos = @inline _replace_once(
            out.io, str.string, bytestart, e1, patterns, replacers, repspans, count, nrep, bytepos)
        first(xspan) >= e1 && break
        nrep += 1
        # NOTE: When the replaced pattern ends with a multi-codeunit character,
        # `xspan` only covers up to the start of that character. However,
        # for us to correctly account for the changes to the string we need
        # the /entire/ span of codeunits that were replaced.
        if !isempty(xspan) && codeunit(str.string, last(xspan)) > 0x80
            xspan = first(xspan):nextind(str.string, last(xspan))-1
        end
        drift = last(replacements).offset
        thisrep = (region = xspan, offset = drift + newbytes - length(xspan))
        destoff = first(xspan) - 1 + drift
        push!(replacements, thisrep)
        replacement = replacers[ridx]
        _isannotated(replacement) || continue
        annots′ = if replacement isa AnnotatedChar
            region = 1:newbytes
            [RegionAnnotation{V}((region, value)) for value in annotations(replacement)]
        else
            convert(Vector{RegionAnnotation{V}}, annotations(replacement))
        end
        _insert_annotations!(newannots, annots′, destoff)
    end
    push!(replacements, (region = e1:(e1-1), offset = last(replacements).offset))
    foreach(_free_pat_replacer, patterns)
    write(out.io, SubString(str.string, bytepos))
    # NOTE: To enable more efficient annotation clearing,
    # we make use of the fact that `_replace_once` picks
    # replacements ordered by their match start position.
    # This means that the start of `.region`s in
    # `replacements` is monotonically increasing.
    isappending || _clear_annotations_in_region!(out.annotations, first(replacements).offset:position(out))
    for (; region, value) in str.annotations
        start, stop = first(region), last(region)
        prioridx = searchsortedlast(
            replacements, (region = start:start, offset = 0),
            by = r -> first(r.region))
        postidx = searchsortedfirst(
            replacements, (region = stop:stop, offset = 0),
            by = r -> first(r.region))
        priorrep, postrep = replacements[prioridx], replacements[postidx]
        if prioridx == postidx && start >= first(priorrep.region) && stop <= last(priorrep.region)
            # Region contained within a replacement
            continue
        elseif postidx - prioridx <= 1 && start > last(priorrep.region) && stop < first(postrep.region)
            # Lies between replacements
            push!(out.annotations, RegionAnnotation{V}(((start + priorrep.offset):(stop + priorrep.offset), value)))
        else
            # Split between replacements
            prevrep = replacements[max(begin, prioridx - 1)]
            for rep in @view replacements[max(begin, prioridx - 1):min(end, postidx + 1)]
                gap = max(start, last(prevrep.region)+1):min(stop, first(rep.region)-1)
                if !isempty(gap)
                    push!(out.annotations, RegionAnnotation{V}(((first(gap) + prevrep.offset):(last(gap) + prevrep.offset), value)))
                end
                prevrep = rep
            end
        end
    end
    append!(out.annotations, newannots)
    out
end

replace(out::IO, str::AnnotatedString, pat_f::Pair...; count=typemax(Int)) =
    replace(out, str.string, pat_f...; count)

function replace(str::AnnotatedString, pat_f::Pair...; count=typemax(Int))
    V = annot_promote_valtype(str, pat_f...)
    # As read back below, so that the type doesn't depend on `count`
    (isempty(pat_f) || iszero(count)) &&
        return AnnotatedString{String, V}(String(str.string), Vector{RegionAnnotation{V}}(str.annotations))
    out = AnnotatedIOBuffer{V}()
    replace(out, str, pat_f...; count)
    read(seekstart(out), AnnotatedString)
end

# Printing

function printstyled end

"""
    AnnotatedDisplay

How annotated strings, substrings, and chars are displayed. A type of annotation value says how
its annotations look in a MIME type with three functions: `context`, `reduction`, and
`transition`. Each region of a string has a state for each value type, reduced from the region's
annotations of that type, and displaying the string writes the transitions between the states
of its regions. As each type's display depends only on its own annotations, a string displays
the same whatever union of value types holds it.

!!! warning "Experimental"
    This interface is experimental and may change or be removed in a future release
    without deprecation.
"""
module AnnotatedDisplay

using ..Base: IO, IOBuffer, SubString, AnnotatedString, AnnotatedChar, AnnotatedIOBuffer
using ..Base: eachregion, ActiveAnnotations, unannotate, raw_substring, annotations, annotatedstring, uniontypes
using ..Base: invoke_in_world, tls_world_age, inferencebarrier, escape_string, annotate!, _clear_annotations_in_region!, RegionAnnotation

public context, reduction, transition

const AnnotatedStr{S, V} = Union{AnnotatedString{S, V}, SubString{AnnotatedString{S, V}}}

## The interface, for each MIME type and value type

"""
    context(mime::MIME, T::Type) -> ctx

What displaying the annotations of value type `T` as `mime` needs to read once per display,
passed on to each `reduction`.
"""
context(::MIME, ::Type) = nothing

"""
    reduction(mime::MIME, T::Type[, ctx], values) -> state

The state of a region, from those of its annotation `values` of type `T`, the concrete type it
is called for, which it selects with `value isa T`. With none, this is the baseline, where
display starts and ends. A type without a method is not displayed, and one without a `context`
can leave out `ctx`.
"""
reduction(mime::MIME, ::Type{T}, _, values) where {T} = reduction(mime, T, values)
reduction(::MIME, ::Type, _) = nothing

"""
    transition(io::IO, mime::MIME, from, to)

Write to `io` the change from the state `from` to the state `to`. Each type of state belongs to
one value type, as `transition` dispatches on the states.

For `text/html`, a state's fields are its visible properties, a transition opens an element
within the current one, and a transition to `nothing` closes the element its state opened.
"""
transition(io::IO, ::MIME, ::Nothing, ::Nothing) = nothing

## Rendering

# A value type with a member that isn't concrete, as `Any`, has its types found from its values
@generated function contexts(mime::MIME, ::Type{U}) where {U}
    all(isconcretetype, uniontypes(U)) || return nothing
    calls = (:(context(mime, $T)) for T in uniontypes(U))
    :(tuple($(calls...)))
end
@generated function reductions(mime::MIME, ::Type{U}, ctxs, annotations) where {U}
    calls = (:(reduction(mime, $T, ctxs[$i], annotations)) for (i, T) in enumerate(uniontypes(U)))
    :($(Expr(:meta, :inline)); tuple($(calls...))) # Keeping each region's states off the heap
end

"""
    render(writetext, mime::MIME, io::IO, x::Union{AnnotatedStr, AnnotatedChar})

Write `x` to `io` as `mime`, with its text written by `writetext(io, str)`.
"""
function render(writetext::F, mime::MIME, io::IO, s::AnnotatedStr{S, V}) where {F, S, V}
    ctxs = contexts(mime, V)
    isnothing(ctxs) && return invoke_in_world(tls_world_age(), renderheld, writetext, mime, io, s)
    render(writetext, mime, io, s, annots -> (@inline; reductions(mime, V, ctxs, annots)))
end

# Compiled once for whatever types `s` holds, in the latest world. Its interface calls go through
# barriers, here and in `render`, so a new value type's methods invalidate nothing compiled for it.
# Taken in a union's order, the types display as that union would.
function renderheld(writetext, mime::MIME, io::IO, @nospecialize(s::AnnotatedStr))
    # As a `Union`, which is built without compiling anything for the types it holds
    types = uniontypes(Union{Type[typeof(value) for (; value) in annotations(s)]...})
    ctxs = Any[inferencebarrier(context)(mime, T) for T in types]
    render(writetext, mime, io, s, annots ->
        Any[inferencebarrier(reduction)(mime, T, ctx, annots) for (T, ctx) in zip(types, ctxs)])
end

render(writetext::F, mime::MIME, io::IO, c::AnnotatedChar) where {F} =
    render(writetext, mime, io, annotatedstring(c))

function render(writetext, mime::MIME, io::IO, s::AnnotatedStr, statesof)
    regions = eachregion(s)
    bases = statesof(ActiveAnnotations(regions, regions.pos)) # Before its first region, the iterator's annotations are none
    # Through a barrier for types found from the values, as in `renderheld`. Applied in the call, as
    # a closure capturing `inferencebarrier(transition)` is specialised on what it captures.
    transit = if bases isa Tuple transition else (args...) -> inferencebarrier(transition)(args...) end
    written = change(transit, io, mime, nothing, bases)
    for (str, annots) in regions
        written = change(transit, io, mime, written, statesof(annots))
        writetext(io, str)
    end
    change(transit, io, mime, written, bases)
end

## Plain text: each type's state changes on its own

change(transit, io::IO, ::MIME"text/plain", ::Nothing, bases) = bases
function change(transit, io::IO, mime::MIME"text/plain", previous, states)
    foreach((from, to) -> transit(io, mime, from, to), previous, states)
    states
end

## HTML: the types share one stack of nested elements
#
# Each frame holds the states within an open element, from the baselines in, and differs from
# the frame outside it only in the state of the element's type. An element still fits when each
# property, or field, of each state is the outer frame's or the new state's. A state without
# fields fits only as a whole.

change(transit, io::IO, ::MIME"text/html", ::Nothing, bases) = [bases]

# For a known union, `frames` holds tuples of states. For types found from the values, it holds
# vectors, and `fits` is dispatched for each type of state, so nothing compiles per combination.
function change(transit, io::IO, mime::MIME"text/html", frames, states)
    function fits(el::T, parent::T, to::T) where {T}
        fieldcount(T) == 0 && return el === parent || el === to
        all(ntuple(i -> getfield(el, i) === getfield(parent, i) || getfield(el, i) === getfield(to, i), Val(fieldcount(T))))
    end
    keep = 1
    while keep < length(frames) && all(map(fits, frames[keep + 1], frames[keep], states))
        keep += 1
    end
    while length(frames) > keep
        inner = pop!(frames)
        foreach(inner, last(frames)) do within, outside
            within === outside || transit(io, mime, within, nothing)
        end
    end
    # An unrolled `map` for a tuple, as a loop would box its states. In reverse, so a later type's
    # element (a link) encloses an earlier type's (a face).
    indices = if states isa Tuple ntuple(identity, length(states)) else eachindex(states) end
    map(reverse(indices), reverse(states)) do i, to
        @inline
        frame = last(frames)
        within = frame[i]
        if within !== to
            transit(io, mime, within, to)
            push!(frames, if frame isa Tuple Base.setindex(frame, to, i) else setindex!(copy(frame), to, i) end)
        end
        nothing
    end
    frames
end

## Writing, printing, escaping, and showing

# Through a buffer, so that the text and the codes between it reach `io` in one write
function writestyled(writetext::F, mime::MIME, io::IO, x) where {F}
    raw = first(Base.unwrapcontext(io)) # Qualified, as `show.jl` defines it after this file
    buf = if raw isa IOBuffer && !raw.append raw else IOBuffer() end
    offset = position(buf)
    render(writetext, mime, buf, x)
    bytes = position(buf) - offset
    buf === raw || write(io, seekstart(buf))
    bytes
end

isstyled(io::IO, x::Union{AnnotatedStr, AnnotatedChar}) =
    get(io, :color, false)::Bool && !isempty(if x isa SubString x.string.annotations else x.annotations end)

Base.write(io::IO, s::Union{AnnotatedString{S}, SubString{<:AnnotatedString{S}}}) where {S} =
    (if isstyled(io, s) writestyled(write, MIME"text/plain"(), io, s) else write(io, unannotate(s)) end)::Int
Base.write(io::IO, c::AnnotatedChar) =
    (if isstyled(io, c) writestyled(write, MIME"text/plain"(), io, c) else write(io, c.char) end)::Int

function Base.write(io::IO, aio::AnnotatedIOBuffer{V}) where {V}
    if get(io, :color, false) == true
        # This does introduce an overhead that technically
        # could be avoided, but I'm not sure that it's currently
        # worth the effort to implement an efficient version of
        # writing from an AnnotatedIOBuffer with style.
        # In the meantime, by converting to an `AnnotatedString` we can just
        # reuse all the work done to make that work.
        write(io, read(aio, AnnotatedString{String, V}))::Int
    else
        write(io, aio.io)
    end
end

# Via `write`, which keeps the annotations when the destination can hold them
Base.print(io::IO, x::Union{AnnotatedString, SubString{<:AnnotatedString}, AnnotatedChar}) = (write(io, x); nothing)

styled_print(io::AnnotatedIOBuffer, msg::Any, kwargs::Any) = print(io, msg...)

styled_print_(io::AnnotatedIOBuffer, @nospecialize(msg), @nospecialize(kwargs)) =
    invoke_in_world(tls_world_age(), styled_print, io, msg, kwargs)::Nothing

Base.printstyled(io::AnnotatedIOBuffer, msg...; kwargs...) =
    styled_print_(io, msg, kwargs)

function Base.escape_string(io::IO, s::AnnotatedStr, esc = ""; keep = (), ascii::Bool=false, fullhex::Bool=false)
    stop = if s isa SubString s.offset + s.ncodeunits else ncodeunits(s) end
    # Before a hex digit, a `\0` or non-ASCII character can escape longer, so escape the region with the digit, then drop it
    function escregion(io, text)
        str, after = text.string, text.offset + text.ncodeunits
        next = if after < stop Char(codeunit(str, after + 1)) else '\0' end
        if isxdigit(next) && (codeunit(str, after) == 0x00 || codeunit(str, after) >= 0x80)
            escaped = escape_string(raw_substring(str, text.offset + 1, text.ncodeunits + 1), esc; keep, ascii, fullhex)
            write(io, raw_substring(escaped, 1, ncodeunits(escaped) - 1 - (next in esc)))
        else
            escape_string(io, text, esc; keep, ascii, fullhex)
        end
        nothing
    end
    escape_annotated(first(Base.unwrapcontext(io)), io, s, escregion)
    nothing
end

function escape_annotated(aio::AnnotatedIOBuffer{V}, _, s, escregion) where {V}
    annots = convert(Vector{RegionAnnotation{V}}, annotations(s)) # As `write` does, before any text is written
    ends, outs = Int[0], Int[position(aio)] # Where each region ends in `s`, and where its escaped text ends in `aio`
    isappending = eof(aio)
    for (text, _) in eachregion(s)
        escregion(aio, text)
        push!(ends, last(ends) + ncodeunits(text))
        push!(outs, position(aio))
    end
    isappending || _clear_annotations_in_region!(aio.annotations, first(outs)+1:last(outs))
    # Where the escaped text of `region` in `s` lies in `aio`, clipped to `s` and widened to whole characters
    outrange(region) = UnitRange(outs[searchsortedlast(ends, clamp(first(region) - 1, 0, last(ends)))] + 1,
                                 outs[searchsortedfirst(ends, clamp(last(region), 0, last(ends)))])
    for (; region, value) in annots # A substring's annotations may reach beyond it
        annotate!(aio, outrange(region), value)
    end
end
escape_annotated(_, io, s, escregion) =
    if isstyled(io, s)
        writestyled(escregion, MIME"text/plain"(), io, s)
    else
        escregion(io, SubString(unannotate(s)))
    end

Base.show(io::IO, m::MIME"text/html", x::Union{AnnotatedString, SubString{<:AnnotatedString}, AnnotatedChar}) =
    (writestyled(escapehtml, m, io, x); nothing)

function escapehtml(io::IO, str::AbstractString)
    for c in str
        print(io, if c == '&' "&amp;" elseif c == '<' "&lt;" elseif c == '>' "&gt;" else c end)
    end
end

end
