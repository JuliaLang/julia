# This file is a part of Julia. License is MIT: https://julialang.org/license

## AnnotatedIOBuffer

struct AnnotatedIOBuffer{V} <: AbstractPipe
    io::IOBuffer
    annotations::Vector{RegionAnnotation{V}}
end

AnnotatedIOBuffer{V}(io::IOBuffer) where {V} = AnnotatedIOBuffer(io, Vector{RegionAnnotation{V}}())
AnnotatedIOBuffer(io::IOBuffer) = AnnotatedIOBuffer{Any}(io)
AnnotatedIOBuffer{V}() where {V} = AnnotatedIOBuffer{V}(IOBuffer())
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

annotate!(io::AnnotatedIOBuffer, range::UnitRange{Int}, label::Symbol, @nospecialize(val::Any)) =
    (_annotate!(io.annotations, range, label, val); io)

function write(io::AnnotatedIOBuffer{V}, astr::Union{AnnotatedString{S}, SubString{<:AnnotatedString{S}}}) where {V, S}
    annots = convert(Vector{RegionAnnotation{V}}, annotations(astr))
    offset = position(io.io)
    eof(io) || _clear_annotations_in_region!(io.annotations, offset+1:offset+ncodeunits(astr))
    _insert_annotations!(io, annots)
    write(io.io, unannotate(astr))::Int
end

write(io::AnnotatedIOBuffer, c::AnnotatedChar) =
    write(io, AnnotatedString(string(c), [(region=1:ncodeunits(c), a...) for a in c.annotations]))
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
         label = annot.label,
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
    annots = Annotation{V}[Annotation{V}((annot.label, annot.value)) for annot in io.annotations if pos+1 in annot.region]
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
                annot.label == last(annots).label || continue
                annot.value === last(annots).value || continue
                all(1:i) do runlen
                    new = newannots[begin+runlen-1]
                    old = annots[end-i+runlen]
                    !(last(old.region) != offset ||
                    first(new.region) != 1 ||
                    old.label != new.label ||
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
        extannot = (region = first(old.region):last(new.region)+offset,
                    label = old.label,
                    value = old.value)
        annots[old_index] = extannot
    end
    for index in run+1:lastindex(newannots)
        annot = newannots[index]
        start, stop = first(annot.region), last(annot.region)
        # REVIEW: For some reason, construction of `newannot`
        # can be a significant contributor to the overall runtime
        # of this function. For instance, executing:
        #
        #     replace(AnnotatedIOBuffer(), S"apple",
        #             'e' => S"{red:x}", 'p' => S"{green:y}")
        #
        # results in 3 calls to `_insert_annotations!`. It takes
        # ~570ns in total, compared to ~200ns if we push `annot`
        # instead of `newannot`. Commenting out the `_insert_annotations!`
        # line reduces the runtime to ~170ns, from which we can infer
        # that constructing `newannot` is somehow responsible for
        # a ~30ns -> ~400ns (~13x) increase in runtime!!
        # This also comes with a marginal increase in allocations
        # (compared to the commented out version) of 2 -> 14 (250b -> 720b).
        #
        # This seems quite strange, but I haven't dug into the generated
        # LLVM or ASM code. If anybody reading this is interested in checking
        # this out, that would be brilliant 🙏.
        #
        # What I have done is found that "direct tuple reconstruction"
        # (as below) is several times faster than using `setindex`.
        newannot = (region = start+offset:stop+offset,
                    label = annot.label,
                    value = annot.value)
        push!(annots, newannot)
    end
end

_insert_annotations!(io::AnnotatedIOBuffer, newannots::Vector{<:RegionAnnotation}, offset::Int = position(io)) =
    _insert_annotations!(io.annotations, newannots, offset)

# String replacement

# REVIEW: For some reason the `Core.kwcall` indirection seems to cause a
# substantial slowdown here. If we remove `; count` from the signature
# and run the sample code above in `_insert_annotations!`, the runtime
# drops from ~4400ns to ~580ns (~7x faster). I cannot guess why this is.
function replace(out::AnnotatedIOBuffer{V}, str::AnnotatedString, pat_f::Pair...; count = typemax(Int)) where {V}
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
        annots = annotations(replacement)
        annots′ = if eltype(annots) <: Annotation # When it's a char not a string
            region = 1:newbytes
            [@NamedTuple{region::UnitRange{Int}, label::Symbol, value::V}((region, label, value))
             for (; label, value) in annots]
        else
            convert(Vector{RegionAnnotation{V}}, annots)
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
    for (; region, label, value) in str.annotations
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
            shiftregion = (start + priorrep.offset):(stop + priorrep.offset)
            shiftann = (region = shiftregion, label, value)
            push!(out.annotations, shiftann)
        else
            # Split between replacements
            prevrep = replacements[max(begin, prioridx - 1)]
            for rep in @view replacements[max(begin, prioridx - 1):min(end, postidx + 1)]
                gap = max(start, last(prevrep.region)+1):min(stop, first(rep.region)-1)
                if !isempty(gap)
                    shiftregion = (first(gap) + prevrep.offset):(last(gap) + prevrep.offset)
                    shiftann = (; region = shiftregion, label, value)
                    push!(out.annotations, shiftann)
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

How an annotated string, substring, or char is written and shown. The value type of its
annotations selects an [`AnnotationStyle`](@ref), and [`awrite`](@ref) renders it under
that style. Base provides `NoStyle`, which writes the plain text; a package whose values
carry display information (such as StyledStrings' `Face`) declares a style for its type
and implements `awrite` for it, and strings holding such values then render statically,
without a lookup at write time.

!!! warning "Experimental"
    This interface is experimental and may change or be removed in a future release
    without deprecation.
"""
module AnnotatedDisplay

using ..Base: IO, SubString, AnnotatedString, AnnotatedChar, AnnotatedIOBuffer
using ..Base: eachregion, unannotate, annotations, annotatedstring, annot_valtype, invoke_in_world, tls_world_age, Fix1
using ..Base: escape_string, annotate!, _clear_annotations_in_region!, RegionAnnotation

public AbstractAnnotationStyle, AnnotationStyle, NoStyle, awrite

# Annotation styles

"""
    AbstractAnnotationStyle

The supertype of the singletons that [`AnnotationStyle`](@ref) selects between.
"""
abstract type AbstractAnnotationStyle end

struct NoStyle <: AbstractAnnotationStyle end

# These are plain functions rather than constructors so that `max_methods` applies to them
# (a constructor shares `DataType`'s limit). When the value type is not a compile-time
# constant, a call sees at least two applicable methods (`NoStyle` and `DynamicStyle`), so
# inference leaves it dynamic and records no method-table edge. A package's style or writer
# definitions therefore cannot invalidate Base's compiled callers.
"""
    AnnotationStyle(::Type{V}) -> AbstractAnnotationStyle

Trait selecting how annotations whose values have type `V` are displayed.

A type that carries display information (such as StyledStrings' `Face`) returns its own
`AbstractAnnotationStyle` singleton, for which [`awrite`](@ref) methods are defined.
A type that is mere metadata returns `NoStyle()`, the default. A `Union` value type reduces
over its members with `AnnotationStyle(a, b)`, so annotations of several types are displayed
by the member with a style. Two members whose styles differ have no display style in
common, and raise an `ArgumentError` until one of their packages defines an
`AnnotationStyle(a, b)` method that settles it, in either order, as a `promote_rule`
settles a promotion; a method may also return `nothing` to leave the pair unsettled. Where
both orders have a rule, the one in the order the styles are combined in is used.
`Any` is `DynamicStyle()`, which finds the style from the values held.
"""
function AnnotationStyle end
typeof(AnnotationStyle).name.max_methods = 0x1 # `Base.Experimental.@max_methods 1`, before it exists

AnnotationStyle(::Type) = NoStyle()
# As `promote_type` reads `promote_rule`, `promotestyle` reads `AnnotationStyle` rules in either
# orientation; the fallback's `nothing` means no rule, so no reflection is needed and it stays foldable
AnnotationStyle(::AbstractAnnotationStyle, ::AbstractAnnotationStyle) = nothing

# The identities belong to the combination, as `promote_type`'s do, so no rule can clash with them
promotestyle(a::S, ::S) where {S <: AbstractAnnotationStyle} = a
promotestyle(a::AbstractAnnotationStyle, ::NoStyle) = a
promotestyle(::NoStyle, b::AbstractAnnotationStyle) = b
promotestyle(::NoStyle, ::NoStyle) = NoStyle()
function promotestyle(a::AbstractAnnotationStyle, b::AbstractAnnotationStyle)
    ab = AnnotationStyle(a, b)
    isnothing(ab) || return ab
    ba = AnnotationStyle(b, a)
    isnothing(ba) || return ba
    throw(ArgumentError(LazyString("annotation styles ", a, " and ", b, " have no style in common: define AnnotationStyle(::",
                                   typeof(a), ", ::", typeof(b), ") to settle it")))
end

Base.@assume_effects :foldable AnnotationStyle(U::Union) =
    promotestyle(AnnotationStyle(U.a), AnnotationStyle(U.b))

style(x) = AnnotationStyle(annot_valtype(x))

# Write

"""
    awrite(textwriter, style::AbstractAnnotationStyle, io::IO, x)
    awrite(style::AbstractAnnotationStyle, io::IO, mime::MIME, x)

Write `x`, an annotated string, substring, or char, to `io` under `style`, with each run of
text written by `textwriter(io, text)`, and return the number of bytes written; or, with a
`mime`, show `x` in that format.

A package implements the first for its [`AnnotationStyle`](@ref), for annotated strings and
substrings (a char is written as a one-character string unless a method is added for it),
and may implement the second. Taking the text writer as an argument lets a transformation
of the text, such as escaping, keep its styling. `NoStyle` writes the plain text and has no
`mime` form.
"""
function awrite end
typeof(awrite).name.max_methods = 0x1 # As for `AnnotationStyle`

awrite(style::AbstractAnnotationStyle, io::IO, x) = awrite(write, style, io, x)

const AnnotatedStr = Union{AnnotatedString, SubString{<:AnnotatedString}}

awrite(textwriter::F, style::AbstractAnnotationStyle, io::IO, c::AnnotatedChar) where {F} =
    awrite(textwriter, style, io, annotatedstring(c))

awrite(textwriter::F, ::NoStyle, io::IO, s::AnnotatedStr) where {F} = textwriter(io, unannotate(s))
awrite(textwriter::F, ::NoStyle, io::IO, c::AnnotatedChar) where {F} = textwriter(io, c.char)

# Thrown here rather than by dispatch, so that a non-constant style still sees two methods
awrite(::NoStyle, io::IO, m::MIME, x) = throw(MethodError(show, (io, m, x)))

# Via the writer form, so that a non-constant style sees two methods there as well
awrite(io::IO, x) = awrite(write, style(x), io, x)

Base.write(io::IO, s::Union{AnnotatedString{S}, SubString{<:AnnotatedString{S}}}) where {S} =
    awrite(io, s)::Int
Base.write(io::IO, c::AnnotatedChar) =
    awrite(io, c)::Int

function Base.write(io::IO, aio::AnnotatedIOBuffer{V}) where {V}
    if get(io, :color, false) == true
        # This does introduce an overhead that technically
        # could be avoided, but I'm not sure that it's currently
        # worth the effort to implement an efficient version of
        # writing from an AnnotatedIOBuffer with style.
        # In the meantime, by converting to an `AnnotatedString` we can just
        # reuse all the work done to make that work.
        awrite(io, read(aio, AnnotatedString{String, V}))::Int
    else
        write(io, aio.io)
    end
end

# Print

# Via `write`, which keeps the annotations when the destination can hold them
Base.print(io::IO, s::Union{<:AnnotatedString, SubString{<:AnnotatedString}}) =
    (write(io, s); nothing)
Base.print(io::IO, s::AnnotatedChar) =
    (write(io, s); nothing)

styled_print(io::AnnotatedIOBuffer, msg::Any, kwargs::Any) = print(io, msg...)

styled_print_(io::AnnotatedIOBuffer, @nospecialize(msg), @nospecialize(kwargs)) =
    invoke_in_world(tls_world_age(), styled_print, io, msg, kwargs)::Nothing

Base.printstyled(io::AnnotatedIOBuffer, msg...; kwargs...) =
    styled_print_(io, msg, kwargs)

# Escape

function Base.escape_string(io::IO, s::AnnotatedStr, esc = ""; keep = (), ascii::Bool=false, fullhex::Bool=false)
    aio = first(Base.unwrapcontext(io)) # Qualified, as `show.jl` defines it after this file
    escape_annotated(aio, io, s, esc, keep, ascii, fullhex)
    nothing
end

# Into an annotated buffer, the escaped text keeps the annotations
function escape_annotated(aio::AnnotatedIOBuffer{V}, _, s, esc, keep, ascii, fullhex) where {V}
    annots = convert(Vector{RegionAnnotation{V}}, annotations(s)) # As `write` does, before any text is written
    ends, outs = Int[0], Int[position(aio)] # Where each region ends in `s`, and where its escaped text ends in `aio`
    for (text, _) in eachregion(s)
        escape_string(aio, text, esc; keep, ascii, fullhex)
        push!(ends, last(ends) + ncodeunits(text))
        push!(outs, position(aio))
    end
    eof(aio) || _clear_annotations_in_region!(aio.annotations, first(outs)+1:last(outs))
    outpos(i) = outs[searchsortedlast(ends, clamp(i, 0, last(ends)))] # Where byte `i` of `s` ends in `aio`, clipped to `s`
    for (; region, label, value) in annots # A substring's annotations may reach beyond it
        annotate!(aio, outpos(first(region) - 1) + 1:outpos(last(region)), label, value)
    end
end
escape_annotated(_, io, s, esc, keep, ascii, fullhex) =
    awrite(style(s), io, s) do io, str
        escape_string(io, str, esc; keep, ascii, fullhex)
    end

# Show

Base.show(io::IO, m::MIME"text/html", s::Union{<:AnnotatedString, SubString{<:AnnotatedString}}) =
    (awrite(style(s), io, m, s); nothing)
Base.show(io::IO, m::MIME"text/html", c::AnnotatedChar) = show(io, m, annotatedstring(c))

function Base.showable(m::MIME"text/html", x::Union{AnnotatedStr, AnnotatedChar})
    s = style(x)
    if s === DynamicStyle()
        s = invoke_in_world(tls_world_age(), valuestyle, x)
    end
    written = if x isa AnnotatedChar AnnotatedString{String, annot_valtype(x)} else typeof(x) end
    s !== NoStyle() && hasmethod(awrite, Tuple{typeof(s), IO, typeof(m), written})
end

# Dynamic styles

# An `Any` value type says nothing about display, so `DynamicStyle` works the style out from
# the values held. It does this in the latest world, so that new style methods do not
# invalidate compiled callers of `Any`-valued strings.
struct DynamicStyle <: AbstractAnnotationStyle end
AnnotationStyle(::Type{Any}) = DynamicStyle()

valuestyle(x) = mapfoldl(a -> AnnotationStyle(typeof(a.value)), promotestyle, annotations(x), init = NoStyle())

dynamic(g, @nospecialize(x), args...) = g(valuestyle(x), args...)

awrite(textwriter::F, ::DynamicStyle, io::IO, @nospecialize(s::AnnotatedStr)) where {F} =
    if isempty(annotations(if s isa SubString s.string else s end)) # Plain text, with no style to find
        textwriter(io, unannotate(s))
    else
        invoke_in_world(tls_world_age(), dynamic, Fix1(awrite, textwriter), s, io, s)
    end
awrite(textwriter::F, ::DynamicStyle, io::IO, @nospecialize(c::AnnotatedChar)) where {F} =
    invoke_in_world(tls_world_age(), dynamic, Fix1(awrite, textwriter), c, io, c)
awrite(::DynamicStyle, io::IO, m::MIME, @nospecialize(x)) =
    invoke_in_world(tls_world_age(), dynamic, awrite, x, io, m, x)

end
