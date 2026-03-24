# This file is a part of Julia. License is MIT: https://julialang.org/license

## AnnotatedIOBuffer

struct AnnotatedIOBuffer <: AbstractPipe
    io::IOBuffer
    annotations::Vector{RegionAnnotation}
end

AnnotatedIOBuffer(io::IOBuffer) = AnnotatedIOBuffer(io, Vector{RegionAnnotation}())
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

function write(io::AnnotatedIOBuffer, astr::Union{AnnotatedString, SubString{<:AnnotatedString}})
    astr = AnnotatedString(astr)
    offset = position(io.io)
    eof(io) || _clear_annotations_in_region!(io.annotations, offset+1:offset+ncodeunits(astr))
    _insert_annotations!(io, astr.annotations)
    write(io.io, String(astr))
end

write(io::AnnotatedIOBuffer, c::AnnotatedChar) =
    write(io, AnnotatedString(string(c), [(region=1:ncodeunits(c), a...) for a in c.annotations]))
write(io::AnnotatedIOBuffer, x::AbstractString) = write(io.io, x)
write(io::AnnotatedIOBuffer, s::Union{SubString{String}, String}) = write(io.io, s)
write(io::AnnotatedIOBuffer, b::UInt8) = write(io.io, b)

function write(dest::AnnotatedIOBuffer, src::AnnotatedIOBuffer)
    destpos = position(dest)
    isappending = eof(dest)
    srcpos = position(src)
    nb = write(dest.io, src.io)
    isappending || _clear_annotations_in_region!(dest.annotations, destpos:destpos+nb)
    srcannots = [@inline(setindex(annot, max(1 + srcpos, first(annot.region)):last(annot.region), :region))
                 for annot in src.annotations if first(annot.region) >= srcpos]
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

function read(io::AnnotatedIOBuffer, ::Type{AnnotatedString{T}}) where {T <: AbstractString}
    start = position(io)
    if start == 0
        AnnotatedString(read(io.io, T), copy(io.annotations))
    else
        annots = [@inline(setindex(annot, UnitRange{Int}(max(1, first(annot.region) - start), last(annot.region)-start), :region))
                  for annot in io.annotations if last(annot.region) > start]
        AnnotatedString(read(io.io, T), annots)
    end
end
read(io::AnnotatedIOBuffer, ::Type{AnnotatedString{AbstractString}}) = read(io, AnnotatedString{String})
read(io::AnnotatedIOBuffer, ::Type{AnnotatedString}) = read(io, AnnotatedString{String})

function read(io::AnnotatedIOBuffer, ::Type{AnnotatedChar{T}}) where {T <: AbstractChar}
    pos = position(io)
    char = read(io.io, T)
    annots = [NamedTuple{(:label, :value)}(annot) for annot in io.annotations if pos+1 in annot.region]
    AnnotatedChar(char, annots)
end
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
function _clear_annotations_in_region!(annotations::Vector{RegionAnnotation}, span::UnitRange{Int})
    # Clear out any overlapping pre-existing annotations.
    filter!(ann -> first(ann.region) < first(span) || last(ann.region) > last(span), annotations)
    extras = Tuple{Int, RegionAnnotation}[]
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
function _insert_annotations!(annots::Vector{RegionAnnotation}, newannots::Vector{RegionAnnotation}, offset::Int = 0)
    run = @label search begin
        if !isempty(annots) && last(last(annots).region) == offset
            for i in reverse(axes(newannots, 1))
                annot = newannots[i]
                first(annot.region) == 1 || continue
                i <= length(annots) || continue
                annot.label == last(annots).label || continue
                annot.value == last(annots).value || continue
                all(1:i) do runlen
                    new = newannots[begin+runlen-1]
                    old = annots[end-i+runlen]
                    !(last(old.region) != offset ||
                    first(new.region) != 1 ||
                    old.label != new.label ||
                    old.value != new.value)
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

_insert_annotations!(io::AnnotatedIOBuffer, newannots::Vector{RegionAnnotation}, offset::Int = position(io)) =
    _insert_annotations!(io.annotations, newannots, offset)

# String replacement

# REVIEW: For some reason the `Core.kwcall` indirection seems to cause a
# substantial slowdown here. If we remove `; count` from the signature
# and run the sample code above in `_insert_annotations!`, the runtime
# drops from ~4400ns to ~580ns (~7x faster). I cannot guess why this is.
function replace(out::AnnotatedIOBuffer, str::AnnotatedString, pat_f::Pair...; count = typemax(Int))
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
        annots′ = if eltype(annots) == Annotation # When it's a char not a string
            region = 1:newbytes
            [@NamedTuple{region::UnitRange{Int}, label::Symbol, value}((region, label, value))
             for (; label, value) in annots]
        else
            annots
        end::Vector{RegionAnnotation}
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
            # Region contained with a replacement
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
    isempty(pat_f) || iszero(count) && return str
    out = AnnotatedIOBuffer()
    replace(out, str, pat_f...; count)
    read(seekstart(out), AnnotatedString)
end

# Printing

function printstyled end

# NOTE: This is an interim solution to the invalidations caused
# by the split styled display implementation. This should be
# replaced by a more robust solution (such as a consolidation of
# the type and method definitions) in the near future.
module AnnotatedDisplay

using ..Base: IO, SubString, AnnotatedString, AnnotatedChar, AnnotatedIOBuffer
using ..Base: eachregion, invoke_in_world, tls_world_age

# Write

ansi_write(f::Function, io::IO, x::Any) = f(io, String(x))

ansi_write_(f::Function, io::IO, @nospecialize(x::Any)) =
    invoke_in_world(tls_world_age(), ansi_write, f, io, x)

Base.write(io::IO, s::Union{<:AnnotatedString, SubString{<:AnnotatedString}}) =
    ansi_write_(write, io, s)::Int

Base.write(io::IO, c::AnnotatedChar) =
    ansi_write_(write, io, c)::Int

function Base.write(io::IO, aio::AnnotatedIOBuffer)
    if get(io, :color, false) == true
        # This does introduce an overhead that technically
        # could be avoided, but I'm not sure that it's currently
        # worth the effort to implement an efficient version of
        # writing from an AnnotatedIOBuffer with style.
        # In the meantime, by converting to an `AnnotatedString` we can just
        # reuse all the work done to make that work.
        ansi_write_(write, io, read(aio, AnnotatedString))::Int
    else
        write(io, aio.io)
    end
end

# Print

Base.print(io::IO, s::Union{<:AnnotatedString, SubString{<:AnnotatedString}}) =
    (ansi_write_(write, io, s); nothing)

Base.print(io::IO, s::AnnotatedChar) =
    (ansi_write_(write, io, s); nothing)

Base.print(io::AnnotatedIOBuffer, s::Union{<:AnnotatedString, SubString{<:AnnotatedString}}) =
    (write(io, s); nothing)

Base.print(io::AnnotatedIOBuffer, c::AnnotatedChar) =
    (write(io, c); nothing)

styled_print(io::AnnotatedIOBuffer, msg::Any, kwargs::Any) = print(io, msg...)

styled_print_(io::AnnotatedIOBuffer, @nospecialize(msg), @nospecialize(kwargs)) =
    invoke_in_world(tls_world_age(), styled_print, io, msg, kwargs)::Nothing

Base.printstyled(io::AnnotatedIOBuffer, msg...; kwargs...) =
    styled_print_(io, msg, kwargs)

# Escape

Base.escape_string(io::IO, s::Union{<:AnnotatedString, SubString{<:AnnotatedString}},
              esc = ""; keep = (), ascii::Bool=false, fullhex::Bool=false) =
    (ansi_write_((io, s) -> escape_string(io, s, esc; keep, ascii, fullhex), io, s); nothing)

# Show

show_annot(io::IO, ::Any) = nothing
show_annot(io::IO, ::MIME, ::Any) = nothing

show_annot_(io::IO, @nospecialize(x::Any)) =
    invoke_in_world(tls_world_age(), show_annot, io, x)::Nothing

show_annot_(io::IO, m::MIME, @nospecialize(x::Any)) =
    invoke_in_world(tls_world_age(), show_annot, io, m, x)::Nothing

Base.show(io::IO, m::MIME"text/html", s::Union{<:AnnotatedString, SubString{<:AnnotatedString}}) =
    show_annot_(io, m, s)

Base.show(io::IO, m::MIME"text/html", c::AnnotatedChar) =
    show_annot_(io, m, c)

end
