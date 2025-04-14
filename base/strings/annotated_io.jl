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
    srcannots = [setindex(annot, max(1 + srcpos, first(annot.region)):last(annot.region), :region)
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
    if (start = position(io)) == 0
        AnnotatedString(read(io.io, T), copy(io.annotations))
    else
        annots = [setindex(annot, UnitRange{Int}(max(1, first(annot.region) - start), last(annot.region)-start), :region)
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
    map!(ann -> setindex(ann, first(ann.region):min(size, last(ann.region)), :region),
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
                setindex(annot,
                         if first(region) < first(span)
                             first(region):first(span)-1
                         else
                             last(span)+1:last(region)
                         end,
                         :region)
            # If `span` fits exactly within `region`, then we've only copied over
            # the beginning overhang, but also need to conserve the end overhang.
            if first(region) < first(span) && last(span) < last(region)
                push!(extras, (i, setindex(annot, last(span)+1:last(region), :region)))
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
function _insert_annotations!(io::AnnotatedIOBuffer, annotations::Vector{RegionAnnotation}, offset::Int = position(io))
    run = 0
    if !isempty(io.annotations) && last(last(io.annotations).region) == offset
        for i in reverse(axes(annotations, 1))
            annot = annotations[i]
            first(annot.region) == 1 || continue
            i <= length(io.annotations) || continue
            if annot.label == last(io.annotations).label && annot.value == last(io.annotations).value
                valid_run = true
                for runlen in 1:i
                    new = annotations[begin+runlen-1]
                    old = io.annotations[end-i+runlen]
                    if last(old.region) != offset || first(new.region) != 1 || old.label != new.label || old.value != new.value
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
        old = io.annotations[old_index]
        new = annotations[begin+runindex]
        io.annotations[old_index] = setindex(old, first(old.region):last(new.region)+offset, :region)
    end
    for index in run+1:lastindex(annotations)
        annot = annotations[index]
        start, stop = first(annot.region), last(annot.region)
        push!(io.annotations, setindex(annotations[index], start+offset:stop+offset, :region))
    end
end
