# This file is a part of Julia. License is MIT: https://julialang.org/license

const AnnotIO = Union{AnnotatedIOBuffer, IOContext{AnnotatedIOBuffer}}

function annotprint(f::Function, args...)
    buf = AnnotatedIOBuffer()
    f(buf, args...)
    read(seekstart(buf), AnnotatedString)
end

"""
    with_output_annotations(f::Function, io::AnnotIO, annots::Pair{Symbol, <:Any}...)

Call `f(io)`, and apply `annots` to the output created by doing so.
"""
function with_output_annotations(f::Function, io::AnnotIO, annots::Pair{Symbol, <:Any}...)
    @nospecialize annots
    aio = if io isa AnnotatedIOBuffer io else io.io end
    start = position(aio) + 1
    f(io)
    stop = position(aio)
    sortedindex = searchsortedlast(aio.annotations, (region=start:stop,), by=a -> a.region)
    for (i, annot) in enumerate(annots)
        insert!(aio.annotations, sortedindex + i, (start:stop, annot...))
    end
end

"""
    wraplines(content::AnnotatedString, width::Integer = 80, column::Integer = 0)

Wrap `content` into a vector of lines of at most `width` (according to
`textwidth`), with the first line starting at `column`.
"""
function wraplines(content::Union{Annot, SubString{<:Annot}}, width::Integer = 80, column::Integer = 0) where { Annot <: AnnotatedString}
    s, lines = String(content), SubString{Annot}[]
    i, lastwrap, slen = firstindex(s), 0, ncodeunits(s)
    most_recent_break_opportunity = 1
    while i < slen
        if isspace(s[i]) && s[i] != '\n'
            most_recent_break_opportunity = i
        elseif s[i] == '\n'
            push!(lines, content[nextind(s, lastwrap):prevind(s, i)])
            lastwrap = i
            column = 0
        elseif column >= width && most_recent_break_opportunity > 1
            if lastwrap == most_recent_break_opportunity
                nextbreak = findfirst(isspace, @view s[nextind(s, lastwrap):end])
                if isnothing(nextbreak)
                    break
                else
                    most_recent_break_opportunity = lastwrap + nextbreak
                end
                i = most_recent_break_opportunity
            else
                i = nextind(s, most_recent_break_opportunity)
            end
            push!(lines, content[nextind(s, lastwrap):prevind(s, most_recent_break_opportunity)])
            lastwrap = most_recent_break_opportunity
            column = 0
        end
        column += textwidth(s[i])
        i = nextind(s, i)
    end
    if lastwrap < slen
        push!(lines, content[nextind(s, lastwrap):end])
    end
    lines
end

# Print horizontal lines between each docstring if there are multiple docs
function insert_hlines(docs)
    if !isa(docs, MD) || !haskey(docs.meta, :results) || isempty(docs.meta[:results])
        return docs
    end
    docs = docs::MD
    v = Any[]
    for (n, doc) in enumerate(docs.content)
        push!(v, doc)
        n == length(docs.content) || push!(v, HorizontalRule())
    end
    return MD(v)
end
