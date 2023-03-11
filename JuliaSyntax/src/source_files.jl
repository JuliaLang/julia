"""
    SourceFile(code [; filename=nothing, first_line=1])

A UTF-8 source code string with associated file name and line number.

`SourceFile` stores the character positions of line starts to facilitate indexing.
"""
struct SourceFile
    # We use `code::String` for now but it could be some other UTF-8 based
    # string data structure with byte-based indexing.
    #
    # For example a rope data structure may be good for incremental editing
    # https://en.wikipedia.org/wiki/Rope_(data_structure)
    code::String
    filename::Union{Nothing,String}
    first_line::Int
    # String index of start of every line
    line_starts::Vector{Int}
end

function SourceFile(code::AbstractString; filename=nothing, first_line=1)
    line_starts = Int[1]
    for i in eachindex(code)
        # The line is considered to start after the `\n`
        # FIXME: \r and \n\r
        code[i] == '\n' && push!(line_starts, i+1)
    end
    if isempty(code) || last(code) != '\n'
        push!(line_starts, ncodeunits(code)+1)
    end
    SourceFile(code, filename, first_line, line_starts)
end

function SourceFile(; filename, kwargs...)
    SourceFile(read(filename, String); filename=filename, kwargs...)
end

# Get line number of the given byte within the code
function source_line_index(source::SourceFile, byte_index)
    lineidx = searchsortedlast(source.line_starts, byte_index)
    return (lineidx < lastindex(source.line_starts)) ? lineidx : lineidx-1
end
_source_line(source::SourceFile, lineidx) = lineidx + source.first_line - 1
source_line(source::SourceFile, byte_index) = _source_line(source, source_line_index(source, byte_index))

"""
Get line number and character within the line at the given byte index.
"""
function source_location(source::SourceFile, byte_index)
    lineidx = source_line_index(source, byte_index)
    i = source.line_starts[lineidx]
    column = 1
    while i < byte_index
        i = nextind(source.code, i)
        column += 1
    end
    _source_line(source, lineidx), column
end

"""
Get byte range of the source line at byte_index, buffered by
`context_lines_before` and `context_lines_after` before and after.
"""
function source_line_range(source::SourceFile, byte_index;
                           context_lines_before=0, context_lines_after=0)
    lineidx = source_line_index(source, byte_index)
    fbyte = source.line_starts[max(lineidx-context_lines_before, 1)]
    lbyte = source.line_starts[min(lineidx+1+context_lines_after, end)] - 1
    fbyte,lbyte
end

function source_location(::Type{LineNumberNode}, source::SourceFile, byte_index)
    LineNumberNode(source_line(source, byte_index),
                   isnothing(source.filename) ? nothing : Symbol(source.filename))
end

function Base.show(io::IO, ::MIME"text/plain", source::SourceFile)
    fn = isnothing(source.filename) ? "" : " $(source.filename)"
    header = "## SourceFile$fn ##"
    print(io, header, "\n")
    heightlim = displaysize(io)[1] ÷ 2
    if !get(io, :limit, false) || length(source.line_starts) <= heightlim
        print(io, source.code)
    else
        r1 = source_line_range(source, 1, context_lines_after=heightlim-3)
        print(io, view(source, r1[1]:r1[2]))
        println(io, "⋮")
    end
end

function Base.getindex(source::SourceFile, rng::AbstractUnitRange)
    i = first(rng)
    # Convert byte range into unicode String character range.
    # Assumes valid unicode! (SubString doesn't give us a reliable way to opt
    # out of the valid unicode check. The SubString{String} inner constructor
    # has some @boundscheck, but using @inbounds depends on inlining choices.)
    j = prevind(source.code, last(rng)+1)
    source.code[i:j]
end

# TODO: Change view() here to `sourcetext` ?
function Base.view(source::SourceFile, rng::AbstractUnitRange)
    i = first(rng)
    j = prevind(source.code, last(rng)+1)
    SubString(source.code, i, j)
end

function Base.getindex(source::SourceFile, i::Int)
    source.code[i]
end

Base.firstindex(source::SourceFile) = firstindex(source.code)
Base.lastindex(source::SourceFile) = lastindex(source.code)

"""
    sourcetext(source::SourceFile)

Get the full source text of a `SourceFile` as a string.
"""
function sourcetext(source::SourceFile)
    return source.code
end


#-------------------------------------------------------------------------------
# Tools for highlighting source ranges
function _print_marker_line(io, prefix_str, str, underline, singleline, color,
                            note, notecolor)
    # Whitespace equivalent in length to `prefix_str`
    # Getting exactly the same width of whitespace as `str` is tricky.
    # Especially for mixtures of tabs and spaces.
    # tabs are zero width according to textwidth
    indent = join(isspace(c) ? c : repeat(' ', textwidth(c)) for c in prefix_str)

    # Assume tabs are 4 wide rather than 0. (fixme: implement tab alignment?)
    w = textwidth(str) + 4*count(c->c=='\t', str)
    if !isempty(indent)
        indent = "#" * (first(indent) == '\t' ? indent : indent[nextind(indent,1):end])
    end

    midchar = '─'
    startstr, endstr, singlestart = underline ? ("└","┘","╙") : ("┌","┐","╓")

    markline =
    if singleline
        w == 0 ? string(indent, startstr)    :
        w == 1 ? string(indent, singlestart) :
                 string(indent, startstr, repeat('─', w-2), endstr)
    else
        if underline && isempty(indent) && w > 1
             string('#', repeat('─', w-2), endstr)
        else
            s,e = underline ? ("", endstr) : (startstr, "")
            w == 0 ? string(indent, s, e) :
                     string(indent, s, repeat('─', w-1), e)
        end
    end
    if note isa AbstractString
        markline *= " ── "
    end
    _printstyled(io, markline; fgcolor=color)
    if !isnothing(note)
        if note isa AbstractString
            _printstyled(io, note, fgcolor=notecolor)
        else
            note(io, indent, w)
        end
    end
end

"""
Print the lines of source code surrounding the given byte `range`, which is
highlighted with background `color` and markers in the text.
"""
function highlight(io::IO, source::SourceFile, range::UnitRange;
                   color=(120,70,70), context_lines_before=2,
                   context_lines_inner=1, context_lines_after=2,
                   note=nothing, notecolor=nothing)
    p = first(range)
    q = last(range)

    x,y = source_line_range(source, p;
                            context_lines_before=context_lines_before,
                            context_lines_after=context_lines_inner)
    a,b = source_line_range(source, p)
    c,d = source_line_range(source, q)
    z,w = source_line_range(source, q;
                            context_lines_before=context_lines_inner,
                            context_lines_after=context_lines_after)

    p_line = source_line(source, p)
    q_line   = source_line(source, q)

    marker_line_color = :light_black

    if p_line == q_line
        # x-----------------
        # a---p-------q----b
        # #   └───────┘ ── note
        # -----------------w

        hitext = source[p:q]
        print(io, source[x:p-1])
        _printstyled(io, hitext; bgcolor=color)
        print(io, source[q+1:d])
        source[d] == '\n' || print(io, "\n")
        _print_marker_line(io, source[a:p-1], hitext, true, true, marker_line_color, note, notecolor)
    else
        # x   --------------
        # #   ┌─────
        # a---p----b
        # --------------y
        # ---------------
        # z--------------
        # c   --------q----d
        # #───────────┘ ── note
        # -----------------w

        prefix1 = source[a:p-1]
        print(io, source[x:a-1])
        _print_marker_line(io, prefix1, source[p:b], false, false, marker_line_color, nothing, notecolor)
        print(io, '\n')
        print(io, prefix1)
        if q_line - p_line - 1 <= 2*context_lines_inner
            # The diagnostic range is compact and we show the whole thing
            _printstyled(io, source[p:q]; bgcolor=color)
        else
            # Or large and we trucate the code to show only the region around the
            # start and end of the error.
            _printstyled(io, source[p:y]; bgcolor=color)
            print(io, "⋮\n")
            _printstyled(io, source[z:q]; bgcolor=color)
        end
        print(io, source[q+1:d])
        source[d] == '\n' || print(io, "\n")
        qline = source[c:q]
        _print_marker_line(io, "", qline, true, false, marker_line_color, note, notecolor)
    end
    if context_lines_after > 0 && d+1 < lastindex(source)
        print(io, '\n')
        w1 = source[w] == '\n' ? w - 1 : w
        print(io, source[d+1:w1])
    end
end
