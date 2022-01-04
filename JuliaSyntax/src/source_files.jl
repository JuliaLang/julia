"""
    SourceFile(code [, filename])

A UTF-8 source code string with associated file name and indexing structures.
"""
struct SourceFile
    # We use `code::String` for now but it could be some other UTF-8 based
    # string data structure with byte-based indexing.
    #
    # For example a rope data structure may be good for incremental editing
    # https://en.wikipedia.org/wiki/Rope_(data_structure)
    code::String
    filename::Union{Nothing,String}
    # String index of start of every line
    line_starts::Vector{Int}
end

function SourceFile(code::AbstractString; filename=nothing)
    line_starts = Int[1]
    for i in eachindex(code)
        # The line is considered to start after the `\n`
        code[i] == '\n' && push!(line_starts, i+1)
    end
    SourceFile(code, filename, line_starts)
end

# Get line number of the given byte within the code
function source_line(source::SourceFile, byte_index)
    searchsortedlast(source.line_starts, byte_index)
end

"""
Get line number and character within the line at the given byte index.
"""
function source_location(source::SourceFile, byte_index)
    line = searchsortedlast(source.line_starts, byte_index)
    i = source.line_starts[line]
    column = 1
    while i < byte_index
        i = nextind(source.code, i)
        column += 1
    end
    line, column
end

"""
Get byte range of the source line at byte_index, buffered by
`context_lines_before` and `context_lines_after` before and after.
"""
function source_line_range(source::SourceFile, byte_index;
                           context_lines_before=0, context_lines_after=0)
    line = searchsortedlast(source.line_starts, byte_index)
    fbyte = source.line_starts[max(line-context_lines_before, 1)]
    lbyte = source.line_starts[min(line+1+context_lines_after, end)] - 1
    fbyte,lbyte
end

function source_location(::Type{LineNumberNode}, source::SourceFile, byte_index)
    LineNumberNode(source_line(source, byte_index), source.filename)
end

function Base.show(io::IO, ::MIME"text/plain", source::SourceFile)
    if !isnothing(source.filename)
        print(io, source.filename, '\n',
              repeat('-', textwidth(source.filename)), '\n')
    end
    print(io, source.code)
end

function Base.getindex(source::SourceFile, rng::AbstractRange)
    i = first(rng)
    # Convert byte range into unicode String character range.
    # Assumes valid unicode! (SubString doesn't give us a reliable way to opt
    # out of the valid unicode check. The SubString{String} inner constructor
    # has some @boundscheck, but using @inbounds depends on inlining choices.)
    j = prevind(source.code, last(rng)+1)
    @view source.code[i:j]
end

function Base.getindex(source::SourceFile, i::Int)
    source.code[i]
end

