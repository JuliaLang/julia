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
function line_number(source::SourceFile, byte_index)
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

function Base.show(io::IO, ::MIME"text/plain", source::SourceFile)
    if !isnothing(source.filename)
        print(io, source.filename, '\n',
              repeat('-', textwidth(source.filename)), '\n')
    end
    print(io, source.code)
end

function Base.getindex(source::SourceFile, rng::AbstractRange)
    @view source.code[rng]
end

function Base.getindex(source::SourceFile, i::Int)
    source.code[i]
end

