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
    filename::String
    # Probably want to maintain a map of byte_offset -> (line, column) here
    # somewhere as well.
end

function SourceFile(code::AbstractString)
    SourceFile(code, "unknown.jl")
end

function Base.show(io::IO, ::MIME"text/plain", source::SourceFile)
    header = "Source: $(source.filename)"
    print(io, header, '\n',
          repeat('-', textwidth(header)), '\n',
          source.code)
end

function Base.getindex(source::SourceFile, rng::AbstractRange)
    @view source.code[rng]
end

