# This file is a part of Julia. License is MIT: http://julialang.org/license

include("table.jl")

function github_paragraph(stream::IO, md::MD)
    skipwhitespace(stream)
    buffer = IOBuffer()
    p = Paragraph()
    push!(md, p)
    while !eof(stream)
        char = read(stream, Char)
        if char == '\n'
            eof(stream) && break
            if blankline(stream) || parse(stream, md, breaking = true)
                break
            else
                write(buffer, '\n')
            end
        else
            write(buffer, char)
        end
    end
    p.content = parseinline(seek(buffer, 0), md)
    return true
end

@flavor github [list, indentcode, blockquote, fencedcode, hashheader,
                github_table, github_paragraph,

                linebreak, escapes, inline_code, asterisk_bold,
                asterisk_italic, image, link]
