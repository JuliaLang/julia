# This file is a part of Julia. License is MIT: https://julialang.org/license

include("table.jl")

function github_paragraph(stream::IO, md::MD)
    skipwhitespace(stream)
    buffer = IOBuffer()
    p = Paragraph()
    push!(md, p)
    for char in readeach(stream, Char)
        if char == '\n'
            eof(stream) && break
            if blankline(stream) || _parse(stream, md, breaking = true)
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

@flavor github [fencedcode, horizontalrule, list, indentcode, blockquote, admonition, footnote, hashheader,
                html_block, html_block_type7, github_table, github_paragraph,

                linebreak, escapes,
                en_or_em_dash, inline_code,
                double_tilde_strikethrough, tilde_strikethrough,
                asterisk_bold, underscore_bold,
                asterisk_italic, underscore_italic,
                image, footnote_link, link, autolink]
