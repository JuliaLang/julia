# This file is a part of Julia. License is MIT: https://julialang.org/license

include("table.jl")

@breaking true ->
function fencedcode(stream::IO, block::MD)
    withstream(stream) do
        startswith(stream, "~~~", padding = true) || startswith(stream, "```", padding = true) || return false
        skip(stream, -1)
        ch = read(stream, Char)
        trailing = strip(readline(stream))
        flavor = lstrip(trailing, ch)
        n = 3 + length(trailing) - length(flavor)

        # inline code block
        ch in flavor && return false

        buffer = IOBuffer()
        while !eof(stream)
            line_start = position(stream)
            if startswith(stream, string(ch) ^ n)
                if !startswith(stream, string(ch))
                    if flavor == "math"
                        push!(block, LaTeX(String(take!(buffer)) |> chomp))
                    else
                        push!(block, Code(flavor, String(take!(buffer)) |> chomp))
                    end
                    return true
                else
                    seek(stream, line_start)
                end
            end
            write(buffer, readline(stream, keep=true))
        end
        return false
    end
end

function github_paragraph(stream::IO, md::MD)
    skipwhitespace(stream)
    buffer = IOBuffer()
    p = Paragraph()
    push!(md, p)
    for char in readeach(stream, Char)
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

@flavor github [list, indentcode, blockquote, admonition, footnote, fencedcode, hashheader,
                github_table, github_paragraph,

                linebreak, escapes, en_dash, inline_code, asterisk_bold,
                underscore_bold, asterisk_italic, underscore_italic, image, footnote_link, link, autolink]

