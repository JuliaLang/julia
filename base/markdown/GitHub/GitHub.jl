include("table.jl")

@breaking true ->
function fencedcode(stream::IO, block::MD, config::Config)
    withstream(stream) do
        startswith(stream, "~~~", padding = true) || startswith(stream, "```", padding = true) || return false
        skip(stream, -2)
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
                    push!(block, Code(flavor, takebuf_string(buffer) |> chomp))
                    return true
                else
                    seek(stream, line_start)
                end
            end
            write(buffer, readline(stream))
        end
        return false
    end
end

function github_paragraph(stream::IO, md::MD, config::Config)
    skipwhitespace(stream)
    buffer = IOBuffer()
    p = Paragraph()
    push!(md, p)
    while !eof(stream)
        char = read(stream, Char)
        if char == '\n'
            eof(stream) && break
            if blankline(stream) || parse(stream, md, config, breaking = true)
                break
            else
                write(buffer, '\n')
            end
        else
            write(buffer, char)
        end
    end
    p.content = parseinline(seek(buffer, 0), config)
    return true
end

@flavor github [list, indentcode, blockquote, fencedcode, hashheader,
                github_table, github_paragraph,

                linebreak, escapes, en_dash, inline_code, asterisk_bold,
                asterisk_italic, image, link]
