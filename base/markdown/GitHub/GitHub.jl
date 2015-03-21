include("table.jl")

@breaking true ->
function fencedcode(stream::IO, block::MD, config::Config)
    start = position(stream)
    startswith(stream, "~~~", padding = true) || startswith(stream, "```", padding = true) || return false
    skip(stream, -2)
    ch = read(stream, Char)
    trailing = strip(readline(stream))
    flavor = lstrip(trailing, ch)
    n = 3 + length(trailing) - length(flavor)

    # inline code block
    if contains(flavor, string(ch) ^ n)
        seek(stream, start)
        return false
    end

    buffer = IOBuffer()
    while !eof(stream)
        if startswith(stream, string(ch) ^ n)
            push!(block, Code(flavor, takebuf_string(buffer) |> chomp))
            return true
        else
            write(buffer, readline(stream))
        end
    end
    seek(stream, start)
    return false
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
