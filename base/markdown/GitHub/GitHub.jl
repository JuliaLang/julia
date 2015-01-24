@breaking true ->
function fencedcode(stream::IO, block::MD, config::Config)
    startswith(stream, "```", padding = true) || return false
    readline(stream)
    buffer = IOBuffer()
    while !eof(stream)
        startswith(stream, "```") && break
        write(buffer, readline(stream))
    end
    push!(block, Code(takebuf_string(buffer) |> chomp))
    return true
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

type Table
    rows::Vector{Vector{Any}}
    align
end

function github_table(stream::IO, md::MD, config::Config)
    withstream(stream) do
        rows = Any[]
        n = 0
        align = :r # default is to align right
        while !eof(stream)
            n += 1
            pos = position(stream)
            skipwhitespace(stream)
            line = readline(stream) |> chomp

            if n == 1
                pipe_border = line[1] == '|'
                if !('|' in line)
                    return false
                end
            end

            row = map(strip, split(line, "|"))
            if pipe_border
                if row[1] == row[end] == ""
                    row = row[2:end-1]
                else
                    return false
                end
            end

            if n == 2 && all(['-' in r && issubset(Set(r), Set(" -:"))
                             for r in row])
                # handle possible --- line
                align = Symbol[]
                for r in row
                    if r[1] == ':'
                        if r[end] == ':'
                            push!(align, :c)
                        else
                            push!(align, :l)
                        end
                    else
                        if r[end] == ':'
                            push!(align, :r)
                        else
                            # default is align right
                            push!(align, :r)
                        end
                    end
                end

            elseif n == 1 || length(rows[1]) == length(row)
                push!(rows, map(x -> parseinline(x, config), row))
            elseif length(row) > 1
                seek(stream, pos)
                break
            else
                return false
            end
        end
        if length(rows) < 2
            return false
        end
        push!(md, Table(rows, align))
        return true
    end
end


@flavor github [list, indentcode, blockquote, fencedcode, hashheader,
                github_table, github_paragraph,
                linebreak, espaces, en_dash, inline_code, asterisk_bold,
                asterisk_italic, image, link]

