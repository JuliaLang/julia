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

function html(io::IO, md::Table)
    withtag(io, :table) do
        for (i, row) in enumerate(md.rows)
            withtag(io, :tr) do
                for c in md.rows[i]
                    t = (i == 1) ? :th : :td
                    withtag(io, t) do
                        htmlinline(io, c)
                    end
                end
            end
        end
    end
end

function plain(io::IO, md::Table)
    col_widths = reduce(max, map(row -> int(map(x -> length(plaininline(x)), row)), md.rows))
    col_widths = max(col_widths, 3)

    for (n, row) in enumerate(md.rows)
        for (i, h) in enumerate(row)
            # TODO use not terminal version of print_align
            (i != 1) && print(io, "  | ")
            print(io, " " ^ (col_widths[i] - length(plaininline(h))))
            print(io, plaininline(h))
        end
        println(io)

        if n == 1
            for (j, w) in enumerate(col_widths)
                if j != 1
                    print(io, "  | ")
                end
                a = typeof(md.align) == Symbol ? md.align : md.align[j]
                print(io, _dash(w, a))
            end
            println(io)
        end
    end
end

function _dash(width, align)
    if align == :l
        return ":" * "-" ^ max(3, width - 1)
    elseif align == :r
        return "-" ^ max(3, width - 1) * ":"
    elseif align == :c
        return ":" * "-" ^ max(3, width - 2) * ":"
    else
        throw(ArgumentError("Unrecognized alignment $align"))
    end
end


function writemime(io::IO, ::MIME"text/latex", md::Table)
    wrapblock(io, "tabular") do
        if typeof(md.align) == Symbol
            align = string(md.align) ^ length(md.rows[1])
        else
            align = md.align
        end
        println(io, "{$(join(align, " | "))}")
        for (i, row) in enumerate(md.rows)
            for (j, cell) in enumerate(row)
                if j != 1
                    print(io, " & ")
                end
                latex_inline(io, cell)
            end
            println(io, " \\\\")
            if i == 1
                println("\\hline")
            end
        end
    end
end

function term(io::IO, md::Table, columns)
    col_widths = reduce(max, map(cell -> int(map(ansi_length, cell)),
                                 md.rows))
    col_widths = max(col_widths, 3)
    for (n, row) in enumerate(md.rows)
        for (i, h) in enumerate(row)
            (i != 1) && print(io, "  ")
            a = typeof(md.align) == Symbol ? md.align : md.align[i]
            print_align(io, h, col_widths[i], a)
        end
        println(io)

        if n == 1
            for (j, w) in enumerate(col_widths)
                (j != 1) && print(io, "  ")
                print(io, "-" ^ w)
            end
            println(io)
        end
    end
end
