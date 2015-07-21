# This file is a part of Julia. License is MIT: http://julialang.org/license

type List
    items::Vector{Any}
    ordered::Bool

    List(x::AbstractVector, b::Bool) = new(x, b)
    List(x::AbstractVector) = new(x, false)
    List(b::Bool) = new(Any[], b)
end

List(xs...) = List(vcat(xs...))

const bullets = "*•+-"
const num_or_bullets = r"^(\*|•|\+|-|\d+(\.|\))) "

# Todo: ordered lists, inline formatting
function list(stream::IO, block::MD)
    withstream(stream) do
        eatindent(stream) || return false
        b = startswith(stream, num_or_bullets)
        (b == nothing || b == "") && return false
        ordered = !(b[1] in bullets)
        if ordered
            b = b[end - 1] == '.' ? r"^\d+\. " : r"^\d+\) "
            # TODO start value
        end
        the_list = List(ordered)

        buffer = IOBuffer()
        fresh_line = false
        while !eof(stream)
            if fresh_line
                sp = startswith(stream, r"^ {0,3}")
                if !(startswith(stream, b) in [false, ""])
                    push!(the_list.items, parseinline(takebuf_string(buffer), block))
                    buffer = IOBuffer()
                else
                    # TODO write a newline here, and deal with nested
                    write(buffer, ' ', sp)
                end
                fresh_line = false
            else
                c = read(stream, Char)
                if c == '\n'
                    eof(stream) && break
                    next = peek(stream)
                    if next == '\n'
                        break
                    else
                        fresh_line = true
                    end
                else
                    write(buffer, c)
                end
            end
        end
        push!(the_list.items, parseinline(takebuf_string(buffer), block))
        push!(block, the_list)
        return true
    end
end

function html(io::IO, md::List)
    withtag(io, md.ordered ? :ol : :ul) do
        for item in md.items
            println(io)
            withtag(io, :li) do
                htmlinline(io, item)
            end
        end
        println(io)
    end
end

function latex(io::IO, md::List)
    env = md.ordered ? "enumerate" : "itemize"
    wrapblock(io, env) do
        for item in md.items
            print(io, "\\item ")
            latexinline(io, item)
            println(io)
        end
    end
end

function plain(io::IO, list::List)
    for (i, item) in enumerate(list.items)
        print(io, list.ordered ? "$i. " : "  * ")
        plaininline(io, item)
        println(io)
    end
end

function term(io::IO, md::List, columns)
    for (i, point) in enumerate(md.items)
        print(io, " "^2margin, md.ordered ? "$i. " : "•  ")
        print_wrapped(io, width = columns-(4margin+2), pre = " "^(2margin+2),
                          i = 2margin+2) do io
            terminline(io, point)
        end
    end
end
