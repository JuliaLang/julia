# This file is a part of Julia. License is MIT: http://julialang.org/license

type Paragraph
    content
end

Paragraph() = Paragraph([])

function paragraph(stream::IO, md::MD)
    buffer = IOBuffer()
    p = Paragraph()
    push!(md, p)
    skipwhitespace(stream)
    while !eof(stream)
        char = read(stream, Char)
        if char == '\n' || char == '\r'
            if blankline(stream) || parse(stream, md, breaking = true)
                break
            else
                write(buffer, ' ')
            end
        else
            write(buffer, char)
        end
    end
    p.content = parseinline(seek(buffer, 0), md)
    return true
end

function html(io::IO, md::Paragraph)
    withtag(io, :p) do
        htmlinline(io, md.content)
    end
end

function latex(io::IO, md::Paragraph)
    for md in md.content
        latexinline(io, md)
    end
    println(io)
end

function plain(io::IO, p::Paragraph)
    plaininline(io, p.content)
    println(io)
end

function term(io::IO, md::Paragraph, columns)
    print(io, " "^margin)
    print_wrapped(io, width = columns-2margin, pre = " "^margin) do io
        terminline(io, md.content)
    end
end
