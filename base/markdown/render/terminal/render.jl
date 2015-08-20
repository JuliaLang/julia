# This file is a part of Julia. License is MIT: http://julialang.org/license

include("formatting.jl")

const margin = 2
cols() = Base.tty_size()[2]

function term(io::IO, content::Vector, cols)
    isempty(content) && return
    for md in content[1:end-1]
        term(io, md, cols)
        println(io)
    end
    term(io, content[end], cols)
end

term(io::IO, md::MD, columns = cols()) = term(io, md.content, columns)

function term(io::IO, md::Paragraph, columns)
    print(io, " "^margin)
    print_wrapped(io, width = columns-2margin, pre = " "^margin) do io
        terminline(io, md.content)
    end
end

function term(io::IO, md::BlockQuote, columns)
    s = sprint(io->term(io, md.content, columns - 10))
    for line in split(rstrip(s), "\n")
        println(io, " "^margin, "|", line)
    end
    println(io)
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

function _term_header(io::IO, md, char, columns)
    text = terminline(md.text)
    with_output_format(:bold, io) do io
        print(io, " "^(2margin), " ")
        line_no, lastline_width = print_wrapped(io, text,
                                                width=columns - 4margin; pre=" ")
        line_width = min(1 + lastline_width, columns)
        if line_no > 1
            line_width = max(line_width, div(columns, 3))
        end
        char != ' ' && println(io, " "^(2margin), string(char) ^ line_width)
    end
end

const _header_underlines = collect("≡=–-⋅ ")
# TODO settle on another option with unicode e.g. "≡=≃–∼⋅" ?

function term{l}(io::IO, md::Header{l}, columns)
    underline = _header_underlines[l]
    _term_header(io, md, underline, columns)
end

function term(io::IO, md::Code, columns)
    with_output_format(:cyan, io) do io
        for line in lines(md.code)
            print(io, " "^margin)
            println(io, line)
        end
    end
end

function term(io::IO, br::LineBreak, columns)
   println(io)
end

function term(io::IO, br::HorizontalRule, columns)
   println(io, " " ^ margin, "-" ^ (columns - 2margin))
end

term(io::IO, x, _) = writemime(io, MIME"text/plain"(), x)

# Inline Content

terminline(md) = sprint(terminline, md)

function terminline(io::IO, content::Vector)
    for md in content
        terminline(io, md)
    end
end

function terminline(io::IO, md::AbstractString)
    print(io, md)
end

function terminline(io::IO, md::Bold)
    with_output_format(:bold, terminline, io, md.text)
end

function terminline(io::IO, md::Italic)
    with_output_format(:underline, terminline, io, md.text)
end

function terminline(io::IO, md::Image)
    print(io, "(Image: $(md.alt))")
end

function terminline(io::IO, md::Link)
    terminline(io, md.text)
end

function terminline(io::IO, code::Code)
    print_with_format(:cyan, io, code.code)
end

terminline(io::IO, x) = writemime(io, MIME"text/plain"(), x)

# Show in terminal

Base.display(d::Base.REPL.REPLDisplay, md::MD) = term(Base.REPL.outstream(d.repl), md)
