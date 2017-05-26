# This file is a part of Julia. License is MIT: https://julialang.org/license

include("formatting.jl")

const MARGIN = 2
cols(io) = displaysize(io)[2]

function term(io::IO, content::Vector, cols, parent = nothing)
    isempty(content) && return
    for md in content[1:end-1]
        term(io, md, cols, parent)
        println(io)
    end
    term(io, content[end], cols, parent)
end

term(io::IO, md::MD, columns = cols(io), parent = nothing) = term(io, md.content, columns, md)

function term(io::IO, md::Paragraph, columns, parent = nothing)
    print(io, " "^MARGIN)
    newline = !isa(parent, List) || parent.isloose
    print_wrapped(io, width = columns-2MARGIN, pre = " "^MARGIN, newline = newline) do io
        terminline(io, md.content)
    end
end

function term(io::IO, md::BlockQuote, columns, parent = nothing)
    s = sprint(term, md.content, columns - 10, md)
    for line in split(rstrip(s), "\n")
        println(io, " "^MARGIN, "|", line)
    end
end

function term(io::IO, md::Admonition, columns, parent = nothing)
    print(io, " "^MARGIN, "| ")
    with_output_format(:bold, print, io, isempty(md.title) ? md.category : md.title)
    println(io, "\n", " "^MARGIN, "|")
    s = sprint(term, md.content, columns - 10, md)
    for line in split(rstrip(s), "\n")
        println(io, " "^MARGIN, "|", line)
    end
end

function term(io::IO, f::Footnote, columns, parent = nothing)
    print(io, " "^MARGIN, "| ")
    with_output_format(:bold, print, io, "[^$(f.id)]")
    println(io, "\n", " "^MARGIN, "|")
    s = sprint(term, f.text, columns - 10, md)
    for line in split(rstrip(s), "\n")
        println(io, " "^MARGIN, "|", line)
    end
end

function term(io::IO, md::List, columns, parent = nothing)
    for (i, point) in enumerate(md.items)
        print(io, " "^2MARGIN, isordered(md) ? "$(i + md.ordered - 1). " : "•  ")
        print_wrapped(io, width = columns-(4MARGIN+2), pre = " "^(2MARGIN+2),
                          i = 2MARGIN+2) do io
            term(io, point, columns - 10, md)
        end
    end
end

function _term_header(io::IO, md, char, columns)
    text = terminline(md.text)
    with_output_format(:bold, io) do io
        print(io, " "^(2MARGIN), " ")
        line_no, lastline_width = print_wrapped(io, text,
                                                width=columns - 4MARGIN; pre=" ")
        line_width = min(1 + lastline_width, columns)
        if line_no > 1
            line_width = max(line_width, div(columns, 3))
        end
        char != ' ' && println(io, " "^(2MARGIN), string(char) ^ line_width)
    end
end

const _header_underlines = collect("≡=–-⋅ ")
# TODO settle on another option with unicode e.g. "≡=≃–∼⋅" ?

function term{l}(io::IO, md::Header{l}, columns, parent = nothing)
    underline = _header_underlines[l]
    _term_header(io, md, underline, columns)
end

function term(io::IO, md::Code, columns, parent = nothing)
    with_output_format(:cyan, io) do io
        for line in lines(md.code)
            print(io, " "^MARGIN)
            println(io, line)
        end
    end
end

function term(io::IO, br::LineBreak, columns, parent = nothing)
   println(io)
end

function term(io::IO, br::HorizontalRule, columns, parent = nothing)
   println(io, " " ^ MARGIN, "-" ^ (columns - 2MARGIN))
end

term(io::IO, x, _) = show(io, MIME"text/plain"(), x)

# Inline Content

terminline(md) = sprint(terminline, md)

function terminline(io::IO, content::Vector)
    for md in content
        terminline(io, md)
    end
end

function terminline(io::IO, md::AbstractString)
    print(io, replace(md, r"[\s\t\n]+", " "))
end

function terminline(io::IO, md::Bold)
    with_output_format(:bold, terminline, io, md.text)
end

function terminline(io::IO, md::Italic)
    with_output_format(:underline, terminline, io, md.text)
end

function terminline(io::IO, md::LineBreak)
    println(io)
end

function terminline(io::IO, md::Image)
    terminline(io, "(Image: $(md.alt))")
end

terminline(io::IO, f::Footnote) = with_output_format(:bold, terminline, io, "[^$(f.id)]")

function terminline(io::IO, md::Link)
    terminline(io, md.text)
end

function terminline(io::IO, code::Code)
    print_with_format(:cyan, io, code.code)
end

terminline(io::IO, x) = show(io, MIME"text/plain"(), x)

# Show in terminal

Base.display(d::Base.REPL.REPLDisplay, md::MD) = term(Base.REPL.outstream(d.repl), md)
