# This file is a part of Julia. License is MIT: https://julialang.org/license

include("formatting.jl")

cols(io) = displaysize(io)[2]

function term(io::IO, content::Vector, cols)
    isempty(content) && return
    for md in content[1:end-1]
        term(io, md, cols)
        print(io, '\n', '\n')
    end
    term(io, content[end], cols)
end

term(io::IO, md::MD, columns = cols(io)) = term(io, md.content, columns)

function term(io::IO, md::Paragraph, columns)
    print(io, ' '^margin)
    print_wrapped(io, width = columns-2margin, pre = ' '^margin) do io
        terminline(io, md.content)
    end
end

function term(io::IO, md::BlockQuote, columns)
    s = sprint(term, md.content, columns - 10; context=io)
    lines = split(rstrip(s), '\n')
    print(io, ' '^margin, '│', lines[1])
    for i = 2:length(lines)
        print(io, '\n', ' '^margin, '│', lines[i])
    end
end

function term(io::IO, md::Admonition, columns)
    col = :default
    # If the types below are modified, the page manual/documentation.md must be updated accordingly.
    if md.category == "danger"
        col = Base.error_color()
    elseif md.category == "warning"
        col = Base.warn_color()
    elseif md.category in ("info", "note")
        col = Base.info_color()
    elseif md.category == "tip"
        col = :green
    end
    printstyled(io, ' '^margin, "│ "; color=col, bold=true)
    printstyled(io, isempty(md.title) ? md.category : md.title; color=col, bold=true)
    printstyled(io, '\n', ' '^margin, '│', '\n'; color=col, bold=true)
    s = sprint(term, md.content, columns - 10; context=io)
    lines = split(rstrip(s), '\n')
    for i in eachindex(lines)
        printstyled(io, ' '^margin, '│'; color=col, bold=true)
        print(io, lines[i])
        i < lastindex(lines) && println(io)
    end
end

function term(io::IO, f::Footnote, columns)
    print(io, ' '^margin, "│ ")
    printstyled(io, "[^$(f.id)]", bold=true)
    println(io, '\n', ' '^margin, '│')
    s = sprint(term, f.text, columns - 10; context=io)
    lines = split(rstrip(s), '\n')
    for i in eachindex(lines)
        print(io, ' '^margin, '│', lines[i])
        i < lastindex(lines) && println(io)
    end
end

function term(io::IO, md::List, columns)
    for (i, point) in enumerate(md.items)
        print(io, ' '^2margin, isordered(md) ? "$(i + md.ordered - 1). " : "•  ")
        print_wrapped(io, width = columns-(4margin+2), pre = ' '^(2margin+2),
                          i = 2margin+2) do io
            term(io, point, columns - 10)
        end
        i < lastindex(md.items) && print(io, '\n', '\n')
    end
end

function _term_header(io::IO, md, char, columns)
    text = terminline_string(io, md.text)
    with_output_color(:bold, io) do io
        print(io, ' '^margin)
        line_no, lastline_width = print_wrapped(io, text,
                                                width=columns - 4margin; pre=" ")
        line_width = min(1 + lastline_width, columns)
        if line_no > 1
            line_width = max(line_width, div(columns, 3))
        end
        char != ' ' && print(io, '\n', ' '^(margin), char^line_width)
    end
end

const _header_underlines = collect("≡=–-⋅ ")
# TODO settle on another option with unicode e.g. "≡=≃–∼⋅" ?

function term(io::IO, md::Header{l}, columns) where l
    underline = _header_underlines[l]
    _term_header(io, md, underline, columns)
end

function term(io::IO, md::Code, columns)
    with_output_color(:cyan, io) do io
        L = lines(md.code)
        for i in eachindex(L)
            print(io, ' '^margin, L[i])
            i < lastindex(L) && println(io)
        end
    end
end

function term(io::IO, tex::LaTeX, columns)
    printstyled(io, ' '^margin, tex.formula, color=:magenta)
end

term(io::IO, br::LineBreak, columns) = nothing # line breaks already printed between subsequent elements

function term(io::IO, br::HorizontalRule, columns)
   print(io, ' '^margin, '─'^(columns - 2margin))
end

term(io::IO, x, _) = show(io, MIME"text/plain"(), x)

# Inline Content

terminline_string(io::IO, md) = sprint(terminline, md; context=io)

terminline(io::IO, content...) = terminline(io, collect(content))

function terminline(io::IO, content::Vector)
    for md in content
        terminline(io, md)
    end
end

function terminline(io::IO, md::AbstractString)
    print(io, replace(md, r"[\s\t\n]+" => ' '))
end

function terminline(io::IO, md::Bold)
    with_output_color(terminline, :bold, io, md.text)
end

function terminline(io::IO, md::Italic)
    with_output_color(terminline, :underline, io, md.text)
end

function terminline(io::IO, md::LineBreak)
    println(io)
end

function terminline(io::IO, md::Image)
    terminline(io, "(Image: $(md.alt))")
end

terminline(io::IO, f::Footnote) = with_output_color(terminline, :bold, io, "[^$(f.id)]")

function terminline(io::IO, md::Link)
    url = !Base.startswith(md.url, "@ref") ? " ($(md.url))" : ""
    text = terminline_string(io, md.text)
    terminline(io, text, url)
end

function terminline(io::IO, code::Code)
    printstyled(io, code.code, color=:cyan)
end

function terminline(io::IO, tex::LaTeX)
    printstyled(io, tex.formula, color=:magenta)
end

terminline(io::IO, x) = show(io, MIME"text/plain"(), x)

# Show in terminal
Base.show(io::IO, ::MIME"text/plain", md::MD) = (term(io, md); nothing)
