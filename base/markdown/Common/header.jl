# This file is a part of Julia. License is MIT: http://julialang.org/license

type Header{level}
    text
end

Header(s, level::Int) = Header{level}(s)
Header(s) = Header(s, 1)

@breaking true ->
function hashheader(stream::IO, md::MD)
    withstream(stream) do
        eatindent(stream) || return false
        level = 0
        while startswith(stream, '#') level += 1 end
        level < 1 || level > 6 && return false

        c = ' '
        # Allow empty headers, but require a space
        !eof(stream) && (c = read(stream, Char); !(c in " \n")) &&
            return false

        if c != '\n' # Empty header
            h = readline(stream) |> strip
            h = match(r"(.*?)( +#+)?$", h).captures[1]
            buffer = IOBuffer()
            print(buffer, h)
            push!(md.content, Header(parseinline(seek(buffer, 0), md), level))
        else
            push!(md.content, Header("", level))
        end
        return true
    end
end

function setextheader(stream::IO, md::MD)
    withstream(stream) do
        eatindent(stream) || return false
        header = readline(stream) |> strip
        header == "" && return false

        eatindent(stream) || return false
        underline = readline(stream) |> strip
        length(underline) < 3 && return false
        u = underline[1]
        u in "-=" || return false
        all(c -> c == u, underline) || return false
        level = (u == '=') ? 1 : 2

        push!(md.content, Header(parseinline(header, md), level))
        return true
    end
end

function html{l}(io::IO, header::Header{l})
    withtag(io, "h$l") do
        htmlinline(io, header.text)
    end
end

function latex{l}(io::IO, header::Header{l})
    tag = l < 4 ? "sub"^(l-1) * "section" : "sub"^(l-4) * "paragraph"
    wrapinline(io, tag) do
        latexinline(io, header.text)
    end
    println(io)
end

function plain{l}(io::IO, header::Header{l})
    print(io, "#"^l*" ")
    plaininline(io, header.text)
    println(io)
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
