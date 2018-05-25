# This file is a part of Julia. License is MIT: https://julialang.org/license

plain(x) = sprint(plain, x)

function plain(io::IO, content::Vector)
    isempty(content) && return
    for md in content[1:end-1]
        plain(io, md)
        println(io)
    end
    plain(io, content[end])
end

plain(io::IO, md::MD) = plain(io, md.content)

function plain(io::IO, header::Header{l}) where l
    print(io, "#"^l*" ")
    plaininline(io, header.text)
    println(io)
end

function plain(io::IO, code::Code)
    # If the code includes a fenced block this will break parsing,
    # so it must be enclosed by a longer ````-sequence.
    n = mapreduce(m -> length(m.match), max, 2, eachmatch(r"^`+"m, code.code)) + 1
    println(io, "`" ^ n, code.language)
    println(io, code.code)
    println(io, "`" ^ n)
end

function plain(io::IO, p::Paragraph)
    plaininline(io, p.content)
    println(io)
end

function plain(io::IO, list::List)
    for (i, item) in enumerate(list.items)
        print(io, isordered(list) ? "$(i + list.ordered - 1). " : "  * ")
        lines = split(rstrip(sprint(plain, item)), "\n")
        for (n, line) in enumerate(lines)
            print(io, (n == 1 || isempty(line)) ? "" : "    ", line)
            n < length(lines) && println(io)
        end
        println(io)
    end
end

function plain(io::IO, q::BlockQuote)
    s = sprint(plain, q.content)
    for line in split(rstrip(s), "\n")
        println(io, isempty(line) ? ">" : "> ", line)
    end
    println(io)
end

function plain(io::IO, f::Footnote)
    print(io, "[^", f.id, "]:")
    s = sprint(plain, f.text)
    lines = split(rstrip(s), "\n")
    # Single line footnotes are printed on the same line as their label
    # rather than taking up an additional line.
    if length(lines) == 1
        println(io, " ", lines[1])
    else
        println(io)
        for line in lines
            println(io, isempty(line) ? "" : "    ", line)
        end
        println(io)
    end
end

function plain(io::IO, md::Admonition)
    s = sprint(plain, md.content)
    title = md.title == uppercasefirst(md.category) ? "" : " \"$(md.title)\""
    println(io, "!!! ", md.category, title)
    for line in split(rstrip(s), "\n")
        println(io, isempty(line) ? "" : "    ", line)
    end
    println(io)
end

function plain(io::IO, md::HorizontalRule)
    println(io, "-" ^ 3)
end

function plain(io::IO, l::LaTeX)
    println(io, '$', '$')
    println(io, l.formula)
    println(io, '$', '$')
end

function plain(io::IO, md)
    show(io,  MIME"text/plain"(), md)
    println(io)
end

# Inline elements

plaininline(x) = sprint(plaininline, x)

function plaininline(io::IO, md...)
    for el in md
        plaininline(io, el)
    end
end

plaininline(io::IO, md::Vector) = !isempty(md) && plaininline(io, md...)

plaininline(io::IO, f::Footnote) = print(io, "[^", f.id, "]")

plaininline(io::IO, link::Link) = plaininline(io, "[", link.text, "](", link.url, ")")

plaininline(io::IO, md::Image) = plaininline(io, "![", md.alt, "](", md.url, ")")

plaininline(io::IO, s::AbstractString) = print(io, s)

plaininline(io::IO, md::Bold) = plaininline(io, "**", md.text, "**")

plaininline(io::IO, md::Italic) = plaininline(io, "*", md.text, "*")

function plaininline(io::IO, md::Code)
    if occursin("`", md.code)
        n = maximum(length(m.match) for m in eachmatch(r"(`+)", md.code))
        s = "`"^((iseven(n) ? 1 : 2) + n)
        print(io, s, Base.startswith(md.code, "`") ? " " : "")
        print(io, md.code, endswith(md.code, "`") ? " " : "", s)
    else
        print(io, "`", md.code, "`")
    end
end

plaininline(io::IO, br::LineBreak) = println(io)

plaininline(io::IO, x) = show(io, MIME"text/plain"(), x)

# show

Base.show(io::IO, md::MD) = plain(io, md)
Base.show(io::IO, ::MIME"text/markdown", md::MD) = plain(io, md)
