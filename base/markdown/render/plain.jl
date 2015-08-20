# This file is a part of Julia. License is MIT: http://julialang.org/license

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

function plain{l}(io::IO, header::Header{l})
    print(io, "#"^l*" ")
    plaininline(io, header.text)
    println(io)
end

function plain(io::IO, code::Code)
    println(io, "```", code.language)
    println(io, code.code)
    println(io, "```")
end

function plain(io::IO, p::Paragraph)
    plaininline(io, p.content)
    println(io)
end

function plain(io::IO, list::List)
    for (i, item) in enumerate(list.items)
        print(io, list.ordered ? "$i. " : "  * ")
        plaininline(io, item)
        println(io)
    end
end

function plain(io::IO, q::BlockQuote)
    s = sprint(buf -> plain(buf, q.content))
    for line in split(rstrip(s), "\n")
        println(io, isempty(line) ? ">" : "> ", line)
    end
    println(io)
end

function plain(io::IO, md::HorizontalRule)
    println(io, "–" ^ 3)
end

plain(io::IO, md) = writemime(io, "text/plain", md)

# Inline elements

plaininline(x) = sprint(plaininline, x)

function plaininline(io::IO, md...)
    for el in md
        plaininline(io, el)
    end
end

plaininline(io::IO, md::Vector) = !isempty(md) && plaininline(io, md...)

plaininline(io::IO, link::Link) = plaininline(io, "[", link.text, "](", link.url, ")")

plaininline(io::IO, md::Image) = plaininline(io, "![", md.alt, "](", md.url, ")")

plaininline(io::IO, s::AbstractString) = print(io, s)

plaininline(io::IO, md::Bold) = plaininline(io, "**", md.text, "**")

plaininline(io::IO, md::Italic) = plaininline(io, "*", md.text, "*")

plaininline(io::IO, md::Code) = print(io, "`", md.code, "`")

plaininline(io::IO, br::LineBreak) = println(io)

plaininline(io::IO, x) = writemime(io, MIME"text/plain"(), x)

# writemime

Base.writemime(io::IO, ::MIME"text/plain", md::MD) = plain(io, md)
Base.writemime(io::IO, ::MIME"text/markdown", md::MD) = plain(io, md)
