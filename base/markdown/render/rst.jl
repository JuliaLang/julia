# This file is a part of Julia. License is MIT: http://julialang.org/license

rst(x) = sprint(rst, x)

function rst(io::IO, content::Vector)
    isempty(content) && return
    for md in content[1:end-1]
        rst(io, md)
        println(io)
    end
    rst(io, content[end])
end

rst(io::IO, md::MD) = rst(io, md.content)

function rst{l}(io::IO, header::Header{l})
    s = rstinline(header.text)
    println(io, s)
    println(io, string("*=-~:.^"[l])^length(s))
    println(io)
end

function rst(io::IO, code::Code)
    if code.language == "jldoctest"
        println(io, ".. doctest::\n")
    elseif code.language != "rst"
        println(io, ".. code-block:: julia\n")
    end
    for l in lines(code.code)
        println(io, "    ", l)
    end
end

function rst(io::IO, p::Paragraph)
    rstinline(io, p.content)
    println(io)
end

function rst(io::IO, list::List)
    for (i, item) in enumerate(list.items)
        print(io, list.ordered ? "$i. " : "  * ")
        rstinline(io, item)
        println(io)
    end
end

function rst(io::IO, q::BlockQuote)
    s = sprint(buf -> plain(buf, q.content))
    for line in split(rstrip(s), "\n")
        println(io, "    ", line)
    end
    println(io)
end

function rst(io::IO, md::HorizontalRule)
    println(io, "–" ^ 5)
end

function rst(io::IO, l::LaTeX)
    println(io, ".. math::\n")
    for l in lines(l.formula)
        println(io, "    ", l)
    end
end

rst(io::IO, md) = writemime(io, "text/rst", md)

# Inline elements

rstinline(x) = sprint(rstinline, x)

function rstinline(io::IO, md...)
    wasCode = false
    for el in md
        wasCode && isa(el, AbstractString) && !Base.startswith(el, " ") && print(io, "\\ ")
        wasCode = (isa(el, Code) || isa(el, LaTeX)) && (wasCode = true)
        rstinline(io, el)
    end
end

rstinline(io::IO, md::Vector) = !isempty(md) && rstinline(io, md...)

# rstinline(io::IO, md::Image) = rstinline(io, ".. image:: ", md.url)

rstinline(io::IO, md::Link) = rstinline(io, "`", md.text, " <", md.url, ">`_")

rstescape(s) = replace(s, "\\", "\\\\")

rstinline(io::IO, s::AbstractString) = print(io, rstescape(s))

rstinline(io::IO, md::Bold) = rstinline(io, "**", md.text, "**")

rstinline(io::IO, md::Italic) = rstinline(io, "*", md.text, "*")

rstinline(io::IO, md::Code) = print(io, "``", md.code, "``")

rstinline(io::IO, br::LineBreak) = println(io)

rstinline(io::IO, l::LaTeX) = print(io, ":math:`", l.formula, "`")

rstinline(io::IO, x) = writemime(io, MIME"text/rst"(), x)

# writemime

Base.writemime(io::IO, ::MIME"text/rst", md::MD) = rst(io, md)
