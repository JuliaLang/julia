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
        print(io, list.ordered ? "$i. " : "* ")
        rstinline(io, item)
        println(io)
    end
end

function rst(io::IO, q::BlockQuote)
    s = sprint(buf -> rst(buf, q.content))
    for line in split(rstrip(s), "\n")
        println(io, "    ", line)
    end
    println(io)
end

function rst(io::IO, md::Admonition)
    s = sprint(buf -> rst(buf, md.content))
    title = md.title == ucfirst(md.category) ? "" : md.title
    println(io, ".. ", md.category, "::", isempty(title) ? "" : " $title")
    for line in split(rstrip(s), "\n")
        println(io, isempty(line) ? "" : "   ", line)
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

rst(io::IO, md) = show(io, "text/rst", md)

# Inline elements

rstinline(x) = sprint(rstinline, x)

function rstinline(io::IO, md...)
    wasCode = false
    for el in md
        wasCode && isa(el, AbstractString) && !Base.startswith(el, " ") && print(io, "\\ ")
        wasCode = (isa(el, Code) || isa(el, LaTeX) || isa(el, Link)) && (wasCode = true)
        rstinline(io, el)
    end
end

rstinline(io::IO, md::Vector) = !isempty(md) && rstinline(io, md...)

# rstinline(io::IO, md::Image) = rstinline(io, ".. image:: ", md.url)

function rstinline(io::IO, md::Link)
    if ismatch(r":(func|obj|ref|exc|class|const|data):`\.*", md.url)
        rstinline(io, md.url)
    else
        rstinline(io, "`", md.text, " <", md.url, ">`_")
    end
end

function rstinline(io::IO, md::Footnote)
    if md.text ≡ nothing
        print(io, "[", md.id, "]_")
    else
        print(io, ".. [", md.id, "]")
        rstinline(io, md.text)
    end
end

rstescape(s) = replace(s, "\\", "\\\\")

rstinline(io::IO, s::AbstractString) = print(io, rstescape(s))

rstinline(io::IO, md::Bold) = rstinline(io, "**", md.text, "**")

rstinline(io::IO, md::Italic) = rstinline(io, "*", md.text, "*")

rstinline(io::IO, md::Code) = print(io, "``", md.code, "``")

rstinline(io::IO, br::LineBreak) = println(io)

rstinline(io::IO, l::LaTeX) = print(io, ":math:`", l.formula, "`")

rstinline(io::IO, x) = show(io, MIME"text/rst"(), x)

# show

Base.show(io::IO, ::MIME"text/rst", md::MD) = rst(io, md)
