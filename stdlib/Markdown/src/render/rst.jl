# This file is a part of Julia. License is MIT: https://julialang.org/license

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

function rst(io::IO, header::Header{l}) where l
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
        bullet = isordered(list) ? "$(i + list.ordered - 1). " : "* "
        print(io, bullet)
        lines = split(rstrip(sprint(rst, item)), '\n')
        for (n, line) in enumerate(lines)
            print(io, (n == 1 || isempty(line)) ? "" : " "^length(bullet), line)
            n < length(lines) && println(io)
        end
        println(io)
    end
end

function rst(io::IO, q::BlockQuote)
    s = sprint(rst, q.content)
    for line in split(rstrip(s), "\n")
        println(io, "    ", line)
    end
    println(io)
end

function rst(io::IO, f::Footnote)
    print(io, ".. [", f.id, "]")
    s = sprint(rst, f.text)
    lines = split(rstrip(s), "\n")
    # Single line footnotes are printed on the same line as their label
    # rather than taking up an additional line.
    if length(lines) == 1
        println(io, " ", lines[1])
    else
        println(io)
        for line in lines
            println(io, isempty(line) ? "" : "   ", rstrip(line))
        end
        println(io)
    end
end

function rst(io::IO, md::Admonition)
    s = sprint(rst, md.content)
    title = md.title == uppercasefirst(md.category) ? "" : md.title
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
    for line in lines(l.formula)
        println(io, "    ", line)
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
    if occursin(r":(func|obj|ref|exc|class|const|data):`\.*", md.url)
        rstinline(io, md.url)
    else
        rstinline(io, "`", md.text, " <", md.url, ">`_")
    end
end

rstinline(io::IO, f::Footnote) = print(io, "[", f.id, "]_")

rstescape(s) = replace(s, "\\" => "\\\\")

rstinline(io::IO, s::AbstractString) = print(io, rstescape(s))

rstinline(io::IO, md::Bold) = rstinline(io, "**", md.text, "**")

rstinline(io::IO, md::Italic) = rstinline(io, "*", md.text, "*")

rstinline(io::IO, md::Code) = print(io, "``", md.code, "``")

rstinline(io::IO, br::LineBreak) = println(io)

rstinline(io::IO, l::LaTeX) = print(io, ":math:`", l.formula, "`")

rstinline(io::IO, x) = show(io, MIME"text/rst"(), x)

# show

Base.show(io::IO, ::MIME"text/rst", md::MD) = rst(io, md)
