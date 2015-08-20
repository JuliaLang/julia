# This file is a part of Julia. License is MIT: http://julialang.org/license

export latex

function wrapblock(f, io, env)
    println(io, "\\begin{", env, "}")
    f()
    println(io, "\\end{", env, "}")
end

function wrapinline(f, io, cmd)
    print(io, "\\", cmd, "{")
    f()
    print(io, "}")
end

# Block elements

latex(io::IO, md::MD) = latex(io, md.content)

function latex(io::IO, content::Vector)
    for c in content
        latex(io, c)
    end
end

function latex{l}(io::IO, header::Header{l})
    tag = l < 4 ? "sub"^(l-1) * "section" : "sub"^(l-4) * "paragraph"
    wrapinline(io, tag) do
        latexinline(io, header.text)
    end
    println(io)
end

function latex(io::IO, code::Code)
    wrapblock(io, "verbatim") do
        # TODO latex escape
        println(io, code.code)
    end
end

function latexinline(io::IO, code::Code)
    wrapinline(io, "texttt") do
        print(io, code.code)
    end
end

function latex(io::IO, md::Paragraph)
    for md in md.content
        latexinline(io, md)
    end
    println(io)
end

function latex(io::IO, md::BlockQuote)
    wrapblock(io, "quote") do
        latex(io, md.content)
    end
end

function latex(io::IO, md::List)
    env = md.ordered ? "enumerate" : "itemize"
    wrapblock(io, env) do
        for item in md.items
            print(io, "\\item ")
            latexinline(io, item)
            println(io)
        end
    end
end

function writemime(io::IO, ::MIME"text/latex", md::HorizontalRule)
    println(io, "\\rule{\\textwidth}{1pt}")
end

# Inline elements

function latexinline(io::IO, md::Vector)
    for c in md
        latexinline(io, c)
    end
end

function latexinline(io::IO, md::AbstractString)
    latexesc(io, md)
end

function latexinline(io::IO, md::Bold)
    wrapinline(io, "textbf") do
        latexinline(io, md.text)
    end
end

function latexinline(io::IO, md::Italic)
    wrapinline(io, "emph") do
        latexinline(io, md.text)
    end
end

function latexinline(io::IO, md::Image)
    wrapblock(io, "figure") do
        println(io, "\\centering")
        wrapinline(io, "includegraphics") do
            print(io, md.url)
        end
        println(io)
        wrapinline(io, "caption") do
            latexinline(io, md.alt)
        end
        println(io)
    end
end

function latexinline(io::IO, md::Link)
    wrapinline(io, "href") do
        print(io, md.url)
    end
    print(io, "{")
    latexinline(io, md.text)
    print(io, "}")
end

const _latexescape_chars = Dict{Char, AbstractString}(
   '~'=>"{\\sim}", '^'=>"\\^{}", '\\'=>"{\\textbackslash}")
for ch in "&%\$#_{}"
    _latexescape_chars[ch] = "\\$ch"
end

function latexesc(io, s::AbstractString)
    for ch in s
        print(io, get(_latexescape_chars, ch, ch))
    end
end

latex(md) = sprint(latex, md)
latexinline(md) = sprint(latexinline, md)
latexesc(s) = sprint(latexesc, s)

writemime(io::IO, ::MIME"text/latex", md::MD) = latex(io, md)
#writemime(io::IO, ::MIME"text/latex", md::MD) = writemime(io, "text/plain", md)
