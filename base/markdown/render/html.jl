# This file is a part of Julia. License is MIT: http://julialang.org/license

include("rich.jl")

# Utils

function withtag(f, io::IO, tag, attrs...)
    print(io, "<$tag")
    for (attr, value) in attrs
        print(io, " ")
        htmlesc(io, attr)
        print(io, "=\"")
        htmlesc(io, value)
        print(io, "\"")
    end
    f == nothing && return print(io, " />")

    print(io, ">")
    f()
    print(io, "</$tag>")
end

tag(io::IO, tag, attrs...) = withtag(nothing, io, tag, attrs...)

const _htmlescape_chars = Dict('<'=>"&lt;",   '>'=>"&gt;",
                               '"'=>"&quot;", '&'=>"&amp;",
                               # ' '=>"&nbsp;",
                               )
for ch in "'`!@\$\%()=+{}[]"
    _htmlescape_chars[ch] = "&#$(Int(ch));"
end

function htmlesc(io::IO, s::String)
    # s1 = replace(s, r"&(?!(\w+|\#\d+);)", "&amp;")
    for ch in s
        print(io, get(_htmlescape_chars, ch, ch))
    end
end
function htmlesc(io::IO, s::Symbol)
    htmlesc(io, string(s))
end
function htmlesc(io::IO, xs::Union{String, Symbol}...)
    for s in xs
        htmlesc(io, s)
    end
end
function htmlesc(s::Union{String, Symbol})
    sprint(htmlesc, s)
end

# Block elements

html(io::IO, md::MD) = html(io, md.content)

html(io::IO, x) = tohtml(io, x)

# Inline elements

function htmlinline(io::IO, md::Union{Symbol, String})
    htmlesc(io, md)
end

htmlinline(io::IO, x) = tohtml(io, x)

# API

export html

html(md) = sprint(html, md)

function writemime(io::IO, ::MIME"text/html", md::MD)
    withtag(io, :div, :class=>"markdown") do
        html(io, md)
    end
end
