# This file is a part of Julia. License is MIT: https://julialang.org/license

include("rich.jl")

# Utils

function withtag(f, io::IO, tag, attrs...)
    print(io, "<$tag")
    for (attr, value) in attrs
        print(io, " ")
        htmlesc(io, attr)
        print(io, "=\"")
        htmlesc(io, value)
        print(io, "\"")
    end
    f === nothing && return print(io, " />")

    print(io, ">")
    f()
    print(io, "</$tag>")
end

tag(io::IO, tag, attrs...) = withtag(nothing, io, tag, attrs...)

const _htmlescape_chars = Dict('<'=>"&lt;",   '>'=>"&gt;",
                               '"'=>"&quot;", '&'=>"&amp;",
                               # ' '=>"&nbsp;",
                               )
for ch in "'`!\$\%()=+{}[]"
    _htmlescape_chars[ch] = "&#$(Int(ch));"
end

function htmlesc(io::IO, s::AbstractString)
    # s1 = replace(s, r"&(?!(\w+|\#\d+);)", "&amp;")
    for ch in s
        print(io, get(_htmlescape_chars, ch, ch))
    end
end
function htmlesc(io::IO, s::Symbol)
    htmlesc(io, string(s))
end
function htmlesc(io::IO, xs::Union{AbstractString,Symbol}...)
    for s in xs
        htmlesc(io, s)
    end
end
function htmlesc(s::Union{AbstractString,Symbol})
    sprint(htmlesc, s)
end

# Block elements

function html(io::IO, content::Vector, parent = nothing)
    for md in content
        html(io, md, parent)
        println(io)
    end
end

html(io::IO, md::MD, parent = nothing) = html(io, md.content, md)

function html{l}(io::IO, header::Header{l}, parent = nothing)
    withtag(io, "h$l") do
        htmlinline(io, header.text)
    end
end

function html(io::IO, code::Code, parent = nothing)
    withtag(io, :pre) do
        maybe_lang = !isempty(code.language) ? Any[:class=>"language-$(code.language)"] : []
        withtag(io, :code, maybe_lang...) do
            htmlesc(io, code.code)
            # TODO should print newline if this is longer than one line ?
        end
    end
end

function html(io::IO, md::Paragraph, parent = nothing)
    if isa(parent, List) && !parent.isloose
        htmlinline(io, md.content)
    else
        withtag(io, :p) do
            htmlinline(io, md.content)
        end
    end
end

function html(io::IO, md::BlockQuote, parent = nothing)
    withtag(io, :blockquote) do
        println(io)
        html(io, md.content, md)
    end
end

function html(io::IO, f::Footnote, parent = nothing)
    withtag(io, :div, :class => "footnote", :id => "footnote-$(f.id)") do
        withtag(io, :p, :class => "footnote-title") do
            print(io, f.id)
        end
        html(io, f.text, f)
    end
end

function html(io::IO, md::Admonition, parent = nothing)
    withtag(io, :div, :class => "admonition $(md.category)") do
        withtag(io, :p, :class => "admonition-title") do
            print(io, md.title)
        end
        html(io, md.content, md)
    end
end

function html(io::IO, md::List, parent = nothing)
    maybe_attr = md.ordered > 1 ? Any[:start => string(md.ordered)] : []
    withtag(io, isordered(md) ? :ol : :ul, maybe_attr...) do
        for item in md.items
            println(io)
            withtag(io, :li) do
                html(io, item, md)
            end
        end
        println(io)
    end
end

function html(io::IO, md::HorizontalRule, parent = nothing)
    tag(io, :hr)
end

html(io::IO, x, parent = nothing) = tohtml(io, x)

# Inline elements

function htmlinline(io::IO, content::Vector)
    for x in content
        htmlinline(io, x)
    end
end

function htmlinline(io::IO, code::Code)
    withtag(io, :code) do
        htmlesc(io, code.code)
    end
end

function htmlinline(io::IO, md::Union{Symbol,AbstractString})
    htmlesc(io, md)
end

function htmlinline(io::IO, md::Bold)
    withtag(io, :strong) do
        htmlinline(io, md.text)
    end
end

function htmlinline(io::IO, md::Italic)
    withtag(io, :em) do
        htmlinline(io, md.text)
    end
end

function htmlinline(io::IO, md::Image)
    tag(io, :img, :src=>md.url, :alt=>md.alt)
end


function htmlinline(io::IO, f::Footnote)
    withtag(io, :a, :href => "#footnote-$(f.id)", :class => "footnote") do
        print(io, "[", f.id, "]")
    end
end

function htmlinline(io::IO, link::Link)
    withtag(io, :a, :href=>link.url) do
        htmlinline(io, link.text)
    end
end

function htmlinline(io::IO, br::LineBreak)
    tag(io, :br)
end

htmlinline(io::IO, x) = tohtml(io, x)

# API

export html

html(md) = sprint(html, md)

function show(io::IO, ::MIME"text/html", md::MD)
    withtag(io, :div, :class=>"markdown") do
        html(io, md)
    end
end
