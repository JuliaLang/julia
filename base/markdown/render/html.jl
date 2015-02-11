include("rich.jl")

# Utils

function withtag(f, io::IO, tag, attrs...)
    print(io, "<$tag")
    for (attr, value) in attrs
        print(io, " $attr=\"$value\"")
    end
    f == nothing && return print(io, " />")

    print(io, ">")
    f()
    print(io, "</$tag>")
end

tag(io::IO, tag, attrs...) = withtag(nothing, io, tag, attrs...)

# Block elements

function html(io::IO, content::Vector)
    for md in content
        html(io, md)
        println(io)
    end
end

html(io::IO, md::MD) = html(io, md.content)

function html{l}(io::IO, header::Header{l})
    withtag(io, "h$l") do
        htmlinline(io, header.text)
    end
end

function html(io::IO, code::Code)
    withtag(io, :pre) do
        withtag(io, :code) do
            print(io, code.code)
        end
    end
end

function html(io::IO, md::Paragraph)
    withtag(io, :p) do
        htmlinline(io, md.content)
    end
end

function html(io::IO, md::BlockQuote)
    withtag(io, :blockquote) do
        html(io, block.content)
    end
end

function html(io::IO, md::List)
    withtag(io, :ul) do
        for item in md.items
            withtag(io, :li) do
                htmlinline(io, item)
                println(io)
            end
        end
    end
end

html(io::IO, x) = tohtml(io, x)

# Inline elements

function htmlinline(io::IO, content::Vector)
    for x in content
        htmlinline(io, x)
    end
end

function htmlinline(io::IO, code::Code)
    withtag(io, :code) do
        print(io, code.code)
    end
end

function htmlinline(io::IO, md::String)
    print(io, md)
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

function writemime(io::IO, ::MIME"text/html", md::MD)
    withtag(io, :div, :class=>"markdown") do
        html(io, md)
    end
end
